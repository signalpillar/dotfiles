#!/usr/bin/env bash
# Cursor CLI status line.
#
# It also keeps a token ledger for the session in ~/.cursor/cost/<session>.json
# and prints an estimated cost from ~/.cursor/model-prices.json.
#
# The cost is an estimate. Cursor meters usage on the server, and Auto hides the
# routed model, so run /usage for the billed figure.
set -uo pipefail

payload=$(cat)

prices_file="${HOME}/.cursor/model-prices.json"
ledger_dir="${HOME}/.cursor/cost"

session_id=$(echo "$payload" | jq -r '.session_id // "?"')
model=$(echo "$payload" | jq -r '.model.display_name // "?"')
params=$(echo "$payload" | jq -r '.model.param_summary // empty')
in_tok=$(echo "$payload" | jq -r '.context_window.total_input_tokens // 0')
out_tok=$(echo "$payload" | jq -r '.context_window.total_output_tokens // "n/a"')
pct=$(echo "$payload" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)
win=$(echo "$payload" | jq -r '.context_window.context_window_size // 0')

last_in=$(echo "$payload" | jq -r '.context_window.current_usage.input_tokens // empty')
last_out=$(echo "$payload" | jq -r '.context_window.current_usage.output_tokens // empty')

# Update the ledger.
#
# total_output_tokens is a monotonic session counter. The CLI raises it once per
# generation, and current_usage then holds that generation's input and cache
# split. A rise in the counter therefore marks one unbilled generation.
cost_field=""
safe_id=${session_id//[^A-Za-z0-9._-]/_}
ledger_file="${ledger_dir}/${safe_id}.json"

if [[ -r "$prices_file" && "$safe_id" != "?" ]]; then
  mkdir -p "$ledger_dir"

  if [[ ! -f "$ledger_file" ]]; then
    # Once per session, drop ledgers of sessions you no longer run.
    find "$ledger_dir" -maxdepth 1 -name '*.json' -mtime +30 -delete 2>/dev/null
    echo '{}' >"$ledger_file"
  fi

  new_state=$(echo "$payload" | jq -c \
    --slurpfile prices "$prices_file" \
    --slurpfile state "$ledger_file" '
    ($prices[0]) as $P
    | ($state[0] // {}) as $raw
    | ({last_total_output: 0, cost_usd: 0, approx: false,
        tokens: {input: 0, output: 0, cache_read: 0, cache_write: 0}} + $raw) as $S
    | (.context_window.total_output_tokens) as $tot
    | (.context_window.current_usage // {}) as $u
    | (.model.id // "unknown") as $mid
    | (.model.param_summary // "") as $psum
    | ($mid | sub("^cursor-"; "")) as $base
    | (($base | test("-fast")) or ($psum | test("fast"; "i"))) as $fast
    | ([$P.models | keys[] | . as $k | select($base | startswith($k))] | sort_by(length) | last) as $match
    | ($P.models[$match // $P.fallback]
       // {input: 0, cache_write: 0, cache_read: 0, output: 0, first_party: true, approximate: true}) as $m
    | (if $fast and ($m.fast != null) then $m + $m.fast else $m end) as $r
    | if $tot == null then $S
      else
        (if $tot < $S.last_total_output then 0 else $S.last_total_output end) as $prev
        | ($tot - $prev) as $dout
        | if $dout <= 0 then ($S | .last_total_output = $tot)
          else
            ($u.input_tokens // 0) as $din
            | ($u.cache_read_input_tokens // 0) as $dcr
            | ($u.cache_creation_input_tokens // 0) as $dcw
            | ((($din * $r.input) + ($dcw * $r.cache_write)
                + ($dcr * $r.cache_read) + ($dout * $r.output)) / 1000000) as $model_cost
            | (if ($r.first_party // false) then 0
               else (($din + $dcw + $dcr + $dout) * ($P.cursor_token_rate_per_mtok // 0)) / 1000000
               end) as $token_rate
            | $S
            | .last_total_output = $tot
            | .cost_usd += ($model_cost + $token_rate)
            | .approx = ($S.approx or ($match == null) or ($r.approximate // false))
            | .tokens.input += $din
            | .tokens.cache_read += $dcr
            | .tokens.cache_write += $dcw
            | .tokens.output += $dout
          end
      end
  ' 2>/dev/null)

  if [[ -n "$new_state" ]]; then
    tmp_file="${ledger_file}.tmp.$$"
    if printf '%s\n' "$new_state" >"$tmp_file" 2>/dev/null; then
      mv -f "$tmp_file" "$ledger_file" 2>/dev/null || rm -f "$tmp_file"
    fi

    cost=$(echo "$new_state" | jq -r '.cost_usd // 0')
    approx=$(echo "$new_state" | jq -r 'if .approx then "?" else "" end')
    if (($(echo "$new_state" | jq -r 'if .cost_usd >= 1 then 1 else 0 end'))); then
      cost_field=$(printf '  ~$%.2f%s' "$cost" "$approx")
    else
      cost_field=$(printf '  ~$%.3f%s' "$cost" "$approx")
    fi
  fi
fi

line="sid:${session_id}  $model${params:+ $params}  in:${in_tok} out:${out_tok}  ctx:${pct}%/${win}"
if [[ -n "$last_in" || -n "$last_out" ]]; then
  line="$line  last:${last_in:-?}/${last_out:-?}"
fi
line="${line}${cost_field}"
printf '\033[90m%s\033[0m\n' "$line"
