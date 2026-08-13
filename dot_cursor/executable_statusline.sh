#!/usr/bin/env bash
payload=$(cat)

model=$(echo "$payload" | jq -r '.model.display_name // "?"')
params=$(echo "$payload" | jq -r '.model.param_summary // empty')
in_tok=$(echo "$payload" | jq -r '.context_window.total_input_tokens // 0')
out_tok=$(echo "$payload" | jq -r '.context_window.total_output_tokens // "n/a"')
pct=$(echo "$payload" | jq -r '.context_window.used_percentage // 0' | cut -d. -f1)
win=$(echo "$payload" | jq -r '.context_window.context_window_size // 0')

last_in=$(echo "$payload" | jq -r '.context_window.current_usage.input_tokens // empty')
last_out=$(echo "$payload" | jq -r '.context_window.current_usage.output_tokens // empty')

line="$model${params:+ $params}  in:${in_tok} out:${out_tok}  ctx:${pct}%/${win}"
if [[ -n "$last_in" || -n "$last_out" ]]; then
  line="$line  last:${last_in:-?}/${last_out:-?}"
fi
printf '\033[90m%s\033[0m\n' "$line"
