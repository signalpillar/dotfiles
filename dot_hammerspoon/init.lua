-- toggle kitty
hs.hotkey.bind({"alt"}, "space", function()
  local app = hs.application.get("kitty")
  if app:isFrontmost() then
      app:hide()
  else
      hs.application.launchOrFocus(app:name())
  end
end)