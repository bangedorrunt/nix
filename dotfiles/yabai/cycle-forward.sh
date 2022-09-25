#!/usr/bin/env sh

## Forward
# SEE: https://github.com/koekeishiya/yabai/issues/203#issuecomment-700527407
yabai -m query --spaces --space |
  jq -re ".index" |
  xargs -I{} yabai -m query --windows --space {} |
  jq -sre 'add | map(select(."is-minimized"==false)) | sort_by(.display, .frame.y, .frame.x, .id) | . as $array | length as $array_length | index(map(select(."has-focus"==true))) as $has_index | if $has_index > 0 then nth($has_index - 1).id else nth($array_length - 1).id end' |
  xargs -I{} yabai -m window --focus {}

# v4+
# SEE: https://github.com/koekeishiya/yabai/issues/203#issuecomment-1088641580
# if [ "$(yabai -m query --spaces --space | jq -r '.type')" = "stack" ]; then (yabai -m window --focus stack.next || yabai -m window --focus stack.first); else yabai -m window --focus next || yabai -m window --focus first; fi
