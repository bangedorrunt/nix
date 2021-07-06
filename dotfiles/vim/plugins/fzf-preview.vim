" FZF-PREVIEW CONFIG
" ------------------

let g:fzf_preview_command = 'bat --color=always --style=grid {-1}'
let g:fzf_preview_filelist_command = 'rg --files --hidden --follow --no-messages -g \!"* *"'
let g:fzf_preview_lines_command = 'bat --color=always --style=grid --theme=ansi-dark --plain'
