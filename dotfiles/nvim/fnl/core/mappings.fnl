(module core.mappings {require-macros [core.macros]})

;; DEFAULT MAP
;; -----------
(let! mapleader " ")
(let! maplocalleader ",")

;; Disable SPC key
;; Note: `which-key` already implemented this
;; (map! [n] "<Space>" :<Nop>)

(noremap! [n] "<TAB>" "<Cmd>bn<CR>")
(noremap! [n] "<S-TAB>" "<Cmd>bp<CR>")

;; Remove search highlights
(noremap! [n] "<F2>" "<Cmd>noh<CR>")
(noremap! [n] "<ESC>" "<Cmd>noh<CR><ESC>")

;; Break lines in normmal mode
(map! [n] "o" "o<ESC>")
(map! [n] "O" "O<ESC>")

;; SEE https://dev.to/snawaz/comment/pa71
;; (noremap! [n] "o" ":<C-u>call append(line(".")   repeat([""] v:count1))<CR>")
;; (noremap! [n] "O" ":<C-u>call append(line(".")-1   repeat([""] v:count1))<CR>")

;; Vim map
(map! [n] "Y" "y$")
(map! [n] "<C-h>" "<C-w>h")
(map! [n] "<C-l>" "<C-w>l")
(map! [n] "<C-j>" "<C-w>j")
(map! [n] "<C-k>" "<C-w>k")
(map! [n] "<A-[>" "<Cmd>vertical resize -5<CR>")
(map! [n] "<A-]>" "<Cmd>vertical resize +5<CR>")
;; Insert
(noremap! [i] "<C-w>" "<C-[>diwa")
(noremap! [i] "<C-h>" "<BS>")
(noremap! [i] "<C-d>" "<Del>")
(noremap! [i] "<C-u>" "<C-G>u<C-U>")
(noremap! [i] "<C-b>" "<Left>")
(noremap! [i] "<C-f>" "<Right>")
(noremap! [i] "<C-a>" "<ESC>^i")
(noremap! [i] "<C-j>" "<Esc>o")
(noremap! [i] "<C-k>" "<Esc>O")
(noremap! [i] "<C-s>" "<Cmd>w<CR>")
(noremap! [i] "<C-q>" "<Cmd>wq<CR>")

;; Command line
(noremap! [c] "<C-b>" "<Left>")
(noremap! [c] "<C-f>" "<Right>")
(noremap! [c] "<C-a>" "<Home>")
(noremap! [c] "<C-e>" "<End>")
(noremap! [c] "<C-d>" "<Del>")
(noremap! [c] "<C-h>" "<BS>")
; (noremap! [c] "<C-t>" [[<C-r>=expand("%:p:h") . "/" <CR>]])

;; Term
(noremap! [t] "<Esc><Esc>" "<C-\\><C-n>")
(noremap! [t] "<C-j>" "<C-\\><C-n><C-w>j")
(noremap! [t] "<C-h>" "<C-\\><C-n><C-w>h")
(noremap! [t] "<C-k>" "<C-\\><C-n><C-w>k")
(noremap! [t] "<C-l>" "<C-\\><C-n><C-w>l")

;; ------------------------
;; Quit
;; ------------------------
(map! [n] "<Leader>qq" ":<C-u>confirm qa<CR>")
(map! [n] "<Leader>qQ" ":<C-u>qa!<CR>")
(map! [n] "<Leader>qs" ":<C-u>wq<CR>")

;; ------------------------
;; FILE & BUFFER NAVIGATION
;; ------------------------

;; Save
(noremap! [n] "<Leader>fs" ":<C-u>w<CR>")
(noremap! [n] "<Leader>bs" ":<C-u>w<CR>")
;; Save all
(noremap! [n] "<Leader>fS" ":<C-u>wa<CR>")
(noremap! [n] "<Leader>bS" ":<C-u>wa<CR>")
;; Close
;; Smart way to close buffers without losing split windows
;; SEE: http://bit.ly/2heyMZ8
(noremap! [n] "<Leader>fd" "<Cmd>bp|bd #<CR>")
(noremap! [n] "<Leader>fc" "<Cmd>bp|bd #<CR>")
(noremap! [n] "<Leader>bd" "<Cmd>bp|bd #<CR>")
(noremap! [n] "<Leader>bc" "<Cmd>bp|bd #<CR>")
;; Create new file under current dir
(noremap! [n] "<Leader>fo" ":<C-u>e <C-r>=expand('%:p:h') . '/'<CR>")
(noremap! [n] "<Leader>bo" ":<C-u>e <C-r>=expand('%:p:h') . '/'<CR>")
;; Rename
(noremap! [n] "<Leader>fr" ":<C-u>Rename ")
(noremap! [n] "<Leader>br" ":<C-u>Rename ")
;; Move
(noremap! [n] "<Leader>fm" ":<C-u>Move ")
(noremap! [n] "<Leader>bm" ":<C-u>Move ")
;; Delete
(noremap! [n] "<Leader>fD" ":<C-u>Delete!<CR>")
(noremap! [n] "<Leader>bD" ":<C-u>Delete!<CR>")

(noremap! [n] "<Leader>bn" "<Cmd>bn<CR>")
(noremap! [n] "<Leader>bp" "<Cmd>bp<CR>")
(noremap! [n] "<Leader>fn" "<Cmd>bn<CR>")
(noremap! [n] "<Leader>fp" "<Cmd>bp<CR>")

;; ------------------------
;; HELP
;; ------------------------
;; Packer

(noremap! [n] "<Leader>hpu" "<Cmd>PackerUpdate<CR>" :nowait)
(noremap! [n] "<Leader>hpi" "<Cmd>PackerInstall<CR>" :nowait)
(noremap! [n] "<Leader>hpc" "<Cmd>PackerCompile<CR>" :nowait)
(noremap! [n] "<Leader>hps" "<Cmd>PackerSync<CR>" :nowait)
(noremap! [n] "<Leader>hpp" "<Cmd>PackerProfile<CR>" :nowait)

;; ------------------------
;; Project
;; ------------------------

;; (noremap! [n] "<Leader>pp" "<Cmd>lua require("session-lens").search_session()<CR>")
;; (noremap! [n] "<Leader>ps" "<Cmd>SaveSessiocope<CR>")
;; (noremap! [n] "<Leader>pd" "<Cmd>DeleteSession<CR>")
;; (noremap! [n] "<Leader>pr" "<Cmd>RestoreSession<CR>")

;; ------------------------
;; WINDOWS NAVIGATION
;; ------------------------
(noremap! [n] "<Leader>wj" "<C-w>j")
(noremap! [n] "<Leader>wk" "<C-w>k")
(noremap! [n] "<Leader>wh" "<C-w>h")
(noremap! [n] "<Leader>wl" "<C-w>l")
(noremap! [n] "<Leader>wJ" "<C-w>J")
(noremap! [n] "<Leader>wK" "<C-w>K")
(noremap! [n] "<Leader>wH" "<C-w>H")
(noremap! [n] "<Leader>wL" "<C-w>L")
(noremap! [n] "<Leader>wc" "<C-w>c")
(noremap! [n] "<Leader>ww" "<C-w>w")
(noremap! [n] "<Leader>w=" "<C-w>=")
(noremap! [n] "<Leader>ws" "<Cmd>sp<CR>")
(noremap! [n] "<Leader>wv" "<Cmd>vsplit<CR>")

