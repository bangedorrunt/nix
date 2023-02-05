(import-macros {: nmap : noremap : g : termcodes} :core.macros)

(fn setup []
  ;; DEFAULT MAP
  ;; -----------
  (g mapleader (termcodes :<Space>))
  (g maplocalleader (termcodes ","))

  ;; Disable SPC key
  (nmap n :<Space> :<Nop>)
  (nmap n :q :<Nop>)

  ;; Default to blackhole registers
  ;; (noremap nx :d "\"_d")
  ;; (noremap n :dd "\"_dd")
  ;; (noremap n :D "\"_D")
  ;; (noremap nx :c "\"_c")
  ;; (noremap n :cc "\"_cc")
  ;; (noremap n :C "\"_C")
  (noremap nx :x "\"_x")
  (noremap n :X "\"_X")

  ;; Theprimeagens greatest remap ever
  (noremap x :<Leader>p "\"_dP")
  (noremap nv :<Leader>d "\"_d")
  (noremap nv :<Leader>y "\"+y")
  (nmap n :<Leader>Y "\"+Y")

  (noremap v :<C-c> "\"+y")
  (noremap v :<C-x> "\"+d")
  (noremap v :<C-v> "\"+p")
  ;; (noremap i :<C-v> :<C-r><C-o>+)

  ;; In favour of moving by displayed line rather than physical line
  ;; except when count is provided which is used when targeting specific
  ;; line with `relativenumber`
  (noremap nv expr :j `(if (not= vim.v.count 0) :j :gj))
  (noremap nv expr :k `(if (not= vim.v.count 0) :k :gk))
  (noremap n :0 :g0)
  (noremap n "$" :g$)

  ;; WARN conflict with mini.animate <01-02-23, bangedorrunt>
  ;; Keep screen at the center when jump
  (noremap n :n :nzzzv)
  (noremap n :N :Nzzzv)
  (noremap n :<C-d> :<C-d>zz)
  (noremap n :<C-u> :<C-u>zz)

  (noremap n :J "mzJ`z")
  (noremap n :Y :yg$)

  ;; Move line up and down
  (noremap v :J ":<C-u>m '>+1<CR>gv=gv")
  (noremap v :K ":<C-u>m '>-2<CR>gv=gv")

  ;; Indent level in visual map
  (noremap v ">" :>gv)
  (noremap v "<" :<gv)

  ;; Escape is hurt
  (noremap i :jj :<Esc>)
  (noremap i :jk :<Esc>)
  (noremap i :fd :<Esc>)

  (noremap n :<Tab> :<Cmd>bn<CR>)
  (noremap n :<S-Tab> :<Cmd>bp<CR>)

  ;; Break lines in normmal mode
  (nmap n :o :o<ESC>)
  (nmap n :O :O<ESC>)

  ;; Vim map
  (nmap n :<C-h> :<C-w>h)
  (nmap n :<C-l> :<C-w>l)
  (nmap n :<C-j> :<C-w>j)
  (nmap n :<C-k> :<C-w>k)
  (nmap n :<C-t> :<Nop>)

  ;; Emacs map
  ;; (noremap i :<C-w> "<C-[>diwa")
  ;; (noremap i :<C-h> :<BS>)
  ;; (noremap i :<C-d> :<Del>)
  ;; (noremap i :<C-u> :<C-G>u<C-U>)
  ;; (noremap i :<C-b> :<Left>)
  ;; (noremap i :<C-f> :<Right>)
  ;; (noremap i :<C-a> :<ESC>^i)
  ;; (noremap i :<C-j> :<Esc>o)
  ;; (noremap i :<C-k> :<Esc>O)
  ;; (noremap i :<C-s> :<Cmd>w<CR>)
  ;; (noremap i :<C-q> :<Cmd>wq<CR>)

  ;; Command line
  (noremap c :<C-b> :<Left>)
  (noremap c :<C-f> :<Right>)
  (noremap c :<C-a> :<Home>)
  (noremap c :<C-e> :<End>)
  (noremap c :<C-d> :<Del>)
  (noremap c :<C-h> :<BS>)

  ;; Term
  (noremap t :<Esc><Esc> "<C-\\><C-n>")
  (noremap t :<C-j> "<C-\\><C-n><C-w>j")
  (noremap t :<C-h> "<C-\\><C-n><C-w>h")
  (noremap t :<C-k> "<C-\\><C-n><C-w>k")
  (noremap t :<C-l> "<C-\\><C-n><C-w>l")

  ;; ------------------------
  ;; Quit
  ;; ------------------------
  (noremap n "quit-with-confirm" :<Leader>qq ":<C-u>confirm qa<CR>")
  (noremap n "quit-all-without-confirm" :<Leader>qQ ":<C-u>qa!<CR>")
  (noremap n "save-and-quit" :<Leader>qs ":<C-u>wq<CR>")

  ;; ------------------------
  ;; FILE & BUFFER NAVIGATION
  ;; ------------------------

  ;; Save
  (noremap n "save-file" :<Leader>fs ":<C-u>w<CR>")
  (noremap n "save-file" :<Leader>bs ":<C-u>w<CR>")
  ;; Save all
  (noremap n "save-all" :<Leader>fS ":<C-u>wa<CR>")
  (noremap n "save-all" :<Leader>bS ":<C-u>wa<CR>")
  ;; Close
  ;; Smart way to close buffers without losing split windows
  ;; SEE: http://bit.ly/2heyMZ8
  (noremap n "delete-buffer" :<Leader>fd "<Cmd>bp|bd #<CR>")
  (noremap n "delete-buffer" :<Leader>fc "<Cmd>bp|bd #<CR>")
  (noremap n "delete-buffer" :<Leader>bd "<Cmd>bp|bd #<CR>")
  (noremap n "delete-buffer" :<Leader>bc "<Cmd>bp|bd #<CR>")
  ;; Create new file under current dir
  (noremap n "new-file-cwd" :<Leader>fo ":<C-u>e <C-r>=expand('%:p:h') . '/'<CR>")
  (noremap n "new-file-cwd" :<Leader>bo ":<C-u>e <C-r>=expand('%:p:h') . '/'<CR>")
  ;; Rename
  (noremap n "rename-file" :<Leader>fr ":<C-u>Rename ")
  (noremap n "rename-file" :<Leader>br ":<C-u>Rename ")
  ;; Move
  (noremap n "move-file-to" :<Leader>fm ":<C-u>Move ")
  (noremap n "move-file-to" :<Leader>bm ":<C-u>Move ")
  ;; Delete
  (noremap n "delete-file" :<Leader>fD ":<C-u>Delete!<CR>")
  (noremap n "delete-file" :<Leader>bD ":<C-u>Delete!<CR>")

  (noremap n "buffer-next" :<Leader>bn :<Cmd>bn<CR>)
  (noremap n "buffer-prev" :<Leader>bp :<Cmd>bp<CR>)
  (noremap n "buffer-next" :<Leader>fn :<Cmd>bn<CR>)
  (noremap n "buffer-prev" :<Leader>fp :<Cmd>bp<CR>)

  ;; ------------------------
  ;; HELP
  ;; ------------------------
  ;; Lazy.nvim

  (noremap n "lazy-update" :<Leader>hlu "<Cmd>Lazy update<CR>")
  (noremap n "lazy-install" :<Leader>hli "<Cmd>Lazy install<CR>")
  (noremap n "lazy-sync" :<Leader>hls "<Cmd>Lazy sync<CR>")

  ;; ------------------------
  ;; WINDOWS NAVIGATION
  ;; ------------------------
  (noremap n "jump-to-bottom" :<Leader>wj :<C-w>j)
  (noremap n "jump-to-top" :<Leader>wk :<C-w>k)
  (noremap n "jump-to-left" :<Leader>wh :<C-w>h)
  (noremap n "jump-to-right" :<Leader>wl :<C-w>l)
  (noremap n "move-to-bottom" :<Leader>wJ :<C-w>J)
  (noremap n "move-to-top" :<Leader>wK :<C-w>K)
  (noremap n "move-to-left" :<Leader>wH :<C-w>H)
  (noremap n "move-to-right" :<Leader>wL :<C-w>L)
  (noremap n "close-window" :<Leader>wc :<C-w>c)
  (noremap n "next-window" :<Leader>ww :<C-w>w)
  (noremap n "balance-window" :<Leader>w= :<C-w>=)
  (noremap n "split" :<Leader>ws :<Cmd>sp<CR>)
  (noremap n "split-vertical" :<Leader>wv :<Cmd>vsplit<CR>))

{: setup}
