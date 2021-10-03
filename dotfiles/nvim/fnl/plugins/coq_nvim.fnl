(module plugins.coq_nvim
  {autoload {nvim aniseed.nvim}
   require-macros [core.macros]})

(vim.cmd "COQnow --shut-up")

;; Use custom mappings
(g coq_settings {:keymap {:recommended false}
                 :clients {:tabnine {:enabled true}}
                 :display {:icons {:mode :none}}})

(noremap [i] "<Tab>" #(if (= (vim.fn.pumvisible) 1) (t "<C-n>") (t "<Tab>")))
(noremap [i] "<S-Tab>" #(if (= (vim.fn.pumvisible) 1) (t "<C-p>") (t "<BS>")))
(noremap [i] "<Esc>" #(if (= (vim.fn.pumvisible) 1) (t "<C-e><Esc>") (t "<Esc>")))
(noremap [i] "<C-c>" #(if (= (vim.fn.pumvisible) 1) (t "<C-e><C-c>") (t "<C-c>")))
(noremap [i] "<BS>" #(if (= (vim.fn.pumvisible) 1) (t "<C-e><BS>") (t "<BS>")))
