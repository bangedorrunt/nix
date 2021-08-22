(module plugins.coq_nvim)

(import-macros {: t : let! : nmap! : noremap!} :core.macros)

(vim.cmd "COQnow --shut-up")
;; (let! g/coq_settings {:auto_start "shut-up"})

;; Use custom mappings
(let! g/coq_settings {:keymap {:recommended false}})

(noremap! [is :expr :silent] "<Tab>" #(if (= (vim.fn.pumvisible) 1) (t "<C-n>") (t "<Tab>")))
(noremap! [is :expr :silent] "<S-Tab>" #(if (= (vim.fn.pumvisible) 1) (t "<C-p>") (t "<BS>")))
(noremap! [is :expr :silent] "<Esc>" #(if (= (vim.fn.pumvisible) 1) (t "<C-e>") (t "<Esc>")))
(noremap! [is :expr :silent] "<C-c>" #(if (= (vim.fn.pumvisible) 1) (t "<C-e><C-c>") (t "<C-c>")))
(noremap! [is :expr :silent] "<BS>" #(if (= (vim.fn.pumvisible) 1) (t "<C-e><BS>") (t "<BS>")))
