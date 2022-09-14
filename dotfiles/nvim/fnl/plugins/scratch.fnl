(import-macros {: noremap} :core.macros)

(local state {:bufnr nil})

;; Assumes state.bufnr ~= nil
(fn new-scratch-split []
  (vim.cmd "botright vsplit")
  (vim.api.nvim_win_set_buf (vim.api.nvim_get_current_win) state.bufnr)
  (vim.cmd "silent exe 'normal! G'"))

(fn initialize []
  (var bufnr (vim.api.nvim_create_buf false false))
  (vim.api.nvim_buf_set_name bufnr :*scratch*)
  (vim.api.nvim_buf_set_option bufnr :filetype :fennel)
  (vim.api.nvim_buf_set_option bufnr :buftype :nofile)
  (vim.api.nvim_buf_set_lines bufnr 0 0 true
                              ["(import-macros {: if-let : when-let} :core.macros)"
                               ""
                               "(local fun (require :luafun.fun))"
                               ""
                               ""])
  (tset state :bufnr bufnr)
  (new-scratch-split)
  (vim.cmd.ConjureEvalBuf))

(fn show []
  (if (= state.bufnr nil)
      ;; We need to initialize the scratch buffer's properties
      (initialize)
      ;; Open an existing scratch buffer in a split window
      (let [winid (. (vim.fn.win_findbuf state.bufnr) 1)]
        (if (= winid nil)
            ;; open a new scratch split
            (new-scratch-split)
            ;; focus the scratch window
            (vim.fn.win_gotoid winid)))))

;; Open scratch
(noremap n nowait silent :<Leader>ts show)
