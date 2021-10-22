(module plugins.scratch)

(fn first [xs]
  (when xs
    (. xs 1)))

(def state {:bufnr nil})

; Assumes state.bufnr ~= nil
(defn new-scratch-split []
  (vim.cmd "botright vsplit")
  (vim.api.nvim_win_set_buf (vim.api.nvim_get_current_win) state.bufnr)
  (vim.cmd "silent exe 'normal! G'"))

(defn initialize []
  (var bufnr (vim.api.nvim_create_buf false false))
  (vim.api.nvim_buf_set_option bufnr :filetype :fennel)
  (vim.api.nvim_buf_set_option bufnr :buftype :nofile)
  (vim.api.nvim_buf_set_lines bufnr 0 0 true ["(module scratch {autoload {aniseed aniseed.core}"
                                              "                 require-macros [core.macros]})"
                                              ""])
  (tset state :bufnr bufnr)
  (new-scratch-split)
  (vim.cmd :ConjureEvalBuf))

(defn show []
  (if (= state.bufnr nil)
    ; we need to initialize the scratch buffer's properties
    (initialize)

    ; open an existing scratch buffer in a split window
    (let [winid (first (vim.fn.win_findbuf state.bufnr))]
      (if (= winid nil)
        ; open a new scratch split
        (new-scratch-split)

        ; focus the scratch window
        (vim.fn.win_gotoid winid)))))
