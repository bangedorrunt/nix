(module plugins.scratch
  {autoload {u core.utils}})

(def- state {:bufnr nil})

; Assumes state.bufnr ~= nil
(defn- new-scratch-split []
  (vim.api.nvim_command "botright vsplit")
  (vim.api.nvim_win_set_buf (vim.api.nvim_get_current_win) state.bufnr)
  (vim.api.nvim_command "silent exe 'normal! G'"))

(defn- initialize []
  (var bufnr (vim.api.nvim_create_buf false false))
  (vim.api.nvim_buf_set_option bufnr :filetype :fennel)
  (vim.api.nvim_buf_set_option bufnr :buftype :nofile)
  (vim.api.nvim_buf_set_lines bufnr 0 0 true ["(module scratch {autoload {aniseed aniseed.core}"
                                              "                 require-macros [core.macros]})"
                                              ""])
  (tset state :bufnr bufnr)
  (new-scratch-split)
  (vim.api.nvim_command :ConjureEvalBuf))

(defn- show []
  (if (= state.bufnr nil)
    ; We need to initialize the scratch buffer's properties
    (initialize)

    ; Open an existing scratch buffer in a split window
    (let [winid (u.first (nvim.fn.win_findbuf state.bufnr))]
      (if (= winid nil)
        ; open a new scratch split
        (new-scratch-split)

        ; focus the scratch window
        (nvim.fn.win_gotoid winid)))))
