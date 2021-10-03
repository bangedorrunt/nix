(module plugins.scratch 
  {autoload {nvim aniseed.nvim
             util aniseed.nvim.util
             core aniseed.core
             str aniseed.string}
   require-macros [core.macros]})

(defonce- state {:bufnr nil})

; assumes state.bufnr ~= nil
(defn- new-scratch-split []
  (nvim.ex.botright :vsplit)
  (nvim.win_set_buf (nvim.get_current_win) state.bufnr)
  (util.normal :G))

(defn- initialize []
  (var bufnr (nvim.create_buf false false))
  (buf-set-opt bufnr :filetype :fennel)
  (buf-set-opt bufnr :buftype :nofile)
  (nvim.buf_set_lines bufnr 0 0 true ["(module scratch {autoload {nvim aniseed.nvim"
                                      "                           core aniseed.core}"
                                      "                 require-macros [core.macros]})"
                                      ""])
  (tset state :bufnr bufnr)
  (new-scratch-split)
  (vim.cmd :ConjureEvalBuf))

(defn show []
  (if (= state.bufnr nil)
    ;; we need to initialize the scratch buffer's properties
    (initialize)

    ;; open an existing scratch buffer in a split window
    (let [winid (core.first (vim.fn.win_findbuf state.bufnr))]
      (if (= winid nil)
        ; open a new scratch split
        (new-scratch-split)

        ; focus the scratch window
        (vim.fn.win_gotoid winid)))))

; in theory this function won't be called at all, the scratch could live along
; the session indefinitely
(defn kill []
  (if (~= -1 state.bufnr)
    (do

      ; maybe close the window
      (let [winid (vim.fn.bufwinid state.bufnr)
            current-winid (vim.fn.winnr :$)]
        (if (and
              ; it's a valid window
              (~= -1 winid)
              ; the cursor is in it
              (= winid current-winid)
              ; it's not the only window remaining
              (~= 1 winid))
          (nvim.win_close winid false)))

      ; delete the buffer
      (nvim.buf_delete state.bufnr {:force true})

      ; reset the state
      (tset state :bufnr nil))

    (nvim.echo "Nothing to kill")))

