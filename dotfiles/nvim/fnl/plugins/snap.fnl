(module plugins.snap
  {autoload {snap snap}})

;; Small actions system
(def- actions {})

;; Adds an action
(defn- action [name action] (tset actions name action))

(let [bottom-layout (fn []
                      (let [lines (vim.api.nvim_get_option :lines)
                            height (-> 0.35 (* lines) (math.floor))
                            width (-> :columns (vim.api.nvim_get_option) (- 4))
                            col 0
                            row (-> lines
                                    (- height)
                                    (- 4))]
                        {: col : row : height : width}))
      ;; layout (snap.get :layout)
      fzy (snap.get :consumer.fzy)
      fzf (snap.get :consumer.fzf)
      limit (snap.get :consumer.limit)
      producer-jumplist (snap.get :producer.vim.jumplist)
      producer-vimgrep (snap.get :producer.ripgrep.vimgrep)
      producer-file (snap.get :producer.ripgrep.file)
      producer-help (snap.get :producer.vim.help)
      select-help (snap.get :select.help)
      select-jumplist (snap.get :select.jumplist)
      select-vimgrep (snap.get :select.vimgrep)
      preview-help (snap.get :preview.help)
      preview-vimgrep (snap.get :preview.vimgrep)
      preview-jumplist (snap.get :preview.jumplist)]

  (defn- create [config]
         (snap.create config {:layout bottom-layout :reverse true}))

  ;; Until camspiers fix this
  (def- reverse true)
  (def- args [:--hidden :--follow :--iglob "!{.git,node_modules}/**"])

  ;; Add my defaults
  (def- file (snap.config.file:with {:preview true
                                     :consumer :fzy
                                     : reverse
                                     :layout bottom-layout
                                     :prompt :Snap
                                     :suffix " ❯"}))

  (def- vimgrep (snap.config.vimgrep:with {: reverse
                                           :layout bottom-layout
                                           :limit 50000
                                           :prompt :RG
                                           :suffix " ❯"}))
  (def- neovim
        (snap.config.file:with {:suffix " ❯"
                                :prompt "Neovim Config"
                                :consumer :fzy
                                :layout bottom-layout
                                :combine [(producer-file.args {}
                                                              (.. tdt.paths.HOME
                                                                  :/nix/dotfiles/nvim))
                                          (producer-file.args {}
                                                              (.. tdt.paths.HOME
                                                                  :/nix/dotfiles/nvim-))]}))
  ;; FIXME use `git.file` instead of `vimgrep.file` 
  ;; paths is not working with `git.file` at the moment
  (def- nix
        (snap.config.file:with {:suffix " ❯"
                                :prompt "Nix Config"
                                :consumer :fzy
                                :layout bottom-layout
                                :producer :ripgrep.file
                                :args [(.. tdt.paths.HOME :/nix) args]}))
  (snap.maps [;; Search files
              [:<Leader><Leader>
               (file {:producer :ripgrep.file : args})
               {:command :files}]
              [:<Leader>0 (neovim {}) ["Neovim Config"]]
              [:<Leader>9 (nix {}) ["Nix Config"]]
              ;; Search git files
              [:<Leader>sg (file {:producer :git.file}) {:command :git.files}]
              ;; Search buffers
              [:<Leader>sb (file {:producer :vim.buffer}) {:command :buffers}]
              ;; Grep
              ["<Leader>;" (vimgrep {}) {:command :grep}]
              ;; Grep
              [:<Leader>*
               (vimgrep {:filter_with :cword})
               {:command :currentwordgrep}]
              ;; Grep
              [:<Leader>sm (vimgrep {:filter_with :selection}) {:modes :v}]
              ;; Oldfiles
              [:<Leader>so
               (file {:producer :vim.oldfile})
               {:command :oldfiles}]
              ;; Special
              [:<Leader>ss
               (file {:try [:git.file :ripgrep.file] : args})
               {:command :git-with-fallback}]])
  (snap.register.map [:n] [:<Leader>hs]
                     (create (fn []
                               {:prompt :Help>
                                :producer (fzf producer-help)
                                :select select-help.select
                                :views [preview-help]})))
  (snap.register.map [:n] [:<Leader>sj]
                     (create (fn []
                               {:prompt :Jumplist>
                                ;; FIXME `fzy` is buggy
                                :producer (fzf producer-jumplist)
                                :select select-jumplist.select
                                :views [preview-jumplist]})))
  ;; Action system
  (snap.register.map [:n] [:<Leader>sa]
                     (create (fn []
                               {:prompt :Action>
                                :producer (fzy (fn []
                                                 (vim.tbl_keys actions)))
                                :select (fn [action]
                                          (vim.schedule (. actions
                                                           (tostring action))))}))))

;; TODO move these highlight to `tokyonight` theme
(vim.cmd "hi! link SnapSelect DiffAdd")
(vim.cmd "hi! SnapPosition guibg=None ctermbg=None ctermfg=Red guifg=Red gui=Bold")
(vim.cmd "hi! SnapBorder guifg=#e1e2e7")
