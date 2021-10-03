(module plugins.which-key
  {autoload {: which-key}})

(which-key.setup {:plugins  {:marks  false
                             :registers false}
                 :hidden ["<silent>" "<cmd>" "<Cmd>" "<CMD>" "<CR>" "<cr>" "call" "lua" "^:" "^ "]
                 :key_labels  {:<space>  "SPC"}
                 :icons  {:separator  " "}
                 :window  {:border  "single"}})

(which-key.register {:1 "which_key_ignore"
                     :2 "which_key_ignore"
                     :3 "which_key_ignore"
                     :4 "which_key_ignore"
                     :5 "which_key_ignore"
                     :6 "which_key_ignore"
                     :7 "which_key_ignore"
                     :8 "which_key_ignore"
                     :9 "Dotfiles"
                     :0 "Nvim Conf"
                     :<Space> "Find All"
                     ";" "Live RipGrep"
                     "*" "RG*"
                     :s {:name "Snap"
                         ;; Snap binding
                         ;; :s "Git+Files"
                         ;; :a "Snap Action"
                         ;; :b "Buffers"
                         ;; :j "Jumplists"
                         ;; :g "Gitfiles"
                         ;; :o "Oldfiles"
                         ;; :m "Visual Selected"

                         ;; Telescope binding
                         :b "Buffers"
                         :c "Commands"
                         :C "Commands History"
                         :k "Keymaps"
                         :g "Gitfiles"
                         :o "Oldfiles"
                         :r "Resume"}
                     :f {:name "Files"
                         :b "File Browser"
                         :c "Close Buffer"
                         :o "New File"
                         :f "Find Files"
                         :d "Delete Buffer"
                         :r "Rename"
                         :m "Move"
                         :n "Next Buffer"
                         :p "Previous Buffer"
                         :D "Delete File!"
                         :s "Save"
                         :S "Save All"}
                     :b {:name "Buffers"
                         :b "List Buffers"
                         :c "Close Buffer"
                         :o "New File"
                         :f "Find Files"
                         :d "Delete Buffer"
                         :r "Rename"
                         :m "Move"
                         :n "Next Buffer"
                         :p "Previous Buffer"
                         :D "Delete File!"
                         :s "Save"
                         :S "Save All"}
                     :c {:name "Code"}
                     :g {:name "Git"
                         :c "Commit"
                         :s "Status"
                         :m "Move"
                         :r "Rename"
                         :b "Browse"
                         :B "Blame"
                         :D "Delete"
                         :d "Remove"
                         :l "Log"
                         :f "Fetch"
                         :p "Pull"
                         :P "Push"
                         }
                     :h {:name "Help"
                         ;; :s {:name "Snap References"}
                         :p {:name "Packer Manager"
                             :i "Install"
                             :u "Update"
                             :c "Compile"
                             :s "Sync"
                             :p "Profile"}}

                     :l {:name "LSP"
                         :a "Code Action"
                         :f "Format"
                         :l "Log"
                         :i "Info"
                         :s "Start"
                         :r "Restart"}
                     :p {:name "Projects"}
                     :q {:name "Quit"
                         :q "Quit Vim"
                         :Q "Quit Without SAVING!"
                         :s "Save then Quit"}
                     :t {:name "Toggles"
                         :t "Tree"
                         :p "Markdown Preview"}
                     :w {:name "Windows"
                         := "Ballance Windows"
                         :s "Split Window"
                         :v "Vertical Split Window"
                         :w "Jump Between"
                         :c "Window Close"
                         :d "Window Close"
                         :h "Jump Left"
                         :j "Jump Down"
                         :k "Jump Up"
                         :l "Jump Right"
                         :H "Move Left"
                         :J "Move Down"
                         :K "Move Up"
                         :L "Move Right"}
                     :x {:name "Text Edit"}}
                    {:prefix "<Leader>"})

