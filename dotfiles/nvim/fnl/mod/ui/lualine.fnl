(import-macros {: setup!} :core.macros)

(local colors {:base "#191724"
               :surface "#1f1d2e"
               :overlay "#26233a"
               :subtle "#c0caf5"
               :text "#ffffff"
               :love "#ff5555"
               :gold "#f1fa8c"
               :rose "#ff79c6"
               :pine "#50fa7b"
               :foam "#8be9fd"
               :iris "#bd93f9"})
(local conditions
       {:buffer_not_empty #(not= (vim.fn.empty (vim.fn.expand "%:t")) 1)
        :hide_in_width #(> (vim.fn.winwidth 0) 80)
        :check_git_workspace #(let [filepath (vim.fn.expand "%:p:h")
                                    gitdir (vim.fn.finddir :.git
                                                           (.. filepath ";"))]
                                (and (and gitdir (> (length gitdir) 0))
                                     (< (length gitdir) (length filepath))))})

(local opts {:options {:component_separators ""
                       :section_separators ""
                       :globalstatus true
                       :theme :catppuccin}
             :sections {:lualine_a {}
                        :lualine_b {}
                        :lualine_y {}
                        :lualine_z {}
                        :lualine_c {}
                        :lualine_x {}}
             :inactive_sections {:lualine_a {}
                                 :lualine_v {}
                                 :lualine_y {}
                                 :lualine_z {}
                                 :lualine_c {}
                                 :lualine_x {}}})

(fn ins_left [component]
  (table.insert opts.sections.lualine_c component))

(fn ins_right [component]
  (table.insert opts.sections.lualine_x component))

(ins_left {1 #"" :color {:fg colors.pine} :padding {:left 0 :right 1}})

(ins_left {1 :mode
           :icon ""
           :color #(let [mode-color {:n colors.rose
                                     :i colors.pine
                                     :v colors.iris
                                     "''" colors.rose
                                     :V colors.iris
                                     :c colors.rose
                                     :no colors.love
                                     :s colors.rose
                                     :S colors.rose
                                     "''" colors.rose
                                     :ic colors.gold
                                     :R colors.iris
                                     :Rv colors.iris
                                     :cv colors.love
                                     :ce colors.love
                                     :r colors.foam
                                     :rm colors.foam
                                     :r? colors.foam
                                     :! colors.love
                                     :t colors.love}]
                     {:fg (. mode-color (vim.fn.mode)) :bg colors.none})
           :padding {:right 1}})

(ins_left {1 :filesize :cond conditions.buffer_not_empty})
(ins_left {1 :filename
           :color {:fg colors.iris :gui :bold}
           :cond conditions.buffer_not_empty})

(ins_left {1 :branch :icon "" :color {:fg colors.pine :bold true}})
(ins_left {1 :diff
           :symbols {:added " " :modified " " :removed " "}
           :cond conditions.hide_in_width})

(ins_left {1 :progress :color {:fg colors.fg :gui :bold}})

(ins_left {1 :diagnostics
           :sources {1 :nvim_diagnostic}
           :symbols {:error " " :warn " " :info " "}})

;; Insert mid section
(ins_left {1 #("%")})

(ins_right {1 :location})
(ins_right {1 "o:encoding"
            :fmt string.upper
            :cond conditions.hide_in_width
            :color {:fg colors.gold :bold true}})

(ins_right {1 :fileformat
            :fmt string.upper
            :icons_enabled false
            :color {:fg colors.gold :bold true}})

(ins_right {1 #"" :color {:fg colors.pine} :padding {:left 1}})

(fn setup []
  (setup! lualine opts))

{: setup}
