(import-macros {: hi} :core.macros)

(let [{: setup} (require :lualine)
      {:get_index_of harpoon_index_of} (require :harpoon.mark)
      harpoon_number (harpoon_index_of (vim.fn.bufname))
      colors {:fg "#bbc2cf"
              ;; Dark mode
              :bg "#191724"
              ;; Light Mode
              ;; :bg "#fff0ec"
              :yellow "#ecbe7b"
              :cyan "#008080"
              :darkblue "#081633"
              :green "#98be65"
              :orange "#ff8800"
              :violet "#a9a1e1"
              :magenta "#c678dd"
              :blue "#51afef"
              :red "#ec5f67"}
      conditions
      {:buffer_not_empty (fn [] (not= (vim.fn.empty (vim.fn.expand "%:t")) 1))
       :hide_in_width (fn [] (> (vim.fn.winwidth 0) 80))
       :check_git_workspace (fn []
                             (let [filepath (vim.fn.expand "%:p:h")
                                   gitdir (vim.fn.finddir :.git (.. filepath ";"))]
                               (and (and gitdir (> (length gitdir) 0))
                                    (< (length gitdir) (length filepath)))))}

      config {:options {:component_separators ""
                        :section_separators ""
                        :theme {:normal {:c {:fg colors.fg :bg colors.bg}}
                        :inactive {:c {:fg colors.fg :bg colors.bg}}}}
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
                                            :lualine_x {}}}

      ins_left (fn [component]
                 (table.insert config.sections.lualine_c component))

      ins_right (fn [component]
                  (table.insert config.sections.lualine_x component))]

  (ins_left {1 (fn [] "")
             :color {:fg colors.blue}
             :padding {:left 0 :right 1}})

  (ins_left {1 (fn [] "")
             :color
             (fn []
               (let [mode-color {:n colors.red
                                :i colors.green
                                :v colors.blue
                                "''" colors.orange
                                :V colors.blue
                                :c colors.magenta
                                :no colors.red
                                :s colors.orange
                                :S colors.orange
                                "''" colors.orange
                                :ic colors.yellow
                                :R colors.violet
                                :Rv colors.violet
                                :cv colors.red
                                :ce colors.red
                                :r colors.cyan
                                :rm colors.cyan
                                :r? colors.cyan
                                :! colors.red
                                :t colors.red}]
                 {:fg  (. mode-color (vim.fn.mode)) :bg colors.bg}))
             :padding {:right 1}})

  (ins_left {1 :filesize :cond conditions.buffer_not_empty})
  (ins_left {1 :filename
            :cond conditions.buffer_not_empty
            :color {:fg colors.magenta :gui :bold}})
  ;; Harpoon indicator
  ;; REF: https://discord.com/channels/478925420616482816/823558498620276856/999648971553259611
  (ins_left {1 (fn [] (if harpoon_number (.. "ﯠ " harpoon_number) "ﯡ "))
             :color (fn [] (if harpoon_number {:fg "#98be65" :gui :bold} {:fg "#ec5f67"}))})
  (ins_left {1 :branch :icon "" :color {:fg colors.violet :gui :bold}})
  (ins_left {1 :diff
             :symbols {:added " " :modified " " :removed " "}
             :diff_color {:added {:fg colors.green}
             :modified {:fg colors.orange}
             :removed {:fg colors.red}}
             :cond conditions.hide_in_width})

  (ins_left {1 :progress :color {:fg colors.fg :gui :bold}})
  (ins_left {1 :diagnostics
             :sources {1 :nvim_diagnostic}
             :symbols {:error " " :warn " " :info " "}
             :diagnostics_color {:color_error {:fg colors.red}
             :color_warn {:fg colors.yellow}
             :color_info {:fg colors.cyan}}})

  (ins_left {1 (fn [] "%=")})

  (ins_right {1 :location})
  (ins_right {1 "o:encoding"
              :fmt string.upper
              :cond conditions.hide_in_width
              :color {:fg colors.green :gui :bold}})

  (ins_right {1 :fileformat
              :fmt string.upper
              :icons_enabled false
              :color {:fg colors.green :gui :bold}})

  (ins_right {1 (fn [] "")
              :color {:fg colors.blue}
              :padding {:left 1}})

  (setup config))
