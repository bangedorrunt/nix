(module plugins.lualine
  {autoload {{: setup} lualine}
   require-macros [core.macros]})

(def- colors {:fg "#bbc2cf"
              ;; Dark mode
              ;; :bg "#191724"
              ;; Light Mode
              :bg "#faf4ed"
              :yellow "#ecbe7b"
              :cyan "#008080"
              :darkblue "#081633"
              :green "#98be65"
              :orange "#ff8800"
              :violet "#a9a1e1"
              :magenta "#c678dd"
              :blue "#51afef"
              :red "#ec5f67"})

(def- conditions
  {:buffer_not_empty (fn []
                       (not= (nvim.fn.empty (nvim.fn.expand "%:t")) 1))
   :hide_in_width (fn []
                    (> (nvim.fn.winwidth 0) 80))
   :check_git_workspace (fn []
                          (let [filepath (nvim.fn.expand "%:p:h")
                                gitdir (nvim.fn.finddir :.git
                                                        (.. filepath ";"))]
                            (and (and gitdir (> (length gitdir) 0))
                                 (< (length gitdir) (length filepath)))))})

(def- config {:options {:component_separators ""
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
                                  :lualine_x {}}})

(defn- ins-left [component]
  (table.insert config.sections.lualine_c component))

(defn- ins-right [component]
  (table.insert config.sections.lualine_x component))

(ins-left {1 (fn [] "▊")
           :color {:fg colors.blue}
           :padding {:left 0 :right 1}})

(ins-left {1 (fn []
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
                 (hi LualineMode {:fg  (. mode-color (nvim.fn.mode)) :bg colors.bg})
                 ""))
           :color :LualineMode
           :padding {:right 1}})

(ins-left {1 :filesize :cond conditions.buffer_not_empty})
(ins-left {1 :filename
           :cond conditions.buffer_not_empty
           :color {:fg colors.magenta :gui :bold}})
;; Harpoon indicator
;; REF: https://discord.com/channels/478925420616482816/823558498620276856/999648971553259611
(ins-left {1 (fn []
               (let [harpoon-number ((. (require :harpoon.mark) :get_index_of) (nvim.fn.bufname))]
                 (if harpoon-number (.. "ﯠ " harpoon-number) "ﯡ ")))
           :color (fn []
                    (if ((. (require :harpoon.mark) :get_index_of) (nvim.fn.bufname))
                      {:fg "#98be65" :gui :bold} {:fg "#ec5f67"}))})
(ins-left {1 :branch :icon "" :color {:fg colors.violet :gui :bold}})
(ins-left {1 :diff
           :symbols {:added " " :modified " " :removed " "}
           :diff_color {:added {:fg colors.green}
                        :modified {:fg colors.orange}
                        :removed {:fg colors.red}}
           :cond conditions.hide_in_width})

(ins-left {1 :progress :color {:fg colors.fg :gui :bold}})
(ins-left {1 :diagnostics
           :sources {1 :nvim_diagnostic}
           :symbols {:error " " :warn " " :info " "}
           :diagnostics_color {:color_error {:fg colors.red}
                               :color_warn {:fg colors.yellow}
                               :color_info {:fg colors.cyan}}})

(ins-left {1 (fn []
               "%=")})

(ins-right {1 :location})
(ins-right {1 "o:encoding"
            :fmt string.upper
            :cond conditions.hide_in_width
            :color {:fg colors.green :gui :bold}})

(ins-right {1 :fileformat
            :fmt string.upper
            :icons_enabled false
            :color {:fg colors.green :gui :bold}})

(ins-right {1 (fn []
                "▊")
            :color {:fg colors.blue}
            :padding {:left 1}})

(setup config)
