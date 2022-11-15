(import-macros {: setup!} :core.macros)

(fn setup []
  (setup! noice
    {:cmdline {:view :cmdline}
     :popupmenu {:enabled false}
     :lsp {:override {:vim.lsp.util.convert_input_to_markdown_lines true
                      :vim.lsp.util.stylize_markdown true
                      :cmp.entry.get_documentation false}}
     :routes [{:filter {:event :msg_show
                        :kind ""}
               :opt {:skip true}}
              {:filter {:event :msg_show
                        :min_height 5
                        :not {:kind [:echo :search_count]}}}
              {:filter {:event :msg_show
                        :kind :search_count}
               :opts {:skip true}}]
     :presets {:bottom_search true
               :command_palette true
               :long_message_to_split true}}))

{: setup}
