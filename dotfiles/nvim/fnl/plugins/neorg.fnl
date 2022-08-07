(module plugins.neorg
  {autoload {{: setup} neorg}})

(setup {:load {:core.defaults {}
               :core.gtd.base {:config {:workspace :personal}}
               :core.norg.concealer {:config {:dim_code_blocks false}}
               :core.norg.qol.toc {}
               :core.norg.completion {:config {:engine :nvim-cmp}}
               :core.norg.dirman {:config {:workspaces {:personal "~/workspace/note-to-self"}
                                           :autodetect true
                                           :autochdir true}}}})
