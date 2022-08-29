(import-macros {: colorscheme} :core.macros)

(let [{: setup} (require :rose-pine)]
  (setup {:dark_variant :main
          :bold_vert_split true
          :dim_nc_background false
          :disable_background false
          :disable_float_background false
          :disable_italics false
          ;; :groups {:background :#fff0ec}
          :highlight_groups
          {:CursorLine {:bg :None}
           :Error {:fg :None :bg :None}
           :ErrorMsg {:fg :None :bg :None}
           :NvimInternalError {:bg :None}
           :RedrawDebugClear {:fg :#eb6f92 :bg :None}
           :RedrawDebugComposed {:fg :#eb6f92 :bg :None}
           :RedrawDebugNormal {:fg :#eb6f92 :bg :None}
           :RedrawDebugRecompose {:fg :#eb6f92 :bg :None}
           :CmpItemAbbr {:fg :#c0caf5}
           :CmpItemMenu {:fg :#db4b4b}
           :CmpItemKind {:fg :#f7768e}
           :CmpItemAbbrDeprecated {:fg :#3b4261}
           :CmpItemAbbrMatch {:fg :#73daca}
           :CmpItemAbbrMatchFuzzy {:fg :#73daca}
           :CmpDocumentation {:fg :#c0caf5}
           :CmpDocumentationBorder {:fg :#c0caf5 :bg :#191724}}})
  (colorscheme rose-pine))
