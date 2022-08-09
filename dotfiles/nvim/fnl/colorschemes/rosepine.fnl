(module colorschemes.rosepine
  {autoload {{: setup} rose-pine}
   require-macros [core.macros]})

(setup {:dark_variant :dawn
        :bold_vert_split true
        :dim_nc_background false
        :disable_background false
        :disable_float_background false
        :disable_italics false})

(colorscheme rose-pine)

(hi NvimInternalError {:bg :None})
(hi RedrawDebugClear {:fg "#eb6f92" :bg :None})
(hi RedrawDebugComposed {:fg "#eb6f92" :bg :None})
(hi RedrawDebugNormal {:fg "#eb6f92" :bg :None})
(hi RedrawDebugRecompose {:fg "#eb6f92" :bg :None})

(hi CmpItemAbbr {:fg "#c0caf5"})
(hi CmpItemMenu {:fg "#db4b4b"})
(hi CmpItemKind {:fg "#f7768e"})
(hi CmpItemAbbrDeprecated {:fg "#3b4261"})
(hi CmpItemAbbrMatch {:fg "#73daca"})
(hi CmpItemAbbrMatchFuzzy {:fg "#73daca"})
(hi CmpDocumentation {:fg "#c0caf5"})
(hi CmpDocumentationBorder {:fg "#c0caf5" :bg "#191724"})
