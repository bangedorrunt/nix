(module colorschemes.iceberg
   {autoload {nvim aniseed.nvim}
    require-macros [core.macros]})

(color iceberg)
(hi Comment {:gui :italic :cterm :italic :term :italic})
(hi NvimInternalError {:bg :None :fg :Green})
