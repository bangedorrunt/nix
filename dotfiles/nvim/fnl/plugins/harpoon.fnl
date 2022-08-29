(import-macros {: nmap : noremap} :core.macros)

(let [{: setup} (require :harpoon)
      {: add_file : rm_file : clear_all} (require :harpoon.mark)
      {: toggle_quick_menu
       : nav_file : nav_next : nav_prev} (require :harpoon.ui)
      format string.format]

  (setup {:global_settings {:enter_on_sendcmd true}})

  (noremap n silent :<Leader>mf add_file)
  (noremap n silent :<Leader>md rm_file)
  (noremap n silent :<Leader>mD clear_all)
  (noremap n silent :<Leader>mm toggle_quick_menu)
  (noremap n silent :<Leader>mn nav_next)
  (noremap n silent :<Leader>mp nav_prev)

  (for [v 1 9]
    (noremap n silent (format "<Leader>%s" v) '(nav_file v))))
