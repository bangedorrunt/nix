(module plugins.cmp-tabnine
  {autoload {tabnine cmp_tabnine.config}})

(tset tabnine :setup {:max_lines 1000
                      :max_num_results 20
                      :sort true
                      :run_on_every_keystroke true})
