(import-macros {: setup! : noremap} :core.macros)

(fn setup []
  (setup! yanky)
  (noremap :nx :p "<Plug>(YankyPutAfter)")
  (noremap :nx :P "<Plug>(YankyPutBefore)")
  (noremap :nx :gp "<Plug>(YankyGPutAfter)")
  (noremap :nx :gP "<Plug>(YankyGPutBefore)")
  (noremap :n :<C-n> "<Plug>(YankyCycleForward)")
  (noremap :n :<C-p> "<Plug>(YankyCycleBackward)")
  )
{: setup}
