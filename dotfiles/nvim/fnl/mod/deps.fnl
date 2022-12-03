(import-macros {: use} :core.macros)

(fn setup []
  (use wbthomason/packer.nvim :commit :5cb06da03cccc7ec9f21405df98abf242c9b7b32 :start true)
  (use rktjmp/hotpot.nvim :start true)
  (use nvim-lua/plenary.nvim :start true))

{: setup}
