require 'core.general'
require 'core.options'
require 'core.mappings'
require 'core.events'
require('core.packer').load_compile()

-- REF: folke/dot
-- No need to load this immediately, since we have packer_compiled
vim.defer_fn(function()
  require('core.packer').bootstrap()
  require('core.packer').load_plugins()
end, 0)
