-- NEURON PLUGIN
-- -------------
require('neuron').setup {
  virtual_titles = true,
  mappings = true,
  run = nil, -- function to run when in neuron dir
  neuron_dir = '~/workspace/notetoself',
  leader = '<Space>z', -- the leader key to for all mappings
}
