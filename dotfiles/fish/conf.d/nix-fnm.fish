# NOTE change `fnm.fish` to `nix-fnm.fish` to make sure
# this loads after `nix-env.fish`, otherwise `fnm` won't execute
fnm env --use-on-cd | source
