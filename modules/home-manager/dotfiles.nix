{ config, pkgs, ... }: {

home.file = {
  ".zshrc".source = ../../dotfiles/.zshrc;
  ".zsh_plugins.sh".source = ../../dotfiles/.zsh_plugins.sh;
  ".zsh_plugins.txt".source = ../../dotfiles/.zsh_plugins.txt;
  ".tmux.conf".source = ../../dotfiles/tmux/.tmux.conf;
  ".skhdrc".source = ../../dotfiles/skhd/skhdrc;
};

# TODO: figure out how to use relative path
## `nix build` complain access to path is forbidden
## in restricted mode
xdg.enable = true;
  xdg.configFile = {
    "nixpkgs/config.nix".source = ../config.nix;
    alacritty = {
      source = ../../dotfiles/alacritty; 
      recursive = true;
    };
    bat = {
      source = ../../dotfiles/bat;
      recursive = true;
    };
    emacs = {
      source = ../../dotfiles/pure-emacs;
      recursive = true;
    };
    nvim = {
      source = ../../dotfiles/nvim;
      recursive = true;
    };
    karabiner = {
      source = ../../dotfiles/karabiner;
      recursive = true;
    };
    skhd = {
      source = ../../dotfiles/skhd;
      recursive = true;
    };
    tmux = {
      source = ../../dotfiles/tmux;
      recursive = true;
    };
    yabai = {
      source = ../../dotfiles/yabai;
      recursive = true;
    };
  };
}
