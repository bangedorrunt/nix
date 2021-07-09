{ config, pkgs, ... }: {
home.file = {
};
# TODO: figure out how to use relative path
## `nix build` complain access to path is forbidden
## in restricted mode
xdg.enable = true;
  xdg.configFile = {
    "nixpkgs/config.nix".source = "${config.home.homeDirectory}/dotfiles/nix/modules/config.nix";
    "alacritty".source = "${config.home.homeDirectory}/dotfiles/alacritty"; 
    bat = {
        source = "${config.home.homeDirectory}/dotfiles/bat";
        recursive = true;
    };
    emacs = {
        source = "${config.home.homeDirectory}/dotfiles/pure-emacs";
        recursive = true;
    };
    nvim = {
      source = "${config.home.homeDirectory}/dotfiles/nvim";
      recursive = true;
    };
    karabiner = {
      source = "${config.home.homeDirectory}/dotfiles/karabiner";
      recursive = true;
    };
    skhd = {
      source = "${config.home.homeDirectory}/dotfiles/skhd";
      recursive = true;
    };
    tmux = {
      source = "${config.home.homeDirectory}/dotfiles/tmux";
      recursive = true;
    };
    yabai = {
      source = "${config.home.homeDirectory}/dotfiles/yabai";
      recursive = true;
    };
  };
}
