{
  config,
  lib,
  ...
}: let
  HOME_DIR = config.tdt.user.home;
  NIX_DIR = "${HOME_DIR}/nix";
in {
  imports = [../../modules/darwin];

  # NOTE: until mkOutOfStoreSymLink is fixed, used activation script instead
  # tdt.modules.shared.dotfiles = true;
  tdt.modules.shared.pkgs.dev.enable = true;

  # use script at system context
  # system.activationScripts.postUserActivation.text = ''

  # use script at hm context
  hm.home.activation.symDotfiles = lib.hm.dag.entryAfter ["writeBoundary"] ''
    echo ":: -> Running dotfiles activationScript..."

    # Handle mutable configs
    if [ ! -e "${HOME_DIR}/.config/nvim" ]; then
      echo ":: -> Linking nvim dir..."
      ln -sf ${NIX_DIR}/dotfiles/nvim ${HOME_DIR}/.config/nvim
    fi

    if [ ! -e "${HOME_DIR}/.config/alacritty" ]; then
      echo ":: -> Linking alacritty dir..."
      ln -sf ${NIX_DIR}/dotfiles/alacritty ${HOME_DIR}/.config/alacritty
    fi

    if [ ! -e "${HOME_DIR}/.config/bat" ]; then
      echo ":: -> Linking bat dir..."
      ln -sf ${NIX_DIR}/dotfiles/bat ${HOME_DIR}/.config/bat
    fi

    if [ ! -e "${HOME_DIR}/.emacs.d" ]; then
      echo ":: -> Linking emacs dir..."
      ln -sf ${NIX_DIR}/dotfiles/chidori-emacs ${HOME_DIR}/.emacs.d
    fi

    if [ ! -e "${HOME_DIR}/.config/yabai" ]; then
      echo ":: -> Linking yabai dir..."
      ln -sf ${NIX_DIR}/dotfiles/yabai ${HOME_DIR}/.config/yabai
    fi

    if [ ! -e "${HOME_DIR}/.config/aerospace" ]; then
      echo ":: -> Linking aerospace dir..."
      ln -sf ${NIX_DIR}/dotfiles/aerospace ${HOME_DIR}/.config/aerospace
    fi

    if [ ! -e "${HOME_DIR}/.config/karabiner/karabiner.edn" ]; then
      echo ":: -> Linking karabiner.edn file..."
      ln -sf ${NIX_DIR}/dotfiles/karabiner/karabiner.edn ${HOME_DIR}/.config/karabiner/karabiner.edn
    fi

    if [ ! -e "${HOME_DIR}/.config/skhd" ]; then
      echo ":: -> Linking skhd dir..."
      ln -sf ${NIX_DIR}/dotfiles/skhd ${HOME_DIR}/.config/skhd
    fi

    if [ ! -e "${HOME_DIR}/.config/tmux" ]; then
      echo ":: -> Linking tmux dir..."
      ln -sf ${NIX_DIR}/dotfiles/tmux ${HOME_DIR}/.config/tmux
    fi

    if [ ! -e "${HOME_DIR}/.config/zelliji" ]; then
      echo ":: -> Linking zelliji dir..."
      ln -sf ${NIX_DIR}/dotfiles/zelliji ${HOME_DIR}/.config/zelliji
    fi

    if [ ! -e "${HOME_DIR}/.zshrc" ]; then
      echo ":: -> Linking .zshrc file..."
      ln -sf ${NIX_DIR}/dotfiles/.zshrc ${HOME_DIR}/.zshrc
    fi

    if [ ! -e "${HOME_DIR}/.config/fish" ]; then
          echo ":: -> Linking fish file..."
          ln -sf ${NIX_DIR}/dotfiles/fish ${HOME_DIR}/.config/fish
        fi

    if [ ! -e "${HOME_DIR}/.tmux.conf" ]; then
      echo ":: -> Linking .tmux.conf file..."
      ln -sf ${NIX_DIR}/dotfiles/tmux/.tmux.conf ${HOME_DIR}/.tmux.conf
    fi

    if [ ! -e "${HOME_DIR}/.skhdrc" ]; then
      echo ":: -> Linking .skhdrc file..."
      ln -sf ${NIX_DIR}/dotfiles/skhd/skhdrc ${HOME_DIR}/.skhdrc
    fi
  '';
}
