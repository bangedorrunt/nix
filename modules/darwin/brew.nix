{
  inputs,
  config,
  pkgs,
  ...
}: let
  checkBrew = "command -v brew > /dev/null";
  installBrew = ''
    ${pkgs.bash}/bin/bash -c "$(${pkgs.curl}/bin/curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"'';
in {
  environment = {
    # Install homebrew
    extraInit = ''
      ${checkBrew} || ${installBrew}
    '';
  };

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
    };
    global = {
      brewfile = true;
      lockfiles = true;
    };

    taps = [
      "FelixKratz/formulae"
      "beeftornado/rmtree"
      "candid82/brew"
      "daipeihust/tap"
      "fwartner/tap"
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/cask-versions"
      "homebrew/command-not-found"
      "homebrew/core"
      "homebrew/services"
      "koekeishiya/formulae"
      "microsoft/git"
      "wez/wezterm"
      "yqrashawn/goku"
      "zdcthomas/tools"
      # "d12frosted/emacs-plus"
    ];

    brews = [
      "beeftornado/rmtree/brew-rmtree"
      "candid82/brew/joker"
      "fwartner/tap/mac-cleanup"
      "reattach-to-user-namespace"
      "sketchybar"
      "yqrashawn/goku/goku"
      # "fish"
      # "zsh"
      # { name = "d12frosted/emacs-plus/emacs-plus@30"; args = [ "with-imagemagick" "with-modern-doom3-icon" "with-native-comp" "with-no-frame-refocus" "with-poll" "with-xwidgets" ]; }
      {
        name = "koekeishiya/formulae/yabai";
        args = ["HEAD"];
      }
    ];
  };
}
