{ inputs, config, pkgs, ... }:
let
  checkBrew = "command -v brew > /dev/null";
  installBrew = ''
    ${pkgs.bash}/bin/bash -c "$(${pkgs.curl}/bin/curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"'';
in
{
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
      "beeftornado/rmtree"
      "candid82/brew"
      # "d12frosted/emacs-plus"
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
      "yqrashawn/goku"
      "zdcthomas/tools"
      "wez/wezterm"
    ];

    brews = [
      "reattach-to-user-namespace"
      "ripgrep"
      "fish"
      "zsh"
      "beeftornado/rmtree/brew-rmtree"
      "candid82/brew/joker"
      # { name = "d12frosted/emacs-plus/emacs-plus@30"; args = [ "with-imagemagick" "with-modern-doom3-icon" "with-native-comp" "with-no-frame-refocus" "with-poll" "with-xwidgets" ]; }
      "fwartner/tap/mac-cleanup"
      { name = "koekeishiya/formulae/yabai"; args = [ "HEAD" ]; }
      "yqrashawn/goku/goku"
    ];
  };
}
