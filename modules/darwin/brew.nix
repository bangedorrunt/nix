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
    # autoUpdate = true;
    # cleanup = "zap";
    global = {
      brewfile = true;
      lockfiles = true;
    };

    taps = [
      "candid82/brew"
      "d12frosted/emacs-plus"
      "daipeihust/tap"
      "fwartner/tap"
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
      "homebrew/command-not-found"
      "homebrew/core"
      "homebrew/services"
      "jesseduffield/lazygit"
      "jimeh/emacs-builds"
      "koekeishiya/formulae"
      "microsoft/git"
      "wez/wezterm"
      "yqrashawn/goku"
      "zdcthomas/tools"
      "beeftornado/rmtree"
    ];

    brews = [
      "brotli"
      "giflib"
      "jpeg-turbo"
      "libpng"
      "libtiff"
      "lz4"
      "xz"
      "zstd"
      "m4"
      "bat"
      "ca-certificates"
      "freetype"
      "fontconfig"
      "gettext"
      "pcre2"
      "glib"
      "libpthread-stubs"
      "xorgproto"
      "libxau"
      "libxdmcp"
      "libxcb"
      "libx11"
      "libxext"
      "libxrender"
      "lzo"
      "pixman"
      "cairo"
      "cmake"
      "coreutils"
      "libidn2"
      "libnghttp2"
      "openssl@1.1"
      "libssh2"
      "openldap"
      "rtmpdump"
      "curl"
      "dbus"
      "exa"
      "expat"
      "fd"
      "lua"
      "fennel"
      "ncurses"
      "fish"
      "fisher"
      "fnlfmt"
      "fnm"
      "fribidi"
      "fzf"
      "fzy"
      "mpfr"
      "readline"
      "gawk"
      "gcc"
      "gdbm"
      "gh"
      "git"
      "git-delta"
      "libtool"
      "libevent"
      "graphite2"
      "harfbuzz"
      "helix"
      "htop"
      "oniguruma"
      "jq"
      "just"
      "kepubify"
      "less"
      "libffi"
      "libgccjit"
      "libgit2"
      "libiconv"
      "pango"
      "libspiro"
      "unibilium"
      "libtermkey"
      "libuninameslist"
      "libuv"
      "libvterm"
      "libxml2"
      "libyaml"
      { name = "luajit"; args = [ "HEAD" ]; }
      "luarocks"
      "luv"
      "make"
      "mas"
      "mpdecimal"
      "msgpack"
      "screenresolution"
      "neofetch"
      "pandoc"
      "pcre"
      "reattach-to-user-namespace"
      "rename"
      "ripgrep"
      "sbcl"
      "shellcheck"
      "shfmt"
      "sqlite"
      "stylua"
      "utf8proc"
      "tmux"
      "tree-sitter"
      "treefmt"
      "wget"
      "zoxide"
      "zsh"
      "beeftornado/rmtree/brew-rmtree"
      "candid82/brew/joker"
      { name = "d12frosted/emacs-plus/emacs-plus@30"; args = [ "with-imagemagick" "with-modern-doom3-icon" "with-native-comp" "with-no-frame-refocus" "with-poll" "with-xwidgets" ]; }
      "fwartner/tap/mac-cleanup"
      { name = "koekeishiya/formulae/yabai"; args = [ "HEAD" ]; }
      "yqrashawn/goku/goku"
      "zdcthomas/tools/dmux"
    ];
  };
}
