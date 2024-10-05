{...}: {
  system.defaults = {
    # Login window settings
    loginwindow = {
      # Disable guest account
      GuestEnabled = false;
      # Show name instead of username
      SHOWFULLNAME = false;
    };

    # File viewer settings
    finder = {
      AppleShowAllExtensions = true;
      FXEnableExtensionChangeWarning = true;
      _FXShowPosixPathInTitle = true;
    };

    # Trackpad settings
    trackpad = {
      # Silent clicking = 0, default = 1
      ActuationStrength = 0;
      # Enable tap to click
      Clicking = true;
      # Firmness level, 0 = lightest, 2 = heaviest
      FirstClickThreshold = 1;
      # Firmness level for force touch
      SecondClickThreshold = 1;
      # Don't allow positional right click
      TrackpadRightClick = false;
      # Three finger drag for space switching
      # TrackpadThreeFingerDrag = true;
    };

    # Firewall
    alf = {
      # 0 = disabled 1 = enabled 2 = blocks all connections except for essential services
      globalstate = 1;
      loggingenabled = 0;
      stealthenabled = 1;
    };

    # Dock
    dock = {
      # Auto show and hide dock
      autohide = true;
      # Remove delay for showing dock
      autohide-delay = 0.0;
      # How fast is the dock showing animation
      autohide-time-modifier = 1.0;
      tilesize = 50;
      static-only = false;
      showhidden = false;
      show-recents = false;
      show-process-indicators = true;
      orientation = "bottom";
      mru-spaces = false;
    };

    NSGlobalDomain = {
      "com.apple.sound.beep.feedback" = 0;
      "com.apple.sound.beep.volume" = 0.0;
      # Allow key repeat
      ApplePressAndHoldEnabled = false;
      # Delay before repeating keystrokes
      InitialKeyRepeat = 10;
      # Delay between repeated keystrokes upon holding a key
      KeyRepeat = 1;
      AppleShowAllExtensions = true;
      AppleShowScrollBars = "Automatic";
    };
  };
  # `nix-darwin` hasn't got this option so I found a hacky way
  # Disable crash reporter
  system.activationScripts.userDefaults.text = "defaults write com.apple.CrashReporter DialogType none";
  # Enable crash reporter
  # system.activationScripts.userDefaults.text = "defaults write com.apple.CrashReporter DialogType crashreport";
}
