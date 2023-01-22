# Nix Configuration

This is a fork from [kclejeune/system](https://github.com/kclejeune/system).
Thank you @kclejeune for helping me set it up. Due to the complexity of the
original repo (well, just for me), my intention is to simplify the set up and
also learn me some _Nix_ on the way

**NOTE:** I'm using macOS and haven't tested Linux config.

## Requirement

- Install `Nix`:

```bash
# Single-User worked for me
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
```

- Install Nix Unstable:

```bash
nix-env -f '<nixpkgs>' -iA nixUnstable
```

- Clone this repo

```bash
git clone https://github.com/babygau/nix.git ~/.nixpkgs
```

- Update

**NOTE:** remove `flake.lock` if existed

```bash
cd ~/.nixpkgs
nix-channel --update
nix flake update --experimantal-features 'nix-command flakes'
```

- Build

```bash
nix build .\#me_at_home_with_macos
```

## Uninstall Nix

For single-user mode:

- Remove Nix from `sudo vifs`
- Remove Nix from `/etc/synthetic.conf`
- Open `Disk Utility` app, delete `Nix Store` volume
- Restart your device
- `sudo rm -rf /nix`

For multi-user mode

- Remove Nix from `sudo vidfs`
- Remove Nix from `/etc/synthetic.conf`

```bash
sudo rm -rf /etc/nix ~root/.nix-profile ~root/.nix-defexpr ~root/.nix-channels ~/.nix-profile ~/.nix-defexpr ~/.nix-channels
sudo launchctl unload /Library/LaunchDaemons/org.nixos.nix-daemon.plist
sudo rm /Library/LaunchDaemons/org.nixos.nix-daemon.plist
```

- Open `Disk Utility` app, delete `Nix Store` volume
- Restart your device
- `sudo rm -rf /nix`
- Open `System Preference | Users and Groups`, delete `Nixbuild` group
