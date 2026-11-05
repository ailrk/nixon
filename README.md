# Nixon

A TUI for everyday Nix tasks.

Nixon is a menu-driven front end for the `nix` command. It covers flakes,
profiles, packages, closures, caches and NixOS/Home Manager, and uses fuzzy
search for selection.

## Demo

Navigating dependencies or configs:

<img width="720" height="450" alt="Image 1" src="https://github.com/user-attachments/assets/dea57de3-ffe6-48ec-81ef-1c9b1150b690" />

Flake:

<img width="720" height="450" alt="Image 2" src="https://github.com/user-attachments/assets/21900686-fde8-4777-84ef-3004acf8e134" />

Inspect:

<img width="720" height="450" alt="Image 3" src="https://github.com/user-attachments/assets/2e6f1cae-879c-4f31-8af7-413b4c846c86" />

Search options:

<img width="720" height="450" alt="Image 4" src="https://github.com/user-attachments/assets/3cb1eb72-6450-472d-bce5-99e865c371f5" />

Work with packages:

<img width="720" height="450" alt="Image 5" src="https://github.com/user-attachments/assets/acb7652a-0295-430b-9ff7-2f7386fa8dae" />

And more...

## Install

With flakes:

```sh
nix run github:ailrk/nixon
```

Or install it into your profile:

```sh
nix profile install github:ailrk/nixon
```

Build from source:

```sh
git clone https://github.com/ailrk/nixon
cd nixon
cabal build nixon
```

Nixon calls the `nix` command, so you need Nix with `nix-command` and `flakes`
enabled.

## Usage

```sh
nixon                  # full menu
```

Menus you can jump to: `packages`, `profile`, `flake`, `inspect`, `options`,
`maintenance`, `caches`, `channels`, `nixos`, `home`, `darwin` (macOS only).

Run `nixon --help` for the full list of commands. Some of them:

```
nixon search [--json] QUERY     search the active channel
nixon shell PKG...              nix shell with packages from the channel
nixon run PKG [-- ARGS...]      run a package
nixon install PKG...            nix profile install
nixon closure TARGET            closure size report
nixon why A B                   why-depends
nixon diff A B                  diff two closures
nixon locate FILE [--regex]     which package provides a file
nixon old PKG [VERSION]         older versions of a package (nixhub)
nixon gc [--all | --older-than AGE] [--dry-run]
nixon channel [get | set REF]   show or set the nixpkgs ref
```

Global flags: `-y`/`--yes` skips confirmations, `--channel REF` overrides the
channel.

## Features

**Flake**: build, run, check, format (`nix fmt`), open a dev shell, update or
override inputs, show the output tree, create a project from a template.

**Profile**: list, remove and upgrade packages, diff generations, roll back.

**Packages**: search nixpkgs, find a package by file (`nix-locate`), find an
older version (nixhub). Pick a package to open it in a shell, run it, show
info, check its closure size, or install it.

**Inspect**: closure sizes, why-depends, build logs, derivations, debug a
build, `nix repl`, store health check, `vulnix` scan.

**Browse**: dependency tree (like `nix-tree`) and config browser (like
`nix-inspect`).

**Options**: search NixOS and Home Manager options.

**Channels**: pick a nixpkgs channel, manage the flake registry.

**Caches**: check substituters, `nix copy` between stores, Cachix, remote
builders.

**Maintenance**: garbage collection, GC roots.

**NixOS / Home Manager / nix-darwin**: build, switch, roll back, list
generations.

**History**: every command Nixon runs is recorded, and you can re-run any of
them.

## Optional tools

Nixon works without these. If a tool is missing, it falls back to
`nix shell` to fetch it when it can.

| Tool | Used for |
|------|----------|
| `fzf` | Faster picking in long lists (flake outputs, closures) |
| `nix-index` | Find a package by file |
| `vulnix` | Vulnerability scan |
| `cachix` | Cachix actions |
| `curl` | Older package versions |
| `home-manager`, `nixos-rebuild`, `darwin-rebuild` | The matching system menus |

## Notes

- The first package or option search is slow. Results are cached per channel
  revision, so later searches are fast.
- State (history) lives in `$XDG_STATE_HOME/nixon`, usually
  `~/.local/state/nixon`. The cache is in `$XDG_CACHE_HOME/nixon`.
- The TUI code lives in [`gum-brick`](./gum-brick), a small library in this
  repo.

## Credits

The menu style comes from [gum](https://github.com/charmbracelet/gum). The Browse menu is modeled on
[nix-tree](https://github.com/utdemir/nix-tree) and
[nix-inspect](https://github.com/bluskript/nix-inspect).
