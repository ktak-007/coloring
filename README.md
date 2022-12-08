# Coloring

A simple console log colorizer with the idea of being modular.

![Screenshot](/Screenshot_20221208_001037.png)

## Program structure

The main program part is the file Application.hs - it collects parsers from modules and compound it into a generic parser. It reads stdin and apply the generic parser line-by-line.

It uses conduit library for piping and rainbow for coloring. Also, program modules have to use rainbow to color output after parsing.

There are three modules: two modules for Plesk (for panel.log and for internal purposes - ATF) and one generic Linux log parser:
* Linux.Syslog
* Plesk.Panel
* Plesk.Atf

Each module have to provide line parser to list of colored chunks.

## Installation

There are three ways to use this program: as nix binary, stack binary and as script (nix or stack).

### Nix (recommend)

* You have to install [the package manager Nix](https://nixos.org/download.html)

Example of single-user nix installation:
```
sh <(curl -L https://nixos.org/nix/install) --no-daemon
```

After correct installation such progams as `nix-env` and `nix-shell` should be available. Probably, relogin required.

* Install the program:
```
nix-env -f https://github.com/ktak-007/coloring/archive/master.tar.gz -i --option sandbox false
```

### Stack

You have to install [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) on you computer:

* Linux/macOS

```
curl -sSL https://get.haskellstack.org/ | sh
```

* Windows 64-bit

[Windows installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)


#### Build

Download sources from github and run the command in the project direcrory:

```
stack build --no-nix
```

#### Install with stack

```
stack install
```

### Script

* You have to download sources to your computer
* You have to install Nix or Stack on your choice as described above
* You have to run wrapper:
a) src/nix-wrapper if your choice is Nix
b) src/stack.hs if your choice is Stack

Script run is the convient way to develop new parsers.
