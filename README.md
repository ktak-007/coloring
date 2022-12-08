# Coloring

A simple console log colorizer with the idea of being modular.

![Screenshot](/Screenshot_20221208_001037.png)

## Program scrtucture

The main program part is the file Application.hs - it collects parsers from modules and compound it into a generic parser. It reads stdin and apply the generic parser line-by-line.

It uses conduit library for piping and rainbow for coloring. Also, program modules have to use rainbow to color output after parsing.

There are three modules: two modules for Plesk (for panel.log and for internal purposes - ATF) and one generic Linux log parser:
* Linux.Syslog
* Plesk.Panel
* Plesk.Atf

Each module have to provide line parser to list of colored chunks.

