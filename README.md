Toolboy
=======

This repository aims to contains a set of tools to work with Gameboy content.
It is still a work in progress.

It contains two directories:
- `lesserboy`: this is the core of the BetterBoy emulator, vendored here. See https://github.com/unsound-io/BetterBoy
- `player`: this is a VGM/VGZ file player based on the aforementioned emulator core.
  It supports only Gameboy sourced VGM files.
  The project is young and unrefined but can already play files fetched from vgmrips and such, using SDL2.

To come:
- A tool to convert VGZ gameboy files to MIDI files.
