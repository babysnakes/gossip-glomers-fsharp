## Gossip Glomers

> [A series of distributed systems challenges brought to you by Fly.io][fly.io].

Implementation of the challenges described in the link above using _F#_.

### Setup for Running in Windows

This section contains instructions for developing the code in _Windows_ while running `maelstrom` from within
_WSL_ (`maelstrom` does not support running in windows). For running in unix like OS (including completely inside WSL)
follow the [getting startred][gs] page and the various documentations.

#### Requirements

* [Taskfile][]
* _WSL_ configured as version 2 with default distribution (I'm using Ubuntu)

#### Preparations

* Follow the [getting started][gs] guide for installing prerequisites in the default _WSL_ distro (e.g. JVM, Gnuplot,
  etc).
* Extract `maelstrom` in your _WSL_ `$HOME` (so the path will be `~/maelstrom/maelstrom`).

#### Running

Run `Task --list` to see available tasks, run the one you want to execute.

[fly.io]: https://fly.io/dist-sys/

[gs]: https://github.com/jepsen-io/maelstrom/blob/main/doc/01-getting-ready/index.md

[Taskfile]: https://taskfile.dev/