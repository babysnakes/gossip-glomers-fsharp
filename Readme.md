## Gossip Glomers

> [A series of distributed systems challenges brought to you by Fly.io][fly.io].

Implementation of the challenges described in the link above using _F#_. I had to decide whether or not to allow types
with invalid state or to use low level json parsing. For now I chose to allow types with invalid state. Once done I may
try the opposite.

### Setup

This repository contains a [Taskfile][] to automate running the various tasks (you can also use the instructions in the
[website][gs] instead while adopting the paths for the executables).

#### Requirements

The following dependencies needs to be installed in order to run the challenges (note the _WSL_ dependency if running on
windows).

* [Taskfile][].
* _Dotnet_ (version 6.x).
* _WSL_ (only if running on Windows - _maelstrom_ does not support running on Windows). If you're using Windows + WSL
  all dependencies below has to be installed inside the default WSL distribution.
* JDK (I'm running with 11).
* _Tar_ with `bzip2`.
* `wget`.
* Gnuplot

> Note: Follow the [getting started][gs] guide for installing any other prerequisites.

#### Running

* Run `task prepare` to install build dependencies and _Maelstrom_.
* Run `Task --list` to see available tasks, run the one you want to execute.

[fly.io]: https://fly.io/dist-sys/

[gs]: https://github.com/jepsen-io/maelstrom/blob/main/doc/01-getting-ready/index.md

[Taskfile]: https://taskfile.dev/