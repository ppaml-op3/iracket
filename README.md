# IRacket

IRacket is a Racket kernel for IPython/Jupyter.

# Installation

## Requirements

* [Racket v6](http://racket-lang.org)
* These Racket packages installed (via `raco pkg install`):
    * [git://github.com/greghendershott/sha#master](https://github.com/greghendershott/sha#master)
    * [git://github.com/tgiannak/racket-libuuid#osx-compat](https://github.com/tgiannak/racket-libuuid#osx-compat)
    * [git://github.com/tgiannak/racket-zeromq#wait-on-fd](https://github.com/tgiannak/racket-zeromq#wait-on-fd)
* [ZeroMQ](http://zeromq.org)

## Installation steps

1. Clone this repository.
2. Run the following
```bash
mkdir -p ~/.ipython/kernels/racket/
cp kernel.json ~/.ipython/kernels/racket/
```
3. Adjust the copied `kernel.json` to refer to iracket in the path of this
   repository.
4. If `racket` isn't on your path, adjust the copied `kernel.json` to refer to
   the absolute path of `racket`.

# Using the kernel

Run the ipython notebook server as you usually do, e.g.
```bash
ipython notebook
```
and create a new notebook with the Racket kernel.
