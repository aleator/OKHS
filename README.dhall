''
<p align="center">
  <img width="500" height="305" src="https://github.com/aleator/OKHS/blob/master/logo.png?raw=true">
</p>

# OKHS -- Simple Runbooks for Terminal Users

Did you just `cd` into an old project and have no clue what to do next?
You need an `.ok` file!

The `.ok` files are the place where you collect all the commands to be run in a
given directory. If you do this, then next time you can just `cd` into that old
project, type `ok` (or your preferred alias) and be greeted with the list of
the commands required to work with your project.

The OKHS program is designed to help you maintain and select commands from you
`.ok` file through a simple terminal user interface.

(Screenshot : Todo)

# The big idea

The OKHS program doesn't do much by itself. It only provides a common place for
you to store your scripts (the `.ok` file) and it gives you a nice TUI for
selecting commands and filling in parameters. The biggest benefit will come
from maintaining `.ok` files for your projects.  Collecting project specific
scripts and commands in a git friendly, known, place is a huge help as many
project specific commands can be hard to remember after any significant amount
of time.

In my experience, having `.ok` files and this small helper program has made it
much more likely that I actually store the important commands *somewhere*. This
has made me more efficient.

# Usage

Clone the repository and install the program doing "[stack](https://www.haskellstack.org) install".
Then create a `.ok` file in your project root. You can either do a simple `ok-bash` style
`.ok` file or a bit more verbose, but more flexible, [dhall](https://github.com/dhall-lang/dhall-lang)
configuration file, or you can simply use an existing makefile.

## ok-bash style .ok files

Here is an example.

```
# Development commands

./build.sh fibblewidget     # Build the main widget
python test.py $widgetName  # Tests the given widget

# Publishing

git commit -am $msg         # Commit stuff
git push                    # Do a push
slack-shout $channel $msg   # Tell coworkers about this
```

There is one command per line, followed by a mandatory freeform comment.
Empty lines are allowed and lines containing only a comment are section
titles. You get a different tab in the user interface for each section.

## Makefiles

Makefiles are a great alternative to `.ok` files. They just aren't too
discoverable. If you have a `Makefile` in the directory you launch OKHS
from the Makefile targets will appear as their own section in the
user interface. 

## Dhall .ok files.

Dhall allows you to do pretty fancy stuff with your ok-files. You can, for
example, import other files to build collections of re-usable command sets or
build parametrised commands that can be tuned for several projects. 

Here is the above ok-bash style file converted into dhall:

```
${./ReadmeExample.ok as Text}
```

The dhall configuration is much more verbose, but it can express lot more than
the ok-bash style files. 

# Alternatives

## `history`

Command history files have similar benefits. However, they are not usually
project or directory based. Also, so any cruft you happen to have typed will be
found in the `.history`, if it is found at all after few months of doing other
work. It is also quite hard to determine if the command in the history was the
right one.  

The `.ok` files are curated (by you) to contain the working versions of the commands.

History files do have one benefit over `.ok`-files: you can see which step do
go together.

## Makefiles (or your favourite alternative)

Makefiles make a great `.ok` files! They even track your command dependencies
for you. However, makefiles serve a bit different target, namely automatically
building some file, and if that isn't what you are doing, then `.ok` might be a
better choice. For example, make targets aren't as easy to parametrize as .ok
commands are and they are usually less discoverable in the sense of "what should
I `make` now?".

OKHS will show you `make` targets as their own section if you execute it in a
directory containing a Makefile.

## ok-bash

This tool was inspired by [`ok-bash`](http://secretgeek.net/ok). It works
much the same way and it can also use (and convert!) `.ok` files meant
for ok-bash. Ok-bash is a great tool and if you don't care for a text-mode
ui or using dhall you might prefer to use that instead of OKHS.

# Hacking

The OKHS has been my 'time off' project. As such, I've taken the liberty of
writing *weird* Haskell code for learning purposes.

Firstly, I'm using the alternative Prelude called
[Relude](https://github.com/kowainik/relude). This changes some basic Haskell
functions, drops some of the historical cruft (`head` etc.) and reduces the
amount of imports I have to type. In practise really like Relude. Besides of
being a better prelude it also gives me a place to put my own common functions
(ie. the `Prelude.hs` wrapper that you use with `base-noprelude.).

Secondly, I use [flow](http://hackage.haskell.org/package/flow) which replaces
Haskell composition operators (`.`,`$` and `&`) with arrow like symbols.  With
flow I can also choose any direction of composition I want and the composition
operator symbols make the reading direction obvious.

I also have made effort in making all the code read from left to right.
Normally, Haskell kind of forces you to occasionally read from right to left
and left to right, which I find bit distracting. In contrast, and after getting
used to it, writing everything in one direction feels really nice.

I think I like flow a lot and the left-to-right idea even more.
''
