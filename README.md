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

<p align="center">
  <img src="https://github.com/aleator/OKHS/blob/master/usage.svg">
</p>
(Sorry about poor recording. TUI's are hard to capture if you don't want a
video file.)


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

# Installation

1. Clone the this repository 
2. Install [stack](https://www.haskellstack.org) for building this program
3. Run `stack --local-bin-path $HOME/.local/bin install`. 
   (You can change the bin path to point to a suitable location)
4. Go make a pot of tea and have a sandwich while your at it 
5. Add an alias:
    * Fish shell: `abbr --add ok $HOME/.local/bin/OKHS-exe`
    * Bash: add the line `alias ok=$HOME/.local/bin/OKHS-exe` to your `.bash_rc`
6. You're done. Type `ok`.

# Usage

To get started, create an `.ok` file in some project directory. You can either
do a simple `ok-bash` style `.ok` file or a bit more verbose, but more
flexible, [dhall](https://github.com/dhall-lang/dhall-lang) configuration file,
or you can simply use an existing makefile.

See examples and instructions below:

## ok-bash style .ok files

Here is an example of `ok-bash` style `.ok` file:

```
# Development commands

./build.sh fibblewidget     # Build the main widget
python test.py $widgetName  # Tests the given widget

# Publishing

git commit -am $msg         # Commit stuff
git push                    # Do a push
slack-shout $channel $msg   # Tell coworkers about this
```

There is one command per line, each followed by a mandatory comment.
Empty lines are allowed and lines containing only a comment are section
titles. Each section is presented in a different tab in the user interface.

## Makefiles

Makefiles are a great alternative to `.ok` files. They just aren't too
discoverable. If you have a `Makefile` in the directory you launch OKHS
from, the Makefile targets will appear as their own section in the
user interface. 

## Dhall .ok files.

Dhall allows you to do pretty fancy stuff with your ok-files. You can, for
example, import other files to build collections of re-usable command sets or
build parametrised commands that can be tuned for several projects. 

Here is the above ok-bash style file converted into dhall:

```
let ok  = ~/.config/OKHS/util            -- OKHS autogenerates some utils in your XDG_CONFIG 
                                         -- writing .ok files nicer. This line imports them.

in  [ ok.section "Development commands"  -- This defines a section
            [ ok.cmd "./build.sh fibblewidget"  -- This one is a command specification
                     "Run the lib in ghcid"

            , ok.cmd "python test.py $widgetName"
                     "Tests the given widget"
              //  {limitations = [ok.needsAFile "manifest.rc"]}
                                          -- OKHS allows you to add limitations to make sure
                                          -- that some simple preconditions are met before running
                                          -- a command. 
            ]

        // ok.docs                         -- This is the 'section docs' block which allows you to
                                           -- add some documentation to the user interface.
            ''
             Here are the essential commands for building this.
             Currently, there are three widgets:

             * fibbleWidget
             * wobbleWidget
             * fooWidget
            ''

    ,   ok.section "Publishing"              -- Another section here!
            [ ok.cmd "git commit -am $msg" 
                     "Commit stuff"
            , ok.cmd "git push" 
                     "Do a push"
            , ok.cmd "slack-shout $channel $msg"
                     "Tell coworkers about this"
                // ok.argDocs               -- You can also document the individual
                                            -- arguments.
                  (toMap 
                    {channel="The slack channel to post (e.g. 'whole-team', 'feature-X')"
                    ,msg="Describe the changes you pushed"}) 
                                            

            ]
      ] 
      : ok.OKHS

```

The dhall configuration is much more verbose, but it can express lot more than
the ok-bash style files. 

# Alternatives

There are few other programs that can serve the same needs. For example,

## `history`

Command history files have similar benefits as `.ok` files. However, they are
not usually project or directory based. Also, so any cruft you happen to have
typed will alsobe found in the `.history`. It can be quite hard to determine
if the command in the history was the right one.  

The `.ok` files are curated (by you) to contain the working versions of the
commands.

History files do have one benefit over `.ok`-files: you can see which steps do
go together and you don't need to write them yourself. But, why have only one
when you can do both?

## Makefiles (or your favourite alternative)

Makefiles make a great `.ok` files! As an added bonus, they can track your
command dependencies for you. However, makefiles serve a bit different need,
namely, automatically building some file. If that isn't what you are doing,
then an `.ok` file might be a better choice.  Make targets aren't as easy
to parametrize as .ok commands are and they are usually less discoverable in
the sense of "what should I `make` now?".

Regardless, OKHS will show you `make` targets as their own section if you
execute it in a directory containing a Makefile.

## ok-bash

The present tool was inspired by [`ok-bash`](http://secretgeek.net/ok). The
`ok-bash` is a similar program, but with a simple command line interface.  OKHS
works much the same way as `ok-bash` and it can also use (and convert!) `.ok`
files meant for ok-bash. Ok-bash is a great tool and if you don't care for a
text-mode ui or using dhall/makefiles.

Also, if you find the compilation time of OKHS too long, you might prefer to
use `ok-bash` instead.

# Hacking

Pulls and issues are welcome. Try to follow the existing style and make
sure OKHS doesn't run arbitrary commands at startup. If you want, you
can also fork this in Rust.

The OKHS has been my 'time off' project. As such, I've taken the liberty of
writing *weird* Haskell code for fun.

Firstly, OKHS is using the alternative Prelude called
[Relude](https://github.com/kowainik/relude). This changes some basic Haskell
functions, drops some of the historical cruft (`head` etc.) and reduces the
amount of imports I have to type. In practise really like Relude. Besides of
being a better prelude it also gives me a place to put my own common functions
(ie. the `Prelude.hs` wrapper that you use with `base-noprelude.).

Secondly, the project uses [flow](http://hackage.haskell.org/package/flow)
which replaces Haskell composition operators (`.`,`$` and `&`) with arrow like
symbols.  With flow you can also choose any direction of composition you want and
the composition operator symbols make the reading direction obvious.

I also have made effort in making all the code read from left to right.
Normally, Haskell forces you to occasionally read from right to left
and left to right, which I find bit distracting. In contrast, and after getting
used to it, writing everything in one direction feels really nice.

I think I like flow a lot and the left-to-right idea even more, so aim to
preserve that if you decide to hack on this.
