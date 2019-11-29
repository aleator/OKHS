# OKHS -- Simple Runbooks for Terminal Users

Did you just `cd` into an old project and have no clue what to do next?
You need an `.ok` file!

The `.ok` files are the place where you collect all the commands to be run in a
given directory. Collecting these in a known place is a huge help as many
project specific commands can be hard to remember after significant amounts of
time.  The OKHS program is designed to help you maintain and select commands from
you `.ok` file.

Just `cd` into that old project, type `ok` (or your preferred alias) and 
be greeted with the list of the commands required to work with your project.

# The big idea

The OKHS program doesn't do much by itself. It only provides a common place for
you to store your scripts (the `.ok`) file and gives a nice TUI for selecting
commands. The biggest benefit will come from maintaining `.ok` files for your
projects.

In my experience, having `.ok` files has made it much more likely that I actually
store the important commands *somewhere*.


# Alternatives

## history

Command history files have similar benefits. However, they are not usually
project or directory based. Also, so any cruft you happen to have typed will be
found in the `.history`, if it is found at all after few months of doing other
work. It is also quite hard to determine if the command in the history was the
right one.  

The `.ok` files are curated (by you) to contain the working versions of the commands.

History files do have one benefit over `.ok`-files: you can see which step do
go together.

# Related tools

This tool was inspired by [`ok-bash`](http://secretgeek.net/ok). It works
much the same way and it can also use (and convert!) `.ok` files meant
for ok-bash.

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
