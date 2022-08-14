# Rinse, Repeat -- `rr`

Rinse repeat. Having gotten fed up with the sheer amount of baggage that a lot
of 'file watchers' come with (forget NPM, not going on my machine, ever) I
decided that what I needed was something utterly simple, that doesn't grind
the CPU and that just does what it says on the tin, I offer `rr`. Rinse,
repeat.

Once installed and on your OS path, all you do is:

    $ rr "command-to-run" list-of-files-to-trigger-a-change

The list can include wildcards, but I am relying on the operating system to
expand that list prior to beginning execution i.e. the code itself does not do
anything when given "*.c", the OS would have scanned the local folder and
handed over all files ending with ".c" as the command line argument list.

My particular use case was to work in conjunction with my own hand-rolled
literate programming tool, `mlpt`, so all I needed was to look for changes to
the Markdown source file and then trigger a rebuild of the Mercury sources in
the file and also to regenerate the PDF documentation at the same time.

`rr` does all of that for me so all I need to do is sit and think and work!


# Requirements

Obviously you need to have installed a working Mercury compiler, but also,
your platform must have available the "C" function to sleep,

    usleep() -- unistd.h

If that's broken for you, I don't know what to suggest. I have seen a smart
piece of code from Peter Wang where he used a socket recv call with an empty
socket list and a timeout to do the same thing but can I find it again?


# Using it

I made it simple, all it does is use the first command line argument as the
command that is to be run after any changes are detected in the remaining list
of files given to it. It does NOT run the command initially, rather it sits
and waits until the *modification time* (mtime to all you *nix heads) changes
for any one of the files, then it executes the first command, then guess
what... it does it all over again. It displays the name of the file that
triggered the change of state from waiting to executing.

To stop it, use CTRL+C on most platforms.

Here are some examples just to make it clearer.


## Working with a build system

    $ rr "make && make install" *.c *.h

This would be given a list of affected files in the current folder, and when
any one of them changes, the command to make and and then install is given. If
the command fails, the output goes to the standard output stream, as for all
commands. It won't stop `rr` from running.


## Using with a literate programming tool

    $ rr "mlpt \"-U *:../gfx.m\"" gfx.md

Note the double escaping of the strings. The `mlpt` command says that it
should process the file called `gfx.md`, render out the code to the file called
`../gfx.m` and start from the root element called "*".


# Bugs / Suggestions

GitHub has a great issue system.
