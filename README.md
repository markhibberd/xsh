xsh
===

```
BEFORE YOUR ARRIVE FOR WORKSHOP:

 - Have GHC 8 installed
 - git clone https://github.com/markhibberd/xsh.git
 - cd xsh
 - one-of:
---
 cabal install --only-dependencies
 cabal configure
 cabal build
---
 or
---
 ./mafia build 
---

```

An experimental toy shell.

`xsh` is designed to be the base of a workshop aimed at building a
unix shell in a functional language.

If you are attempting to take on this tutorial, then hopefully you
have seen the talk to give you an idea of the structure of a shell
(and this shell specifically).

The workshop is very open-ended, and the point is to work on something
at your appropriate knowledge level. For someone new to Haskell or
functional programming, that may be retracing the initial steps
covered by the talk, for someone experienced it will be taking that
initial base and extending it in an interesting direction.


Initial baseline
----------------


```
grep -R 'BASELINE' src
```

The first step is to get a very basic shell, that can execute and
compose programs. As discussed in the talk, there are four basic areas
to a shell:

 1. Line Editing
 2. Parsing
 3. Expansion
 4. Builtins
 5. Process Managment

Our initial cut is going to concentrate on parsing (a POSIX shell like
syntax) and process management. We will kick line-editing to a
library for now (as do many real shells), expansion will be kept at a
minimum and we won't have any builtins.

If you think you have a handle on the parsing and process management based
on the talk, you can skip ahead on branch `workshop/baseline`. Which will
provide this initial baseline to start from. Please only do this if you
are really comfortable with Haskell and the concepts, if you are not you
may get stuck pretty quickly and you are better off working through to
getting an actual functioning shell in the two hours.

Phase Two
---------

```
grep -R 'PHASE 2' src
```

Lets add some basic configuration, environment variables and use that
to improve our command expansion and add some basic builtins.

Tasks:

 - Add a built-in for `cd` (to see why, try running `cd` program in
   your shell and see what happens).

 - Add variable resolution into expansion.

 - Add the ability to set an environment variable.

Optional Tasks:

 - Add a built-in `set` for setting configuration.

 - Implement the `pipefail` setting, that when set fails the pipeline
   when any part of it exits non-zero.

Phase Three
-----------

What features you want to add to your shell now are up to you. There are
four interesting paths to take:

 - A POSIX compatible but better shell. There are other shells in this
   space (for example `zsh`) that can operate in a POSIX compatible
   way but offer significantly more features for safety or
   productivity, however there is still massive potential for
   improvement.

 - A shell optimised for interactive use. A comparison point in this
   space might be something like the `fish` shell. A shell that breaks
   away from POSIX, and chooses to optimise syntax and features for
   use by humans rather than computers.

 - A shell optimised for program composition. A comparison point to
   this might be more general purpose language - except that they are
   all increadibly bad at it.

 - Step back from the "shell" nature and build a linter or quality
   checker from the parsing infrastructure we are building up.

Where you go from here is up to you, I will break possible features
down into two sets, the "feature parity" for things you need for your
shell to be useful, and the "winning at life" features that might make
your shell something exceptional.

### Feature Parity

`Signal Handling` - This has been ignored so far. `haskeline` does
some basics (and can be improved with `handleInterrupt` /
`withInterrupt`, but there are lots of gotchas. This needs to be
tightly integrated with process management, as signals often need to
be handled contextually depending on how a child process handles it
(a reasonable write-up https://www.cons.org/cracauer/sigint.html)

`Job Control` - `jobs`/`fg`/`bg`.

`Sub-Shells` - `()`

`Expansion` - globbing, sub-shell support, arithmetic expansion etc...

`Control Flow` - `if`/`case`/`while`/`{}`.

`Here-Docs` - `<<`, `<<-`

`Functions` - `function()`

`Bang` - pipelines need to support being `!`'d.

`Complete Set of Built-Ins`


### Winning at Life

`Better History` - Contextual history (for example history specific to a
directory, or to your last command).

`Better Line Editing` - Things like better suggestions / autocomplete (for
example integrating something like https://github.com/junegunn/fzf).

`Data Types` - Lots of shells have lists, some of association lists,
but none just have arbitrary data-types. When are algebraic data types
not useful.

`Better Functions` - Real types + arguments.  Externally representable
so functions can be called by programs.

`First Class Parsers` - If we have data-types, a first class way to
define parsers in pipelines would be amazing, `cat file | Person <$>
string <*> int | Person.name`

`Parse-time checks / linter` - There are lots of basics that can be
checked earlier, or poor patterns.

`Resource Blocks` - Something better than traps for managing resource
clean-ups. Bracketing or resource manager based etc...

`First Class Parallelism` - A parallel-for or similar for more direct
encoding of parallel structures, Control.Concurrent.Async like constructs
(race, waitBoth, waitEither, ...).

`Persist / Restore State` - The ability to save off the current state
of your shell (CWD, history, local variables) and restore later.

`Not Pipes` - Implement a program composition strategy besides pipes
which while very powerful, are not actually great for safe program
composition / resource usage.

`Scoping` - Yep.


References
----------

POSIX documentation including tokenisation and parsing rules -
http://pubs.opengroup.org/onlinepubs/9699919799/ - regrettably deep
linking doesn't work, so you need to select 'Shell & Utilities` in the
top-right iframe.

Signal handling - https://www.cons.org/cracauer/sigint.html

A good comparison of shells, and their quoting / expansion etc... -
http://hyperpolyglot.org/unix-shells

Megaparsec Hackage Docs - https://hackage.haskell.org/package/megaparsec-5.3.0

Process Management Hierarchy Approaches - http://web.cse.ohio-state.edu/~mamrak.1/CIS762/pipes_lab_notes.html
