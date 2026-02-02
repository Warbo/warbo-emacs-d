# Warbo's emacs config #

This repo contains an Emacs configuration, suitable for use as `~/.emacs.d`. It
was forked from [Emacs Prelude](https://github.com/bbatsov/prelude) many years
ago, and has diverged from it since: those parts I wasn't using have been
removed, those I do use have become hard-coded, and there is an ongoing effort
to migrate the Prelude remnants to the more modular `use-package` approach.

## Layout ##

The `early-init.el` file loads modular customisations from `personal/preload/`,
then the `init.el` file loads modular customisations from `core/` (old Prelude
definitions) and the `personal/warbo.el` package.

That `warbo` package loads other files from `personal/`, organised by their
functionality (e.g. `warbo-programming.el` for programming languages and tools;
`warbo-writing.el` for writing and rendering documents; etc.). Anything which
affects Emacs itself (regardless of task) lives in `warbo-generic.el`.

Configuration can also vary between machines, and between projects:

- Machine-dependent configuration branches on `machine-id`. Its value is
  inferred during `personal/preload` based on hostname, user accounts, etc.
- Project-dependent configuration uses `.dir-locals.el`/`.dir-locals2.el`. Those
  files should be kept minimal: choosing between settings that we define here.

### Style ###

Most definitions and configuration should go inside an appropriate `use-package`
block, where possible. Even config for built-in Emacs functionality.

Bespoke functionality should live in a separate file, which our config should
load via `use-package` as if it were a third-party package.

### Shells ###

I run most of my shells inside Emacs. This config is opinionated as follows:

- `shell-mode` has been my default for many years. I like how it is an ordinary
  buffer I can navigate, select, copy, and even edit in-place; with the shell
  only seeing input that I enter after the prompt. I don't mind that it can't
  handle TUI/ncurses; I do mind that it can't handle e.g. `aider --watch`.
- `mistty` is closer to a "proper" terminal emulator which is better at handling
  interaction, escape sequences, etc. I've configured it to feel closer to
  `shell-mode`.
- `vterm` is a dependency of some packages (notably `aidermacs`). It's the
  fastest, most-compliant terminal emulator, but I don't like its modal nature:
  to get an ordinary buffer we have to toggle "copy mode". Good for specific
  commands that are too awkward for mistty or shell-mode, but not much else.
- `eshell`/`term`/`ansi-term` are built in, but I don't like them. I used
  `eshell` a long time ago but I prefer running Bash, for consistency with
  ordinary terminals and shell scripts (also why I don't bother with zsh, etc.).
  `term`/`ansi-term` are slower and more awkward than `mistty`/`vterm`.

## My usage ##

I tend to have a single Emacs instance running, as a SystemD user service;
socket-activated when I first run `emacsclient`.

My preferred UI is the Lucid/Xaw3D GUI. I prefer accessing remote instances via
a TTY `emacsclient`, rather than X11 forwarding, etc.

I only care about running this config on Linux; and mostly on NixOS. It was used
on macOS at a previous workplace, but that was a while ago.

See [nix-config](http://chriswarbo.net/git/nix-config) for more details about my
system setup.

## Testing ##

Ideally, our settings and functionality should have some corresponding tests. If
something isn't working as expected, or we're encountering an annoyance, it's a
good idea to capture that using regression tests.

Tests live in `tests/` and should be invoked using `./test-runner.sh`. That
spawns a fresh Emacs instance (to avoid messing up the one we're using). It runs
in "batch mode", which may affect how certain ELisp functions execute.

Setting the `EMACS_TEST_REGEX` env var will filter which tests are run.

Mocking should only ever be a last resort. Tests that use mocks are not testing
the real functionality, which makes them less useful.

### Good tests ###

We are testing an Emacs config, not the underlying packages; hence our tests
should operate at a "functional" level. For example:

> Opening a `.foo` file, then pressing C-p in its buffer, should put the point
> on a line containing `:=`.

This is a good test of our config, since it's checking the functionality that we
as a user will actually perform (pressing `C-p` when viewing a file of a certain
type/extension).

It's also a good idea to include extra information *if* the test fails, to help
diagnose the problem. In this case, we might include the current major mode, the
current keybinding for `C-p`, the current buffer contents, the position of point
before and after pressing `C-p`, etc. Such information *must not* be logged when
the test passes.

### Bad tests ###

In contrast, the following would be poor tests of this functionality:

> `auto-mode-alist` should associate `.foo` files to `foo-mode`.

The particular choice of mode is an irrelevant implementation detail. Perhaps we
switch to an alternative (like `foo-ts-mode`); this test would break, even if
the functionality we care about still works! Also, this is only testing what our
config *is*; rather than the effect that config *has*. The only reason we write
any config is for the effect it has; so we should be checking effects directly.

It's a good idea to report the `.foo` entry of `auto-mode-alist` in the failure
branch of our test; but we shouldn't be making assertions about it.

> Opening a `.foo` file should enable `foo-mode`.

This is more functional than the `auto-mode-alist` test, since it's checking an
effect (that `foo-mode` gets enabled). However, the actual mode is still an
implementation detail. Again, including the current mode in a failure report is
a good idea; but we shouldn't make assertions about it.

> `foo-mode-map` should bind `C-p` to `some-function`.

Again, this is checking a config rather than an effect, and it's checking an
implementation detail. If we switched to a different setup, like `foo-ts-mode`,
then this test would be misleading, since it's checking a map that we don't use.
Likewise, if we decide to make our own `warbo-some-function` instead, then this
test would break in an unhelpful way. Again, good to include in a report, but
not to make assertions about.

> Opening a `.foo` file, then invoking `some-function` in its buffer, should put
> the point on a line containing `:=`.

This is more functional than checking key maps, but it's again relying on
implementation details which shouldn't matter. If we decide to make our own
`warbo-some-function` instead, then this test becomes misleading.

> Opening a `.foo` file, then enabling `foo-mode` in its buffer, then pressing
> C-p in its buffer, should put the point on a line containing `:=`.

This is close to the good test, but our config should take care of enabling the
modes that implement our desired functionality. If opening a file does not start
the correct mode automatically, then that is a failure we should know about!
Also, tests like this will become misleading if we switch to using a different
setup (like `foo-ts-mode`).

> Opening a `.foo` file, then pressing C-p in its buffer, should set the
> `foo-matched` variable to `5`.

Again, this is close to the good test; but it's checking some implementation
detail, rather than the user-visible effect we care about. It's a good idea to
include such variables in our test's failure report, but we shouldn't be making
assertions about them.

> Advising `some-function` with a wrapper that sets a `some-function-is-called`
> boolean to `t`, then pressing C-p in the buffer of a `.foo` file, should set
> the value of `some-function-is-called` to `t`.

This is a terrible test! Not only is it checking implementation details which
should not matter (that `some-function` was called), but it's doing so in a
convoluted, unrealistic way. For example, it may break or be misleading if our
setup also uses advice to affect `some-function`.

## Tips ##

[emacsclient-commands](https://github.com/szermatt/emacsclient-commands) lets us
interact with our running Emacs instance; e.g. to pipe data into a buffer, or to
dump out the contents of a buffer.

The `personal/warbo-lisp.el` file provides extra helper functions, for example
to show the source code of an Emacs Lisp function. New helper functions can be
added as and when needed.
