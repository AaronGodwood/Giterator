# Giterator

Inspect and rewrite git history — timestamps, file contents, even the commit hashes themselves.

Giterator is a Haskell command-line tool and terminal UI that works directly with git's object model. It reads and writes commits, trees and blobs byte-for-byte, so it can do things like:

- spread a day's worth of commits across a month, on weekdays, during working hours
- scrub a leaked secret from every version of every file
- delete or rename a file or directory throughout history
- give a commit an id that starts with `cafe` or `c0ffee`
- show you exactly which bytes git hashes to get a commit id

Every rewrite is previewable, backed up and undoable.

```sh
# Make today's work look like it happened over the last month, on weekdays, 9 to 6
giterator rewrite --spread 2026-08-24..2026-09-24T17:00 --jitter 3h --weekdays --work-hours 09:00-18:00
```

## Contents

- [Installing](#installing)
- [Quick tour](#quick-tour)
- [Commands](#commands)
- [Rewrite options](#rewrite-options)
- [The TUI](#the-tui)
- [Safety](#safety)
- [How it works](#how-it-works)
- [Development](#development)
- [Limitations](#limitations)

## Installing

You need [GHC](https://www.haskell.org/ghcup/) 9.4 or newer, cabal 3.10 or newer, and git on your `PATH`.

```sh
git clone https://github.com/AaronGodwood/Giterator.git
cd Giterator
cabal install exe:giterator --install-method=copy --overwrite-policy=always
```

This copies `giterator` into cabal's install directory; make sure that directory is on your `PATH`. To put it somewhere specific, add `--installdir=<dir>`.

The first build takes a while (the TUI pulls in [brick](https://hackage.haskell.org/package/brick) and [vty](https://hackage.haskell.org/package/vty)). Giterator is developed and tested on Windows. Nothing in it is Windows-specific, but it hasn't been tried on Linux or macOS yet.

## Quick tour

Every command runs in the current directory; use `-C <path>` to point at another repository.

**See what git actually hashes.** A commit id is the SHA-1 of a short header plus the commit's text:

```text
$ giterator show --raw HEAD
type:    commit
size:    275 bytes
hashed:  sha1("commit 275\0" <> body)
result:  87309ed1e49e8c3391d0c24bb3251c2183bef628  (matches git)
---
tree 6af0eb2a2acf5a00a42c91f443200d722bab976a
parent d1036b63a67c546e9c16e3ba84653c97e8447be0
author …
committer …

feat: TUI apply/undo, search, help and date column
```

Trees are binary, so `--raw` escapes them, showing the 20 raw hash bytes after each name:

```text
$ giterator show --raw 'HEAD^{tree}'
…
100644 .gitignore\0\xc39T\xf5:\x06\x80Yo\xaa\xee\xc8s\x99\x1c\xfe\xd6<|\x9d
40000 app\0\xb4\xb5\xc88\x98j\xb3\x91\xfe\x9a\xab@\xa8w9\xf5\x94H\x1a\xf7
```

**Change dates, then look before you leap:**

```sh
giterator rewrite --dry-run --shift -7d --tz +0900
giterator rewrite --shift -7d --tz +0900
giterator log
giterator undo          # changed your mind
```

**Remove a file from all of history,** dropping commits that only touched it:

```sh
giterator rewrite --delete secrets.env --prune-empty
```

**Scrub a leaked token** from every text file in every commit:

```sh
giterator rewrite --replace 'sk_live_abc123=>REDACTED'
giterator purge --yes   # then actually delete the old objects locally (see Safety)
```

**Mine a vanity hash** for the tip of the current branch:

```text
$ giterator vanity --prefix c0ffee
  mined 47128f0 -> c0ffee7f79cc2700d39fabff36ba6b26f8f1b50d  (13317149 attempts)
rewrote 4 commits, 1 changed
```

**Or do all of it interactively:**

```sh
giterator tui
```

## Commands

| Command | What it does |
|---|---|
| `tui [BRANCH]` | Interactive browser with live rewrite previews (see [The TUI](#the-tui)) |
| `log [REV] [-n N]` | Commits with author dates; a second line appears when the committer or commit date differs |
| `show REV [--raw]` | A readable view of any commit, tree, blob or tag; `--raw` shows the exact bytes and recomputes the hash |
| `verify [REV]` | Checks that every commit and tree round-trips byte-for-byte through Giterator's parser (default: all refs) |
| `rewrite [BRANCH...]` | Rewrites history of the given branches (default: all) — see below |
| `vanity [BRANCH...] --prefix HEX` | Gives branch tips an id starting with `HEX` (default branch: the current one) |
| `undo` | Restores the refs from before the most recent rewrite |
| `purge --yes` | Permanently deletes backups, reflogs and unreachable objects |

Add `--help` to any command for its full options.

## Rewrite options

`rewrite` takes any combination of these. Content changes run first, then date changes in the order listed.

### Content

| Option | Effect |
|---|---|
| `--replace 'OLD=>NEW'` | Replace text in every version of every text file (repeatable) |
| `--replace-regex 'RE=>NEW'` | Same, with a POSIX regular expression; applied after literal replacements |
| `--path GLOB` | Only apply replacements to matching files, e.g. `'*.env'` or `'src/**/*.hs'` |
| `--delete GLOB` | Remove matching files or whole directories from every commit |
| `--rename 'FROM=>TO'` | Move a file or directory in every commit |
| `--prune-empty` | Drop commits that end up changing nothing |

Globs without a slash match a name at any depth, like `.gitignore`; `**` crosses directories. Binary files (a NUL byte in the first 8000 bytes, git's own test) are never edited, and neither are symlinks or submodules.

### Dates

| Option | Effect |
|---|---|
| `--spread FROM..TO` | Stretch or squash history linearly into a range (UTC), keeping order and relative gaps. Dates look like `2024-01-31` or `2024-01-31T09:30` |
| `--shift DURATION` | Move every date, e.g. `3d`, `-2h30m`, `1w` |
| `--jitter DURATION [--seed TEXT]` | Move each commit by a random offset of up to ±DURATION. The same seed always gives the same result |
| `--weekdays` | Keep commits off weekends: Friday–Sunday is squeezed into Friday, Monday–Thursday are untouched |
| `--work-hours HH:MM-HH:MM` | Squeeze each day's commits into these local hours, keeping their order |
| `--tz +HHMM [--keep-wall-clock]` | Change the timezone. By default the moment stays the same and the clock time changes; `--keep-wall-clock` does the opposite |
| `--dates author\|committer\|both` | Which dates the options apply to (default: both) |

Git records two dates per commit: when it was authored and when it was committed (they differ after a rebase or cherry-pick). `git log` shows the first; GitHub mostly shows the second. By default Giterator changes both.

If a rewrite leaves a commit dated before its parent, you'll get a warning — git doesn't mind, but GitHub and `git log` sort by date and will show it out of place.

### General

| Option | Effect |
|---|---|
| `--dry-run` | Show old → new ids (and dates) for every changed commit without moving any refs |

### Vanity options

| Option | Effect |
|---|---|
| `--prefix HEX` | The wanted start of the commit id, 1–16 hex digits |
| `--method whitespace` | *(default)* Hide a counter in spaces and tabs at the end of the message's last line — invisible, and practically unlimited |
| `--method seconds` | Nudge the author and committer times a little later instead |
| `--chain` | Mine every commit on the branch, not just the tip |

Each extra hex digit is 16× more work. On an 8-core machine Giterator manages about 1.3 million attempts a second: four digits take a second or two, six take about ten seconds, seven a few minutes.

## The TUI

`giterator tui` opens the current branch (or the one you name):

- **Left:** the history. Merges are marked `M`, and `•` marks commits whose committer date differs from the author date.
- **Right:** the selected commit, readable or as raw bytes.
- **`t`** opens the transform form — the same options as `rewrite`, validated as you type. Every change runs a preview in the background, entirely in memory: rewritten commits turn green with their new ids and dates, dropped ones red, and the details pane lists exactly what changes for the selected commit.
- **`a`** applies the previewed rewrite (after a confirmation), and **`u`** undoes the last one.

| Key | Action |
|---|---|
| `↑ ↓ PgUp PgDn Home End` (or `j k g G`) | Move through history |
| `/` | Filter by hash, subject or author (`Enter` keeps it, `Esc` clears it) |
| `d` | Show author or committer dates |
| `r` | Readable view or raw object bytes |
| `J` / `K` | Scroll the details pane |
| `t` | Transform form (`Tab` / `Shift-Tab` between fields, `Space` toggles, `Esc` returns) |
| `a` / `u` | Apply / undo |
| `?` | Help |
| `q` / `Esc` | Quit |

## Safety

Rewriting history is destructive by nature, so Giterator tries hard to make every step reversible.

- **Preview first.** `--dry-run` and the TUI's live preview show every change before anything moves.
- **Every rewrite is backed up.** The old and new position of each ref are stored under `refs/giterator/<n>/`, and branches move in a single atomic transaction.
- **`undo` is careful.** It refuses to run if a branch has moved since the rewrite, so it can't throw away commits you made afterwards.
- **Uncommitted changes block rewrites.** After a rewrite of the checked-out branch, the working tree is updated to match.
- **Signatures are removed, not left broken.** A GPG or SSH signature covers the exact commit bytes, so any changed signed commit loses its signature, and you're told how many.
- **Annotated tags are reported, not moved.** Lightweight tags follow their commits; annotated tags are listed as still pointing at the old history.

Things Giterator can't do for you:

- **Old content isn't gone until you purge.** Backups and reflogs keep the previous history reachable. After scrubbing a secret, `giterator purge --yes` deletes backups (so `undo` stops working), expires reflogs and prunes unreachable objects.
- **Other copies are unaffected.** Rewritten branches need `git push --force`, and anyone who already cloned or fetched still has the old history. If a secret was ever pushed, rotate it — rewriting history is not enough.
- **Hosts keep their own records.** GitHub, for example, logs when pushes happened, independently of commit dates.

Please use it on your own repositories, and don't use it to misrepresent who did what, or when, to other people.

## How it works

A commit is a small text object: a tree id, parent ids, author and committer lines (name, email, Unix time, timezone), optional extra headers such as `gpgsig`, and the message. Its id is `sha1("commit <size>\0" <> text)`. Change any byte — including a parent id — and the id changes, which is why editing one commit changes every commit after it.

- **Byte-exact parsing.** `Git.Object` parses and renders commits and trees so that `render (parse bytes) == bytes` for every object git produces. A rewrite that changes nothing therefore reproduces every hash; `giterator verify` checks this across a whole repository (all 1,975 commits and 5,495 trees of [haskell/bytestring](https://github.com/haskell/bytestring) round-trip exactly).
- **Rewriting is a fold.** History is visited oldest-first, carrying a map from old to new ids. Each commit's parents are remapped, then a `Transform` (a `Commit -> GitM Commit` with a `Monoid` instance) edits it. Transforms compose with `<>`, and `mempty` is the identity rewrite.
- **Content rewrites are memoised** on `(path, tree id)`, so each distinct subtree is processed once per rewrite rather than once per commit.
- **Reading** goes through one long-running `git cat-file --batch` process. **Writing** buffers new objects in memory and hands them to `git index-pack` as a single packfile — much faster on Windows than thousands of loose files.
- **Vanity mining** hashes the unchanging prefix of the commit once, then only hashes the varying suffix for each attempt, searching in parallel across all cores. It always takes the lowest matching attempt, so results are reproducible.
- **The TUI** keeps a pure model (tested without a terminal) and one worker thread that owns the git process. Previews are cancelled cooperatively at commit boundaries, when the `cat-file` pipe is idle.

## Development

```sh
cabal build
cabal test
```

The test suite (hspec + QuickCheck) builds throwaway repositories with pinned identities and dates, so their hashes are deterministic, and covers parsing round-trips, every transform, rewrites through merges, undo, purging, vanity mining and the TUI model.

| Path | Contents |
|---|---|
| `src/Git/` | Object model, parsing, the object store, refs and backups, packfiles, the rewrite engine |
| `src/Transform/` | Date, content and vanity transforms |
| `src/Tui/` | Model, view, form, preview and worker for `giterator tui` |
| `app/Main.hs` | Command-line interface |
| `test/` | Test suite and fixture-repository helpers |

## Limitations

- Annotated tags aren't rewritten (they're reported instead).
- Packfiles are read through `git cat-file` rather than parsed directly, so git must be installed.
- The TUI form takes one value each for replace, delete and rename; the CLI accepts several.
- `--jitter` can put a commit before its parent when commits are close together; you'll get a warning, and a different `--seed` usually fixes it.
- A very large content rewrite in the TUI preview (over 64 MB of new objects) writes an unreferenced pack, which `git gc` later removes.
