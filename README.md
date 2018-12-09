
Introduction
============

This repo ...

Highlights
==========

...

Developer and API Conventions
=============================

+ A dot prefix is used for un-"exposed" functions that are not intended to be used directly by the user.
(i.e. like how "helper" functions in a package are not exported).

+ The `auto_` prefix is used for "exposed" functions with arguments set equal to command line `args`.
This prefix indicates (to the user) that these are to be used for batch-style automated runs.

+ "path"-like (or, in more programmatic terms, `path_format`-like) function parameters 
are converted to variables borrowing their namesake.
For example, `path_raw_play_by_play_format` is converted to `raw_play_by_play`,
`path_raw_game_summary_format` is converted to `raw_game_summary`, etc., where 
the `format` suffix is a placeholder for the `season` (and the `path` prefix siimply
indicates that the variable is to be imported from a file). `path_format`-like
parameters are converted to and from their variable equivalents vai the 
internal `.[import|export]_data_from_path_format()` functions.

+ The names of path-like parameters in exposed functions are intentionally
named in a verbose manner (somewhat compromising the "abstractness" of the functions).
This is done to implicitly indicate to the user (and the unacquainted developer) what the intended
value of the parameter is to be (e.g. `path_raw_players_format` is used, as opposed to
something simpler/more generic like `path_input_format` in order to indicate that the path format
is intended to be filled with a value representing the path format of `players` data,
as described in the previous point.)
This convention is applied even in the case that only a single "significant" 
path-like format is required as an input
(or exported as an output) for a given function.
While such verboseness may seem cumbersome and/or unnecessary, this style is
useful in the cases where more than one "significant" input
is required for a given function. For example, the `clean_raw_play_by_play()`
function depends on both `raw_play_by_play` and `raw_game_summary` 
(imported from their corresponding path-like parameters).

This convention for path-like paramters also applies to first-ini arguments that
would otherwise just be named something like `data` (in accordance with `{tidyverse}`
conventions). As with the path-like arguments, this is done primarily for
"self-documenting" purposes.

+ Use all-caps variable names to indicate any one of the following:
    + a global default to pass to any argument that uses it (e.g. `VERBOSE`, `EXPORT`, etc.)
    + a "hard-coded" variable in a function (e.g. `.SUFFIX_COLS_STATS_ORDER1`)
  
+ Save result from `.export_data_from_path_format()` as `path_export` always
(no matter what the `data` or `path_format` is).


TODO
====

+ Improve `skip` logic/messages?
+ Batch clean all raw files and skip them in the future?
