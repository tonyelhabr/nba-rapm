
# This Series

+ [An introduction to the project](/post/nba-rapm-project-intro) (this post)
+ [The "quantitative" side of the project](/post/nba-rapm-project-quantitative)
+ [The "qualitative" side of the project](/post/nba-rapm-project-qualitative)

In this "mini"-series, I'm going to describe a personal project from which I hope others
can extract value in one way or another. Specifically, I'll dedicate
one post to what I call the "quantitative" side of the project, which may
only be relevant to those who like the NBA and statistics, and another post
to the "qualitative" side of the project, which I hope is interesting to anyone
who has worked on their own data analysis project (using R or maybe some other programming language).
And, of course, there's this post, which is just a setup to the others.

# So, What's This Project All About?

This primary goal of this project was to calculate 
[Regularized Adjusted Plus-Minus](https://www.nbastuffer.com/analytics101/regularized-adjusted-plus-minus-rapm/) 
(RAPM)---an "advanced statistic"---for NBA players. [^#] [^#]

[^#] The calculated values can be found in the set of `rapm_estimates` 
CSVs in the project's repository.

[^#] I've always told myself (and others) that I'm a "sports analytics" guy,
but I had never done something "rigorous" like this myself. Up to this point, I have
mostly been a consumer of the work and writing done by other people who
write "smart" things about sports.

Additionally,
the project provided supplementary benefits to me as a developer and, more specifically,
as an R user. Among other things, I was challenged to tackle questions about

    + how best to work with (relatively) large data sets,
    + how to create an efficient data cleaning/processing pipeline,
    + what functionality to "expose" to the user, and
    + how and when to notify the user of warnings and errors.
    
The following posts in this series describe these two 
aspects---(1) the "quantitative" efforts resulting in the RAPM values, 
and the (2) the "qualitative" process self-improvement as a programmer.

## 1. The Quantitative Side


### What is [Regularized Adjusted Plus-Minus](https://www.nbastuffer.com/analytics101/regularized-adjusted-plus-minus-rapm/) (RAPM)?

It's difficult to describe the calculation of RAPM in one or two concise sentences,
so perhaps it's better to understand it by first acknowledging how it's
used---as an "all-in-one" metric to quantify individual player skill. In leymen's
terms, the quantification is based upon how well a given player's team does
with them in the game versus out of the game, with adjustments for player skill
on both teams.
To better understand
the calculations involved, I encourage the reader to browse the link provided,
as well as the pages linked from it, such as the one on 
[Adjusted Plus-Minus](https://www.nbastuffer.com/analytics101/adjusted-plus-minus/),
upon which RAPM is based.


Additonally, RAPM comes in many different "flavors" (e.g. "Four Factors" RAPM,
"Multi-Year" RAPM, "Luck-Adjusted" RAPM). One of these is 
[Real Plus-Minus](http://www.espn.com/nba/story/_/id/10740818/introducing-real-plus-minus) (RPM),
which is published by [^#][ESPN](http://www.espn.com). [#] More specifically, it is
created former [Phoenix Suns](http://www.espn.com/nba/team/_/name/phx/phoenix-suns) affiliate 
[Jeremias Engelmann](https://twitter.com/JerryEngelmann).

[^#]:  Unfortunately, the linked article doesn't describe exactly how ESPN's version
differs from "traditional" RAPM.
(It simply states that "RPM is based on Engelmann's xRAPM (Regularized Adjusted Plus-Minus)".)
Nonetheless, I believe it is not **too** different, so using it as a "baseline"
for comparison should be reasonable.

For a couple of reason, I have chosen to use ESPN's RPM a "baseline" to compare
my results and judge the reasonability of my calculations.

    + ESPN is as reputable of a source as they come when it comes to sports statistics. [#]
    + ESPN's values are public and are simple to scrape/retrieve. [#]

[^#]: Despite this undeniable truth, some might argue that ESPN isn't exaclty
the "World Wide Leader" when it comes to **advanced** analytics.

[^#]: This is opposed to several other sources---which may post their results
occassionally on Twitter or in some other medium---but don't have a reliable
means of extracting their values.

### About the Data

The raw data comes from the `play_by_play_with_lineup/` zip file from
[this public Google Drive folder](https://drive.google.com/drive/folders/1GMiP-3Aoh2AKFCoGZ8f0teMYNlkm87dm),
which is generously provided by [Ryan Davis](https://twitter.com/rd11490), who
is himself an active member of the online NBA analytics community.
Gathering the data needed to calculate RAPM would have been a large task itself,
so I was glad to find that someone else had already done that work and was willing
to share! [^#]

[^#]: Although the NBA provides an API for accessing its data, combining
5-man lineup data with play by play data is not straightforward whatsoever.
Check out [Ryan's explanation](https://github.com/rd11490/NBA-Play-By-Play-Example)
to get a feel for the difficulty of the task.

Not having to collect this data myself---which would have been a challenge
in and of itself---gave me a nice "head start" towards
my goal of calculating RAPM. Nonetheless, this raw data---as most raw data seems 
to be---required some non-trivial cleaning and other "munging" in order to 
create a data set in the format that I could use for modelling.
Initially, I naively assumed the data was "perfect" and wrote the code to do the modeling,
only to realize that I would have to do some major work to process the raw data
when I came up with extremely "unexpected" results.

While I won't bother to describe all of the details involved in the data cleaning
and re-formatting process, here is a quick list of some of the notable parts and
the repercussions of each.

    + The home and away team point totals were swapped! Of course, leaving these values
    "as is" would certainly cause the final calculations to be erroneous.
    + Certain possession types (e.g. timeouts and other kinds of time stoppages)
    needed to be removed. Irrelevant possessions need to be filtered out
    in order to prevent "over-counting" of possession totals, which, consequently,
    can "deflate" the final calculated values.
    + Some possessions (albeit, a very small proportion of them) are listed "out-of-order" (as implied
    by a running index of possessions for each game). If these kinds of plays
    had not been addressed, the running team point totals would have been thrown off,
    which, in turn, throws off the calculation of single-possession points derived
    from the running point totals.

I don't mean to criticize Ryan whatsoever for the "cleanliness" of his provided data. [^#]
I think maybe only the first of the items listed above---regarding the swapped
team point totals---may have been an error on his part.
The second item---regarding the irrelevant possession types---isn't actually an
error at all [^#]; it's just not something that I needed/wanted in the data for modeling.
And the third item---regarding the incorrect "indexing" of plays---seems to me to
be an "upstream" data recording error. I checked some of these problemmatic
plays myself with [basketball-reference](https://www.basketball-reference.com/)'s [^#]
play-by-play logs and saw that it had the plays listed in the same manner.
(TODO: Provide an example here?)

[^#]: In fact, I tried at least one other "raw" data source and found that it did
not provide as "nice" as a start point as Ryan's data.

[^#]: After all, it's just a part of raw play-by-play data.

[^#]: While I believe Ryan retrieved his data directly from the [NBA's stats API](https://stats.nba.com/),
[basketball-reference](https://www.basketball-reference.com/) retrieves its data
from this source as well, meaning that Ryan's data and [basketball-reference](https://www.basketball-reference.com/)
would have the same incorrect play indexing.

Anyways, the need for some kind of "data validation"
compelled me to write some code (actually, lots of code) to verify
the integrity of the processed data. See my other post about this project
for some more specifics about how I went about doing this.


### Other References

+ http://www.espn.com/nba/story/_/id/10740818/introducing-real-plus-minus
+ http://www.espn.com/nba/statistics/rpm/_/year/2018
+ http://apbr.org/metrics/viewtopic.php?f=2&t=9491
    + http://web.archive.org/web/20150408042813/http://stats-for-the-nba.appspot.com:80/
    + https://sites.google.com/site/rapmstats/
    + http://basketball-analytics.gitlab.io/rapm-data/season/2016-17/regular-season/

## 2. The Qualitative Side

~~
[implementing custom conditions](https://adv-r.hadley.nz/conditions.html).
~~

### Cleaning the Data

I touched upon some of the "imperfections" in the raw data in my previous post.
I wanted to describe in more detail exactly how I addressed these issues.
to download player-specific and team-level
data from [basketball-reference](https://www.basketball-reference.com/)
and compare my "calculated" values with the "actual" values for various 
things---such as team wins and points,
individual player minutes and games played, and even five-man lineup plus-minus 
totals.


### Features

+ The project provides a [command-line interface](https://en.wikipedia.org/wiki/Command-line_interface) 
(CLI) that blends seamlessly with interactive use (i.e. not via the 
command line). This is achieved by
adept use of the `{config}` and `{argparser}` packages for providing
APIs to work with `yaml` and the command line respectively. [^1]

    To provide more detail, two `yaml` files are used: (1) a "static" one to specify
parameters that are not configurable via the command line, but which the developer
would like to specify in order to keep file names consistent across different
raw data sets.
    


### Developer and API Conventions

+ A dot prefix is used for un-"exposed" functions that are not intended to be used directly by the user.
(i.e. like how "helper" functions in a package are not exported). Part
of the reason for doing this is to maintain a "minimal" Environment pane
in the RStudio editor. (One might argue that this is a good reason to 
hide the Environment pane entirely.

+ The `auto_` prefix is used for "exposed" functions with arguments set equal to command line `args`.
This prefix indicates (to the user) that these are to be used for batch-style automated runs.

+ `path`-like function parameters 
are converted to variables borrowing their namesake.
For example, `path_raw_play_by_play` is converted to `raw_play_by_play`,
`path_raw_game_summary` is converted to `raw_game_summary`, etc. 
The season from which the data comes from is "attached" as a suffix to the corresponding file.
These `path`-like parameters are converted to/from their variable equivalents via the 
internal `.[import|export]_data_from_path()` functions.

+ The names of `path`-like parameters in exposed functions are intentionally
named in a verbose manner (somewhat compromising the "abstractness" of the functions).
This is done to implicitly indicate to the user 
(and the unacquainted developer, such as myself looking back at this in
the future)  what the intended
value of the parameter is to be (e.g. `path_play_by_play` is used, as opposed to
something simpler/more generic like `path` in order to indicate that the path format
is intended to be filled with a value representing the path format of
`play_by_play` data.
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

+ Note the following idiom:
1. Beforehand, create a ".get" function to retrieve/recreate 
some relatively trivial data  (e.g. `players`, `teams`, `game_summary`)
2. In function that is capable of recreating it and is dependent on it,
check if the data exists in the following manner:
  + Create a `.import_*_partially()` function (passing on the calling functions
  shared parameters).
  + Create a `.import_*_possibly()` function, using the aforementioned `_partially()`
  function as the `.f` parameter. (Note that this step and the previous one
  could be combined so that no explicity `partially()` function is created.)
  Set `.otherwise` to some sentinetl (e.g. `NULL`)
  + Call the `possibly()` function.
  + Check if the result is the sentinel (e.g. is.null(...))
  + If the result is the sentinel, then call the `.get_*` function. Pass the shared
  parameters from the calling function.

+ `.validate_*()` functions are not called in functions where there target of validation
is not a "requirement". (To provide an example, `.validate_season()` is called in the
`.import_data_from_path_format()` function--where `path` depends on the value of `season`---and
not from `clean_play_by_play()`, which itself calls `.import_data_from_path_format()`
and is not directly dependent on the value of `season`.

+ `season` always follows the `path_format`-like parameters in functions.
This is done simply to maintain some kind of consistency. (An equally valid choice
would have been to place `season` before the `path_format`-like parameters.)
`...` is left as the last paramter in all functions so that "side-effect"-like
paramters (i.e. `verbose`, `export`, `backup`, etc.) are passed "efficiently".

+ If a function exports some data, then return the data it exports silently with `invisible()`.

+ If more than one import data set is created by an exposed function, then
the results are returned in a named list.

+ Must include `skip` as an explicit paramter in exposed, non-"auto" functions because
the function may be exited early depending on its value. This "power" of the `skip`
parameter indicates that it cannot really be interpreted to have the same
level of significance as the "side"-effect set of paremeters.

TODO
====

+ DONE: Convert all usage of `sprintf()` to `glue::glue()` (and experiment with `glue_fmt()`).
+ Fix stint function.

================================================================================

[^]: although any packages that provide good `yaml` and CLI functionality certainly could have been used

[^]: