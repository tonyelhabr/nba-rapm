
# Introduction

This command line program calculates Regularized Adjusted Plus-Minus given
data in a CSV file in the same format defined by the "rapm_data.csv" file.
It uses R "in the back end" for processing, so the user must have R (and Rscript.exe) installed.

See below for an example of how the program can be called.
The command line  API is essentially as follows:

```
> [/path/to/Rscript.exe] "[R script name]" [parameters to pass to R script]
```

Note that is not assumed that Rscript executable is in the user's environment variables
(although it might be). To ensure that the program will run regardless of whether
Rscript is in the environment variables, the user should specify it explicitly.
If the Rscript executable is in the user's "Program Files" folder (and the 
user is on a Windows computer),
then the user should use the prefix "C:/PROGRA~1/"
(instead of the "C:/Users/aelhabr/Documents/" used in the examples below),
as this is defined as an alias for the "Program Files" folder on (most) Windows computers.

As shown in the example below, the "--help" option is useful for displaying
more details about the program.

```
C:\Users\aelhabr>C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R" --help
Loading required package: methods
usage: NBA RAPM Script [--] [--help] [--opts OPTS] [--dir DIR] [--input INPUT] [--output OUTPUT] [--verbose VERBOSE] [--multi_core MULTI_CORE] [--cores CORES] [--optimize OPTIMIZE] [--seed SEED] [--export EXPORT] [--import IMPORT]

A script to calculate NBA RAPM. Uses R for data processing.


flags:
  -h, --help                    show this help message and exit

optional arguments:
  -x, --opts OPTS                       RDS file containing argument values
  -d, --dir DIR                 Working directory. MUST exist. [default: C:/Users/aelhabr/Documents/projects/nba-rapm]
  -i, --input INPUT                     Input file path. MUST exist. [default: rapm_data.csv]
  -o, --output OUTPUT                   Output file path. MUST exist. [default: rapm_estimates.csv]
  -v, --verbose VERBOSE                 Boolean to display INFO messages. (Other message types are always shown.) [default: TRUE]
  -m, --multi_core MULTI_CORE                   Boolean to indicate whether or not to use multiple cores. [default: TRUE]
  -c, --cores CORES                     Number of cores to use. Only used if --multi_core is TRUE.
(Requires `{parallel}` package, as well as `{doParallel}` package on Windows.) [default: 4]
  -opt, --optimize OPTIMIZE                     Boolean to indicate whether to calculate optimal `lambda` value for
cross-validated ridge regression. If FALSE, uses default values
(148.003020 and 225.611373 for offense and defense respectively). [default: FALSE]
  -s, --seed SEED                       Seed to set prior to running cross-validated ride regression.
Only used if --optimize is TRUE. [default: 42]
  -ex, --export EXPORT                  Boolean to indicate whether to export "intermediary" data.
If TRUE, will export the following:
- offensive possession data in "wide" format to "poss-data-wide-o.rds".
- defensive possession data in "wide" format to "poss-data-wide-d.rds".
 [default: TRUE]
  -in, --import IMPORT                  Boolean to indicate whether to import "intermediary" data (i.e. from a previous run, assuming the files exist).
If TRUE, will import the following:
- offensive possession data in "wide" format to "poss-data-wide-o.rds".
- defensive possession data in "wide" format to "poss-data-wide-d.rds".
 [default: TRUE]Loading required package: methods
usage: NBA RAPM Script [--] [--help] [--verbose] [--multi_core] [--optimize] [--export] [--import] [--opts OPTS] [--input INPUT] [--output OUTPUT] [--cores CORES]

A script to calculate NBA RAPM. Uses R for data processing.
Requires `{tidyverse}` and `{glmnet}` packages.


flags:
  -h, --help                    show this help message and exit
  -v, --verbose                 Flag to display INFO messages. (Other message types are always shown.) [default: TRUE]
  -m, --multi_core                      Flag to indicate whether or not to use multiple cores. [default: TRUE]
  --optimize                    Flag to indicate whether to calculate optimal `lambda` value for ridge regression.
If FALSE, uses default values (162.430100 and 205.568700 for offense and defense respectively). [default: FALSE]
  -e, --export                  Flag to indicate whether to export "intermediary" data.
If TRUE, will export the following:
- offensive possession data in "wide" format to "poss-data-wide-o.rds".
- defensive possession data in "wide" format to "poss-data-wide-d.rds".
 [default: TRUE]
  --import                      Flag to indicate whether to import "intermediary" data (i.e. from a previous run, assuming the files exist).
If TRUE, will import the following:
- offensive possession data in "wide" format to "poss-data-wide-o.rds".
- defensive possession data in "wide" format to "poss-data-wide-d.rds".
 [default: TRUE]

optional arguments:
  -x, --opts OPTS                       RDS file containing argument values
  -i, --input INPUT                     Input file path. [default: rapm_data.csv]
  -o, --output OUTPUT                   Output file path. [default: rapm_estimates.csv]
  -c, --cores CORES                     Number of cores to use. Only used if --multi_core is TRUE.
(Requires `{parallel}` package, as well as `{doParallel}` package on Windows.)
```

See the next section for some examples of how the program may be used.

The output should be a CSV that looks as follows:

```
id,drapm,orapm,rapm
101158,-1.6307051074857595,8.565990386465952,6.935285278980192
101235,3.1889848331538477,3.696619884757517,6.885604717911365
2765,2.2681452149375905,4.079842597646357,6.347987812583947
101120,2.467629620151526,3.158201248876278,5.625830869027804
201286,-0.46397882517825323,5.839008890222761,5.375030065044508
201607,-0.840273287871746,5.819509449806114,4.979236161934368
202862,5.355286755409465,-1.2587905019708125,4.096496253438652
```

# Examples

Being explicit with the argument objects that must exist. (`dir` and`input`) and specifying `output` explicitly.
This is the best way for the user to ensure that everything works on their system
(because the location of the script and the name of the input file will be dependent on the user).

Note that the warnings seen in this example (and the others) arise because 
the boolean `import` is set to `TRUE` by default, but there are
no files to import from a previous run.


```
C:\Users\aelhabr>C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R" --dir "C:/Users/aelhabr/Documents/projects/nba-rapm/" --input "rapm_data.csv" --output "my output.csv"
Loading required package: methods
INFO: `dir` is "C:/Users/aelhabr/Documents/projects/nba-rapm/".
INFO: `input` is "rapm_data.csv".
INFO: `output` is "rapm_estimates.csv".
WARNING: Can't import because orapm possession data from "poss-data-wide-o.rds".
WARNING: Can't import because drapm possession data from "poss-data-wide-d.rds".
INFO: It may take some time to process the data...
INFO: Trying to export "o" possession data to "poss-data-wide-o.rds".
INFO: Trying to export "d" possession data to "poss-data-wide-d.rds".
INFO: Exported final rapm estimates to "my_output.csv".
```

Not being explicit with the required argument objects.
Instead, depending on the default values being correct and using the default `output` value.
(Again, the alternative input files to use do not exist, so `input` is used directly.)


```
C:\Users\aelhabr>C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R"
Loading required package: methods
INFO: `dir` is "C:/Users/aelhabr/Documents/projects/nba-rapm/".
INFO: `input` is "rapm_data.csv".
INFO: `output` is "rapm_estimates.csv".
WARNING: Can't import because orapm possession data from "poss-data-wide-o.rds".
WARNING: Can't import because drapm possession data from "poss-data-wide-d.rds".
INFO: It may take some time to process the data...
INFO: Trying to export "o" possession data to "poss-data-wide-o.rds".
INFO: Trying to export "d" possession data to "poss-data-wide-d.rds".
INFO: Exported final rapm estimates to "rapm_estimates.csv".
```

Using "cached" input files (from a previous run), meaning that `input` is ignored.
(Also, depending on default values).

```
C:\Users\aelhabr>C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R"
Loading required package: methods
INFO: `dir` is "C:/Users/aelhabr/Documents/projects/nba-rapm".
INFO: `input` is "rapm_data.csv".
INFO: `output` is "rapm_estimates.csv".
INFO: It may take some time to process the data...
INFO: Importing orapm possession data from "poss-data-wide-o.rds".
INFO: Importing drapm possession data from "poss-data-wide-d.rds".
INFO: Exported final rapm estimates to "rapm_estimates.csv".
```

Ignoring "cached" input files (from a previous run). (Also, depending on default values).

```
C:\Users\aelhabr>C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R" -in FALSE
Loading required package: methods
INFO: `dir` is "C:/Users/aelhabr/Documents/projects/nba-rapm".
INFO: `input` is "rapm_data.csv".
INFO: `output` is "rapm_estimates.csv".
INFO: It may take some time to process the data...
INFO: Trying to export "o" possession data to "poss-data-wide-o.rds".
INFO: Trying to export "d" possession data to "poss-data-wide-d.rds".
INFO: Exported final rapm estimates to "rapm_estimates.csv".
```

Setting some of the arguments related to processing and choosing values for lambda.
(Also, depending on default values).

```
C:\Users\aelhabr>C:/Users/aelhabr/Documents/R/R-3.4.4/bin/Rscript.exe "C:/Users/aelhabr/Documents/projects/nba-rapm/_main.R" --cores 4 --optimize TRUE --seed 1
Loading required package: methods
INFO: `dir` is "C:/Users/aelhabr/Documents/projects/nba-rapm".
INFO: `input` is "rapm_data.csv".
INFO: `output` is "rapm_estimates.csv".
INFO: Importing orapm possession data from "poss-data-wide-o.rds".
INFO: Importing drapm possession data from "poss-data-wide-d.rds".
INFO: It may take some time to optimize...
INFO: Found 178.266887 to be the optimal `lambda` for orapm.
INFO: Found 225.611373 to be the optimal `lambda` for drapm.
Warning message:
executing %dopar% sequentially: no parallel backend registered
INFO: Exported final rapm estimates to "rapm_estimates.csv".
```

# Package Requirements

Rscript.exe will load the following R packages automatically at run-time.
These should not be problematic because they come "pre-installed" with R.

- `{base}`
- `{methods}`
- `{datasets}`
- `{utils}`
- `{grDevices}`
- `{graphics}`
- `{stats}`

Additionally, the following packages (3) are required. Their dependencies
are listed as sub-lists.

- `{argparser}`
- `{tidyverse}`
    - `{broom}`
    - `{cli}`
    - `{crayon}`
    - `{dplyr}`
    - `{dbplyr}`
    - `{forcats}`
    - `{haven}`
    - `{hms}`
    - `{httr}`
    - `{jsonlite}`
    - `{lubridate}`
    - `{magrittr}`
    - `{modelr}`
    - `{pillar}`
    - `{purrr}`
    - `{readr}`
    - `{readxl}`
    - `{reprex}`
    - `{rlang}`
    - `{rstudioapi}`
    - `{rvest}`
    - `{stringr}`
    - `{tibble}`
    - `{tidyr}`
    - `{xml2}`
- `{glmnet}`
    - `{Matrix}`
    - `{utils}`
    - `{foreach}`

If the `multi_core`parameter is `TRUE`, then the following packages
should also be installed. If the system is not a Windows system, then
`multi_core` is ignored.

- `{parallel}`
    - `{tools}`
    - `{compiler}`
- `{doParallel}`(for Windows) [1]
    - `{foreach}`
    - `{iterators}`
    - `{parellel}`
    - `{utils}`


[1]: I believe the equivalent package for Mac OS X and Linux is `doMC`.
