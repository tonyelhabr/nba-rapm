
Introduction
============

This repo ...

Highlights
==========

...

TODO
====

1. Check that new data source provides valid results.
    - Correlate with another online data source?
    - If correlations are low, check/correlate player stats 
    (e.g. plus-minus, games played, minutes played, etc.) from basektballreference.com
    versus those derived from data source.
    
2. Add functions to convert yaml to argparser. (This is useful to demonstrate
how a "config"-based script can be converted to a command line tool.)
Should probably add these "general" functions to `{teproj}` and/or `{tetidy}`,
so consider doing that first.s

3. Combine cleaning and processing scripts into one, making them dependent on inputs
from yaml/argparser.
