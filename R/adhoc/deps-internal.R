
# Reference: https://stackoverflow.com/questions/44143110/visualizing-r-function-dependencies
# library("DependenciesGraphs")
# dep1 <- funDependencies("package:dplyr", "select")
# plot(dep1)

ex <- profr::profr(clean_play_by_play_auto())
ex
pv <- profvis::profvis(munge_play_by_play_auto())
pv
