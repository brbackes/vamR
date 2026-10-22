# vamR 0.0.0.9011

* Speed and memory improvements: the residualization, variance, and collapse steps run in `data.table` without repeated per-group copies of the full data, and all three leave-out VA measures are computed in a single C++ pass instead of building nested tibbles.
* Residualization runs one `feols` per by-group on that group's rows instead of `feols(split = )`, which spent much of its time copying each group's rows out of a full-data model matrix.
* Residualization regressions (and the CFR test regression) now use `fixef.rm = "none"` so that singleton / perfect-fit fixed effects are kept, as in Stata.
* `dplyr`, `purrr`, and `tidyr` are no longer dependencies. Now requires `data.table` >= 1.15.0.

# vamR 0.0.0.9010

* Fixed breaking change from `fixest` (thanks to @jecowan). Now requirest `fixest` >= 0.12.1

# vamR 0.0.0.9009

* Fixed error caused when driftlimit was not specified

# vamR 0.0.0.9008

* Sped up VAM step by using `Rcpp`
* Implemented `quasi` and additional tests to ensure identical output with Stata's `quasi`
* Additional speed improvements in VAM step
* Implemented `cfr_test` to perform the CFR test and report result
