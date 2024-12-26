# vamR 0.0.0.9010

* Fixed breaking change from `fixest` (thanks to @jecowan). Now requirest `fixest` >= 0.12.1

# vamR 0.0.0.9009

* Fixed error caused when driftlimit was not specified

# vamR 0.0.0.9008

* Sped up VAM step by using `Rcpp`
* Implemented `quasi` and additional tests to ensure identical output with Stata's `quasi`
* Additional speed improvements in VAM step
* Implemented `cfr_test` to perform the CFR test and report result
