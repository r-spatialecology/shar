# shar 0.3
* Improvements
  * `plot_randomized_pattern()` now uses envelopes to plot randomized summary functions
  * `plot_randomized_pattern()` includes a quantum bar
  * `plot_randomized_pattern()` now can return plots after each other (Press <Enter>)
  * `calculate_engery()` can also calculate the energy for marked reconstructions
  * Improved warning messages
* Bugfixes
  * Explicitly C++11 compiler
* New functionality
  * `plot_randomized_pattern()` to plot randomized rasters

# shar 0.2.1
* Improvements
  * minor speed improvment for `reconstruct_pattern()`, `reconstruct_marks()` and `calculate_energy()`
    * The starting pattern is now identical for all n_random and only the relocation process differs between randomizations
    * All summary functions are only calculated for 250 steps from 0 to rmax
* Bugfixes
* New functionality
  * `rcpp_sample()` as a faster Rcpp implementation of `sample()`

# shar 0.2
* Improvements
  * Replaced `cat()` with `message()` for all printing to console
  * All defaults set  to `n_random = 1`
  * `comp_fast` argument equals TRUE if number of points exceed threshold
  * `reconstruct_pattern()` stops if energy did not decrease for n iterations
  * `reconstruct_marks()` stops if energy did not decrease for n iterations
  * `plot_randomized_pattern()` can also plot point patterns
* Bugfixes
  * Bug in `fit_point_process()` that more points as present could be removed from simulated pattern
  * Bug in `reconstruct_pattern()` that more points as present could be removed from simulated pattern

# shar 0.1
* First submission to CRAN
