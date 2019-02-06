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
