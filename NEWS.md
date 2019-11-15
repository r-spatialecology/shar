# shar 1.1
* Improvements
  * Use energy_df to get energy for printing if available
  * Updated tests
  * More stable progress printing

# shar 1.0.1
* Improvements
  * No calculation of energy for printig (too slow)

# shar 1.0
* Improvements
  * Printing methods for most objects
  * Possibility to specify intervals of r for all reconstruction functions
* New functionality
  * `plot_energy()` to plot energy over iterations for reconstructed patterns
  * `reconstruct_pattern_hetero()` allows to reconstruct heterogeneous patterns
* Renameing/Structure
  * `reconstruct_pattern()` was split to three functions: `reconstruct_pattern_homo()`, `reconstruct_pattern_hetero()`, `reconstruct_pattern_cluster()`,
  * `reconstruct_marks()` is now called `reconstruct_pattern_marks()`

# shar 0.5
* Improvements
  * Annealing probability can be specified for reconstruction

# shar 0.4
* Improvements
  * Easier user experience because classes are used to specify provided input
  * `results_habitat_associations()` checks if extent of inputs is identical
  * `reconstruct_marks()` and `calculate_energy()` use now weights for the summary functions
* Bugfixes
  * Bug in `calculate_energy()` for reconstructed marks

# shar 0.3.1
* Improvements
  * Better structure of vignettes
  * Adding CONTRIBUTING.md
  * Trying to fix some failing tests for older R versions
 * New functionality
  * Allowing to translate raster only in n steps

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
