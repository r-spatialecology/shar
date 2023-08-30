# shar 2.1
* Improvements
  * Remove `comp_fast` argument
  * Speed improvements of computation
  * General updates to code structure 
* Bugfixes
  * Removed `n_points` and `window` argument from reconstruction due to methodological issues

# shar 2.0.4
* Improvements
  * Remove `Sys.sleep` for verbose reconstruction

# shar 2.0.3
* Improvements
  * Allow fixed max r distance during pattern reconstruction

# shar 2.0.2
* Various
  * Update `spatstat` dependency

# shar 2.0.1
* Improvements
  * Better approach for external repo
* Various
  * Fixing typo in maintainer name

# shar 2.0.0
* Improvements
  * Using `terra` instead of `raster`
* Bugfixes
  * Naming raster values "layer"
* New functionality
  * Adding `pack_randomized()` and `unpack_randomized()` due to `terra` saving behavior

# shar 1.3.2
* Improvements
  * Improvement of `classify_habitats()` to be more variable
  * Adding breaks argument to `results_habitat_association()`
  * Adding `classint_to_vector()` helper function
  * Include new `spatstat` package structure

# shar 1.3.1
* Bugfixes
  * Bugfix in `plot.rd_pat()` and `plot.rd_mar()`
* New functionality
  * Adding internal `sample_randomized()` function used during plotting

# shar 1.3
* Improvements
  * Better documentation
  * Combine `reconstruct_pattern_homo()`, `reconstruct_pattern_cluster()`, and `reconstruct_pattern_hetero()` to `reconstruct_pattern()`
  * Replaced `plot_randomized_*()` function with generic `plot()` methods
  * Adding warnings and errors if `NA` values are present  
* New functionality
  * `list_to_randomized()` function
  * Parallelization article
* Adding JOSS paper as reference

# shar 1.2.1
* Improvements
  * `reconstruct_pattern_homo()` has arguments to specify number of points and window
  * `reconstruct_pattern_marks()` allows to have different number of points for `pattern` and `marked_pattern` argument

# shar 1.2
* Improvements
  * Include new `spatstat` package structure
  * Use GPL3 License

# shar 1.1.1
* Improvements
   * Add logo
   * Update to MIT License
   * renamed `master` to `main` branch

# shar 1.1
* Improvements
  * Use `energy_df` to get energy for printing if available
  * Updated tests
  * More stable progress printing

# shar 1.0.1
* Improvements
  * No calculation of energy for printing (too slow)

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
  * Minor speed improvement for `reconstruct_pattern()`, `reconstruct_marks()` and `calculate_energy()`
    * The starting pattern is now identical for all n_random and only the relocation process differs between randomization
    * All summary functions are only calculated for 250 steps from 0 to `rmax`
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
