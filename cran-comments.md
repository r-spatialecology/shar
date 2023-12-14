For details changes, please see NEWS.md

# shar 2.2 
Added new functions

## shar 2.1.1
Minor bug fixes

## shar 2.1
Speed improvements

## shar 2.0.4
Small speed improvements

## shar 2.0.3
This is a re-submission. The below error has now been fixed
 
```
Package CITATION file contains call(s) to old-style citEntry(). Please use bibentry() instead.
```

Improvements of existing functions

## shar 2.0.2
Update `spatstat` dependency

## shar 2.0.1
Fixing external repo, typo maintainer name

## shar 2.0.0
* Replace `raster` with `terra` and several code changes related to this
* Fixed issue of earlier submission "Unknown, possibly misspelled, fields in DESCRIPTION: 'Remotes'"

## shar 1.3.2
Improvements of existing functions and `spatstat` update

## shar 1.3.1
Bug fixes

## shar 1.3
Improvements of general package structure

## shar 1.2.1
Improvement of existing functions

## shar 1.2
Update `spatstat` dependencies

## shar 1.1.1
Minor improvements and new license

## shar 1.1
Improvements of existing functions

## shar 1.0.1
Some minor improvements to existing functions

## shar 1.0
New functionality and renaming/splitting of some functions

## shar 0.5
Improvements of existing functions

## shar 0.4
Improvements of general package structure

## shar 0.3.1
Some minor bugfixes

## shar 0.3
Some bugfixes and improvements of existing functions as well as new functions

## shar 0.2 
Some bugfixes and improvements of existing functions

## Review CRAN submission
1. Thanks. Please omit the redundant "in R". 

* Done as suggested
  
2. Is there some reference about the method you can add in the Description field in the form Authors (year) <doi:.....>? 

* Added "Methods are mainly based on Plotkin et al. (2000) <doi:10.1006/jtbi.2000.2158> and Harms et al. (2001) <doi:10.1111/j.1365-2745.2001.00615.x>." to the description field

3. Thanks, we see: 
  running tests for arch 'i386' ... [320s] OK 
  running tests for arch 'x64' ... [356s] OK
which is together alreadyv more than 10 min which is the CRAN threshold for a package check. Can you pls simplify the test cases or run less important tests only conditionally if some env var is set that you only define on your machine?

* The test run faster now (checked on win-builder.r-project)
  running tests for arch 'i386' ... [152s] OK
  running tests for arch 'x64' ... [164s] OK

* Renamed package from `SHAR` to `shar`
