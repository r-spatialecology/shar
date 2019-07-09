## shar 0.5
Improvments of existing functions

## shar 0.4
Improvments of general package structure

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

## Test environments
* Windows 10, R 3.5.1
* macOS Mojave, R 3.5.1
* https://win-builder.r-project.org (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 note

## Reverse dependencies
There are currently no reverse dependencies.
