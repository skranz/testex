[![Build Status](https://travis-ci.org/skranz/testex.svg?branch=master)](https://travis-ci.org/skranz/testex)

## Tools for testing packages by comparing results from examples

The standard approach of unit testing, e.g. via [testthat](https://testthat.r-lib.org/) is to write explicit test commands, like

```r
test_that("Distinct roots", {

    roots <- real.roots(1, 7, 12)

    expect_that( roots, is_a("numeric") )
    expect_that( length(roots), equals(2) )
    expect_that( roots[1] < roots[2], is_true() )
})
```
Copied from [this](https://www.johndcook.com/blog/2013/06/12/example-of-unit-testing-r-code-with-testthat/) blog entry on testing.

While very useful, writing comprehensive tests is also quite time intensive. While I have not written explicit tests for my packages, they have a lot of examples, partly in separate R files, partly inside vignettes.

If these examples worked correctly in a previous version of my package, one way of testing a new package version is that all those examples again run without error and yield the same results as earlier (unless I changed something on purpose that would cause different results).

The package `testex` shall help to test your package using your examples. Install it from Github via:

```
devools::install_github("skranz/testex")
```

Below is a simple illustration. 


```r
# Assume f is a function in your package
f = function(x) {
  x
}

# Some example code
code = "
f(1)
f(2)
f(3)
"

library(testex)

# Specify all example sources. Can be files
# or direct code as strings.
sources = testex_sources(ex.text=c(myexample=code))

# Create an example test object by running
# all examples. It uses all objects from
# the global environment and currently loaded
# packages.
et = testex_create(sources)

# You may want to save et somewhere
# saveRDS(et, "example_tests.Rds")
```

Let us mimick a change in the package by changing the function `f`:


```r
f = function(x) {
  if (x==2) stop("Error")
  x*x
}

# Load old example test
# et = readRDS("example_tests.Rds")

# Evaluate all examples again
# and compare with previous results
res = testex_run(et)
```

You will see the message:
```
Eval  myexample
2 issues found when testing examples. See log in
example_test_log.Rmd
```
The created log file `example_test_log.Rmd` will show you where the errors have occured in a human readable form.

The returned list `res` will look like:
```
res

$`num.issues`
[1] 2

$issue.df
# A tibble: 2 x 7
  file  part      call       class   fun   differs error
  <chr> <chr>     <list>     <chr>   <chr> <lgl>   <lgl>
1 ""    myexample <language> numeric f     TRUE    TRUE 
2 ""    myexample <language> numeric f     TRUE    FALSE

$stat.df
# A tibble: 1 x 3
  fun   differs error
  <chr>   <int> <int>
1 f           2     1
```

## Integrating with Travis CI

For an example of how to combine `testex` with [Travis CI](https://docs.travis-ci.com/user/languages/r/), take a look at the github page of my [RelationContracts](https://github.com/skranz/RelationalContracts) package. In particular look at the [.travis.yml](https://github.com/skranz/RelationalContracts/blob/master/.travis.yml) file and the folder [testex](https://github.com/skranz/RelationalContracts/tree/master/testex).

You can also look at the corresponding files on the [testex](https://github.com/skranz/testex) github page, but those might be less clear due to the recursive nature of testing `testex` with `testex`.
