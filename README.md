---
output: 
  html_document: 
    keep_md: yes
---
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

While very useful, writing comprehensive tests is also quite time intensive. While my packages don't currently have such tests, they have a lot of examples, partly in separate R files, partly inside vignettes.

If these examples worked correctly in a previous version of my package one way of testing a new package version is that all those examples again run without error and yield the same results as earlier (unless I changed something on purpose that would cause different results).

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
testex_run(et)
```

```
## 
## 2 issues found when testing examples. See log in
## example_test_log.Rmd
```

The created log file `example_test_log.Rmd` would looks as follows:

```
    # Comparison of examples
    
    2019-11-27 14:57:45 (new) vs
    2019-11-27 14:57:45 (old)
    
    ##  my_example
    
    ```{r eval=FALSE}
    f(1)
    f(2)
    ### !! THROWS NEW ERROR !!
    f(3)
    ### RESULTS DIFFER
    ```
```


