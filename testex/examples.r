testex.example = function() {
  f = function(x) {
    x
  }
  code = "library(dplyr);f(1);f(2);f(3)"
  sources = testex_sources(ex.text=code,rmd.files="testex/examples.Rmd")
  et = testex_create(sources,verbose = FALSE)
  et$ex.df

  f = function(x) {
    if (x==2) stop("Error")
    x*x
  }
  res = testex_run(et,log.file = "testex/inner_log.Rmd",stat.file = "testex/inner_stats.csv", verbose=FALSE)
  res
}
