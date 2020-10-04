cat("\nRun example tests...")
library(testex)

et = readRDS("testex/et.Rds")

exemptions=testex_exemptions(classes = c("testex_object","testex_run_results"))
res = testex_run(et,log.file = "testex/log.Rmd",stat.file = "testex/stats.csv", exemptions=exemptions, print.code=TRUE, print.output=TRUE)

if (res$num.issues>0) {
  stop("Example tests failed!")
}
