cat("\nRun example tests...")
library(testex)


cat("\nJust digest")
cat("\n",digest("Hello", serializeVersion = 2))

cat("\nManually serialize with version 2")
ser = serialize("Hello", NULL, version=2)
cat("\n", digest(ser,serialize=FALSE, skip=14L))


cat("\nNow with refhook")
ser = serialize("Hello",NULL,refhook = function(...) "", version=2)
cat("\n",digest(ser,serialize = FALSE, skip=14L))


et = readRDS("testex/et.Rds")

exemptions=testex_exemptions(classes = c("testex_object"))
res = testex_run(et,log.file = "testex/log.Rmd",stat.file = "testex/stats.csv", exemptions=exemptions, cat.code=TRUE, cat.output=TRUE)

if (res$num.issues>0) {
  stop("Example tests failed!")
}
