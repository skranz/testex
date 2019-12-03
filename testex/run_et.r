cat("\nRun example tests...")
library(testex)


cat("\n",digest("Hello"))
ser = serialize("Hello", NULL)
cat("\n", digest(ser,serialize=FALSE, skip=14))

cat("\n",digest(serialize("Hello",NULL),serialize = FALSE, skip=14))
cat("\n",digest(serialize("Hello",NULL,refhook = function(...) NULL),serialize = FALSE, skip=14))

ser = serialize("Hello", NULL)
digest(ser,serialize=FALSE)
cat("\n",digest(serialize("Hello",NULL),serialize = FALSE))


et = readRDS("testex/et.Rds")

exemptions=testex_exemptions(classes = c("testex_object"))
res = testex_run(et,log.file = "testex/log.Rmd",stat.file = "testex/stats.csv", exemptions=exemptions)

if (res$num.issues>0) {
  stop("Example tests failed!")
}
