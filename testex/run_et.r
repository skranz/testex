cat("\nRun example tests...")
library(testex)


cat("\n",digest("Hello", serializeVersion = 2))
ser = serialize("Hello", NULL, version=2)
cat("\n", digest(ser,serialize=FALSE, skip=14))
cat("\n",digest(serialize("Hello",NULL, version=2),serialize = FALSE, skip=14))
cat("\nNow with refhook")
cat("\n",digest(serialize("Hello",NULL,refhook = function(...) ""),serialize = FALSE, skip=14))


cat("\nNow with and without refhook skipping 40")
ser = serialize("Hello",NULL,refhook = function(...) "")
cat("\n",digest(ser,serialize = FALSE, skip=40L))
ser = serialize("Hello",NULL)
cat("\n",digest(ser,serialize = FALSE, skip=40L))


cat("\nNow ascii with and without refhook")
ser = serialize("Hello",NULL,ascii=TRUE,refhook = function(...) "")
cat("\n",digest(ser,serialize = FALSE, skip=digest:::set_skip(ser, TRUE)))
ser = serialize("Hello",NULL,ascii=TRUE)
cat("\n",digest(ser,serialize = FALSE, skip=digest:::set_skip(ser, TRUE)))


et = readRDS("testex/et.Rds")

exemptions=testex_exemptions(classes = c("testex_object"))
res = testex_run(et,log.file = "testex/log.Rmd",stat.file = "testex/stats.csv", exemptions=exemptions)

if (res$num.issues>0) {
  stop("Example tests failed!")
}
