setwd("C:/libraries/testex/testex")

library(testex)

sources = testex_sources(ex.in.fun.files = "testex/examples.r")
et = testex_create(sources)
saveRDS(et, "testex/et.Rds")
