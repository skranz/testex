#' Create exceptions, i.e. functions or classes that will not be compared
#'
testex_exceptions = function(funs=NULL, classes=NULL, ignore.error.funs=NULL, global.ignore.funs = default.global.ignore.funs(), global.ignore.error.funs=NULL) {
  list(funs = union(funs, global.ignore.funs), classes=classes, ignore.error.funs = union(ignore.error.funs, global.ignore.error.funs))
}



default.global.ignore.funs = function() {
  c("library", "runif","rnorm","rexp")
}


