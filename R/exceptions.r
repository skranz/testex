#' Create exemptions, i.e. functions or classes that will not be compared
#'
testex_exemptions = function(funs=NULL, classes=NULL, exempt.error.funs=NULL, global.exempt.funs = default.global.exempt.funs(), global.exempt.error.funs=NULL, global.classes=default.global.exempt.classes()) {
  list(funs = union(funs, global.exempt.funs), classes=union(classes, global.classes), exempt.error.funs = union(exempt.error.funs, global.exempt.error.funs))
}

default.global.exempt.classes = function() {
  c("gg","ggplot")
}

default.global.exempt.funs = function() {
  c("library", "runif","rnorm","rexp")
}


