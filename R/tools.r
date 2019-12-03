file.after.root = function(file, root) {
  if (startsWith(file, root)) return(substring(file, nchar(root)+1))
  file
}

is.true = function(x) {
  x[is.na(x)] = FALSE
  x
}

name.of.call = function(call) {
  ###restore.point("name.of.call")
  if (is.symbol(call)) {
    name = as.character(call)
    if (is.na(name)) return("NA")
    return(name)
  }
  name = as.character(call[[1]])
  if (identical(name,"<-") | identical(name,"=")) {
    name = name.of.call(call[[3]])
  } else if (length(name)>1) {
    name = paste0(name, collapse="")
  }
  name
}

deparse1 = function(call, collapse="\n") {
  paste0(deparse(call, width=500, control = c("keepNA", "keepInteger", "niceNames", "showAttributes", "useSource")),collapse=collapse)
}

quick_df = function(...) {
  as_tibble(list(...),validate=FALSE)
}

write.log = function(log.file,...) {
  txt = paste0(..., sep="")
  con = file(log.file,"at")
  writeLines(txt, con)
  close(con)
}

