file.after.root = function(file, root) {
  if (startsWith(file, root)) return(substring(file, nchar(root)+1))
  file
}

is.true = function(x) {
  x[is.na(x)] = FALSE
  x
}

name.of.call = function(call) {
  if (is.symbol(call)) {
    name = as.character(call)
    if (is.na(name)) return("NA")
    return(name)
  }
  name = as.character(call[[1]])
  if (name == "<-" | name == "=") {
    name = as.character(call[[3]][[1]])
  }
  name
}

deparse1 = function(call, collapse="") {
  paste0(deparse(call, width=500),collapse=collapse)
}

quick_df = function(...) {
  as_data_frame(list(...))
}

write.log = function(log.file,...) {
  txt = paste0(..., sep="")
  con = file(log.file,"at")
  writeLines(txt, con)
  close(con)
}

