#' Specify sources files or texts from which examples shall be extracted.
#'
#' There are different sources that differ how examples are written in the file
#'
#' @param ex.in.fun.files Files that contain one or several examples within functions that have no argument. E.g. a function named \code{my.example()} whose body contains the examples.
#' @param ex.files Files whose code directly specifies an examples, i.e. examples are not within functions.
#' @param rmd.files Rmd files whose chunks specify examples. Chunks will be evaluated sequentially step by step. Chunks with \code{include=FALSE} or \code{eval=FALSE} will be ignored.
#' @param ex.in.fun.text Similar to \code{ex.in.fun.files} but code will be provided as string.
#' @param ex.text Similar to \code{ex.files} but code will be provided as string.

testex_sources = function(ex.in.fun.files=NULL, ex.files=NULL, rmd.files = NULL,ex.in.fun.text=NULL, ex.text=NULL) {
  res = list(ex.in.fun.files=ex.in.fun.files, ex.files=ex.files, rmd.files=rmd.files, ex.text=ex.text, ex.in.fun.text=ex.in.fun.text)
  class(res) = c("testex_sources","list")
  res
}

#' Parse examples.
#'
#' @param sources example sources defined by a call to \code{\link{testex_sources}}
parse.examples = function(sources) {
  restore.point("parse.examples")
  if (!is(sources, "testex_sources"))
    stop("Please create the examples sources with a call to testex_sources.")
  res1 = bind_rows(lapply(sources$ex.in.fun.files,parse.ex.in.fun.file))
  res2 = bind_rows(lapply(sources$ex.in.fun.text,parse.ex.in.fun.text))


  if (!is.null(names(sources$ex.text))) {
    parts = names(sources$ex.text)
  } else {
    parts = as.character(seq_along(sources$ex.text))
  }
  res3 = bind_rows(lapply(seq_along(sources$ex.text), function(i) {
    parse.ex.text(sources$ex.text[i], part=parts[i])
  }))

  res4 = bind_rows(lapply(seq_along(sources$ex.file), function(i) {
    parse.ex.file(sources$ex.file[i])
  }))


  bind_rows(res1,res2, res3, res4)
}

parse.ex.file = function(file, part="", root.dir=getwd()) {
  restore.point("parse.ex.file")
  text = readLines(file,warn=FALSE)
  parse.ex.text(text, file=file.after.root(file, root.dir), part="")
}


parse.ex.text = function(text, file="", part="") {
  restore.point("parse.ex.text")

  env = new.env()
  calls = as.list(parse(text=text))
  call.names = unlist(lapply( calls, name.of.call))

  code.li = unlist(lapply( calls, deparse1))

  ex.df = tibble(file=file, part=part, calls=list(calls), call.names = list(call.names), code=list(code.li),  extra.funs = list(list()))
  ex.df
}


parse.ex.in.fun.text = function(text) {
  parse.ex.in.fun.file(text=text)
}

parse.ex.in.fun.file = function(file, root.dir=getwd(), text=NULL) {
  restore.point("parse.fun.example.file")
  env = new.env()
  if (missing(file) & !is.null(text)) {
    calls = parse(text=text)
    eval(calls, env)
    file = ""
  } else {
    source(file, env)
  }

  fns = as.vector(lsf.str(envir = env))
  if (length(fns)==0) {
    cat(paste0("\nNo functions found in example file ", file))
    return(NULL)
  }

  fn = fns[[1]]
  no.args = sapply(fns, function(fn) {
    fun = env[[fn]]
    length(formalArgs(fun))  == 0
  })
  extra.fns = fns[!no.args]
  fns = fns[no.args]

  funs = lapply(fns, function(fn) env[[fn]])
  call.li = lapply(funs, function(fun) {
    bo = body(fun)
    calls = vector("list",length(bo)-1)
    for (i in setdiff(seq_along(bo),1)) {
      calls[i-1] = bo[i]
    }
    calls
  })


  extra.funs = lapply(extra.fns, function(fn) {
    fun = env[[fn]]
    environment(fun) = emptyenv()
    fun
  })
  names(extra.funs) = extra.fns

  if (startsWith(file, root.dir)) {
    file = substring(file, nchar(root.dir))
  }
  call.names = lapply(call.li, function(calls) {
    lapply( calls, name.of.call)
  })

  code.li = lapply(call.li, function(calls) {
    unlist(lapply( calls, deparse1))
  })


  ex.df = tibble(file=file.after.root(file, root.dir), part=fns, calls=call.li, call.names = call.names, code=code.li,  extra.funs = replicate(NROW(fns),list(extra.funs)))
  ex.df
}
