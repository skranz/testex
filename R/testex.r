example = function() {
  f = function(x) {
    x
  }
  code = "library(dplyr);f(1);f(2);f(3)"
  sources = testex_sources(ex.in.fun.files = "../ex_fun.R", ex.text=code)
  et = testex_create(sources)

  f = function(x) {
    if (x==2) stop("Error")
    x*x
  }
  res = testex_run(et)
  res

  ex.df = parse.example.files(files)
  res = eval.examples(ex.df)

  ex = ex.df[1,]
  eval.example(ex)
}

#' Run example tests
#'
#' Reruns all examples that have been originally evaluated and stored in an example test.
#'
#' Returns the number of calls that now return different values or throw an error and writes a detailed log as Rmd file.
#'
#' @param et An example test originally created with \code{\link{testex_create}}
#' @param log.file The file name of the log. Should be an Rmd file.
#' @param stat.file The name of the csv file that contains statistics about how many function calls of particular type failed.
#' @param parent.env The parent environment in which examples are evaluated.
testex_run = function(et, log.file = "example_test_log.Rmd", stat.file="example_test_stats.csv", parent.env = parent.frame(), exemptions=et$exemptions) {
  restore.point("testex_run")
  writeLines(paste0(
"# Comparison of examples

", Sys.time(), " (new) vs
", et$time, " (old)
"), log.file)

  num.issues = 0
  stats = vector("list", NROW(et$ex.df))
  for (i in seq_len(NROW(et$ex.df))) {
    ex = et$ex.df[i,]
    write.log(log.file,"## ", ex$file, " ", ex$part,"\n")
    new.res = eval.example(ex,parent.env = parent.env)
    old.res = et$ex.res[[i]]
    res = compare.example.results(ex,old.res,new.res, exemptions=exemptions)
    num.issues = num.issues + res$num.issues
    write.log(log.file, res$log)
    stats[[i]] = res$stat.df
  }

  stat.df = bind_rows(stats) %>%
    group_by(fun) %>%
    summarize(differs=sum(differs), error=sum(error)) %>%
    ungroup() %>%
    arrange(-differs, -error)

  write.csv(stat.df,stat.file, row.names = FALSE)


  if (num.issues==0) {
    cat("\nNo issues found when testing examples.")
  } else {
    cat(paste0("\n",num.issues, " issues found when testing examples. See log in\n", log.file,"\n"))

    funs = setdiff(stat.df$fun, default.global.exempt.funs())
    if (length(funs)>0) {

      cat("\nIf you want to except the functions that lead to different arguments use the argument:\n")
      exemption.call = paste0("exemptions=testex_exemptions(funs = c(",paste0('"',funs,'"', collapse=", "),"))")
      cat("\n", exemption.call,"\n")
    }


  }
  invisible(list(num.issues=num.issues, stat.df=stat.df))
}



#' Create new example tests
#'
#' @param sources example sources defined by a call to \code{\link{testex_sources}}
#' @param exemptions possible exemptions of function calls or returned classes that shall not be compared, generated with \code{\link{testex_exemptions}}.
#' @param parent.env The parent environment in which examples are evaluated. By default \code{parent.frame()}.
#' @param verbose Shall extra information be shown while creating the tests?
testex_create = function(sources,exemptions=testex_exemptions(), parent.env=parent.frame(),  verbose=TRUE) {
  restore.point("testex_create")
  ex.df = parse.examples(sources)
  ex.res = eval.examples(ex.df, parent.env=parent.env)
  list(
    time=Sys.time(),
    exemptions=testex_exemptions(),
    ex.df = ex.df,
    ex.res = ex.res
  )
}

compare.example.results = function(ex,old.res, new.res, exemptions=NULL) {
  restore.point("compare.example.results")

  same = is.true(
    old.res$digest == new.res$digest &
    old.res$error == new.res$error
  )

  no.val = old.res$call.name %in% union(exemptions$funs, exemptions$exempt.error.funs) | old.res$class %in% exemptions$classes

  no.error = old.res$call.name %in% exemptions$exempt.error.funs


  ok = TRUE
  num.issues = 0
  log = NULL

  no.problem = no.error | ((no.val | same) & !new.res$error)

  if (all(no.problem)) {
    log = "Everything ok."
  } else {
    code = unlist(ex$code)
    rows = !no.val & !same & !new.res$error &!old.res$error
    code[rows] = paste0(code[rows],"\n### RESULTS DIFFER")

    rows = !no.error & !same & new.res$error
    code[rows] = paste0(code[rows],"\n### !! THROWS NEW ERROR !!")

    rows = !no.error & !same & !new.res$error & old.res$error
    code[rows] = paste0(code[rows],"\n### previous error corrected.")

    rows = !no.error & new.res$error & old.res$error
    code[rows] = paste0(code[rows],"\n### As before an error is thrown.")

    num.issues = sum(!no.problem)
    ok = num.issues == 0
    log = paste0("```{r eval=FALSE}\n", paste0(code, collapse="\n"),"\n```")

  }
  rows = which(!no.problem)
  stat.df = quick_df(fun=new.res$call.name[rows], differs=!same[rows], error=new.res$error[rows])


  list(ok=ok,stat.df = stat.df, num.issues=num.issues, log=log)
}


eval.examples = function(ex.df, parent.env = parent.frame()) {
  ex.res = lapply(seq_len(NROW(ex.df)), function(i){
    eval.example(ex.df[i,], parent.env=parent.env)
  })
  ex.res
}


eval.example = function(ex, env=create.example.env(ex, parent.env), parent.env = parent.frame(), verbose=TRUE) {
  restore.point("eval.example")

  if (verbose) {
    cat(paste0("\nEval ", ex$file, " ", ex$part))
  }
  call.name = unlist(ex$call.names)
  code = unlist(ex$code)
  df = ex %>%
    select(-extra.funs, -call.names,-code) %>%
    unnest(calls) %>%
    mutate(call.name = call.name, step = seq_len(n()), digest=NA, error=FALSE, class=NA, secs=NA_real_, code=code)
  #df$value = vector("list",NROW(df))

  for (i in seq_len(NROW(df))) {
    call = df$calls[[i]]
    start = Sys.time()
    res = try(eval(call, env),silent = TRUE)
    stop = Sys.time()
    secs = as.double(stop-start)
    df$secs[i] = secs
    if (is(res,"try-error")) {
      df$error[i] = TRUE
    } else {
      df$digest[i] = digest(res)
      df$class[i] = class(res)[1]
      #df$value[[i]] = get.short.value(res)
    }
  }
  df
}

get.short.value = function(x) {
  if (is.data.frame(x) | is.matrix(x)) {
    if (NROW(x)>1) {
      return(x[1,])
    } else {
      return(x)
    }
  }
  if (is.numeric(x) | is.character(x) | is.logical(x)) {
    if (NROW(x)>2) {
      return(x[1:3,])
    } else {
      return(x)
    }
  }
  return(NA)
}

create.example.env = function(ex, parent.env=globalenv()) {
  restore.point("create.example.env")

  extra.funs = ex$extra.funs[[1]]
  env = new.env(parent = parent.env)
  if (length(extra.funs)>0) {
    for (fn in names(extra.funs)) {
      fun = extra.funs[[fn]]
      environment(fun) = env
      env[[fn]] = fun
    }
  }
  env
}

