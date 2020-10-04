example = function() {
  code = "
  set.seed(1)
  f = function(x) {
    if (x==2) stop('Error')
    x*x*runif(1)
  }

  library(dplyr);f(1);f(2);f(3)
  "
  sources = testex_sources(rmd.files = "../test.Rmd", ex.text=code)
  et = testex_create(sources, stop.on.error = FALSE)
  et$ex.df
  df = et$ex.df
  res = testex_run(et, allow.old.errors = TRUE)
  res
}


default.ignore.test.calls = function() {
  # Some calls return system specific results and
  # their value will be ignored (not the error state though)
  c("library")
}


#' Create new example tests
#'
#' @param sources example sources defined by a call to \code{\link{testex_sources}}
#' @param exemptions possible exemptions of function calls or returned classes that shall not be compared, generated with \code{\link{testex_exemptions}}.
#' @param parent.env The parent environment in which examples are evaluated. By default \code{parent.frame()}.
#' @param verbose Shall extra information be shown while creating the tests?
testex_create = function(sources,exemptions=testex_exemptions(), parent.env=parent.frame(), print.code=FALSE, print.output = FALSE, print.error=TRUE, stop.on.error=TRUE, verbose=TRUE, mode = c("v","d","dv")[1], ignore.calls = default.ignore.test.calls()) {
  restore.point("testex_create")
  ex.df = parse.examples(sources)
  ex.res = eval.examples(ex.df, parent.env=parent.env, print.code=print.code, print.output=print.output, print.error=print.error, stop.on.error=stop.on.error, verbose=verbose, mode=mode, ignore.calls=ignore.calls)
  et = list(
    time=Sys.time(),
    exemptions=testex_exemptions(),
    ex.df = ex.df,
    ex.res = ex.res,
    mode = mode
  )
  class(et) = c("testex_object","list")
  et
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
#' @param verbose Shall extra information be shown?
testex_run = function(et, log.file = "example_test_log.Rmd", stat.file=NULL, parent.env = parent.frame(), exemptions=et$exemptions, print.code=FALSE, print.output=FALSE, verbose=TRUE, allow.old.errors=FALSE) {
  restore.point("testex_run")
  writeLines(paste0(
"# Comparison of examples

", Sys.time(), " (new) vs
", et$time, " (old)
"), log.file)

  num.issues = 0
  ex.df = et$ex.df
  issues = vector("list", NROW(ex.df))
  for (i in seq_len(NROW(ex.df))) {
    ex = ex.df[i,]
    if (!is.null(ex$include))
      if (ex$include==FALSE) next
    write.log(log.file,"## ", ex$file, " ", ex$part,"\n")
    old.res = et$ex.res[[i]]
    ignore.calls = unique(old.res$call.name[old.res$ignore])


    # Always evaluate example in new
    # environment unless we
    # have examples from the same Rmd file
    make.new.env = TRUE
    if (i > 1) {
      is.rmd = tolower(tools::file_ext(ex$file))=="rmd"
      if (is.rmd & (ex.df$file[i-1] == ex$file))
        make.new.env = FALSE
    }
    if (make.new.env)
      env = create.example.env(ex, parent.env)

    new.res = eval.example(ex,env = env, parent.env = parent.env, verbose = verbose, print.code=print.code, print.output=print.output,mode=et$mode, ignore.calls=ignore.calls)


    res = compare.example.results(ex,old.res,new.res, exemptions=exemptions, allow.old.errors=allow.old.errors)
    num.issues = num.issues + res$num.issues
    write.log(log.file, res$log)
    issues[[i]] = res$issue.df
  }

  issue.df = bind_rows(issues)

  stat.df = issue.df %>%
    group_by(fun) %>%
    summarize(differs=sum(differs), error=sum(error)) %>%
    ungroup() %>%
    arrange(-differs, -error)

  if (!is.null(stat.file))
    write.csv(stat.df,stat.file, row.names = FALSE)


  if (num.issues==0) {
    if (verbose)
      cat("\nNo issues found when testing examples.")
  } else {
    if (verbose) {
      cat(paste0("\n",num.issues, " issues found when testing examples. See log in\n", log.file,"\n"))

      funs = setdiff(stat.df$fun, exemptions$funs)
      if (length(funs)>0) {
        funs = setdiff(stat.df$fun, default.global.exempt.funs())

        cat("\nIf you want to exempt the functions that lead different results call testex_run with the argument:\n")
        exemption.call = paste0("exemptions=testex_exemptions(funs = c(",paste0('"',funs,'"', collapse=", "),"))")
        cat("\n", exemption.call,"\n")
      }
      classes = setdiff(unique(issue.df$class),exemptions$classes)
      spec.classes = setdiff(classes, c("numeric","character","list","data.frame","matrix","logical","tbl_df"))
      if (length(spec.classes)>0) {
        ex.classes = setdiff(spec.classes, default.global.exempt.classes())

        cat("\nIf you want to exempt special classes that lead different results call testex_run with the argument:\n")
        exemption.call = paste0("exemptions=testex_exemptions(classes = c(",paste0('"',ex.classes,'"', collapse=", "),"))")
        cat("\n", exemption.call,"\n")
      }
    }
  }
  res = list(num.issues=num.issues, issue.df=issue.df,  stat.df=stat.df)
  class(res) = c("testex_run_results","list")
  invisible(res)
}



compare.example.results = function(ex,old.res, new.res, exemptions=NULL, allow.old.errors=FALSE) {
  restore.point("compare.example.results")

  same = is.true(
    identical(old.res$digest,new.res$digest) &
    identical(old.res$error,new.res$error) &
    identical(old.res$value, new.res$value)
  )

  no.val = old.res$call.name %in% union(exemptions$funs, exemptions$exempt.error.funs) | old.res$class %in% exemptions$classes

  no.error = old.res$call.name %in% exemptions$exempt.error.funs


  ok = TRUE
  num.issues = 0
  log = NULL

  if (allow.old.errors) {
    no.problem = (no.val | same)
  } else {
    no.problem = no.error | ((no.val | same) & !new.res$error)
  }

  if (all(no.problem)) {
    log = "Everything ok."
  } else {
    code = unlist(ex$code)
    rows = !no.val & !same & !new.res$error &!old.res$error
    code[rows] = paste0(code[rows],"\n### RESULTS DIFFER")

    rows = !no.error & !same & new.res$error
    code[rows] = paste0(code[rows],"\n### !! THROWS NEW ERROR !!\n", new.res$error.msg[rows])

    rows = !no.error & !same & !new.res$error & old.res$error
    code[rows] = paste0(code[rows],"\n### previous error corrected.")

    rows = !no.error & new.res$error & old.res$error
    code[rows] = paste0(code[rows],"\n### As before an error is thrown.\n", new.res$error.msg[rows])

    num.issues = sum(!no.problem)
    ok = num.issues == 0
    log = paste0("```{r eval=FALSE}\n", paste0(code, collapse="\n"),"\n```")

  }
  rows = which(!no.problem)

  if (length(rows)>0) frow=1 else frow = rows
  issue.df = quick_df(file=ex$file[frow], part=ex$part[frow],call=old.res$calls[rows], class=old.res$class[rows], fun=old.res$call.name[rows], differs=!same[rows], error=new.res$error[rows], code=old.res$code[rows], error.msg = new.res$error.msg[rows])

  list(ok=ok,issue.df = issue.df, num.issues=num.issues, log=log)
}


eval.examples = function(ex.df, parent.env = parent.frame(), print.code=FALSE, print.output=FALSE,  print.error=FALSE, stop.on.error=FALSE,  verbose=TRUE, mode="v", ignore.calls=NULL) {
  restore.point("eval.examples")

  res.li = vector("list",NROW(ex.df))

  for (i in seq_len(NROW(ex.df))) {
    ex = ex.df[i,]

    # Always evaluate example in new
    # environment unless we
    # have examples from the same Rmd file
    make.new.env = TRUE
    if (i > 1) {
      is.rmd = tolower(tools::file_ext(ex$file))=="rmd"
      if (is.rmd & (ex.df$file[i-1] == ex$file))
        make.new.env = FALSE
    }
    if (make.new.env)
      env = create.example.env(ex, parent.env)

    res.li[[i]] = eval.example(ex, env=env, parent.env=parent.env, verbose=verbose, print.code=print.code, print.output=print.output,  print.error=print.error, stop.on.error=stop.on.error, mode=mode, ignore.calls =ignore.calls)
  }

  res.li
}


eval.example = function(ex, env=create.example.env(ex, parent.env), parent.env = parent.frame(), print.code=FALSE, print.output=FALSE,  print.error=FALSE,  verbose=TRUE, stop.on.error=FALSE, mode=c("v","d","dv")[1], ignore.calls=ignore.calls) {
  restore.point("eval.example")

  if (verbose) {
    cat(paste0("\nEval ", ex$file, " ", ex$part))
  }
  call.name = unlist(ex$call.names)
  code = unlist(ex$code)
  df = ex %>%
    select(-extra.funs, -call.names,-code) %>%
    unnest(calls) %>%
    mutate(call.name = call.name, step = seq_len(n()), digest="", error=FALSE, class="", secs=NA_real_, code=code, error.msg = "", ignore = call.name %in% ignore.calls)
  df$value = vector("list",NROW(df))

  for (i in seq_len(NROW(df))) {
    restore.point("eval.example.inner")
    call = df$calls[[i]]
    if (print.code) {
      cat("\n>",df$code[i])
    }
    start = Sys.time()
    res = try(eval(call, env),silent = TRUE)
    stop = Sys.time()
    if (print.output) {
      assign = FALSE
      if (length(call)>1) {
        sym = as.character(call[[1]])
        if (sym=="<-" | sym == "=")
          assign = TRUE
      }
      if (!assign) {
        cat("\n")
        print(res)
      }
    }
    secs = as.double(stop-start)
    df$secs[i] = secs
    if (is(res,"try-error")) {

      df$error[i] = TRUE
      df$error.msg[i] = paste0(as.character(res), collapse="\n")
      if (print.error) {
        if (!print.code) {
          cat("Error when evaluating:\n",df$code[i] )
        }
        if (!stop.on.error)
          cat("\n",as.character(res))
      }
      if (stop.on.error) {
        msg = paste0(as.character(res),"\nCall with the argument stop.on.error = FALSE to evaluate examples even if there are errors")
        stop(msg, call. = FALSE)
      }

    } else {
      # Serialize ignoring environment references
      if (df$ignore[[i]]) next

      if (mode=="d" | mode=="dv") {
        ser = serialize(res, NULL, refhook=function(...) "", version=2)
        df$digest[i] = digest(ser,serialize = FALSE, skip=14)
      }
      df$class[i] = class(res)[1]
      if (mode == "v" | mode == "dv") {
        df$value[[i]] = get.short.value(res)
      }
    }
  }
  df
}

get.short.value = function(x) {
  restore.point("get.short.value")
  if (is.data.frame(x) | is.matrix(x)) {
    if (NROW(x)>2) {
      return(list(short = x[c(1,NROW(x)),], dim=dim(x)))
    } else {
      return(x)
    }
  }
  if (is.numeric(x) | is.character(x) | is.logical(x)) {
    if (NROW(x)>2) {
      return(list(short=x[1,NROW(x)],nrow=nrow(x)))
    } else {
      return(x)
    }
  }
  return("")
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

