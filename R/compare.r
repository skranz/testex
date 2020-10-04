
compare.example.results = function(ex,old.res, new.res, exemptions=NULL, allow.old.errors=FALSE) {
  restore.point("compare.example.results")

  same.val = sapply(seq_along(old.res$value), function(i) {
    identical(old.res$value[[i]], new.res$value[[i]],ignore.environment = TRUE)
  })
  same = is.true(
    old.res$digest == new.res$digest &
      old.res$error == new.res$error &
      same.val
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
    compare.res.txt = sapply(which(rows), function(row) {
      compare.res.row.text(old.res[row,], new.res[row,])
    })

    code[rows] = paste0(code[rows],"\n### RESULTS DIFFER:\n\n", compare.res.txt)

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

compare.res.row.text = function(old.res, new.res) {
  restore.point("compare.res.row.text")
  old.res = as.list(old.res)[c("digest", "class", "value")]
  new.res = as.list(new.res)[c("digest", "class", "value")]

  res = diffobj::diffPrint(old.res,new.res, format="raw")
  res = paste0(res, collapse="\n")
  res
}
