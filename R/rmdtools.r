parse.rmd.chunks = function(rmd, add.code = TRUE, add.args=TRUE) {
  restore.point("find.rmd.chunks")
  chunk.start = startsWith(rmd,"```{r")
  chunk.end = which(startsWith(rmd,"```") & !chunk.start)
  chunk.start = which(chunk.start)
  chunk.end = remove.verbatim.end.chunks(chunk.start,chunk.end)

  df = quick_df(ind=seq_along(chunk.start),start=chunk.start, end=chunk.end)
  if (add.code) {
    df$code = sapply(seq_len(NROW(df)), function(r) {
      paste0(rmd[(df$start[r]+1):(df$end[r]-1)], collapse="\n")
    })
  }
  if (add.args) {
    df$args = lapply(seq_along(df$start), function(i) {
      head = trimws(rmd[df$start[i]])
      head = simple.left.of(head,"```{r")
      head = simple.right.of(head, "}")
      parse.chunk.args(head)
    })
  }
  df
}

remove.verbatim.end.chunks = function(chunk.start, chunk.end) {
  restore.point("remove.verbatim.end.chunks")
  df = quick_df(
    ind =c(0, seq_along(chunk.start), seq_along(chunk.end)),
    row=c(0, chunk.start,chunk.end),
    type=c("f",
           rep("s",length(chunk.start)),
           rep("e",length(chunk.end))
         )
  )
  df = arrange(df,row)
  df$del =  df$type == "e" & !is.true(lag(df$type) == "s")

  keep.ind = df$ind[df$type=="e" & !df$del]
  chunk.end[keep.ind]
}

parse.chunk.args = function(arg.str) {
  restore.point("parse.chunk.args")
  if (!require(knitr))
    stop("Please install knitr in order to parse examples from Rmd files.")
  knitr:::parse_params(arg.str)

  #code = paste0("list(",quote_label(arg.str),")")
  #eval(base::parse(text=code,srcfile=NULL))
}


# quote the chunk label if necessary
quote_label = function(x) {
  x = gsub('^\\s*,?', '', x)
  if (grepl('^\\s*[^\'"](,|\\s*$)', x)) {
    # <<a,b=1>>= ---> <<'a',b=1>>=
    x = gsub('^\\s*([^\'"])(,|\\s*$)', "'\\1'\\2", x)
  } else if (grepl('^\\s*[^\'"](,|[^=]*(,|\\s*$))', x)) {
    # <<abc,b=1>>= ---> <<'abc',b=1>>=
    x = gsub('^\\s*([^\'"][^=]*)(,|\\s*$)', "'\\1'\\2", x)
  }
  x
}

simple.left.of = function(str, pattern) {
  pos = regexpr(pattern, str, fixed=TRUE)
  len = attr(pos, "match.length")
  substring(str, pos+len)
}

simple.right.of = function(str, pattern) {
  pos = max(gregexpr(pattern, str, fixed=TRUE)[[1]])
  substring(str,1, pos-1)
}

