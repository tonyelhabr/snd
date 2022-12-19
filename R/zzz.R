
## https://github.com/tonyelhabr/bdb2021/blob/master/R/zzz.R
.onLoad <- function(libname, pkgname) {

  ops <- options()
  pkg_ops <- list(
    snd.dir.data = 'data',
    snd.dir.figs = 'figs'
  )

  toset <- !(names(pkg_ops) %in% names(ops))
  if(any(toset)) options(pkg_ops[toset])

  invisible()
}
