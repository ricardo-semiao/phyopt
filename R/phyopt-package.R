#' @keywords internal
#' @importFrom mathjaxr preview_rd
"_PACKAGE"

## usethis namespace: start
#' @importFrom glue glue
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  rlang::run_on_load()
  #options(rlang_use_cli_format = TRUE)
}

on_load({
  rlang::local_use_cli()
})
