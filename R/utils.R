#' convenience function for setting environment variables
#'
#' @param value of the environment variable
#' @param name of the environment variable
#' @keywords internal
set_env_var = function(value, name) {
  setter = list(value)
  names(setter) = name
  do.call(Sys.setenv, setter)
}

with_class = function(x, cls) {
  if (!inherits(x, cls)) {
    class(x) = c(cls, class(x))
  }
  x
}

ensure_trailing_slash = function(x) {
  if (!stringr::str_detect(x, "/$")) {
    x = glue::glue("{x}/")
  }
  x
}

ensure_leading_slash = function(x) {
  if (!stringr::str_detect(x, "^/")) {
    x = glue::glue("/{x}")
  }
  x
}

ensure_slashes = function(x) {
  x %>%
    ensure_leading_slash() %>%
    ensure_trailing_slash()
}

ensure_no_slash = function(x) {
  x %>%
    stringr::str_remove("^/") %>%
    stringr::str_remove("/$")
}
