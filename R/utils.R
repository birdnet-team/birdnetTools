#' Convert a datetime format string into a regular expression
#'
#' Internal helper for [birdnet_add_datetime()]. Translates a
#' [strptime()]-style format (e.g. `"%Y%m%d_%H%M"`) into a regular expression
#' that matches text of the same shape, so users can describe a file name
#' pattern with a familiar format instead of writing a regex.
#'
#' Supported codes are `%Y` (4-digit year, 1900-2099), `%y`, `%m`, `%d`, `%H`,
#' `%M` and `%S` (2 digits each). All other characters are matched literally,
#' with regex special characters escaped. The regex only checks the *shape* of
#' the text (the number of digits in each position), not whether the values
#' are valid dates. That check is left to [strptime()]. The match is wrapped in
#' lookarounds so it only matches a digit run of exactly this shape and never
#' part of a longer run of digits (e.g. the prefix in `00014530_20230518T133500`).
#'
#' @param fmt A single character string giving a [strptime()]-style format.
#'
#' @return A single character string containing a regular expression (using
#'   lookarounds, so it needs the ICU engine used by stringr).
#'
#' @examples
#' \dontrun{
#' format_to_regex("%Y%m%d_%H%M")
#'
#' stringr::str_extract(
#'   "00014530_20230518T133500.WAV",
#'   format_to_regex("%Y%m%dT%H%M%S")
#' )
#' }
#'
#'
#' @keywords internal
format_to_regex <- function(fmt) {
  tokens <- c(
    "%Y" = "(?:19|20)[0-9]{2}", "%y" = "[0-9]{2}",
    "%m" = "[0-9]{2}", "%d" = "[0-9]{2}",
    "%H" = "[0-9]{2}", "%M" = "[0-9]{2}", "%S" = "[0-9]{2}"
  )
  # escape regex special characters in the literal parts (e.g. "." or "-")
  esc <- stringr::str_replace_all(fmt, "([.|()\\[\\]{}^$*+?\\\\-])", "\\\\\\1")
  rx <- stringr::str_replace_all(esc, tokens)
  # only match a digit run of exactly this shape, so prefix digits are ignored
  paste0("(?<![0-9])", rx, "(?![0-9])")
}
