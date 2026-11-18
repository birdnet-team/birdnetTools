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




#' Allocate samples evenly across bins
#'
#' Internal helper for [birdnet_subsample()]. Decides how many detections to
#' draw from each confidence bin when sampling evenly across bins
#' (`method = "even_stratified"`).
#'
#' The function works in passes. In each pass, every bin that still has
#' unsampled detections gets an equal share of the samples left. A bin with
#' fewer detections than its share gives everything it has, and the unused
#' part is carried into the next pass and shared among the remaining bins. When
#' fewer samples are left than there are open bins, one extra sample goes to
#' each of a randomly chosen subset of bins, so the result can depend on the
#' random number generator (set a seed beforehand for reproducible results).
#' Unlike a fixed `floor(n / n_bins)` quota, this always returns the full
#' number of samples when enough detections exist.
#'
#' @param counts A numeric vector of whole numbers giving the number of
#'   detections in each bin.
#' @param n A single positive whole number giving the total number of
#'   detections to sample.
#'
#' @return An integer vector the same length as `counts`, giving the number of
#'   detections to sample from each bin. No value exceeds the matching value in
#'   `counts`, and the values sum to `min(n, sum(counts))`.
#'
#' @examples
#' \dontrun{
#' # small bins give everything they have; the rest is shared by the others
#' allocate_even(counts = c(2, 5, 1000, 1000), n = 99)
#' #> 2 5 46 46
#'
#' # fewer detections than n: everything is kept
#' allocate_even(counts = c(3, 4), n = 100)
#' #> 3 4
#' }
#'
#' @seealso [allocate_proportional()], [birdnet_subsample()]
#'
#' @keywords internal
allocate_even <- function(counts, n) {
  alloc <- rep(0, length(counts))
  open <- seq_along(counts)
  remaining <- min(n, sum(counts))

  while (remaining > 0 && length(open) > 0) {
    quota <- remaining %/% length(open)

    if (quota == 0) {
      # fewer samples left than open bins: one each for a random subset
      pick <- open[sample.int(length(open), remaining)]
      alloc[pick] <- alloc[pick] + 1
      break
    }

    take <- pmin(quota, counts[open] - alloc[open])
    alloc[open] <- alloc[open] + take
    remaining <- remaining - sum(take)
    open <- open[alloc[open] < counts[open]]
  }

  return(as.integer(alloc))
}


#' Allocate samples in proportion to bin size
#'
#' Internal helper for [birdnet_subsample()]. Decides how many detections to
#' draw from each confidence bin when sampling in proportion to the number of
#' detections in each bin (`method = "proportional_stratified"`).
#'
#' Each bin's share is `n * counts / sum(counts)`. Shares are rounded down, and
#' the samples left over from rounding go to the bins with the largest
#' fractional parts (the largest-remainder method). A bin can never give more
#' than it has, so if a bin's share exceeds its size, the shortfall is
#' re-spread in proportion across the bins that still have detections. The total
#' is therefore always the full number of samples when enough detections exist.
#' Bins with very few detections can receive none.
#'
#' @param counts A numeric vector of whole numbers giving the number of
#'   detections in each bin.
#' @param n A single positive whole number giving the total number of
#'   detections to sample.
#'
#' @return An integer vector the same length as `counts`, giving the number of
#'   detections to sample from each bin. No value exceeds the matching value in
#'   `counts`, and the values sum to `min(n, sum(counts))`.
#'
#' @examples
#' \dontrun{
#' # allocation follows bin size; the two smallest bins get none
#' allocate_proportional(counts = c(2, 5, 1000, 3000), n = 100)
#' #> 0 0 25 75
#' }
#'
#' @seealso [allocate_even()], [birdnet_subsample()]
#'
#' @keywords internal
allocate_proportional <- function(counts, n) {
  alloc <- rep(0, length(counts))
  open <- seq_along(counts)
  remaining <- min(n, sum(counts))

  while (remaining > 0 && length(open) > 0) {
    share <- remaining * counts[open] / sum(counts[open])
    base <- floor(share)
    extra <- remaining - sum(base)

    if (extra > 0) {
      top <- order(share - base, decreasing = TRUE)[seq_len(extra)]
      base[top] <- base[top] + 1
    }

    take <- pmin(base, counts[open] - alloc[open])
    alloc[open] <- alloc[open] + take
    remaining <- remaining - sum(take)
    open <- open[alloc[open] < counts[open]]
  }

  return(as.integer(alloc))
}


# Number of trailing path components shared by two paths
shared_suffix_length <- function(a, b) {
  split_rev <- function(p) rev(strsplit(gsub("\\\\", "/", p), "/")[[1]])
  a <- split_rev(a)
  b <- split_rev(b)
  n <- min(length(a), length(b))
  k <- 0
  while (k < n && a[k + 1] == b[k + 1]) k <- k + 1
  k
}


# Find the audio file for one detection:
# 1. the original path, if it still exists
# 2. a file with the same name in the indexed audio folder (subfolders included)
# 3. if several files share the name, the best match of the parent folders
resolve_audio_path <- function(original, index) {

  if (!is.na(original) && file.exists(original)) {
    return(original)
  }

  if (is.na(original) || is.null(index) || nrow(index) == 0) {
    return(NA_character_)
  }

  # file name of the original path, whichever slash style it uses
  original_name <- stringr::str_extract(original, "[^\\\\/]+$")

  hits <- index$path[index$name == original_name]
  if (length(hits) == 0) return(NA_character_)
  if (length(hits) == 1) return(hits)

  scores <- vapply(hits, shared_suffix_length, numeric(1), b = original)
  hits[which.max(scores)]
}
