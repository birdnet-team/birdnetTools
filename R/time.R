#' Current time
#'
#' Returns a sentence with the current time
#'
#' @param city City either "Vancouver" or "Chemnitz"
#'
#' @returns A character string
#' @export
#'
#' @examples
#' what_time()
what_time <- function(city = "Vancouver") {

  rlang::arg_match0(city, c("Vancouver", "Chemnitz"))

  Vancouver_time <- format(Sys.time(), tz = "America/Vancouver", format = "%H:%M")

  Chemnitz_time <- format(Sys.time(), tz = "Europe/Berlin", format = "%H:%M")

  exclamation <- praise::praise("${Exclamation}")

  switch(
    city,
    Vancouver = sprintf("%s! It is %s now!", exclamation, Vancouver_time)
                        ,
    Chemnitz = sprintf("%s! It is %s now!", exclamation, Chemnitz_time)
  )
}

