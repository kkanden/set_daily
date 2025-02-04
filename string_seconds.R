#' Pretty print number of seconds
#'
#' Transforms the input into m:ss.sss or m:ss format (1 or more minute digits, 2 second digits,
#' 3 millisecond digits - optional).
#'
#' @param seconds
#' a numeric value representing the number of seconds
#' @param ms
#' logical. Should milliseconds be printed?
#'
#' @return
#' a character vector or `NA` if `seconds` is `NA`
#'
seconds_to_string <- function(seconds, ms = TRUE) {
  minutes <- as.integer(seconds / 60)
  secs <- as.integer(seconds %% 60)
  milliseconds <- as.integer((seconds - floor(seconds)) * 1000)

  formatted_time <- sapply(
    seconds,
    function(x) {
      if (is.na(x)) {
        return(NA_character_)
      } else if (ms) {
        sprintf(
          "%d:%02d.%03d",
          as.integer(x / 60),
          as.integer(x %% 60),
          round((x - floor(x)) * 1000)
        )
      } else {
        sprintf(
          "%d:%02d",
          as.integer(x / 60),
          as.integer(x %% 60)
        )
      }
    }
  )

  return(formatted_time)
}

#' Turn time string to number of seconds
#'
#' Inverse function to `seconds_to_string()`. Transforms a character of mm:ss.sss
#' or mm:ss format (1 or more minute digits, 2 second digits, 3 millisecond digits - optional)
#' into number of seconds.
#'
#' @param string
#'  a character vector, whose elements represent time in mm:ss.sss
#'  or mm:ss format
#'
#' @return
#' a numeric vector representing the number of seconds or `NA` if `string` is `NA`
#' @export stringi stri_split
#' @export data.table fcase
#'
string_to_seconds <- function(string) {
  split <- stringi::stri_split(string, regex = "[:.]")

  time_sec <- lapply(
    split,
    function(x) {
      if (any(is.na(x))) {
        return(NA)
      }
      mins <- as.integer(x[1])
      secs <- as.integer(x[2])
      msecs <- ifelse(
        length(x) == 3,
        x[3],
        "0"
      )

      msecs <- data.table::fcase(
        msecs == "0",
        0L,
        nchar(msecs) == 1,
        as.integer(msecs) * 100L,
        nchar(msecs) == 2,
        as.integer(msecs) * 10L,
        nchar(msecs) == 3,
        as.integer(msecs)
      )
      time_sec <- mins * 60 + secs + msecs / 1000
      return(time_sec)
    }
  ) |>
    unlist()

  return(time_sec)
}
