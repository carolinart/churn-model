#' Checks if a character or a vector of characters starts with a substring
#' @param vars: (character or vector of characters)
#' @param match: substring (character)
#' @return: vector indicating if the condition is met (logical or vector of logicals)
starts_with <- function(vars, match, ignore.case = TRUE) {
       if (ignore.case) match <- tolower(match)
       n <- nchar(match)
       
       if (ignore.case) vars <- tolower(vars)
       substr(vars, 1, n) == match
}

#' Checks if a character or a vector of characters ends with a substring
#' @param vars: (character or vector of characters)
#' @param match: substring (character)
#' @return: vector indicating if the condition is met (logical or vector of logicals)
ends_with <- function(vars, match, ignore.case = TRUE) {
       if (ignore.case) match <- tolower(match)
       n <- nchar(match)
       
       if (ignore.case) vars <- tolower(vars)
       length <- nchar(vars)
       
       substr(vars, pmax(1, length - n + 1), length) == match
}