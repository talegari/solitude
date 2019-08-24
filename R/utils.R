#' @name is_integerish
#' @title Check for a single integer
#' @description for a single integer
#' @param x input
#' @return TRUE or FALSE
#' @examples
#' \dontrun{is_integerish(1)}
is_integerish = function(x){
    res = is.integer(x) || (is.numeric(x) && all(x == trunc(x)) &&
        !is.na(x))
    res
}