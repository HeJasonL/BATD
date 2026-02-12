#' Not In Operator
#'
#' Convenience operator for negating the \code{\%in\%} operator.
#' Equivalent to \code{!(x \%in\% y)}.
#'
#' @param x A vector to check
#' @param y A vector to check against
#'
#' @return A logical vector indicating elements of x not in y
#'
#' @examples
#' c(1, 2, 3) %ni% c(2, 4)
#'
#' @keywords internal
#' @export

'%ni%' <- function(x, y) {
  !(x %in% y)
}
