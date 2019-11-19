#' z.to.r
#'
#' Written by P.Kabaila for the following paper:
#' Kabaila, P. (2005). Assessment of a preliminary
#' F-test solution to the Behrens-Fisher problem.
#' Communications in Statistics - Theory and Methods,
#' 34, 291-302.
#' This function may be used freely, provided the
#' user makes reference to this paper.
#' This function converts z to r.
#' The notation used in this function is the same as
#' in this paper.
#'
#' @param z     This is the parameter z
#' @param rho   This is the parameter rho
#' @param f1    First degrees of freedom
#' @param f2    Second degrees of freedom
#'
#'
#' @export
z.to.r <- function(z, rho, f1, f2)
{
    num <- rho * f2 * (f2 + 1.)
  denom <- f1 * (f1 + 1.) * ((1./z) - 1.)
  r <- num/denom
  r
}
