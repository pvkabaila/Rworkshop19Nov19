#' rhs1.welch.adj
#'
#' Written by P.Kabaila for the following paper:
#' Kabaila, P. (2005). Assessment of a preliminary
#' F-test solution to the Behrens-Fisher problem.
#' Communications in Statistics - Theory and Methods,
#' 34, 291-302.
#' This function may be used freely, provided the
#' user makes reference to this paper.
#' This function calculates
#'  c C_{\alpha/2}(r)
#'  expressed as a function of z.
#' The notation used in this function is the same as
#' in this paper.
#'
#' @param z
#' @param rho
#' @param f1
#' @param f2
#' @param alpha   The nominal coverage probability is 1-alpha
#' @param const
#'
#' @return
#' @export
#'
#' @examples
rhs1.welch.adj <- function(z, rho, f1, f2, alpha, const)
{
  #  const is a value of c
  #  dof is the degrees of freedom of the t-distribution
  #       calculated using (2.1) of Wang (1971)
  #  f1 is a degrees of freedom
  #  f2 is a degrees of freedom
  #  rho is sigma1.squared / sigma2.squared
  #
  #
  r <- z.to.r(z, rho, f1, f2)
  denom1 <- ((r/(1. + r))^2.)/f1
  denom2 <- ((1./(1. + r))^2.)/f2
  #
  #
  denom <- denom1 + denom2
  dof <- 1./denom
  out <- const * qt(1. - (alpha/2.), dof)
  out
}
