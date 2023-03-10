% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeCBMeasure.R
\name{computeCBMeasure}
\alias{computeCBMeasure}
\title{Dependence measures for the checkerboard copula}
\usage{
computeCBMeasure(A, method = c("spearman", "kendall", "bkr", "dss", "zeta1"))
}
\arguments{
\item{A}{A (possibly non-square) checkerboard mass density.}

\item{method}{Determines the underlying dependence measure. Options include "spearman", "kendall", "bkr", "dss", "chatterjee" and "zeta1".}
}
\value{
The value of \eqn{\mu(C^{\#}(A))}. For a sorted A, this corresponds to the rearranged dependence measure \eqn{R_{\mu}(C^{\#}(A))}.
}
\description{
Computes \eqn{\mu(C^{\#}(A))} for some underlying measure for the checkerboard copula \eqn{C^{\#}(A)}.
This measure depends only on the input matrix A.
}
\details{
This function computes \eqn{\mu(C^{\#}(A))} for one of several underlying measures for a given checkerboard copula \eqn{C^{\#}(A)}.
Most importantly, the value only depends on the (possibly non-square) matrix \eqn{A} and implicitly assumes the form of \eqn{C^{\#}(A)} given in Strothmann, Dette and Siburg (2022) <arXiv:2201.03329>.
Currently, the following underlying measures are implemented:
\itemize{
 \item{"spearman"}{ Implements the concordance measure Spearman's \eqn{\rho},}
 \item{"kendall"}{ Implements the concordance measure Kendall's \eqn{\tau},}
 \item{"bkr"}{ Implements the Blum–Kiefer–Rosenblatt \eqn{R}, also known as the \eqn{L^2}-Schweizer-Wolff-measure <doi:10.1214/aos/1176345528>,}
 \item{"dss"}{ Implements the Dette-Siburg-Stoimenov measure of complete dependence <doi:10.1111/j.1467-9469.2011.00767.x>, also known as Chatterjee's \eqn{\xi} <doi:10.1080/01621459.2020.1758115>,}
 \item{"zeta1"}{ Implements the \eqn{\zeta_1}-measure of complete dependence established by W. Trutschnig <doi:10.1016/j.jmaa.2011.06.013>.}
}
}
\examples{
n <- 10
A <- diag(n)/n
computeCBMeasure(A, method="spearman")
}
