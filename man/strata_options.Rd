% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strata_options.R
\name{strata_options}
\alias{strata_options}
\title{Strata options.}
\usage{
strata_options(
  getwt = FALSE,
  getwtvar = "P1POINTCNT",
  strwtvar = "strwt",
  stratcombine = TRUE,
  minplotnum.strat = 2,
  pivot = FALSE,
  nonresp = FALSE,
  ...
)
}
\arguments{
\item{getwt}{Logical. If TRUE, calculates strata weights from stratatlut
getwtvar.  If FALSE, strwtvar variable must be in stratalut.}

\item{getwtvar}{String. If getwt=TRUE, name of variable in stratalut to
calculate weights (Default = 'P1POINTCNT').}

\item{strwtvar}{String. If getwt=FALSE, name of variable in stratalut with
calculated weights (Default = 'strwt').}

\item{stratcombine}{Logical. If TRUE, and strata=TRUE, automatically combines
strata categories if less than minplotnum.strat plots in any one stratum. 
See notes for more info.}

\item{minplotnum.strat}{Integer. Minimum number of plots for a stratum
within an estimation unit.}

\item{pivot}{Logical. If TRUE, pivot stratalut.}

\item{nonresp}{Deprecated.}

\item{...}{For extendibility.}
}
\value{
A list of user-supplied parameters and parameter values for strata.
}
\description{
Returns a list of user-supplied parameters and parameter values for strata.
}
\details{
If no parameters, an empty list is returned.
}
\examples{

strata_options(getwt = FALSE)

}
\author{
Grayson W. White
}
\keyword{options}
