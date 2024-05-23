#' bayes output options.
#' 
#' Returns a list of user-supplied parameters and parameter values for outputting
#' multest with custom aesthetics.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param model.form Model form for Bayesian small area estimation model. 
#' Currently, accepted options are "lm" for linear model, "dvi" for domain 
#' varying intercept model, "dvc" for domain varying coefficient(s) model, "svi"
#' for space varying intercept model, and "svc" for space varying coefficient(s) 
#' model. "dvi" and "svi" models have a random effect on the intercept, and 
#' "dvc" and "svc" have a random effect on the intercept, along with any 
#' selected predictors from `dvcs` or `svcs`.
#' @param dvcs Character vector. Only used when `model.form` is "dvc". Names of
#' the columns for which coefficients should vary by domain. Implicitly the 
#' intercept is included and does not need to specified. 
#' @param svcs Character vector. Only used when `model.form` is "svc". Names of
#' the columns for which coefficients should vary in space. Implicitly the
#' intercept is included and does not need to specified. 
#' @param coord.names Character vector of length 2. Name of columns for the X
#' and Y coordinates of plots. Default parameters are tuned for coordinates in 
#' meters. Used for fitting spatial models. 
#' @param n.batch Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param batch.length Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param accept.rate Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param n.omp.threads Additional argument to be passed to spAbundance::abund 
#' or spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param verbose Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param n.report Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param n.burn Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param n.thin Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param n.chains Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param save.fitted Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param n.batch Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param batch.length Additional argument to be passed to spAbundance::abund or 
#' spAbundance::svcAbund. See documentation in `spAbundance` package for 
#' details.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for 
#' fitting Bayesian small area estimation models. 
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' bayes_options()
#' 
#' @export bayes_options

bayes_options <- function(model.form = "dvi",
                          dvcs = NULL,
                          svcs = NULL,
                          coord.names = c("X", "Y"),
                          n.batch = 4,
                          batch.length = 500,
                          accept.rate = 0.43,
                          n.omp.threads = 1,
                          verbose = TRUE,
                          n.report = 100,
                          n.burn = 1000,
                          n.thin = 40,
                          n.chains = 4,
                          save.fitted = TRUE,
                          ...) {
  
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(multest_options)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  # removes input parameters to create l correctly
  rm(input.params, formallst)
  
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}
