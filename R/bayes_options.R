#' bayes output options.
#' 
#' Returns a list of user-supplied parameters and parameter values for outputting
#' multest with custom aesthetics.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param n.batch See spAbundance::abund or spAbundance::svcAbund.
#' @param batch.length See spAbundance::abund or spAbundance::svcAbund.
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
