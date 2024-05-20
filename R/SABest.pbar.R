#' @rdname estimation_desc
#' @export
SABest.unit <- function(fmla.dom.unit, 
                        pltdat.dom,
                        yn,
                        model.form) {
  pltdat.unit <- data.frame(pltdat.dom)
  if (SApackage == "spAbundance") {
    y <- pltdat.unit[[yn]]
    X <- model.matrix(fmla.dom.unit[-2], pltdat.unit)[,-1]
    covs <- split(X, col(X))
    names(covs) <- colnames(X)
  
    dat.list <- list(
      y = y,
      covs = covs
    )
  
    if (model == "lm") {

    }
    
    if (model == "dvi") {
      
    }
    
    if (model == "dvc") {
      
    }
    
    if (model == "svi") {
      
    }
    
    if (model == "svc") {
      
    }
    
    mod <- spAbundance::abund(formula = fmla.dom.unit[-2], 
                              data = dat.list,
                              family = "Gaussian",
                              n.batch = 4, 
                              batch.length = 250,
                              n.chains = 4, 
                              n.thin = 20,
                              n.burn = 500)
    

  
  return(mod)
}