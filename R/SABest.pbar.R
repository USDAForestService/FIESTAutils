#' @rdname estimation_desc
#' @export
SABest.fit <- function(fmla.dom.unit, 
                       pltdat.dom,
                       yn,
                       dunitvar,
                       dvcs = NULL,
                       svcs = NULL,
                       model.form = "lm") {
  
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
    
    if (model.form %in% c("lm", "dvi", "dvc")) {
      
      if (model == "lm") {
        fmla.abund <- fmla.dom.unit[-2]
      }
      
      if (model == "dvi") {
        # add domain varying intercept term based on dunitvar
        dvi.term <- reformulate(c(".", paste0("(1 | ", dunitvar, ")")))
        fmla.abund <- update.formula(fmla.dom.unit[-2], dvi.term)
      }
      
      if (model == "dvc" && length(dvcs) != 0) {
        # domain varying coefficients and possibly intercepts i.e (tcc | domain)
        # start by assuming a vector of dvc is passed
        dvcs <- c("1", dvcs)
        dvc.term <- reformulate(c(".", paste0("(", dvcs, "|", dunitvar, ")")))
        fmla.abund <- update.formula(fmla.dom.unit[-2], dvc.term)
      }
      
      mod <- spAbundance::abund(formula = fmla.abund, 
                                data = dat.list,
                                family = "Gaussian",
                                n.batch = 4, 
                                batch.length = 250,
                                n.chains = 4, 
                                n.thin = 20,
                                n.burn = 500)
      
    } else if (model.form %in% c("svi", "svc")) {
      
      if (model == "svi") {
      
      }
      
      if (model == "svc") {
        
      }
      
      # use svcAbund here
      # with svc.cols argument
      
    } else {
      stop("")
    }
  }
  
  return(mod)
}