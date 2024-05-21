#' @rdname estimation_desc
#' @export
SABest.fit <- function(fmla.dom.unit, 
                       pltdat.dom,
                       yn,
                       dunitvar,
                       pltassgn,
                       dvcs = NULL,
                       svcs = NULL,
                       model.form = "lm") {
  
  pltdat.unit <- data.frame(pltdat.dom)
  

  y <- pltdat.unit[[yn]]
  X <- model.matrix(fmla.dom.unit[-2], pltdat.unit)[,-1]
  covs <- split(X, col(X))
  names(covs) <- colnames(X)
  
  if (model.form %in% c("lm", "dvi", "dvc")) {
    
    dat.list <- list(
      y = y,
      covs = covs
    )
    
    if (model.form == "lm") {
      fmla.abund <- fmla.dom.unit[-2]
    }
    
    if (model.form == "dvi") {
      # add domain varying intercept term based on dunitvar
      dvi.term <- reformulate(c(".", paste0("(1 | ", dunitvar, ")")))
      fmla.abund <- update.formula(fmla.dom.unit[-2], dvi.term)
    }
    
    if (model.form == "dvc" && length(dvcs) != 0) {
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
    
    fmla.svcAbund <- fmla.dom.unit[-2]
    
    dat.list <- list(
      y = y,
      covs = covs,
      coords = samp_coords # need to fix here, currently hardcoded
    )
    
    dist.mat <- dist(dat.list$coords)
    min.dist <- min(dist.mat)
    max.dist <- max(dist.mat)
    priors <- list(
      sigma.sq.ig = list(a = 2, b = 1),
      phi.unif = list(a = 3 / max.dist, b =  3 / min.dist)
    )
    
    if (model.form == "svi") {
      svcs <- "(Intercept)"
    }
    
    if (model.form == "svc") {

    }
    
    inits <- list(
      phi = 3 / mean(dist.mat),
      w = matrix(data = 0, nrow = length(svcs), ncol = length(dat.list$y)),
      sigma.sq = 1
    )
    
    mod <- svcAbund(formula = fmla.svcAbund,
                    data = dat.list,
                    inits = inits,
                    priors = priors,
                    svc.cols = svcs,
                    family = "Gaussian",
                    n.batch = 4, 
                    batch.length = 250,
                    n.chains = 4, 
                    n.thin = 20,
                    n.burn = 500)
    
  } else {
    stop("")
  }

  return(mod)
}