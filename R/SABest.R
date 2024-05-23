#' @rdname estimation_desc
#' @export
SABest.fit <- function(fmla.dom.unit, 
                       pltdat.dom,
                       yn,
                       dunitvar,
                       coord.names = c("X", "Y"),
                       dvcs = NULL,
                       svcs = NULL,
                       model.form = "lm",
                       bayes_opts = bayes_options(),
                       ncores = 1) {
  
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
    
    if (model.form %in% c("dvi", "dvc")) {
      pltdat.unit[[dunitvar]] <- as.numeric(factor(pltdat.unit[[dunitvar]])) 
      dat.list$covs[[dunitvar]] <- pltdat.unit[[dunitvar]]
    }
    
    if (model.form == "lm") {
      fmla.abund <- fmla.dom.unit[-2]
    }
    
    if (model.form == "dvi") {
      dvi.term <- reformulate(c(".", paste0("(1 | ", dunitvar, ")")))
      fmla.abund <- update.formula(fmla.dom.unit[-2], dvi.term)
    }
    
    if (model.form == "dvc") {
      if (length(dvcs) == 0) {
        stop("Must supply dvcs if model.form is set to dvc")
      }
      dvcs <- c("1", dvcs)
      dvc.term <- reformulate(c(".", paste0("(", dvcs, "|", dunitvar, ")")))
      fmla.abund <- update.formula(fmla.dom.unit[-2], dvc.term)
    } 
     
    mod <- spAbundance::abund(formula = fmla.abund, 
                              data = dat.list,
                              family = "Gaussian",
                              n.batch = 4, 
                              batch.length = 500,
                              n.chains = 4, 
                              n.thin = 40,
                              n.burn = 1000,
                              n.omp.threads = ncores)
    
  } else if (model.form %in% c("svi", "svc")) {
    
    fmla.svcAbund <- fmla.dom.unit[-2]
    
    samp_coords <- as.matrix(pltdat.unit[,coord.names])
    
    dat.list <- list(
      y = y,
      covs = covs,
      coords = samp_coords 
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
      if (length(svcs) == 0) {
        stop("Must supply svcs if model.form is svc")
      }
      svcs <- c("(Intercept)", svcs)
    }
    
    inits <- list(
      phi = 3 / mean(dist.mat),
      w = matrix(data = 0, nrow = length(svcs), ncol = length(dat.list$y)),
      sigma.sq = 1
    )
    
    mod <- spAbundance::svcAbund(
      formula = fmla.svcAbund,
      data = dat.list,
      inits = inits,
      priors = priors,
      svc.cols = svcs,
      family = "Gaussian",
      n.batch = 4,
      batch.length = 500,
      n.chains = 2, 
      n.thin = 20,
      n.burn = 1000,
      n.neighbors = 10,
      n.report = 5,
      n.omp.threads = ncores
    )
    
  } else {
    stop("")
  }

  return(mod)
  
}
