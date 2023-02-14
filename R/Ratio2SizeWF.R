#' @rdname internal_desc
#' @export
Ratio2Size <- function(yn, y2n = NULL, yd, y2d = NULL, ysum, dsum, 
                       uniqueid, esttype, ratiotype, 
                       stratalut, RHGlut,
                       unitvar, strvar, domain){
  ########################################################################################
  ## DESCRIPTION: Ratio-to-size estimators
  ## ARGUMENTS:
  ## yn	- String. Name of numerator response for plot size #1 (e.g., Subplot)
  ## y2n	- String. Name of numerator response for plot size #2 (e.g., Microplot)
  ## yd	- String. Name of denominator response for plot size #1 (e.g., Subplot)
  ## y2d	- String. Name of denominator response for plot size #2 (e.g., Microplot)
  ## ysum	- Data frame. Plot-level table with numerator response(s) for all plot sizes
  ## dsum	- Data frame. Plot-level table with denominator response(s) for all plot sizes
  ## uniqueid - String. Name of unique identifier of plots in ysum and dsum
  ## esttype - String. Type of estimation ('AREA', 'TREE', 'RATIO')
  ## ratiotype - String. Type of ratio estimate ('PERACRE', 'PERTREE')
  ## stratalut - Data frame. Estimation Unit/Stratum look-up table, including:
  ##		P2POINTCNT - Number of plots in Estimation Unit
  ##		n.total - Number of sampled plots in Estimation Unit
  ##		n.strata - Number of sampled plots in Estimation Unit/Stratum
  ## RHGlut - Data frame. Estimation Unit/Stratum/RHG look-up table, including:
  ##		RHG - Response Homogeneity Group (A:All; F:Field; O:Office)
  ##		n.resp - Number of response plots in Estimation Unit/Stratum/RHG 
  ##		n.nonresp - Number of nonresponse plots in Estimation Unit/Stratum/RHG 
  ##		P2POINTCNT - Number of total plots in Estimation Unit/Stratum/RHG 
  ## unitvar 	- name of variable defining estimation unit
  ## domain	- name of variable defining domain (e.g., forest type)
  ## areavar 	- name of variable defining area
  ##
  ## VALUE:
  ## ybar		- estimate proportion of land covered by condition, for numerator
  ## ybar.var	- variance of estimated proportion, for numerator
  ########################################################################################
  strunitvars <- c(unitvar, strvar)

  ## Set global variables
  n.resp=P2POINTCNT=yn.pltdom=yd.pltdom=y2n.pltdom=y2d.pltdom=covyn.pltdom=
	ynsq.pltdom=nd.pltdom=y2nsq.pltdom=nd2.pltdom=ybar.dom=yn.dom=yd.dom=
	y2bar.dom=y2n.dom=y2d.dom=ssq.dom=ynsq.dom=nd.dom=ydsq.dom=y2nsq.dom=
	nd2.dom=y2dsq.dom=covyd.dom=ssq2.dom=cov.dom=RHG.strwt=n.totresp=
	ybar.str=comp1=comp2=comp3=comp4=var.str=n.total=strwt=n.strata <- NULL


  ## Check stratalut
  ##############################################################################
  if (!"n.strata" %in% names(stratalut)) stop("need n.strata in stratalut")
  if (!"n.total" %in% names(stratalut)) stop("need n.total in stratalut")


  ## RHG look-up table: Calculate totals and strata weights by stratum/estimation unit
  ##############################################################################
  RHGlut[, c("n.totresp", "n.total", "RHG.strwt") := list(
		sum(n.resp), 
		sum(P2POINTCNT), 
		P2POINTCNT / sum(P2POINTCNT)), by=c(strunitvars)]


  ##############################################################################
  ## yn level
  ##############################################################################

  ## NUMERATOR
  ##############################################################################

  ## DOMAIN look-up table: Get unique domains by stratum/estimation unit
  ##############################################################################
  domlut <- unique(ysum[ysum$RHG != "A", c(strunitvars, domain), with=FALSE]) 

  ## RHG/PLOT LEVEL: Summed Y by plot (NUMERATOR)
  setkeyv(ysum, c(strunitvars, "RHG", uniqueid))
 
  ## RHG/PLOT LEVEL: Merge numerator and denominator for variance calculations 
  ## (Unpublished Patterson: WestFest algorithm #3p)
  ysum <- merge(ysum, dsum)


  ## RHG/PLOT/DOMAIN LEVEL: Change name and square sum and multiply sum for denominator (SUBPLOT)
  ysum[, ':=' (yn.pltdom = get(yn), yd.pltdom = get(yd))][,
         ':=' (ynsq.pltdom = yn.pltdom^2, nd.pltdom = yn.pltdom * yd.pltdom)]

  if (!is.null(y2n)) {
    ## RHG/PLOT/DOMAIN LEVEL: Change name and square sum and multiply sum for denominator (MICRPLOT)
    ysum[, ':=' (y2n.pltdom = get(y2n), y2d.pltdom = get(y2d))][,
         ':=' (y2nsq.pltdom = y2n.pltdom^2, nd2.pltdom = y2n.pltdom * y2d.pltdom)]

    ## RHG/PLOT/DOMAIN LEVEL: Covariance for yn and y2n
    ysum[, covyn.pltdom := yn.pltdom * y2n.pltdom]
  }

  ############################################
  ## Aggregate to RHG
  ############################################

  ## DENOMINATOR
  ##############################################################################

  ## RHG/PLOT LEVEL: Summed measured proportion (MEASPROP_UNADJ) by plot (DENOMINATOR)
  setkeyv(dsum, c(strunitvars, "RHG", uniqueid))

  ## RHG LEVEL: Aggregate plot-level measured proportions by unit/strata/RHG (DENOMINATOR)
  dsum.RHG <- dsum[, list(yd.dom = sum(get(yd), na.rm=TRUE),
				    ydsq.dom = sum(get(yd)^2, na.rm=TRUE)),
 						by = c(strunitvars, "RHG")]

  if (!is.null(y2d)) {
    ## RHG LEVEL: Aggregate plot-level measured proportions by unit/strata/RHG (DENOMINATOR)
    dsum2.RHG <- dsum[, list(y2d.dom = sum(get(y2d), na.rm=TRUE),
				      y2dsq.dom = sum(get(y2d)^2, na.rm=TRUE),
                            covyd.dom = sum(get(yd) * get(y2d))),
 						by = c(strunitvars, "RHG")]
    dsum.RHG <- merge(dsum.RHG, dsum2.RHG)
  }


  ## NUMERATOR
  ##############################################################################

  ## RHG/DOMAIN LEVEL: Aggregate plot-level sums and squared sums by unit/strata/RHG/domain
  ysum.RHG <- ysum[, list(yn.dom = sum(yn.pltdom, na.rm=TRUE),
    				    ynsq.dom = sum(ynsq.pltdom, na.rm=TRUE), 
				    nd.dom = sum(nd.pltdom, na.rm=TRUE)),
    						 by=c(strunitvars, "RHG", domain)]

  if (!is.null(y2n)) {
    ysum2.RHG <- ysum[, list(y2n.dom = sum(y2n.pltdom, na.rm=TRUE),
    				    y2nsq.dom = sum(y2nsq.pltdom, na.rm=TRUE), 
				    nd2.dom = sum(nd2.pltdom, na.rm=TRUE),
                          covyn.dom = sum(covyn.pltdom, na.rm=TRUE)),
    						 by=c(strunitvars, "RHG", domain)]
    ysum.RHG <- merge(ysum.RHG, ysum2.RHG)
  }


  ## Rbind 0 values for Office visits (RHG != 'A') by domain and add to strata-level sums
  ###################################################################
  domlut <- unique(ysum.RHG[ysum.RHG$RHG != "A", c(strunitvars, domain), with=FALSE]) 
  domlut[, ':=' (RHG = "O", yn.dom = 0, ynsq.dom = 0, nd.dom = 0)]
  if (!is.null(y2n)) {
    domlut[, ':=' (y2n.dom = 0, y2nsq.dom = 0, nd2.dom = 0, covyn.dom = 0)]
  }

  ysum.RHG <- rbind(ysum.RHG, domlut)
  setkeyv(ysum.RHG, c(strunitvars, "RHG"))

  ## RHG/DOMAIN LEVEL: Merge numerator and denominator domain-level sums
  ysum.RHG <- merge(ysum.RHG, dsum.RHG, all.x=TRUE)
  ysum.RHG[is.na(ysum.RHG)] <- 0

#######################

  ## STRATA/DOMAIN LEVEL: Merge domain-level sums to strata table
  setkeyv(RHGlut, c(strunitvars, "RHG"))
  RHGdat <- merge(ysum.RHG, RHGlut, all.x=TRUE)
  RHGdat[is.na(RHGdat)] <- 0

  ## RHG/DOMAIN LEVEL: Step 1 - Calculate ybar and ssq by stratum and domain
  RHGdat[, ybar.dom := yn.dom / yd.dom]

  if (!is.null(y2n)) {
    RHGdat[, y2bar.dom := y2n.dom / y2d.dom]
  }

  RHGdat[, ssq.dom := n.resp^2 / (n.resp - 1) * 
		     	(ynsq.dom - 2 * ybar.dom * nd.dom + ybar.dom^2 * ydsq.dom) / (yd.dom^2) ]

  if (!is.null(y2n)) {
    RHGdat[, ':=' (ssq2.dom = n.resp^2 / (n.resp - 1) * 
					(y2nsq.dom - 2 * y2bar.dom * nd2.dom + y2bar.dom^2 * y2dsq.dom) / (y2d.dom^2),
                   cov.dom = n.resp^2 / (n.resp - 1) * 
           				(covyd.dom - y2bar.dom * nd.dom - ybar.dom * nd2.dom + 
					ybar.dom * y2bar.dom * covyd.dom) / (yd.dom * y2d.dom) ) ] 

    RHGdat[, ':=' (ybar.dom = ybar.dom + y2bar.dom,
                   ssq.dom = ssq.dom + ssq2.dom + 2 * cov.dom)]                
  }


  ## STRATA/DOMAIN LEVEL: Calculate strata-level mean for substrata (RHG) 
  ##################################################################################
  ybar <- RHGdat[, list(ybar.str = sum(ybar.dom * RHG.strwt)), 
						by=c(strunitvars, domain)]

  ## Merge ybar back to RHGdat to use in EQ #5   
  RHGdat <- merge(RHGdat, ybar, by=c(strunitvars, domain))


  ####################################################################################
  ## Calculate strata-level variance for substrata (i.e., RHG-F/O) - EQ #5 in Westfall
  ####################################################################################

  ## STRATA/DOMAIN LEVEL: Step 2 - Calculate components of EQ #5 
  eq5 <- RHGdat[, list(
		comp1 = (P2POINTCNT - 1) / (n.total - 1) * ssq.dom / n.totresp,
		comp2 = (P2POINTCNT - 1) / (n.total - 1) * (1 - RHG.strwt) * ssq.dom / n.totresp^2 / RHG.strwt,
		comp3 = P2POINTCNT / (n.total * (n.total - 1)) * (ybar.dom - ybar.str)^2, 
		comp4 = (P2POINTCNT / n.total)^2 * (1 - yd.dom / P2POINTCNT) / yd.dom * ssq.dom   
		), by = c(strunitvars, "RHG", domain)]

  ## STRATA/DOMAIN LEVEL: Calculate strata-level variance
  eq5var <- eq5[, list(var.str = sum(comp1 + comp2 + comp3 + comp4)), by=c(strunitvars, domain)]

  ## STRATA/DOMAIN LEVEL: Merge var.str with ybar.str by estimation unit/strata
  ybardat <- merge(ybar, eq5var, by=c(strunitvars, domain))


  ## STRATA/DOMAIN LEVEL: Merging strata-level ybar/var with strata-level lut
  ## This removes estimation units with no response 
  ##############################################################
  #ybardat <- merge(stratalut, ybardat, by=strunitvars, all.x=TRUE)
  ybardat <- merge(stratalut, ybardat, by=strunitvars)

################
## CHECK THIS LATER
#dim(unique(ybardat[, strunitvars, with=FALSE]))
#test <- ybardat[is.na(ybardat$ybar), ]
#test
#################


  ####################################################################################
  ## Calculate estimation unit-level ybar and variance 
  ####################################################################################
  est.unit <- ybardat[, list(ybar = sum(strwt * ybar.str),
		                  ybar.var = sum( (strwt + (1 - strwt) / n.total) * n.strata / n.total * var.str )   
		                  ), by = c(unitvar, domain)]

  ## Append number of non-zero plots and merge to est.unit
  NBRPLT.gt0 <- ysum[, list(NBRPLT.gt0 = sum(yn.pltdom != 0, na.rm=TRUE)), by=c(unitvar, domain)]
  setkeyv(NBRPLT.gt0, unitvar)

  est.unit <- merge(est.unit, NBRPLT.gt0, by=c(unitvar, domain))
  return(est.unit)
}


