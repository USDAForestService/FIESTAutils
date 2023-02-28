#' @rdname internal_desc
#' @export
subpsamp <- function(cond, subp_cond, subplot, data_dsn = NULL,
                     subpuniqueid = "PLT_CN", subpid = "SUBP") {

  ## DESCRIPTION: Summarize sampled subplot condition proportions.

  ## Check cond
  condx <- pcheck.table(cond, tab_dsn=data_dsn, tabnm="cond", 
			caption="Condition table?")

  ## Check subplot
  subplotx <- pcheck.table(subplot, tab_dsn=data_dsn, tabnm="subplot",
			caption="Subplot table table?")

  ## Check cond
  subp_condx <- pcheck.table(subp_cond, tab_dsn=data_dsn, tabnm="subp_cond",
			caption="Subplot condition table?")

  ## Remove nonsampled conditions by subplotx
  if (!is.null(subplotx)) {
    if ("SUBP_STATUS_CD" %in% names(subplotx)) {
      subp.nonsamp.filter <- "SUBP_STATUS_CD != 3"
      nonsampn <- sum(subplotx[["SUBP_STATUS_CD"]] == 3, na.rm=TRUE)
      if (length(nonsampn) > 0) {
        message("removing ", nonsampn, " nonsampled forest conditions")
      } else {
        message("assuming all sampled conditions in subplot")
      }
    } else {
      message("assuming all sampled conditions in subplot")
    }
      
#     if (ACI && "NF_COND_STATUS_CD" %in% names(subplotx)) {
#       subp.nonsamp.filter.ACI <- "(is.na(NF_SUBP_STATUS_CD) | NF_SUBP_STATUS_CD != 3)"
#       message("removing ", sum(is.na(NF_SUBP_STATUS_CD) & NF_SUBP_STATUS_CD == 3, na.rm=TRUE), 
# 		" nonsampled nonforest conditions")
#       if (!is.null(subp.nonsamp.filter)) {
#         subp.nonsamp.filter <- paste(subp.nonsamp.filter, "&", subp.nonsamp.filter.ACI)
#       }
#     }
    subplotx <- datFilter(x=subplotx, xfilter=subp.nonsamp.filter, 
		title.filter="subp.nonsamp.filter")$xf


    ## Check subpuniqueid and subpid in subplot table
    subpuniqueid <- pcheck.varchar(var2check=subpuniqueid, varnm="subpuniqueid", 
		checklst=names(subplotx), caption="Plot ID - subplot", 
		warn=paste(subpid, "not in tree table"))
    subpid <- pcheck.varchar(var2check=subpid, varnm="subpid", 
		checklst=names(subplotx), caption="subplot ID - subplot", 
		warn=paste(subpid, "not in tree table"))
    setkeyv(subplotx, c(subpuniqueid, subpid))
 
    ## Check subpuniqueid and subpid in subpcond table
    if (!all(c(subpuniqueid, subpid) %in% names(subp_condx))) {
      stop("need ", subpuniqueid, " and ", subpid, " variables in subp_cond")
    }
    setkeyv(subp_condx, c(subpuniqueid, subpid))

    ## Generate query for calculating subplot-level adjustments
    sumcprop.qry <- paste0(
		 "SELECT c.PLT_CN, c.CONDID, c.COND_STATUS_CD, subc.SUBP, 
			 SUM(COALESCE(subc.SUBPCOND_PROP, 0)) AS SUBPPROP_UNADJ,
			 SUM(COALESCE(subc.MICRCOND_PROP, 0)) AS MICRPROP_UNADJ,
			 SUM(COALESCE(subc.MACRCOND_PROP, 0)) AS MACRPROP_UNADJ,
			 SUM(CASE WHEN subc.MACRCOND_PROP IS NULL 
				     THEN subc.MACRCOND_PROP 
				     ELSE subc.SUBPCOND_PROP end) AS CONDPROP_UNADJ
		  FROM condx c 
			 JOIN subplotx subp ON (subp.PLT_CN = c.PLT_CN)
			 JOIN subp_condx subc ON (subc.PLT_CN = c.PLT_CN 
									    and subc.CONDID = c.CONDID
									    and subc.SUBP = subp.SUBP)
		  WHERE subp.SUBP_STATUS_CD < 3
                  AND c.COND_STATUS_CD <> 5 
             GROUP BY c.PLT_CN, c.CONDID, subc.SUBP, c.COND_STATUS_CD")

  } else {

    ## Check subpuniqueid and subpid in subpcond table
    if (!all(c(subpuniqueid, subpid) %in% names(subp_condx))) {
      stop("need ", subpuniqueid, " and ", subpid, " variables in subp_cond")
    }
    setkeyv(subp_condx, c(subpuniqueid, subpid))

    ## Generate query for calculating subplot-level adjustments
    sumcprop.qry <- paste0(
		 "SELECT c.PLT_CN, c.CONDID, c.COND_STATUS_CD, subc.SUBP, 
			 SUM(COALESCE(subc.SUBPCOND_PROP, 0)) AS SUBPPROP_UNADJ,
			 SUM(COALESCE(subc.MICRCOND_PROP, 0)) AS MICRPROP_UNADJ,
			 SUM(COALESCE(subc.MACRCOND_PROP, 0)) AS MACRPROP_UNADJ,
			 SUM(CASE WHEN subc.MACRCOND_PROP IS NULL 
				     THEN subc.MACRCOND_PROP 
				     ELSE subc.SUBPCOND_PROP end) AS CONDPROP_UNADJ
		  FROM condx c 
			 JOIN subp_condx subc ON (subc.PLT_CN = c.PLT_CN 
									    and subc.CONDID = c.CONDID)
		  WHERE c.COND_STATUS_CD <> 5 
             GROUP BY c.PLT_CN, c.CONDID, subc.SUBP, c.COND_STATUS_CD")
  }
  subpcx <- data.table(sqldf::sqldf(sumcprop.qry))
  return(subpcx)
}


