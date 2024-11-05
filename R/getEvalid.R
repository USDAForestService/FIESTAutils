getevalid <- function(states = NULL, 
                      RS = NULL, 
                      evalCur = TRUE, 
                      evalEndyr = NULL, 
                      evalid = NULL, 
                      evalAll = FALSE, 
                      evalType = "VOL", 
                      dbconn = NULL,
                      schema = NULL,
                      dbconnopen = FALSE) {
  ###############################################################################
  ## DESCRIPTION: Get or check evalid from FIA database.
  ################################################################################
  
  ## Set global variables
  EVAL_GRP_Endyr=evalTypelist=STATECD=EVALID=evaltyp=invyrs=statecd=end_invyr <- NULL
  returnevalid <- FALSE
  
  ## Define variables
  SCHEMA. <- ""

  ## Define evalType choices
  evalTypelst <- unique(c(sub("EXP", "", FIESTAutils::ref_evaltyp$EVAL_TYP), "GRM"))
  invtype <- "ANNUAL"
  ann_inv <- "Y"  
  surveynm <- "survey" 
  popevalgrpnm <- "pop_eval_grp"
  popevaltypnm <- "pop_eval_typ"
  popevalnm <- "pop_eval"
  SCHEMA. <- ""
  invyrtab <- NULL
  
  
  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################}
  
  ##################################################################
  ## CHECK PARAMETER NAMES
  ##################################################################
  getlistfromdt <- function(dt, x, xnm="STATECD") {
    ## DESCRIPTION: generates a list of 1 or more values from a data table
    dtunique <- dt[, lapply(get(x), unique), by=xnm]
    xnames <- dtunique[[xnm]]
    dtlst <- as.list(data.frame(t(dtunique[, -1])))
    
    if (xnm == "STATECD") {
      names(dtlst) <- pcheck.states(xnames)
    } else {
      names(dtlst) <- xnames
    }    
    return(dtlst)
  }
  
  ## Check database connection
  ######################################################
  if (!DBI::dbIsValid(dbconn)) {
    stop("invalid database connection")
  }
  dbtablst <- DBI::dbListTables(dbconn)
  if (length(dbtablst) == 0) {
    stop("no data in database")
  }
  if (!is.null(schema)) {
    SCHEMA. <- paste0(schema, ".")
  }

  ## Check evalid, invyrtab, and state/RS parameters
  ######################################################
  rslst <- c("RMRS","SRS","NCRS","NERS","PNWRS")
  if (!is.null(evalid)) {
    evalid <- unique(unlist(evalid)) 
    if (any(nchar(evalid) > 6)) {
      stop("invalid evalid")
    }
    stcdlst <- unique(substr(evalid, 1, nchar(evalid)-4))
    states <- pcheck.states(stcdlst, "MEANING")
  } else if (!is.null(invyrtab)) {
    if (!all(class(invyrtab) %in% c("data.frame", "data.table"))) {
      stop("invyrtab must be a data frame or data table") 
    }
    statenm <- findnm("STATECD", names(invyrtab), returnNULL=FALSE) 
    if (is.null(statenm)) {
      stop("STATECD must be in invyrtab")
    } else {
      stcdlst <- unique(invyrtab[[statenm]])
      states <- pcheck.states(stcdlst, "MEANING")
    }
  } else {
    ## Check RS states
    #####################################################
    RS <- pcheck.varchar(var2check=RS, varnm="RS", 
                         checklst=rslst, caption="Research Unit?", multiple=TRUE)
    if (!is.null(RS) && !is.null(states)) {     
      RSstatelst <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$RS %in% RS,"MEANING"]
      if (!all(states %in% RSstatelst)) {
        msg <- paste("RS and states are invalid...", 
                     toString(states[!states %in% RSstatelst]))
        warning(msg)
        states <- toString(states[states %in% RSstatelst])
        if (is.null(states) || states == "") {
          stop("")
        } else {
          message("getting coordinates for ", states)
        }
      }
    } else {
      states <- pcheck.states(states, RS=RS)
      if (is.null(states)) {
        states <- pcheck.states(states, RS=rslst)
      }
    }
    stcdlst <- pcheck.states(states, "VALUE")
  }
  rslst <- unique(FIESTAutils::ref_statecd[match(states, FIESTAutils::ref_statecd$MEANING), 
                                           "RS"])
  rslst[rslst %in% c("NERS", "NCRS")] <- "NRS"
  rslst <- unique(rslst)
  
  

  ######################################################################################
  ## Query tables - SURVEY, POP_EVAL, POP_EVAL_GRP, POP_EVAL_TYP
  ######################################################################################
  surveywhere.qry <- paste0(
      "\nWHERE ann_inventory IN (", addcommas(ann_inv, quotes=TRUE), ")",
      "\n   AND ", surveynm, ".statecd IN(", toString(stcdlst), ")",
      "\n   AND ", surveynm, ".invyr <> 9999 AND p3_ozone_ind = 'N'")
    
  survey.qry <- paste0(
    "SELECT * ",
    "\nFROM ", SCHEMA., surveynm,
    surveywhere.qry)
  
  SURVEY <- DBI::dbGetQuery(dbconn, survey.qry) 
  names(SURVEY) <- tolower(names(SURVEY))

  
  pop_eval_typ_qry <- paste0(
    "SELECT ptyp.* ",
    "\nFROM ", SCHEMA., "POP_EVAL_TYP ptyp ",
    "\nJOIN ", SCHEMA., "POP_EVAL_GRP pgrp ON(pgrp.CN = ptyp.EVAL_GRP_CN) ",
    "\nWHERE pgrp.statecd IN (", toString(stcdlst), ")")
  POP_EVAL_TYP <- setDT(DBI::dbGetQuery(dbconn, pop_eval_typ_qry)) 
  names(POP_EVAL_TYP) <- tolower(names(POP_EVAL_TYP))
  
  ## Define query POP_EVAL, POP_EVAL_TYP table
  popevalvars <- c("CN", "EVAL_GRP_CN", "RSCD", "EVALID", 
                     "EVAL_DESCR", "STATECD", "START_INVYR", "END_INVYR", "LOCATION_NM")
  pop_eval_qry <- paste0(
    "SELECT ", toString(paste0("pev.", popevalvars)), ", pet.eval_typ",
    "\nFROM ", SCHEMA., popevaltypnm, " pet ",
    "\nJOIN ", SCHEMA., popevalnm, " pev ON (pev.cn = pet.eval_cn) ",
    "\nWHERE pev.STATECD ", paste0("IN(", toString(stcdlst), ")"))
  POP_EVAL <- setDT(DBI::dbGetQuery(dbconn, pop_eval_qry)) 
  names(POP_EVAL) <- tolower(names(POP_EVAL))
  
  pop_eval_grp_qry <- paste0(
    "SELECT * ",
    "\nFROM ", SCHEMA., popevalgrpnm, 
    "\nWHERE statecd IN(", toString(stcdlst), ")")
  POP_EVAL_GRP <- setDT(DBI::dbGetQuery(dbconn, pop_eval_grp_qry)) 
  names(POP_EVAL_GRP) <- tolower(names(POP_EVAL_GRP))
  
  
  ## Add a parsed EVAL_GRP endyr to POP_EVAL_GRP
  eval_grpnm <- "eval_grp"
  POP_EVAL_GRP[, EVAL_GRP_Endyr := as.numeric(substr(POP_EVAL_GRP[[eval_grpnm]], 
                       nchar(POP_EVAL_GRP[[eval_grpnm]]) - 3, nchar(POP_EVAL_GRP[[eval_grpnm]])))]

  ## Create state filter
  stfilter <- getfilter("statecd", stcdlst, syntax='sql')
  

  ######################################################################################
  ## Generate invyrtab
  ######################################################################################
  if (!is.null(evalid)) {
  
    evalidnm <- "evalid"
    etypcd <- substr(evalid, nchar(evalid)-1, nchar(evalid))
    if (any(etypcd == "06")) {
      evalid <- sub("06", "03", evalid)
    }
  
    if (!all(evalid %in% POP_EVAL[[evalidnm]])) {
      notin <- evalid[!evalid %in% POP_EVAL[[evalidnm]]]
      stop("invalid EVALID: ", toString(notin))
    } else {
      ## Create invyrtab (if pop tables exist)
      invyrs <- list()
      evalidlist <- list()
      evalTypelist <- list()
      evalEndyrlist <- list()
      for (i in 1:length(evalid)) { 
        eval <- evalid[[i]]
        st <- substr(eval, 1, nchar(eval)-4)
        etypcd <- substr(eval, nchar(eval)-1, nchar(eval))
        state <- pcheck.states(st, "MEANING")
        pop_eval <- POP_EVAL[POP_EVAL[[evalidnm]] == eval,]
        startyr <- unique(min(pop_eval$start_invyr))
        endyr <- unique(min(pop_eval$end_invyr))
        ann_inventory <- SURVEY[SURVEY$statecd == st & SURVEY$invyr == endyr, 
                                  "ann_inventory"][[1]]
        stinvyr <- startyr:endyr
        if (length(unique(pop_eval$eval_typ)) > 1 && 
              all(unique(pop_eval$eval_typ) %in% c("EXPCURR", "EXPVOL"))) { 
          poptyp <- "EXPVOL"
        } else {
          poptyp <- unique(pop_eval$eval_typ)
        }
        evalTypelist[[state]] <- sub("EXP", "", unique(c(evalTypelist[[state]], poptyp))[1])
        evalEndyrlist[[state]] <- endyr
        if (state %in% names(invyrs)) {
          invyrs[[state]] <- sort(unique(c(invyrs[[state]], stinvyr)))
          evalidlist[[state]] <- sort(unique(c(evalidlist[[state]], eval)))
        } else {
          invyrs[[state]] <- stinvyr
          evalidlist[[state]] <- eval
        }
        invyrtab <- invyrtab[invyrtab$ANN_INVENTORY == ann_inventory,]
      }
      returnevalid <- TRUE
    } 
  }
  
  
  ######################################################################################
  ## If evalid was not input
  ###################################################################################### 
  if (!returnevalid) {
    
    ## Create invyrtab. Data frame with inventory years by state
    invyrqry <- paste0(
      "SELECT DISTINCT statecd, statenm, stateab, ann_inventory, invyr",
      "\nFROM SURVEY", 
      surveywhere.qry,
      "\nORDER BY statecd, invyr")
      invyrtab <- sqldf::sqldf(invyrqry, connection = NULL)
      cat("Inventory years by state...", "\n" )
      message(paste0(utils::capture.output(invyrtab), collapse = "\n"))

        
    statecdnm <- "statecd" 
    invyrnm <- "invyr" 
    ## Get possible range of inventory years from invyrtab
    stinvyr.vals <- as.list(by(invyrtab[[invyrnm]], invyrtab[[statecdnm]], range))
    names(stinvyr.vals) <- pcheck.states(names(stinvyr.vals), "MEANING")
    stinvyr.min <- lapply(stinvyr.vals, '[[', 1)
    stinvyr.max <- lapply(stinvyr.vals, '[[', 2)
    invyr.min <- min(unlist(stinvyr.min))
    invyr.max <- max(unlist(stinvyr.max))
      
    if (!all(states %in% names(stinvyr.vals))) {
      missnames <- states[!states %in% names(stinvyr.vals)]
      misscodes <- pcheck.states(missnames, "VALUE")
      warning("there is no data in the database for: ", toString(missnames))
      stcdlst <- stcdlst[!stcdlst %in% misscodes]
      states <- states[!states %in% missnames]
    }

    if (!is.null(evalid)) {
      evalresp <- TRUE
      
    } else if (is.null(evalEndyr)) {
      
      ## Check evalAll
      evalAll <- FIESTAutils::pcheck.logical(evalAll, varnm="evalAll", 
                                title="All evaluations?", first="YES")
      
      if (is.null(evalAll) || !evalAll) {
        ## Check evalCur
        evalCur <- pcheck.logical(evalCur, varnm="evalCur", 
                                  title="Most current evaluation?", first="YES")
        if (evalCur) evalresp <- TRUE
      } else {
        if (evalAll) {
          evalCur <- FALSE
          evalresp <- TRUE
        }
      }
      
      if ((is.null(evalCur) || !evalCur) && (is.null(evalAll) || !evalAll)) {
        returnlst <- list(states=states, rslst=rslst, 
                          evalidlist=NULL, 
                          invtype=invtype, 
                          invyrtab=invyrtab, 
                          evalType=evalTypelist)
          
        ## Return population information
        if (!is.null(surveynm)) {
          returnlst$surveynm <- surveynm
          if (exists("SURVEY") && is.data.frame(SURVEY)) {
            returnlst$SURVEY <- SURVEY
          }
        }
        if (!dbconnopen) {
          DBI::dbDisconnect(dbconn)
        } else {
          returnlst$dbconn <- dbconn
        }
        return(returnlst)
      }
    }
    
    ## Check evalEndyr
    if (!is.null(evalEndyr)) {
      evalresp <- TRUE
      if (class(evalEndyr)[1] != "list") {
        if (!is.vector(evalEndyr) || !is.numeric(as.numeric(evalEndyr))) {
          stop("invalid evalEndyr")
        }
        evalEndyr <- sapply(states, function(x) list(evalEndyr))
      } else {
        if (length(evalEndyr) > 1 && is.null(names(evalEndyr))) {
          stop("invalid evalEndyr... names do not match states")
        }
      }
      if (!is.null(invyrtab)) {
        for (st in names(evalEndyr)) {
          evalendyr <- evalEndyr[[st]]
          invendyr.min <- stinvyr.min[[st]]
          invendyr.max <- stinvyr.max[[st]]
          
          if (all(evalendyr < invendyr.min) || any(evalendyr > invendyr.max)) {
            warning(paste("check evalEndyr.. outside of range in database:", st))    
            evalEndyr[[st]] <- invendyr.max
            #evalresp <- FALSE
          }
        }
      } 
    }
    
    ## Get last year of evaluation period and the evaluation type
    if (evalresp) {
      ## Get the evalidation type
      if (is.list(evalType)) evalType <- unlist(evalType)
      evalType <- pcheck.varchar(var2check=evalType, varnm="evalType", 
                                 checklst=evalTypelst, caption="Evaluation type", multiple=TRUE, 
                                 preselect="VOL")
      if (is.null(evalType)) {
        evalType <- "VOL"
      }
      
      invyrs <- list()
      evalidlist <- sapply(states, function(x) NULL)
      evalEndyrlist <- sapply(states, function(x) NULL)
        
      ## check evalType 
      if (invtype == "PERIODIC" && evalType == "ALL") {
        evalType <- "CURR"
      } else {
        if (length(grep("VOL", evalType, ignore.case=TRUE)) > 0) {
          evalType[grep("VOL", evalType, ignore.case=TRUE)] <- "VOL" 
        }
        if (length(grep("VOL", evalType, ignore.case=TRUE)) > 0 && 
            length(grep("CURR", evalType, ignore.case=TRUE)) > 0) {
          evalType <- evalType[-grep("CURR", evalType, ignore.case=TRUE)]
        }  
        if (length(grep("GRM", evalType, ignore.case=TRUE)) > 0) {
          evalType[grep("GRM", evalType, ignore.case=TRUE)] <- "GROW"  
        }
      }
        
      evalTypelist <- sapply(states, function(x) list(unique(evalType)))
      evalTypelist <- lapply(evalTypelist, function(x) paste0("EXP", x))
        
      ## Loop thru states
      for (stcd in stcdlst) {
        state <- pcheck.states(stcd, "MEANING")
        stabbr <- pcheck.states(stcd, "ABBR")
        message("getting FIA Evaluation info for: ", state, "(", stcd, ")...")
          
        stinvyrs <- unique(stinvyr.vals[[state]])
        invtype.invyrs <- setDT(invyrtab)[invyrtab$statecd == stcd][["invyr"]]
        if (stcd == 64) {
          invtype.invyrs[invtype.invyrs == 2016] <- 6416
        }
          
        ## In POP_EVAL table, Texas has several evaluations based on East, West, Texas
        ## Remove East and West in LOCATION_NM and EVAL_DESCR
        #          if (stcd == 48) {
        #            POP_EVAL_GRPstcd <- POP_EVAL_GRP[STATECD == stcd & 
        #		            grepl("EAST", POP_EVAL_GRP$EVAL_GRP_DESCR, ignore.case=TRUE) & 
        #		            grepl("WEST", POP_EVAL_GRP$EVAL_GRP_DESCR, ignore.case=TRUE), ]
        #          } else {
        POP_EVAL_GRPstcd <- POP_EVAL_GRP[statecd == stcd,]
        
        ## Get evalid and inventory years from POP_EVAL table
        setkey(POP_EVAL, "eval_grp_cn")
        setkey(POP_EVAL_GRPstcd, "cn")
            
        ## Subset POP_EVAL/POP_EVAL_GRP by state and inventory type
        #         popevaltab <- POP_EVAL[POP_EVAL$EVAL_GRP_CN %in% POP_EVAL_GRPstcd$CN,]
        popevalgrptab <- POP_EVAL_GRPstcd[POP_EVAL_GRPstcd$EVAL_GRP_Endyr %in% invtype.invyrs,]
        if (stcd == 48) {
          #            POP_EVAL_GRPstcd <- POP_EVAL_GRP[STATECD == stcd & 
          #		            grepl("EAST", POP_EVAL_GRP$EVAL_GRP_DESCR, ignore.case=TRUE) & 
          #		            grepl("WEST", POP_EVAL_GRP$EVAL_GRP_DESCR, ignore.case=TRUE), ]
          popevalgrptab <- popevalgrptab[
              (!grepl("EAST", popevalgrptab$eval_grp_descr, ignore.case=TRUE) & 
                   !grepl("WEST", popevalgrptab$eval_grp_descr, ignore.case=TRUE)), ]
        }
        if (nrow(popevalgrptab) == 0) {
          if (nrow(popevalgrptab) == 0) {
            returnlst <- list(states = states, rslst = rslst,
                              evalidlist = evalidlist,
                              invtype = invtype, invyrtab = invyrtab,
                              invyrs = invyrs, evalType = evalTypelist)		  
          }
        }
        popevaltab <- POP_EVAL[POP_EVAL$eval_grp_cn %in% popevalgrptab$cn,]
        POP_EVAL_endyrs <- na.omit(unique(popevalgrptab[["EVAL_GRP_Endyr"]]))
            
        if (!is.null(evalEndyr)) {
          Endyr <- evalEndyr[[state]]
          if (!all(Endyr %in% POP_EVAL_endyrs)) {
            missEndyr <- Endyr[!Endyr %in% POP_EVAL_endyrs]
            stop(paste0(toString(missEndyr), " data are not in ", 
                            stabbr, "_", "POP_EVAL: ", toString(POP_EVAL_endyrs)))
          }  
        } else {   ## is.null(evalEndyr)
          if (evalCur) {
            Endyr <- max(POP_EVAL_endyrs)
          } else if (evalAll) {
            Endyr <- POP_EVAL_endyrs
          } else {
            if (length(POP_EVAL_endyrs) > 1) {
              Endyr <- select.list(as.character(POP_EVAL_endyrs), 
                                       title="Eval End Year?", multiple=FALSE)
              if (Endyr == "") stop("")
            } else {
              Endyr <- max(POP_EVAL_endyrs)
              warning("No end year specified.. using most current year in database")
            }
          }
        }
            
        ## Populate evalEndyrlist
        evalEndyrlist[[state]] <- Endyr
            
        ## Subset popevaltab by Endyr
        popevaltab <- popevaltab[end_invyr %in% Endyr,]
        popevalgrptab <- popevalgrptab[popevalgrptab$EVAL_GRP_Endyr %in% Endyr,]
        #popevalgrptab <- POP_EVAL_GRPstcd[POP_EVAL_GRPstcd$EVAL_GRP_Endyr %in% Endyr,]
        popevaltab <- POP_EVAL[POP_EVAL$eval_grp_cn %in% popevalgrptab$cn,]
            
            
        ## Check evalType with evalType in database for state
        evalType.chklst <- unique(popevaltab$eval_typ)
            
        if (invtype %in% c("ANNUAL", "BOTH")) {
          #if (invtype == "ANNUAL") {
          if (!all(evalTypelist[[state]] %in% evalType.chklst)) {
            eType.invalid <- evalTypelist[[state]][!evalTypelist[[state]] %in% evalType.chklst]
            warning("removing invalid evalType for ", state, ": ", 
                        toString(eType.invalid), "... \nmust be following list: ", 
                        toString(evalType.chklst))
            evalTypelist[[state]] <- evalTypelist[[state]][!evalTypelist[[state]] %in% eType.invalid]
          }
          evalidall <- unique(popevaltab$evalid[!is.na(popevaltab$evalid)])
          evalidlist[[state]] <- 
                sort(unique(popevaltab$evalid[popevaltab$eval_typ %in% evalTypelist[[state]]]))
          invyrs[[state]] <- 
                min(popevaltab$start_invyr, na.rm=TRUE):max(popevaltab$end_invyr, na.rm=TRUE)
              
        } else {
          if (!all(evalTypelist[[state]] %in% evalType.chklst)) { 
            evalid.min <- min(popevaltab$evalid)
            evalTypelist[[state]] <- 
                  popevaltab[popevaltab$evalid == min(popevaltab$evalid), "eval_typ"][1]
                warning(paste("invalid evalType for", state, "...using", evalTypelist[[state]]))
          }
          evalidlist[[state]] <- 
                sort(unique(popevaltab$evalid[popevaltab$eval_typ %in% evalTypelist[[state]]]))
          invyrs[[state]]  <- ifelse (any(is.na(popevaltab$end_invyr)), 
                                          unique(as.numeric(popevaltab$report_year_nm)), 
                                          min(popevaltab$start_invyr, na.rm=TRUE):max(popevaltab$end_invyr, na.rm=TRUE))
        }  ## ANNUAL/BOTH
      }  ## invtype
    }  ## for state loop
  }  ## returnevalid
  
  returnlst <- list(states = states, rslst = rslst, 
                    evalidlist = evalidlist, 
                    invtype = invtype, invyrtab = invyrtab, 
                    evalTypelist = evalTypelist, 
                    evalEndyrlist = evalEndyrlist,
                    invyrs = invyrs)
  
  if (!dbconnopen) {
    DBI::dbDisconnect(dbconn)
  } else {
    returnlst$dbconn <- dbconn
  }
  
  return(returnlst)
}
