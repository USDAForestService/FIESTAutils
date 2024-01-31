# getext - get extent of filename
# checkfilenm
# getoutfn
# addcommas
# pastevars
# stopQ
# removecols
# DT_NAto0
# changeNull
# getdups
# getlistparam
# getnm
# checknm			If nm exists, changes to nm_*
# check.namedlist
# capfirst
# nbrdecimals
# nbrdigits
# wraptitle
# addclass
# xtabf
# recodelut
# findnm
# chkdbtab  Checks if table exists in list of database tables
# RtoSQL    Convert logical R statement syntax to SQL syntax
# int64tochar  convert columns with class integer64 to character
# messagedf - write a df to screen
# getSPGRPCD - get spgrpcd attribute(s) in ref_species from ref_statecd
# addFORTYPGRPCD - adds FORTYPGRPCD to a table with FORTYPCD variable
# date2char - convert date columns (POSIXct) to formatted character
# getfilter - create filter string from an attribute (att) and values (val)
# checklevels - check for matching levels in x and xunique

#' @rdname internal_desc
#' @export
getext <- function(x) {
  xbasename <- basename(x)
  strsplit(xbasename, paste0(basename.NoExt(xbasename), "."))[[1]][2]
}

#' @rdname checks_desc
#' @export
checkfilenm <- function(fn, outfolder=NULL, ext=NULL,
	stopifnull=FALSE) {

  if (is.null(fn)) {
    #message("file name is NULL")
    if (stopifnull) {
      stop()
    } else {
      return(NULL)
    }
  }
  if (!is.character(fn)) {
    message("file name must be a character string")
  }
  if (is.null(outfolder)) {
    if (file.exists(fn)) {
      return(normalizePath(fn))
    } else {
      outfolder <- normalizePath(dirname(fn))
      fn <- basename(fn)
    }
  }

  outfolder <- tryCatch(pcheck.outfolder(outfolder),
     	 	error=function(e) {
			return(NULL) })
  if (is.null(outfolder)) {
    stop("invalid outfolder or file name")
  }

  if (file.exists(file.path(outfolder, fn))) {
    return(file.path(outfolder, fn))
  } else if (!is.null(ext)) {
    if (substring(ext, 1, 1) != ".") {
      ext <- paste0(".", ext)
    }
    if (!file.exists(file.path(outfolder, paste0(fn, ext)))) {
      if (stopifnull) {
        stop("file name does not exist")
      } else {
        return(NULL)
      }
    } else {
      return(file.path(outfolder, paste0(fn, ext)))
    }
  } else {
    return(NULL)
  }
}

#' @rdname internal_desc
#' @export
getoutfn <- function(outfn, outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE,
	overwrite=FALSE, ext=NULL, baseonly=FALSE, noext=FALSE,
	outfn.default="outfile", add=TRUE, append=FALSE, gui=FALSE) {
  ## DESCRIPTION: get full pathname

  ## Check outfn
  if (is.null(outfn)) {
    if (!is.null(outfn.default)) {
      outfn <- outfn.default
    } else {
      stop("outfn and outfn.default is null")
    }
  }
  if (!is.character(outfn)) {
    stop("outfn must be a character string")
  }
  extfn <- getext(outfn)

  ## Check ext
  DefaultExt <- c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", 
		"bil", "img", "vrt")
  extlst <- c("sqlite", "sqlite3", "db", "db3", "csv", "txt", "gdb",
		"shp", "gpkg", "jpg", "png", "pdf", "rda", "rds", "llo",
           DefaultExt)
  if (!is.na(extfn) && extfn %in% extlst) {
    ext <- extfn
  } else if (!is.null(ext)) {
    if (startsWith(ext, "."))
      ext <- sub(".", "", ext)
    if (!ext %in% extlst)
      stop("ext not supported")

    if (any(is.na(getext(outfn))) || any(!extfn %in% extlst))
      outfn <- paste0(outfn, ".", ext)
  } else if (is.na(getext(outfn))) {
    stop("specify out_format")
  } else {
    stop(extfn, " not supported")
  }
 
  ## Get basename
  outfn.base <- basename.NoExt(outfn)
  outfn.dir <- dirname(outfn)

  if (outfn.dir != ".") {
    if (is.null(outfolder)) {
      outfolder <- outfn.dir
    } else {
      if (dir.exists(file.path(outfolder, outfn.dir))) {
        outfolder <- file.path(outfolder, outfn.dir)
      }
    }
  }

  ## Check if outfolder
  if ((any(is.na(extfn)) || any(extfn=="NA")) && any(dir.exists(outfn))) {
    message("outfn is a folder name... must be a file name")
    return(outfn)
  }
  if (is.null(outfolder)) {
    if (!dir.exists(dirname(outfn)) && !dir.exists(dirname(normalizePath(outfn)))) {
      stop(outfn, " does not exist")
    } else if (dir.exists(dirname(outfn))) {
      outfolder <- dirname(outfn)
    } else {
      outfolder <- dirname(normalizePath(outfn))
    }
  }

  ## Check outfn.pre
  if (!is.null(outfn.pre) && is.character(outfn.pre)) {
    outfn.base <- paste(outfn.pre, outfn.base, sep="_")
  }
  ## DESCRIPTION: gets outfile name
  if (outfn.date) {
    outfn.base <- paste0(outfn.base, "_", format(Sys.time(), "%Y%m%d"))
  }
  ## Get full path filename
  outfolder <- pcheck.outfolder(outfolder, gui=gui)
  outfilenm <- file.path(outfolder, outfn.base)

  if (overwrite) {
    nm <- paste0(outfilenm, ".", ext)

    if (file.exists(nm)) {
      test <- tryCatch(
        file.remove(nm),
			warning=function(war) {
             			#stop(war,"\n")
             			stop("cannot overwrite file... permission denied\n")
			}, error=function(err) {
					message(err)
			} )
      if (is.null(test)) {
        test <- tryCatch(
          unlink(nm),
			warning=function(war) {
             			#stop(war,"\n")
             			stop("cannot overwrite file... permission denied\n")
			}, error=function(err) {
					message(err)
			} )
        if (is.null(test)) {
          stop("permission denied")
        }
      }
      message("overwriting ", nm, "...")
    }
  } else if (!append && !add) {
    outfn.base <- fileexistsnm(outfolder, outfn.base, ext)
  }
  if (!baseonly) {
    ## Check outfolder
    #outfolder <- pcheck.outfolder(outfolder, gui=gui)
    outfilenm <- file.path(normalizePath(outfolder), outfn.base)
  } else {
    outfilenm <- outfn.base
  }
  if (!noext) {
    ext <- ifelse(ext=="sqlite", "db", ext)
    if (substring(ext, 1, 1) == ".") {
      outfilenm <- paste0(outfilenm, ext)
    } else {
      outfilenm <- paste0(outfilenm, ".", ext)
    }
  }
  return(outfilenm)
}

#' @rdname internal_desc
#' @export
addcommas <- function(vars, ALIAS=NULL, sepchar=",", quotes=FALSE, paren=FALSE){
  ## DESCRIPTION:
  ## Internal function to input a vector of string variables and outputs a string of
  ##  variables separated by commas. If an alias is included, the function will
  ##   concatenate the alias before each variable before a dot.
  ## ARGUMENTS:
  ##   vars - Sring vector.
  ##  ALIAS -String. A shortname to put in front of variable before a dot (ex. ALIAS.var)
  ## VALUE:
  ##  A string of variables separated by commas and with or without alias.
  ## EXAMPLE:
  ##   avector <- c("CAT", "DOG", "MOUSE")
  ##  addcommas(avector)
  ##  addcommas(avector, "a")

  if (is.null(vars)) return(NULL)

  if (!is.null(ALIAS)) {
    xvars <- paste(ALIAS, vars, sep=".")
  } else {
    xvars <- vars
  }
  if (quotes) {
    newvars <- paste0("'", xvars[1], "'")
    if (length(xvars) > 1)
      for (j in 2:length(xvars)) newvars <- paste0(newvars, sepchar, "'", xvars[j], "'")

  } else {
    newvars <- paste(xvars, collapse=sepchar)

    #newvars <- xvars[1]
    #if(length(xvars) > 1){
    #  for(j in 2:length(xvars)) { newvars <- paste(newvars, sepchar, xvars[j], sep=" ") }
    #}
  }
  if (paren) newvars <- paste0("(", newvars, ")")

  return(newvars)
}

#' @rdname internal_desc
#' @export
pastevars <- function(vars1, vars2, sep=",") {
  if (is.null(vars1) && is.null(vars2)) {
    return(NULL)
  } else if (is.null(vars1)) {
    return(vars2)
  } else if (is.null(vars2)) {
    return(vars1)
  } else {
    paste(vars1, vars2, sep=sep)
  }
}

#' @rdname internal_desc
#' @export
stopQ <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

#' @rdname internal_desc
#' @export
removecols <- function (x, vars) {
  ## DESCRIPTION: Removes columns by name from data frame
  x[,which(!names(x) %in% vars)]
}

#' @rdname internal_desc
#' @export
DT_NAto0 <- function (DT, cols, changeto=0) {
  ## DESCRIPTION: Change NA values to 0 values in data.table

  if (!any(class(DT) %in% "data.table")) DT <- setDT(DT)

  cols <- cols[which(cols %in% names(DT))]

  for (col in cols) {
    if (is.logical(DT[[col]]) && all(is.na(DT[[col]])))
      DT[[col]] <- as.numeric(DT[[col]])

    if (is.numeric(DT[[col]])) {
      if (is.null(changeto)) changeto <- 0
      if (is.character(changeto)) {
        DT[[col]] <- as.character(DT[[col]])
        message("changed ", col, " to character")
      }
      set(DT, which(is.na(DT[[col]])), col, changeto)
    } else {
      if (is.null(changeto)) changeto <- 0
      if (is.factor(DT[[col]])) {
        DT[[col]] <- as.character(DT[[col]])
        set(DT, which(is.na(DT[[col]])), col, changeto)
      } else {
        set(DT, which(is.na(DT[[col]])), col, changeto)
      }
    }
  }
  return(data.table(DT))
}

#' @rdname internal_desc
#' @export
changeNULL <- function(x, xvar, changeto=NULL){
  ## DESCRIPTION: changes NULL values to the value of changeto

  for(var in xvar){
    if(is.null(changeto)){
      changeto <- "NONFOREST"

      ## CHANGE rowvar domain values from NA values to 0 values
      if(is.numeric(x[,var])){
        x[is.na(x[,var]), var] <- 0
      }else if(is.factor(x[,var])){
        x[,var] <- as.character(x[,var])
        x[is.na(x[,var]),var] <- changeto
      }else{
        x[is.na(x[,var]),var] <- changeto
      }
    }else{
      if(is.factor(x[,var])){
        x[,var] <- as.character(x[,var])
        x[is.na(x[,var]),var] <- changeto
      }else{
        x[is.na(x[,var]) | is.null(x[,var]) | x[,var] == "", var] <- changeto
      }
    }
  }

  return(x)
}

#' @rdname internal_desc
#' @export
getdups <- function(cx, cuniqueid="PLT_CN", varnm, fun) {
  ## DESCRIPTION: FUNCTION TO GET DUPLICATE PLOTS

  undups <- aggregate(cx[,varnm], list(cx[,cuniqueid]), fun)
  names(undups) <- c(cuniqueid, varnm)
  conddups <- merge(cx, undups, by=c(cuniqueid, varnm))
  return(conddups)
}

#' @rdname internal_desc
#' @export
getlistparam <- function(lst)
  paste0("list(",toString(paste(names(lst), lst, sep="=")),")")

#' @rdname internal_desc
#' @export
getnm <- function (xvar, group=FALSE) {
  ## DESCRIPTION: creates a name variable from a code variable.
  ## If 'CD' is at the end of the variable name, changes CD to NM, else adds NM
  ## to variable name.

  CDchar <- as.vector(gregexpr("CD", xvar)[[1]])
  if (CDchar > 0) {
    xvarnm <- xvar
    substring(xvarnm, CDchar[length(CDchar)], CDchar[length(CDchar)]+1) <- "NM"
    if (group) {
      pre <- substr(xvar, 1, CDchar[length(CDchar)]-1)
      post <- substr(xvar, CDchar[length(CDchar)]+2, nchar(xvar))
      grpcode <- paste0(pre, "GRPCD", post)
      grpname <- paste0(pre, "GRPNM", post)
    }
  } else {
    xvarnm <- paste0(xvar, "NM")
    if (group) {
      grpcode <- paste0(xvar, "GRPCD")
      grpname <- paste0(xvar, "GRPNM")
    }
  }
  xnames <- list(xvarnm = xvarnm)
  if (group) {
    xnames$grpcode <- grpcode
    xnames$grpname <- grpname
  }
  return(xnames)
}

#' @rdname checks_desc
#' @export
checknm <- function(nm, nmlst, ignore.case=TRUE) {
  ## if nm already exists in nmlst, change nm to nm_*
  i <- 0
  while (any(grepl(nm, nmlst, ignore.case=ignore.case))) {
  #while (nm %in% nmlst) {
    i <- i + 1
    nm <- paste(nm, 1, sep="_")
    message("name exists... changed name to ", nm)
  }
  return(nm)
}

#' @rdname checks_desc
#' @export
check.namedlist <- function(xlst, checknms=NULL, modetype="numeric") {
  # xlst		String. Name of list.
  # checknms	String vector. Names to check with list names.

  x <- get(xlst)
  if (class(x)[1] != "list")
    stop(xlst, " must be a named list")

  ## Check if xlst is a list
  xnms <- names(x)
  if (any(is.null(xnms)))
    stop(paste(xnms, "must be a named list"))

  ## Check if all names in checknms
  if (!is.null(checknms)) {
    if (!all(xnms %in% checknms))
      stop(xlst, " has invalid names")
  }

  ## Check if all values have correct modetype
  if (any(lapply(x, mode) != modetype))
    stop(xlst, " must be ", modetype)
}

#' @rdname internal_desc
#' @export
capfirst <- function(x, allwords=FALSE){
  ## DESCRIPTION: Internal function to capitalize first character.
  ## allwords - If allwords=TRUE, capitalize first letter of each word

  capfun <- function(y) {
    substring(y, 1, 1) <- toupper(substring(y, 1, 1))
    return(y)
  }

  x <- tolower(x)
  if (allwords) {
    x2 <- strsplit(x, " ")
    x2 <- lapply(x2, capfun)
    x <- unlist(lapply(x2, paste, collapse=" "))
  } else {
    x <- capfun(x)
  }
  return(x)
}

#' @rdname internal_desc
#' @export
nbrdecimals <- function(x) {
#  if ((x %% 1) != 0) {
#    strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
#    n <- nchar(strs[[1]][2])
#  } else {
#    n <- 0
#  }
  strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
  n <- nchar(strs[[1]][2])
  if (is.na(n)) n <- 0
  return(n)
}

#' @rdname internal_desc
#' @export
nbrdigits <- function(x) {
  ## DESCRIPTION: get max number of digits in front of decimal point
  ## x - vector of numbers

  max(nchar(sapply(strsplit(as.character(x), "\\."), "[[", 1)), na.rm=TRUE)
}

#' @rdname internal_desc
#' @export
getfilter <- function(att, val, syntax="R") {
## DESCRIPTION: create filter string from att and val
## syntax - ('R', 'sql')
  if (is.character(val)) {
    val <- encodeString(val, quote="'")
  }
  filter <- paste0(att, " %in% c(", toString(val), ")")

  if (syntax == 'sql') {
    filter <- gsub("%in% c", "in", filter)
  }
  return(filter)
}

#' @rdname internal_desc
#' @export
filter2qry <- function(filt, layernm) {
  if (grepl("==", filt)) {
    part2 <- sub("==", "=", filt)
  } else if (grepl("%in%", filt)) {
    part2 <- sub("%in% c", "in", filt)
  } else if (grepl("!=", filt)) {
    part2 <- sub("!=", "<>", filt)
  }
  paste("select * from", layernm, "where", part2)
}

#' @rdname internal_desc
#' @export
wraptitle <- function(x, len=10) {
  sapply(x, function(y) paste(strwrap(y, len),
         collapse = "\n"), USE.NAMES = FALSE)
}

# addclass <- function(x, xtab, xvar, brks) {
#   ## DESCRIPTION: Adds a class based on breaks to a table in modGBpop
#   x[[xtab]] <- datLUTclass(x=x[[xtab]], xvar=xvar, cutbreaks=brks)$xLUT
#   return(x)
# }

#' @rdname internal_desc
#' @export
xtabf <- function(x, y, levels) {
  table(factor(x, levels=levels), factor(y, levels=levels))
}

#' @rdname internal_desc
#' @export
recodelut <- function(lut, minvar="min", maxvar="max", classvar="class") {
  ## DESCRIPTION: converts lut with min/max values for continuous data to a
  ## lookup table by value
  lut2 <- lapply(lut[[classvar]], function(x, lut) {
          data.frame(value=c(lut[lut[[classvar]] == x, minvar]:lut[lut[[classvar]] == x, maxvar]),
 				class=rep(x))
      	}, lut)
  lut2 <- do.call(rbind, lut2)
  return(lut2)
}

#' @rdname internal_desc
#' @export
findnm <- function(x, xvect, returnNULL=FALSE) {
  if (is.null(x)) {
    if (returnNULL) {
      return(NULL)
    } else {
      stop("variable is NULL")
    }
  }
  test <- grepl(x, xvect, ignore.case=TRUE)
  if (sum(test) == 0) {
    if (returnNULL) {
      return(NULL)
    }
    stop("name not found")
  } else {
    testnames <- xvect[test]
    test <- sapply(testnames, function(y,x) match(tolower(y), tolower(x)), x)
    test <- names(test)[!is.na(test)]
    if (length(test) == 1) {
      if (all(is.na(test))) {
        if (returnNULL) {
          return(NULL)
        } else {
          stop("no matching names found")
        }
      }
      return(test)
	} else if (length(test) == 0) {
      if (returnNULL) {
        return(NULL)
      } else {
        stop("no matching names found")
      }	  
    } else {
	  message("more than 1 name found: ", toString(test))
      return(test)
    }
  } 
}

#' @rdname internal_desc
#' @export
chkdbtab <- function(dbtablst, tab, stopifnull=FALSE) {
  ## DESCRIPTION: checks if table exists in list of database tables
  ## 		If doesn't exist, returns NULL, else returns table name
  
  ## check tab
  if (is.null(tab)) {
    if (stopifnull) {
      stop(tab, "is NULL")
    } else {
      return(NULL)
    }
  } else if (!is.character(tab)) {
	return(NULL)
  }
  
  ## check dtablst
  if (!is.vector(dbtablst) || !is.character(dbtablst)) {
	return(NULL)
  }
  
  if (tolower(tab) %in% tolower(dbtablst)) {
    return(dbtablst[tolower(dbtablst) == tolower(tab)])
  } else {
    message(tab, " does not exist in database")
    return(NULL)
  }
}


#' @rdname internal_desc
#' @export
RtoSQL <- function(filter, x=NULL) {
  ## DESCRIPTION: Convert logical R statement syntax to SQL syntax

  ## Check filter
  if (is.null(filter) || filter == "") {
    return(NULL)
  }

  ## Check logic
  if (!is.null(x)) {
    sql <- check.logic(x=x, statement=filter)
  } else {
    sql <- filter
  }


  checkpart <- function(part) {
    ## Function to convert logical statement by part
    part <- trimws(part)


    ## Check for !
    not <- ifelse(grepl("\\!", part), TRUE, FALSE)

    # Replace R comparison operators with SQL operators
    if (grepl("==", part)) {
      if (not) {
        part <- gsub("!", "", part)
        part <- gsub("==", "<>", part)
      } else {
        part <- gsub("==", "=", part)
      } 
    }
    part <- gsub("!=", "<>", part)

    if (grepl("%in%", part)) {
      if (not) {
        part <- gsub("!", "", part)
        part <- gsub("%in% c", "not in", part)
      } else {
        part <- gsub("%in% c", "in", part)
		if (grepl(":", part)) {
		  p1 <- strsplit(part, ":")[[1]][1]
		  p1 <- strsplit(p1, "\\(")[[1]][2]
		  p2 <- strsplit(part, ":")[[1]][2]
		  p2 <- strsplit(p2, "\\)")[[1]][1]
		  part <- gsub(paste0(p1, ":", p2), toString(seq(p1,p2)), part)	
        }		  
      }
    }

    # Replace is.na() and !is.na() with SQL equivalents
    if (grepl("is.na", part)) {
      pre <- strsplit(part, "is.na\\(")[[1]][1]
      basetmp <- strsplit(part, "is.na\\(")[[1]][-1]
      base <- strsplit(basetmp, "\\)")[[1]][1]
      post <- ifelse (is.na(strsplit(basetmp, "\\)")[[1]][2]), "", strsplit(basetmp, "\\)")[[1]][2])

      if (not) {
        part <- paste0(pre, base, " is not NULL", post)
        part <- gsub("!", "", part)
      } else {
        part <- paste0(pre, base, " is NULL", post)
      }
    }

    return(part)
  }

  if (grepl("&", filter)) {
    sql <- paste(sapply(unlist(strsplit(filter, "&")), checkpart), collapse = " and ")
  } else {
    sql <- checkpart(filter)
  }
  if (grepl("&", filter)) {
    sql <- paste(sapply(unlist(strsplit(sql, "\\|")), checkpart), collapse = " or ")
  }

  return(sql)
}

#' @rdname internal_desc
#' @export
int64tochar <- function(df) {
  ## DESCRIPTION: convert columns with class integer64 to character
  if (any(grepl("integer64", lapply(df, class)))) {
    int64cols <- names(df)[grepl("integer64", lapply(df, class))]
    df <- setDF(df)
    df[int64cols] <- lapply(df[int64cols], as.character)
  }
  return(df)
}

#' @rdname internal_desc
#' @export
messagedf <- function(df) {
  message(paste0(utils::capture.output(df), collapse = "\n"))
}


#' @rdname internal_desc
#' @export
getSPGRPCD <- function(states) {
  ## DESCRIPTION: get spgrpcd attribute(s) in ref_species from ref_statecd
  ##				ordered by majority

  states <- pcheck.states(states)
  ref_state <- FIESTAutils::ref_statecd[FIESTAutils::ref_statecd$MEANING %in% states, ]

  if (length(unique(ref_state$REGION_SPGRPCD)) == 1) {
    grpnames <- paste0(unique(ref_state$REGION), "_SPGRPCD")
    #grpcode <- grpnames
  } else {
    grpnames <- paste0(names(table(ref_state$REGION_SPGRPCD))[
	   table(ref_state$REGION_SPGRPCD) == max(table(ref_state$REGION_SPGRPCD))], 
			"_SPGRPCD")
    #grpcode <- grpnames[1]
  } 
  return(grpnames)
}


#' @rdname internal_desc
#' @export
addFORTYPGRPCD <- function(cond) {
  ## DESCRIPTION: adds FORTYPGRPCD to a table with FORTYPCD variable
  
  if ("FORTYPGRPCD" %in% names(cond)) {
    message("FORTYPGRPCD already in table")
	return(cond)
  }
  if (!"FORTYPCD" %in% names(cond)) {
    message("the input table must include FORTYPCD")
	return(NULL)
  }
  
  ref_fortyp <- FIESTAutils::ref_codes[FIESTAutils::ref_codes$VARIABLE == "FORTYPCD", 
                       c("VALUE", "GROUPCD")]
  names(ref_fortyp) <- c("FORTYPCD", "FORTYPGRPCD")
  
  tabs <- check.matchclass(cond, ref_fortyp, "FORTYPCD",  
		         tab1txt="cond", tab2txt="ref_codes")
  cond <- tabs$tab1
  ref_fortyp <- tabs$tab2
 
  cond <- merge(cond, ref_fortyp, by="FORTYPCD", all.x=TRUE)
  return(cond)
}



#' @rdname internal_desc
#' @export
date2char <- function(df, col, formatstr = '%Y-%m-%d') {
  ## DESCRIPTION: convert date columns (POSIXct) to formatted character

  if (is.null(formatstr)) {
    df[[col]] <- as.character(df[[col]])
  } else {
    df[[col]] <- as.character(format(df[[col]], formatstr))
  }
  return(df)
}


#' @rdname internal_desc
#' @export
getfilter <- function(att, val, syntax="R", quote=FALSE) {
## DESCRIPTION: create filter string from att and val
## syntax - ('R', 'sql')
  if (is.character(val) || quote) {
    val <- encodeString(val, quote="'")
  } 
  filter <- paste0(att, " %in% c(", toString(val), ")")

  if (syntax == 'sql') {
    filter <- gsub("%in% c", "IN", filter)
  }
  return(filter)
}


#' @rdname internal_desc
#' @export
checklevels <- function(x, uniquex, xvar, keepNA=FALSE) {
  ## DESCRIPTION: Check for matching levels in x and xunique
  
  ## Add any factors levels that are missing
  if (is.factor(uniquex[[xvar]])) {
    xvarlevels <- levels(uniquex[[xvar]])
    xvals <- sort(unique(x[[xvar]]), na.last = TRUE)
	xmisslevels <- as.character(xvals[!xvals %in% xvarlevels])	    
	if (length(xmisslevels) > 0) {
	  if (any(is.na(xmisslevels)) && keepNA) {
		xmisslevels[is.na(xmisslevels)] <- "NA"
      }
      levels(uniquex[[xvar]]) <- c(xvarlevels, xmisslevels)
    }
    if (is.factor(x[[xvar]])) {
	  if (!identical(levels(x[[xvar]]), levels(uniquex[[xvar]]))) {
        levels(x[[xvar]]) <- levels(uniquex[[xvar]])
      }   
    } else {
      x[[xvar]] <- factor(x[[xvar]], levels=levels(uniquex[[xvar]]))
    }	  
  }
  
  return(list(x=x, uniquex=uniquex))
}

