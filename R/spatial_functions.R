## polyfix.sf
## trunc10shp - truncates all variables in Spatial object to 10 characters of less
## getESPG - gets table of ESPG codes, filtering with input parameters
## merge_extents - merge extents of 2 spatial layers
## check.extents - check extents of 2 spatial layers.
## getprjatt - gets the desired attribute from the proj4string.
## areacalc.poly - calculates area of polygons and appends to polygon attribute table.
## writeESRIprj - Adds *.prj file to folder with *.bil file.
## checksf.longlat
## crsCompare - compares projections, if different projects
## sf_dissolve - dissolve vector polygons
## closest_poly - get polygon of y with closest polygon to x (slower than centroid)
## getIntersect - get intersection of layer1 with layer2
## clip.othertables
## check.area


#' @rdname spatial_desc
#' @export
polyfix.sf <- function(x) {
  # any bad polys?

  valid <- NULL
  message("checking polygons...")
  if (suppressWarnings(sum(sf::st_is_valid(x) == FALSE)) > 0) {
    message("poly invalid")

    ## Remove empty geometries
 	if (sum(sf::st_is_empty(x)) > 0) {
	  x <- x[!sf::st_is_empty(x),]
	}

    ## Check 
    if (sf::st_is_longlat(x)) {
      stop("polygons layer must be projected")
    }
	
    # this is a well known R / GEOS hack (usually combined with the above) to
    # deal with "bad" polygons
    message("buffering poly with 0 width...")
    x <- sf::st_buffer(x[!is.na(valid)], 0.0)

    message("checking polygons after buffer...")
    if (sum(sf::st_is_valid(x) == FALSE) > 0) {
	  message("invalid polygons exist")
	  sf::st_make_valid(x)
	}
  }
  return(x)
}

#' @rdname spatial_desc
#' @export
build.prj4str <- function(prj, datum=NULL, ellps=NULL, zone=NULL, zoneS=FALSE,
                          aea.param="USGS", gui=FALSE) {
  
  #######################################################################
  ## DESCRIPTION:
  ## Builds the proj4string from input parameters.
  ##
  ## ARGUMENTS:
  ## prj - String. Projection
  ## datum - String. Datum
  ## zone - String. If prj="utm", UTM zone
  ## zoneS - Logical. If prj="utm", if UTM zone is in Southern hemisphere
  ## aea.param - If prj="aea", parameters
  #######################################################################
  
  ## Set variable lists
  prjlst <- c(
   "adams_hemi", "adams_ws1", "adams_ws2", "aea", "aeqd", "affine",
   "airy", "aitoff", "alsk", "apian", "august", "axisswap",
   "bacon", "bertin1953", "bipc", "boggs", "bonne", "calcofi", 
   "cart", "cass", "cc", "ccon", "cea", "chamb", 
   "collg", "col_urban", "comill", "crast", "defmodel", "deformation",
   "denoy", "eck1", "eck2", "eck3", "eck4", "eck5", 
   "eck6", "eqearth", "eqc", "eqdc", "euler", "etmerc", 
   "fahey", "fouc", "fouc_s", "gall", "geoc", "geogoffset", 
   "geos", "gins8", "gn_sinu", "gnom", "goode", "gs48", 
   "gs50", "guyou", "hammer", "hatano", "healpix", "rhealpix",
   "helmert", "hgridshift", "horner", "igh", "igh_o", "imoll",
   "imoll_o", "imw_p", "isea", "kav5", "kav7", "krovak",
   "labrd", "laea", "lagrng", "larr", "lask", "lonlat",
   "latlon", "lcc", "lcca", "leac", "lee_os", "loxim",
   "lsat", "mbt_s", "mbt_fps", "mbtfpp", "mbtfpq", "mbtfps",
   "merc", "mil_os", "mill", "misrsom", "moll", "molobadekas",
   "molodensky", "murd1", "murd2", "murd3", "natearth", "natearth2",
   "nell", "nell_h", "nicol", "nsper", "nzmg", "noop",
   "ob_tran", "ocea", "oea", "omerc", "ortel", "ortho",
   "pconic", "patterson", "peirce_q", "pipeline", "poly", "pop",
   "push", "putp1", "putp2", "putp3", "putp3p", "putp4p",
   "putp5", "putp5p", "putp6", "putp6p", "qua_aut", "qsc",
   "robin", "rouss", "rpoly", "s2", "sch", "set",
   "sinu", "somerc", "stere", "sterea", "gstmerc", "tcc",
   "tcea", "times", "tinshift", "tissot", "tmerc", "tobmerc",
   "topocentric", "tpeqd", "tpers", "unitconvert", "ups", "urm5",
   "urmfps", "utm", "vandg", "vandg2", "vandg3", "vandg4",
   "vertoffset", "vitk1", "vgridshift", "wag1", "wag2", "wag3",
   "wag4", "wag5", "wag6", "wag7", "webmerc", "weren",
   "wink1", "wink2", "wintri", "xyzgridshift")
  #datumlst <- as.character(rgdal::projInfo(type="datum")$name)
  ellpslst <- c(
    "MERIT", "SGS85", "GRS80", "IAU76", "airy", "APL4.9", "NWL9D", "mod_airy",
    "andrae", "danish", "aust_SA", "GRS67", "GSK2011", "bessel", "bess_nam", "clrk66",
    "clrk80", "clrk80ign", "CPM", "delmbr", "engelis", "evrst30", "evrst48", "evrst56", 
    "evrst69", "evrstSS", "fschr60", "fschr60m", "fschr68", "helmert", "hough", "intl",
    "krass", "kaula", "lerch", "mprts", "new_intl", "plessis", "PZ90", "SEasia",
    "walbeck", "WGS60", "WGS66", "WGS72", "WGS84", "sphere"   
  )
  zonelst <- c(1:60)
  
  
  prj <- pcheck.varchar(var2check=prj, varnm="prj", checklst=prjlst,
                        caption="Projection?", gui=gui, stopifnull=TRUE)
  if (prj == "latlong") prj <- "longlat"
  
  #  datum <- pcheck.varchar(var2check=datum, varnm="datum", checklst=datumlst,
  #		caption="Datum?", gui=gui)
  ellps.gui <- ifelse(is.null(datum), TRUE, FALSE)
  ellps <- pcheck.varchar(var2check=ellps, varnm="ellps", checklst=ellpslst,
                          caption="Ellipse?", gui=ellps.gui)
  if (is.null(ellps))
    stop("both datum and ellpse are NULL.. cannot reproject")
  
  if (prj == "utm") {
    zone <- pcheck.varchar(var2check=as.character(zone), varnm="zone",
                           checklst=zonelst, caption="UTM zone?", gui=gui)
    if (is.null(zone)) stop("must include zone number")
    
    zoneS <- pcheck.logical(zoneS, varnm="zoneS", title="UTM South?",
                            first="NO", gui=gui)
  }
  
  ###########################################
  prj4str <- paste0("+proj=", prj)
  
  if (prj == "longlat") {
    if (!is.null(datum)) {
      prj4str = paste0(prj4str, " +datum=", datum, " +no_defs")
    } else {
      prj4str = paste0(prj4str, " +ellps=", ellps, " +no_defs")
    }
  } else if (prj == "utm") {
    prj4str <- paste0(prj4str, " +zone=", zone, " +datum=", datum)
    if (zoneS) prj4str <- paste(prj4str, "+south")
  } else if (prj == "aea") {
    if (aea.param == "USGS") {
      prj4str <- paste("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0",
                       "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
    } else {
      param <- " +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0"
      
      if (!is.null(datum)) {
        prj4str <- paste0(prj4str, param, " +datum=", datum, " +units=m +no_defs")
      } else {
        prj4str = paste0(prj4str, param, " +ellps=", ellps, " +no_defs")
      }
    }
  }
  
  return(prj4str)
}


#' @rdname spatial_desc
#' @export
trunc10shp <- function(x) {

  #######################################################################
  ## DESCRIPTION:
  ## Changes names of Spatial object to < 10 characters.
  ## Returns shp with new names and data table of old and new names.
  ##
  ## ARGUMENTS:
  ## x - Spatial object
  #######################################################################

  NAMES_gt10 <- names(x)[nchar(names(x)) > 10]
  othernms <- names(x)[nchar(names(x)) <= 10]

  if (length(NAMES_gt10) > 0) {
    newnms <- data.frame(NAMES_gt10, NEWNAME="0", stringsAsFactors = FALSE)
    for (nm in NAMES_gt10) {
      allnms <- c(newnms[["NAMES_gt10"]][newnms[["NAMES_gt10"]] != nm],
			othernms, newnms[newnms$NEWNAME != 0, "NEWNAME"])
      newnm <- unique(getlt10char(dbname=nm))
      cnt <- 0
      while (newnm %in% allnms) {
        cnt <- cnt + 1
        substr(newnm, 10, 10) <- as.character(cnt)
        allnms <- c(newnms[["NAMES_gt10"]][!newnms[["NAMES_gt10"]] %in% newnm],
			othernms)
      }
      newnms[newnms[["NAMES_gt10"]] == nm, "NEWNAME"] <- newnm
    }
  } else {
    newnms <- NULL
  }

  ## Change name in shp and print changes to screen (Note: NULL at end)
  if (length(newnms > 0)) {
    names(x)[names(x) %in% newnms[["NAMES_gt10"]]] <- newnms[["NEWNAME"]]
    apply(newnms, 1, function(x) message(paste("Changed name:", x[1], "to", x[2], "\n")))
  }

  return(list(shp=x, newnms=newnms))
}

#' @rdname spatial_desc
#' @export
merge_extents <- function(layer1, layer2) {
  ## DESCRIPTION: merges 2 extents
  return(append(sf::st_as_sfc(sf::st_bbox(layer1)), sf::st_as_sfc(sf::st_bbox(layer2))))
}

#' @rdname spatial_desc
#' @export
check.extents <- function(bbox1, bbox2, showext=FALSE, layer1nm=NULL,
	layer2nm=NULL, stopifnotin=FALSE, quiet=FALSE) {

  ##########################################################################
  ## DESCRIPTION
  ## Check extents of layer1 and layer2
  ## stopifnotin	- stop if bbox2 is not all in bbox1

  if (is.null(layer1nm)) layer1nm <- "layer1"
  if (is.null(layer2nm)) layer2nm <- "layer2"

  if (sum(is.na(as.vector(bbox1))) == length(bbox1)) {
    warning(layer1nm, " bbox is invalid")
    return()
  }
  if (sum(is.na(as.vector(bbox2))) == length(bbox2)) {
    warning(layer2nm, " bbox is invalid")
    return()
  }

  ## Check extents
  bbox1sfc <- sf::st_as_sfc(bbox1)
  bbox2sfc <- sf::st_as_sfc(bbox2)

  bbox1sf <- sf::st_as_sf(bbox1sfc)
  bbox2sf <- sf::st_as_sf(bbox2sfc)

  bbox1sf$fld <- 1
  bbox2sf$fld <- 1

  ## bbox2 within bbox1
  intpct1 <- suppressWarnings(tabulateIntersections(layer1=bbox2sf,
			layer1fld="fld", layer2=bbox1sf))$int.pct

  ## bbox1 within bbox2
#  intpct2 <- suppressWarnings(tabulateIntersections(layer1=bbox1sf,
#			layer1fld="fld", layer2=bbox2sf))$int.pct


  if (showext) {
    bbox12sfc <- append(bbox1sfc, bbox2sfc)
    plot(sf::st_geometry(bbox12sfc))

    plot(sf::st_geometry(bbox2sfc), add=TRUE)
    plot(sf::st_geometry(bbox1sfc), add=TRUE, border="red")
  }

  if (any(is.na(intpct1)) || any(intpct1 == 0)) {
    msg <- paste(layer1nm, "does not overlap", layer2nm)
    if (stopifnotin) {
      stop(msg)
    } else {
      return(NULL)
    }
  } else {
    if (intpct1 < 100 && !quiet) {
      message(layer2nm, " is not completely contained within ", layer1nm)
      message("...intersection of ", intpct1, "%")
    }
    return(intpct1)
  }
}

#' @rdname spatial_desc
#' @export
getprjatt <- function(prj4str, prjatt, stopifnull=FALSE) {

  ## DESCRIPTION:
  ## Gets the desired attribute from the proj4string.

  if (prjatt == "units" && sf::st_is_longlat(prj4str))
    stop("must be a projected coordinate system")
  prjatt2 <- paste0("+", prjatt, "=")

  if (grepl(prjatt2, prj4str)) {
    ## prjatt ("datum", "units", "proj")

    att.split <- strsplit(prj4str, prjatt2)[[1]][2]
    att.val <- strsplit(att.split, " ")[[1]][1]
  } else {
    if ((any(prjatt == "datum") && !grepl('ellps', prj4str)) ||
		(any(prjatt == 'ellps') && !grepl('datum', prj4str))) {
      if (stopifnull) {
        stop(prjatt, " does not exist in prj4str")
      } else {
        warning(prjatt, " does not exist in prj4str")
      }
    }
    return(NULL)
  }
  return(att.val)
}

#' @rdname spatial_desc
#' @export
areacalc.poly <- function(polyv, polyv_dsn=NULL, areaprj="aea", zone=NULL,
	unit="ACRES", areavar=NULL) {

  ## DESCRIPTION:
  ## Calculates area of polygons, appending the new variable (AREA_*),
  ## to the attribute table. If polyv is longlat projection, it is projected
  ## to Albers Equal Area projection before calculating area, then
  ## reprojected back to longlat to return.

  ## Set global variables
  acre=hectare=km <- NULL

  polyv <- pcheck.spatial(layer=polyv, dsn=polyv_dsn)

  unitlst <- c("ACRES", "HECTARES", "SQMETERS", "SQKM")
  unit <- pcheck.varchar(var2check=unit, varnm="unit",
		checklst=unitlst, caption="area units?", stopifnull=TRUE)
  if (is.null(areavar)) areavar <- paste0(unit, "_GIS")


  isll <- FALSE

  if (sf::st_is_longlat(polyv)) {
    isll <- TRUE
    message("area can only be calculated with projected coordinate system... projecting layer")

    #prj4str <- sf::st_crs(polyv)$proj4string

    ## Get longlat crs
    crs.longlat <- sf::st_crs(polyv)

    ## Check if polyv is projected
    polyv <- checksf.longlat(polyv)
  }

  ## Calculate area
  polyv[["AREA_TMP"]] <- sf::st_area(polyv)

  ## Get polygon units
#  polyv.units <- unique(units(polyv[["AREA_GIS"]])$numerator)
#  if (polyv.units %in% c("m", "meters")) {
#    cfactor.ac <- 0.00024711
#    cfactor.ha <- 0.0001
#  } else if (polyv.units %in% c("ft", "us-ft")) {
#    cfactor.ac <- 0.00002296
#    cfactor.ha <- 0.0000092903
#  } else {
#    stop("no conversion factor defined")
#  }

  ## Convert square meters to area unit
#  if (units == "ACRES") {
#    polyv[["ACRES_GIS"]] <- round(polyv[["AREA_TMP"]] * cfactor.ac, 6)
#    areavar <- "ACRES_GIS"
#  } else if (units == "HECTARES") {
#    polyv[["HECTARES_GIS"]] <- round(polyv[["AREA_TMP"]] * cfactor.ha, 6)
#    areavar <- "HECTARES_GIS"
#  } else {
#    areavar <- "SQMETERS_GIS"
#  }

  if (unit == "ACRES") {
    polyv[["ACRES_GIS"]] <- units::set_units(x=polyv[["AREA_TMP"]], value=acre)
    areanm <- "ACRES_GIS"
  } else if (unit == "HECTARES") {
    polyv[["HECTARES_GIS"]] <- units::set_units(x=polyv[["AREA_TMP"]], value=hectare)
    areanm <- "HECTARES_GIS"
  } else if (unit == "SQKM") {
    polyv[["SQKM_GIS"]] <- units::set_units(x=polyv[["AREA_TMP"]], value=km^2)
    areanm <- "SQKM_GIS"
  } else {
    polyv[["SQMETERS_GIS"]] <- polyv[["AREA_TMP"]]
    areanm <- "SQMETERS_GIS"
  }

  if (isll) {
    polyv <- sf::st_transform(polyv, crs.longlat, quiet=TRUE)
  }

  if (!is.null(areavar)) {
    names(polyv)[names(polyv) == areanm] <- areavar
  }
  polyv[["AREA_TMP"]] <- NULL

  return(units::drop_units(polyv))
}


#' @rdname spatial_desc
#' @export
checksf.longlat <- function(x, nolonglat=TRUE, crs.default=NULL) {
  ##################################################################################
  ## DESCRIPTION: Check for longlat Geodetic coordinate system.
  ##
  ## Default projection: NAD83 - Conus Albers
  ## +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96,
  ##		+x_0=0 +y_0=0", "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  ##################################################################################
  crs.albersUS <- 5070

  if (canCoerce(x, "sf")) {
    x <- sf::st_as_sf(x, stringsAsFactors=FALSE)
  }

  ## Define default coordinate System
  if (is.null(crs.default)) {
    crs.default <- crs.albersUS
  }
  crs.default <- sf::st_crs(crs.default)
  prj4str.default <- crs.default$proj4string

  ## Check if projection defined
  if (is.na(sf::st_crs(x))) {
    stop("no projection defined")
  }

  ## Reproject coordinate system
  if (sf::st_is_longlat(x) && nolonglat) {
    x <- sf::st_transform(x, crs.default, quiet=TRUE)
  }

  return(x)
}

#' @rdname spatial_desc
#' @export
crsCompare <- function(x, ycrs=NULL, x.crs=NULL, nolonglat=FALSE,
	checkonly=FALSE, crs.default=NULL){
  ##################################################################################
  ## DESCRIPTION: Compare Coordinate Reference System (CRS) of x to y CRS
  ##		string. If not the same, project x to y CRS.
  ## ARGUMENTS:
  ##   x - sf object or crs. CRS to check.
  ##   ycrs - sf object or crs. CRS to compare to x.
  ##   x.crs - CRS. If x has no defined projection info, use x.CRS to define it
  ##   nolonglat - Logical. If TRUE, stop if both layers are in Geographic
  ##			Coordinate System.
  ## VALUE:
  ## 	 xprj - x object, in original format
  ##	 crs	- crs object, in original format
  ###################################################################################
  ## Default projection: NAD83 - Conus Albers
  ## +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96,
  ##		+x_0=0 +y_0=0", "+ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  ## EPSG:5070 NAD83/Conus Albers
  crs.albersUS <- paste("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5",
			"+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs")

  if (canCoerce(x, "sf"))
    x <- sf::st_as_sf(x, stringsAsFactors=FALSE)
  if (canCoerce(ycrs, "sf"))
    ycrs <- sf::st_as_sf(ycrs, stringsAsFactors=FALSE)

  ## Define default Coordinate System as USGS albers
  if (is.null(crs.default) || any(is.na(crs.default)) || any(crs.default=="")) {
    crs.default <- crs.albersUS
  }

  ## Check x
  #############################
  crs.x <- sf::st_crs(x)

  if (is.na(crs.x)) {
    if (is.null(x.crs)) {
      stop("x has no projection defined... must include projection in x.crs")
    } else {
      crs.x <- x.crs
      if ("sf" %in% class(x)) {
        sf::st_crs(x) <- crs.x
      }
    }
  }

  ## Check ycrs
  #############################
  if (is.null(ycrs)) {
    warning("ycrs is NULL...  using default projection:")
    message("EPSG:5070 - NAD83/Conus Albers")
    ycrs <- crs.default
  }
  crs.y <- sf::st_crs(ycrs)
  if (is.na(crs.y)) {
    stop("ycrs must have defined projection")
  }
  if ("sf" %in% class(ycrs)) {
    ycrs <- sf::st_transform(ycrs, crs.y, quiet=TRUE)
  }
  ## Check if longlat
  #############################
  if (nolonglat && sf::st_is_longlat(crs.y)) {
    if (!sf::st_is_longlat(crs.x)) {
      crs.y <- crs.x
    } else {
      message("crs.default must be in a projected CRS... using US albers")
      message(crs.albersUS)
      crs.y <- crs.albersUS
    }
    if ("sf" %in% class(ycrs)) {
      ycrs <- sf::st_transform(ycrs, crs.y, quiet=TRUE)
    }
  }

  prj4str.x <- sf::st_crs(x)$input
  prj4str.y <- sf::st_crs(ycrs)$input


  ## if projection is not the same, reproject layer1 to prj4crs
  if (!checkonly) {
    if (crs.x != crs.y) {
      xprj <- sf::st_transform(x, crs=crs.y, quiet=TRUE)
      message(paste("reprojecting layer... \n", "from:", prj4str.x,
		"\n to:", prj4str.y))
      returnlst <- list(x=xprj)
    } else {
      returnlst <- list(x=x)
    }
    returnlst$ycrs <- ycrs

    return(returnlst)
  } else {
    return(NULL)
  }
}

#' @rdname spatial_desc
#' @export
sf_dissolve <- function(sflayer, col=NULL, areacalc=TRUE) {
  if (is.null(col)) {
    col <- "tmp"
    sflayer[[col]] <- 1
  }

  ## Dissolve polygons
  geocol <- attr(sflayer, "sf_column")
  sfd <- aggregate(sflayer[, geocol], by=sf::st_drop_geometry(sflayer[, col, drop=FALSE]), sum)
  names(sfd) <- c(col, "geometry")

  if (areacalc) 
    sfd <- areacalc.poly(sfd)
  return(sf::st_cast(sfd))
}

#' @rdname spatial_desc
#' @export
closest_poly <- function(x.centroid, ypoly, ypoly.att=NULL, nbr=NULL, returnsf=TRUE) {
  ## DESCRIPTION: Get polygon(s) in y closest to x (centroid or polygon)

  ## Changed because of error in sf version (sf_1.0-9)
  ## 'length(x) == length(y) is not TRUE'
  ypoly$dist <- as.vector(sf::st_distance(x.centroid, ypoly, by_element=FALSE)[1,])
  ypoly <- ypoly[order(ypoly$dist, decreasing=FALSE),]
  if (is.null(nbr)) nbr <- nrow(ypoly)
  ypoly.near <- ypoly[1:nbr,]

  if (returnsf) {
    return(ypoly.near)
  } else {
    dist <- ypoly.near$dist
    if (is.null(ypoly.att)) {
      names(dist) <- row.names(ypoly.near)
    } else {
      names(dist) <- ypoly.near[[ypoly.att]]
    }
    return(dist)
  }
}

#' @rdname spatial_desc
#' @export
getIntersect <- function(layer1, layer2, layer1.unique, layer2fld, overlapThreshold=0) {

  ## get intersection of layer1 with layer2
  layer1.int <- sf::st_join(layer1, layer2, left=FALSE)
  layer1.intlst <- unique(layer1.int[[layer1.unique]])
  layer1.int <- layer1[layer1[[layer1.unique]] %in% layer1.intlst,]
  layer1.intd <- sf_dissolve(layer1.int, layer1.unique, areacalc=FALSE)

  layer1.pct <- suppressWarnings(tabulateIntersections(layer1=layer2,
 		layer1fld=layer2fld, layer2=sf::st_make_valid(layer1.intd),
		layer2fld=layer1.unique))

  #layer1.intd <- layer1.intd[layer1.intd[[layer1.unique]] %in%
  #		layer1.pct[layer1.pct$int.pct != 0, layer1.unique],]
  layer1.int <- layer1[layer1[[layer1.unique]] %in%
		layer1.pct[layer1.pct$int.pct != 0, layer1.unique],]

  return (layer1.int)
}

#' @rdname spatial_desc
#' @export
clip.othertables <- function(inids, othertabnms, othertabs=NULL, uniqueid="PLT_CN",
	savedata=FALSE, outfn.pre=NULL, outfolder=NULL, out_fmt="csv",
	out_dsn=NULL, outfn.date=FALSE, overwrite_layer=FALSE, gui=FALSE) {

  ## Adds to file filters to Cran R Filters table.
  if (.Platform$OS.type=="windows")
    Filters=rbind(Filters,csv=c("Comma-delimited files (*.csv)", "*.csv"))


  #if (is.null(outfn.pre)) outfn.pre <- "clip"

  ## Check savedata
  #############################################################################
  savedata <- pcheck.logical(savedata, varnm="savedata",
		title="Save data?", first="NO", gui=gui)
  if (savedata && out_fmt %in% c("sqlite", "gpkg")) {
    gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)
    out_dsn <- DBtestSQLite(out_dsn)
  }

  # GET A LIST AND CHECK THE OTHER TABLES
  if (is.null(othertabnms)) {
    if (gui) {
      othertabnms.resp <- select.list(c("NO", "YES"), title = "Other tables to subset?",
		    multiple = FALSE)
      if (othertabnms.resp == "") stop("")
      while (othertabnms.resp == "YES") {
        tabresp <- select.list(c("RObject", "CSV"), title = "RObject or CSV?",
          multiple = FALSE)
        if (tabresp == "RObject") {
          otabnmlst <- c(ls(pos=1, all.names = TRUE),
			ls(envir = as.environment("package:FIESTA"), pattern = "WY"))
          otabnm <- select.list(otabnmlst, title = "Other table", multiple = TRUE)
          if (length(otabnm) == 0) stop("")
          for (tabnm in otabnm)
            otablst[[tabnm]] <- get(tabnm)
        } else if (tabresp == "CSV" && .Platform$OS.type == "windows") {
          otabnm <- choose.files(default = getwd(), caption = "Other table",
                filters = Filters["csv",], multi = TRUE)
          if (length(otabnm) == 0) stop("")
          for (tabnm in otabnm) {
            nm <- unlist(strsplit(basename(tabnm), "\\.shp"))[1]
            otablst[[nm]] <- fread(tabnm)
          }
        }
        othertabnms.resp <- select.list(c("NO", "YES"), title = "Other tables to clip?",
		      multiple = FALSE)
        if (othertabnms.resp == "") stop("")
      }
    }
  }
  if (length(othertabnms) > 0 && length(othertabs) == 0) {
    othertabs <- list()
    for (othertabnm in othertabnms) {
      if (exists(othertabnm)) {
        othertabs[[othertabnm]] <- get(othertabnm)
      } else {
        stop(othertabnm, " is invalid")
      }
    }
  }

  # Clips tables in othertabs to inids
  if (!is(othertabs, "list")) stop("othertabs must be a list")
  if (length(othertabs) > 0) {
    intablst <- list()
    namesintablst <- {}

    for (i in 1:length(othertabs)) {
      otab <- othertabs[[i]]
      otabnm <- othertabnms[i]
      message(paste("Clipping", otabnm, ".."))

      # Set new name of return table
      returnnm <- paste("clip", otabnm, sep="_")
      outnm <- otabnm
      if (!is.null(outfn.pre)) outnm <- paste(outfn.pre, otabnm, sep="_")

      if (substr(returnnm, nchar(returnnm) - 3, nchar(returnnm)) == ".csv")
        returnnm <- strsplit(returnnm, ".csv")[[1]]

      namesintablst <- c(namesintablst, returnnm)
      if (!is.null(otab)) {
        if (!"data.frame" %in% class(otab))
          stop(otabnm, " must be a data.frame")
        # Check uniqueid of other table.. change if PLT_CN/CN conflict
        otabvars <- names(otab)
        if (!uniqueid %in% otabvars) {
          if (uniqueid == "PLT_CN" && "CN" %in% otabvars) {
            otabid <- "CN"
          } else if (uniqueid == "CN" && "PLT_CN" %in% otabvars) {
            otabid <- "PLT_CN"
          } else {
            stop("uniqueid not in", otabnm)
          }
        } else {
          otabid <- uniqueid
        }
        if (otabnm == "cond" && "CONDID" %in% names(otab)) {
          idx.unique <- c(otabid, "CONDID")
        } else if (otabnm == "tree" && all(c("CONDID", "SUBP", "TREE") %in% names(otab))) {
          idx.unique <- c(otabid, "CONDID", "SUBP", "TREE")
        } else {
          idx.unique <- otabid
        }
        ## Subset data frame
        assign(returnnm, otab[otab[[otabid]] %in% inids, ])
        if (savedata) {
          datExportData(get(returnnm),
                savedata_opts=list(out_fmt=out_fmt,
                                   outfolder=outfolder,
                                   out_dsn=out_dsn,
                                   out_layer=outnm,
                                   overwrite_layer=overwrite_layer,
                                   index.unique=idx.unique))
        }
        intablst[[returnnm]] <- get(returnnm)

      } else {
          intablst[returnnm] <- otab
      }
    }
  } else {
    intablst <- NULL
  }
  return(intablst)
}


#' @rdname spatial_desc
#' @export
check.area <- function(bnd, bnd_dsn, bnd.att=NULL, 
	areaunits="acres", min.area=6000) {

  ## check areaunits
  areaunitslst <- c("acres", "hectares", "sqmeters") 
  areaunits <- pcheck.varchar(var2check=areaunits, varnm="areaunits", 
	checklst=areaunitslst, caption="Area units?", stopifnull=TRUE)
  areaunits <- toupper(areaunits)

  ## Check bnd
  bndx <- pcheck.spatial(layer=bnd, dsn=bnd_dsn, 
	caption="Area of interest?")


  ## Calculate area
  areavar <- "AREA_TEST"
  bndx <- areacalc.poly(bndx, unit=areaunits)

  if (!is.null(bnd.att)) {
    ## Aggregate area to bnd.att
    bndarea <- aggregate(bndx[[areavar]], list(bndx[[bnd.att]]), sum)
    names(bndarea) <- c(bnd.att, areavar)
  } else {
    bndarea <- data.frame(1, sum(bndx[[areavar]]))
    names(bndarea) <- c("ONEATT", areavar)
  }

  bndarea$TEST <- bndarea[[areavar]] < min.area
  pctless <- sum(bndarea$TEST) / nrow(bndarea)

  if (pctless > .50) {
    warning("more than half of domain units are less than ", 
				min.area, " ", tolower(areaunits), "\n", 
				"...consider using FIESTA Small Area module")

  }
}


