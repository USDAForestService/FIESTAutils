# functions for raster data
# Chris Toney, chris.toney at usda.gov

# depends on packages Rcpp, gdalraster, sf

# r_rasterize.cpp file required - implements RasterizePolygon()

# for stand-alone use:
# library(gdalraster)
# library(sf)
# library(parallel)
# Rcpp::sourceCpp("r_rasterize.cpp")
# source("raster_analysis.R")


#' @rdname raster_desc
#' @export
getDefaultNodata <- function(GDT_name) {
    return(gdalraster::DEFAULT_NODATA[[GDT_name]])
}

#' @rdname raster_desc
#' @export
getOffset <- function(coord, origin, gt_pixel_size) {
    return((coord-origin)/gt_pixel_size)
}

#' @rdname raster_desc
#' @export
getGDALformat <- function(file) {
    # GDAL format code from file extension for common output formats
    file <- as.character(file)
    if (endsWith(file, ".img")) {
        return("HFA")
    }
    if (endsWith(file, ".tif")) {
        return("GTiff")
    }
    if (endsWith(file, ".vrt")) {
        return("VRT")
    }

    return(NULL)
}

#' @rdname raster_desc
#' @export
basename.NoExt <- function(filepath) {
    return(tools::file_path_sans_ext(basename(filepath)))
}

#' @rdname raster_desc
#' @export
Mode <- function(x, na.rm=FALSE) {
    #Ties handled as first-appearing value
    if (na.rm) x <- na.omit(x)
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
}

#' @rdname raster_desc
#' @export
Modes <- function(x, na.rm=FALSE) {
    if (na.rm) x <- na.omit(x)
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    return(ux[tab == max(tab)])
}

#' @rdname raster_desc
#' @export
northness <- function(asp_deg) {
    ## transform aspect degrees to northness
    # set flat to east (90 degrees) for neutral value
    asp_deg[asp_deg == -1] <- 90
    return(cos(asp_deg*pi/180))
}

#' @rdname raster_desc
#' @export
eastness <- function(asp_deg) {
    ## transform aspect degrees to eastness
    # set flat to north (0 degrees) for neutral value
    asp_deg[asp_deg == -1] <- 0
    return(sin(asp_deg*pi/180))
}

#' @rdname raster_desc
#' @export
roughness <- function(x, na.rm=FALSE, asInt=TRUE) {
    ## a terrain variability parameter
    # For example, applied to a neighborhood of elevation pixel values
    # Wilson et al. 2007. Marine Geodesy, 30: 3–35.
    # Calculated as the difference between the maximum and minimum values
    r <- max(x, na.rm=na.rm) - min(x, na.rm=na.rm)
    return(ifelse(asInt, round(r), r))
}

#' @rdname raster_desc
#' @export
TRI <- function(x, na.rm=FALSE, asInt=TRUE) {
    ## terrain ruggedness index
    # Wilson et al. 2007. Marine Geodesy, 30: 3–35.
    # Calculated by comparing a central pixel with its neighbors, taking the
    # absolute values of the differences, and averaging the result.
    if (length(x) < 3) return(NA_real_)
    center <- ceiling(length(x)/2)
    i <- mean(abs(x[-center] - x[center]), na.rm=na.rm)
    return(ifelse(asInt, round(i), i))
}

#' @rdname raster_desc
#' @export
TPI <- function(x, na.rm=FALSE, asInt=TRUE) {
    ## topographic position index
    # Provides an indication of whether any	particular pixel forms part of a
    # positive (e.g., crest) or negative (e.g., trough) feature of the
    # surrounding terrain.
    # Calculated as the difference between a central pixel and the mean of its
    # surrounding cells. The surrounding neighborhood is often defined in terms
    # of a circle or annulus.
    # see Wilson et al. 2007. Marine Geodesy, 30: 3–35.
    if (length(x) < 3) return(NA_real_)
    center <- ceiling(length(x)/2)
    i <- x[center] - mean(x[-center], na.rm=na.rm)
    return(ifelse(asInt, round(i), i))
}

#' @rdname raster_desc
#' @export
getPixelValue <- function(pt, ds, ri=NULL, band=1, interpolate=FALSE,
                          windowsize=1, statistic=NULL, na.rm=TRUE) {
    # pt - vector containing a single coordinate c(x,y)
    # ds - GDALRaster object for the raster
    # ri - object returned by rasterInfo()  ***deprecated/no longer needed
    # if interpolate is TRUE, return an interpolated value for pt
    # (bilinear interpolation, windowsize and statistic args ignored)
    # windowsize - extract a square window of pixels centered on pt
    # (for example, windowsize=3 for a 3x3 window of pixels)
    # statistic - summary statistic to calculate on the window of pixels
    # statistic one of: "mean", "min", "max", "median", "sum", "var", "sd",
    #                   "rsd", "mode"
    # if windowsize > 1 and statistic is NULL, returns a vector of the raw
    # pixel values in window
    # na.rm only applies to summary statistics on windowsize > 1

    # *** getPixelValue() is not normally called directly ***
    # *** see extractPtsFromRaster() and extractPtsFromRasterList() ***

    if (interpolate) {
        if (windowsize > 2 || !is.null(statistic)) {
            warning("interpolate is TRUE, ignoring windowsize and statistic",
                    call.=FALSE)
        }
        windowsize <- 2
        statistic <- NULL
    }

    ptX <- pt[1]
    ptY <- pt[2]

    nrows <- ds$getRasterYSize()
    ncols <- ds$getRasterXSize()
    gt <- ds$getGeoTransform()
    xmin <- ds$bbox()[1]
    ymin <- ds$bbox()[2]
    xmax <- ds$bbox()[3]
    ymax <- ds$bbox()[4]
    cellsizeX <- gt[2]
    cellsizeY <- gt[6]

    #check that point is inside extent rectangle
    if (ptX > xmax || ptX < xmin) {
        warning("point X value is outside raster extent", call.=FALSE)
        return(NA)
    }
    if (ptY > ymax || ptY < ymin) {
        warning("point Y value is outside raster extent", call.=FALSE)
        return(NA)
    }

    #get pixel offsets
    if (windowsize == 1) {
        offX <- floor(getOffset(ptX, xmin, cellsizeX))
        offY <- floor(getOffset(ptY, ymax, cellsizeY))
    }
    else {
        if (interpolate) {
            # 2x2 window for bilinear interpolation
            offX <- floor(getOffset(ptX, xmin, cellsizeX) - 0.5)
            offY <- floor(getOffset(ptY, ymax, cellsizeY) - 0.5)
        }
        else {
            offX <- floor(getOffset(ptX, xmin, cellsizeX)) - trunc(windowsize/2)
            offY <- floor(getOffset(ptY, ymax, cellsizeY)) - trunc(windowsize/2)
        }

        #entire window should be inside else return NA
        if (offX < 0 || (offX+windowsize-1) > ncols ||
                offY < 0 || (offY+windowsize-1) > nrows) {
            warning("window is not completely within raster extent",
                    call.=FALSE)
            return(NA)
        }
    }

    #get pixel value(s)
    a <- ds$read(band = band,
                 xoff = offX,
                 yoff = offY,
                 xsize = windowsize,
                 ysize = windowsize,
                 out_xsize = windowsize,
                 out_ysize = windowsize)

    if (interpolate) {
        #return interpolated value at pt

        #convert to unit square coordinates for the 2x2 window
        #center of ll pixel in the window is 0,0
        x <- getOffset(ptX, xmin, cellsizeX) - (offX + 0.5)
        y <- (offY + 1.5) - getOffset(ptY, ymax, cellsizeY)

        #pixel values in the square
        #0,0: a[1,2]
        #1,0: a[2,2]
        #0,1: a[1,1]
        #1,1: a[2,1]
        pixelValue <- (a[1,2]*(1-x)*(1-y) + a[2,2]*x*(1-y) +
                           a[1,1]*(1-x)*y + a[2,1]*x*y)
    }
    else if (windowsize==1) {
        #return value of the single pixel containing pt
        pixelValue <- a[1]
    }
    else if (is.null(statistic) && windowsize > 1) {
        #return a vector of the raw pixel values in the window centered on pt
        pixelValue <- as.vector(a)
    }
    else if (!is.null(statistic) && windowsize > 1) {
        #return a summary statistic for the window
        if (statistic == "mean") {
            pixelValue <- mean(a, na.rm=na.rm)
        }
        else if (statistic == "min") {
            pixelValue <- min(a, na.rm=na.rm)
        }
        else if (statistic == "max") {
            pixelValue <- max(a, na.rm=na.rm)
        }
        else if (statistic == "median") {
            pixelValue <- median(a, na.rm=na.rm)
        }
        else if (statistic == "sum") {
            pixelValue <- sum(a, na.rm=na.rm)
        }
        else if (statistic == "range") {
            r <- range(a, na.rm=na.rm)
            pixelValue <- r[2]-r[1]
        }
        else if (statistic == "var") {
            pixelValue <- var(a, na.rm=na.rm)
        }
        else if (statistic == "sd") {
            pixelValue <- sd(a, na.rm=na.rm)
        }
        else if (statistic == "rsd") {
            pixelValue <- sd(a, na.rm=na.rm) / mean(a, na.rm=na.rm)
        }
        else if (statistic == "mode") {
            # TODO: use Modes() and add option to return ties?
            if (length(a[!is.na(a)]) > 0) {
                pixelValue <- Mode(a)
            }
            else {
                pixelValue <- NA
            }
        }
        else {
            warning("invalid summary statistic", call.=FALSE)
            pixelValue <- NA
        }
    }
    else {
        warning("invalid option", call.=FALSE)
        pixelValue <- NA
    }

    return(pixelValue)
}

#' @rdname raster_desc
#' @export
.getPixelValue <- function(pt, rasterfile, ds, ...) {
    if (!missing(rasterfile)) {
        ds <- new(GDALRaster, rasterfile, read_only=TRUE)
        return(getPixelValue(pt, ds, ...))
    }
    else if (missing(ds)) {
        if (exists(".cl_ds")) {
            # use the GDAL dataset opened on a cluster node
            return(getPixelValue(pt, ds=.cl_ds, ...))
        }
        else {
            stop("GDAL dataset object does not exist")
        }
    }
    else {
        return(getPixelValue(pt, ds, ...))
    }
}

#' @rdname raster_desc
#' @export
extractPtsFromRaster <- function(ptdata, rasterfile, band=NULL, var.name=NULL,
                                 interpolate=FALSE, windowsize=1,
                                 statistic=NULL, na.rm=TRUE, ncores=1) {
    # ptdata is a dataframe with three columns: point id, x, y

    ri <- rasterInfo(rasterfile)
    if (is.null(ri)) {
        message(rasterfile)
        stop("open raster failed")
    }

    if (ncores > 1) {
        if (!isNamespaceLoaded("parallel")) {
            stop("ncores > 1 requires 'parallel' namespace")
        }
        cl <- parallel::makeCluster(ncores)
        parallel::clusterEvalQ(cl, library(gdalraster))
        # open GDAL datasets on the cluster
        parallel::clusterExport(cl, "rasterfile", envir=environment())
        parallel::clusterEvalQ(cl, {.cl_ds <- new(GDALRaster, rasterfile,
                                                  read_only=TRUE); NULL})

        # TODO: this seems not correct / not needed?
        # export functions needed by .getPixelValue
        parallel::clusterExport(cl, c("getOffset", "getPixelValue", "Mode"))
    }
    else {
        ds <- new(GDALRaster, rasterfile, read_only=TRUE)
    }

    df.out <- data.frame(pid = ptdata[,1])

    if (is.null(var.name)) {
        var.name <- basename.NoExt(rasterfile)
    }
    if (is.null(band)) {
        nbands <- ri$nbands
        bands <- 1:nbands
    }
    else {
        bands <- band
    }

    for (b in bands) {
        this.name <- var.name
        if (length(bands) > 1) {
            this.name <- paste0(var.name,"_b",b)
        }
        if (ncores > 1) {
            values <- parallel::parApply(cl, ptdata[-1], 1, .getPixelValue,
                                         ri=ri, band=b,
                                         interpolate=interpolate,
                                         windowsize=windowsize,
                                         statistic=statistic, na.rm=na.rm)
        }
        else {
            values <- apply(ptdata[-1], 1, getPixelValue, ds=ds, ri=ri, band=b,
                            interpolate=interpolate, windowsize=windowsize,
                            statistic=statistic, na.rm=na.rm)
        }
        if (windowsize > 1 && is.null(statistic)) {
            # raw pixel values from a window, values as an array
            for (p in 1:(windowsize*windowsize)) {
                df.out[paste0(this.name,"_",p)] <- values[p,]
            }
        }
        else {
            # a vector of values
            df.out[this.name] <- values
        }
    }

    if (ncores > 1) {
        parallel::clusterEvalQ(cl, .cl_ds$close())
        parallel::stopCluster(cl)
    }
    else {
        ds$close()
    }

    return(df.out)
}

#' @rdname raster_desc
#' @export
extractPtsFromRasterList <- function(ptdata, rasterfiles, bands=NULL,
                                     var.names=NULL, interpolate=FALSE,
                                     windowsizes=NULL, statistics=NULL,
                                     na.rm=TRUE, ncores=1) {
    # call extractPtsFromRaster for a list of rasters
    # allows for specific bands, or specific var.names by band, etc.

    if (!all(file.exists(rasterfiles))) {
        message(rasterfiles[which(!file.exists(rasterfiles))])
        stop("file not found")
    }

    nrasters <- length(rasterfiles)

    # argument lengths must all be the same
    if (!is.null(bands)) {
        if (length(bands) != nrasters) {
            stop("list of band numbers must be same length as raster list")
        }
    }
    if (!is.null(var.names)) {
        if (length(var.names) != nrasters) {
            stop("list of variable names must be same length as raster list")
        }
    }
    if (!is.null(windowsizes)) {
        if (length(windowsizes) != nrasters) {
            stop("list of window sizes must be same length as raster list")
        }
    }
    if (!is.null(statistics)) {
        if (length(statistics) != nrasters) {
            stop("list of statistics must be same length as raster list")
        }
    }

    if (is.null(windowsizes)) {
        # default to windowsize = 1
        windowsizes <- rep(1,nrasters)
    }

    df.out <- data.frame(pid = ptdata[,1])

    for (n in 1:nrasters) {
        df.tmp <- extractPtsFromRaster(ptdata,
                                       rasterfiles[n],
                                       band=bands[n],
                                       var.name=var.names[n],
                                       interpolate=interpolate,
                                       windowsize=windowsizes[n],
                                       statistic=statistics[n],
                                       na.rm=na.rm,
                                       ncores=ncores)
        df.out <- merge(df.out, df.tmp)
    }

    return(df.out)
}

#' @rdname raster_desc
#' @export
rasterInfo <- function(srcfile) {
    ds <- new(GDALRaster, srcfile, read_only = TRUE)
    ri <- list()
    ri$format <- ds$getDriverShortName()
    ri$xsize <- ds$getRasterXSize()
    ri$ysize <- ds$getRasterYSize()
    ri$geotransform <- ds$getGeoTransform()
    ri$bbox <- ds$bbox()
    ri$cellsize <- ds$res()
    ri$crs <- ds$getProjectionRef()
    ri$nbands <- ds$getRasterCount()
    ri$datatype <- c()
    ri$has_nodata_value <- c()
    ri$nodata_value <- c()
    for (b in 1:ri$nbands) {
        ri$datatype <- c(ri$datatype, ds$getDataTypeName(b))
        ri$nodata_value <- c(ri$nodata_value, ds$getNoDataValue(b))
        ri$has_nodata_value <- c(ri$has_nodata_value,
                                 !is.na(ds$getNoDataValue(b)))
    }
    ds$close()
    return(ri)
}

#' @rdname raster_desc
#' @export
reprojectRaster <- function(srcfile, dstfile, t_srs, overwrite=TRUE,
                            s_srs=NULL, of=NULL, ot=NULL,
                            te=NULL, tr=NULL, r=NULL,
                            dstnodata=NULL, co=NULL, addOptions=NULL) {

# Wrapper of gdalraster::warp() with arguments for common options.
# See full documentation for gdalwarp at gdal.org/programs/gdalwarp.html.

# Arguments that require multiple values should be given as a vector of values.
# For example, target resolution: tr=c(30,30)
# Arguments will be coerced to character.

#t_srs: <srs def> target spatial reference. EPSG:code, WKT, etc.
#s_srs: <srs def> source spatial reference. Normally read from the source file.
#overwrite: If TRUE, overwrite the target dataset if it already exists.
#of: <format> output raster format, e.g., HFA, GTiff, ...
#ot: <type> Force an output data type (i.e., Byte, Int16, ...)
#te: <xmin ymin xmax ymax> georeferenced extents of output file to be created.
#tr: <xres> <yres> output file resolution (in target georeferenced units)
#r: <resampling_method> Available methods are: near (the default), bilinear,
#	cubic, cubicspline, lanczos, average, mode, min, max, med, q1, q3
#dstnodata: <value> Set nodata values for output raster. New files will be
#	initialized to this value and if possible the nodata value will be recorded
#	in the output file. Use a value of None to ensure that nodata is not
#	defined. If this argument is not used then nodata values will be copied
#	from the source.
#co: <NAME=VALUE> Format-specific creation options
#	(e.g., enable compression, see format driver documentation at www.gdal.org)
#addOptions: additional options. See gdalwarp documentation. Pass additional
#	options as a character vector of command-line switches and their values,
#	for example, addOptions=c("-multi","-wo","NUM_THREADS=ALL_CPUS")


    opt <- vector("character")
    if (!is.null(s_srs)) {
        opt <- c(opt, "-s_srs", as.character(s_srs))
    }
    if (!is.null(of)) {
        opt <- c(opt, "-of", as.character(of))
    }
    if (!is.null(ot)) {
        opt <- c(opt, "-ot", as.character(ot))
    }
    if (!is.null(te)) {
        opt <- c(opt, "-te", as.character(te))
    }
    if (!is.null(tr)) {
        opt <- c(opt, "-tr", as.character(tr))
    }
    if (!is.null(r)) {
        opt <- c(opt, "-r", as.character(r))
    }
    if (!is.null(dstnodata)) {
        opt <- c(opt, "-dstnodata", as.character(dstnodata))
    }
    if (!is.null(co)) {
        for (this_co in co)
            opt <- c(opt, "-co", as.character(this_co))
    }
    if (overwrite) {
        opt <- c(opt, "-overwrite")
    }
    opt <- c(opt, "-ovr", "NONE")
    opt <- c(opt, addOptions)
    if (length(opt) == 0)
        opt <- NULL

    return(gdalraster::warp(srcfile, dstfile, t_srs, cl_arg=opt))
}

#' @rdname raster_desc
#' @export
rasterFromVectorExtent <- function(src, dstfile, res, fmt=NULL, nbands=1,
                                   dtName="Int16", options=NULL, init=NULL,
                                   dstnodata=init) {
# create a new blank raster (dst) using existing vector layer as template
# src layer can be either an sf or Spatial object
# coordinate system and extent are taken from the src layer
# must specify the pixel resolution in layer CRS units
# dtName is often one of 'Byte','UInt16','Int16','UInt32','Int32','Float32'
# optinally pass driver-specific dataset creation options to GDAL
# optinally initialize to a value

    if (canCoerce(src, "sf")) {
        src <- sf::st_as_sf(src)
    }
    else {
        stop("Cannot coerce src object to sf.")
    }

    if (is.null(fmt)) {
        fmt <- getGDALformat(dstfile)
        if (is.null(fmt)) {
            stop("Use fmt argument to specify a GDAL raster format code.")
        }
    }

    ext <- as.numeric(sf::st_bbox(src))
    srs <- sf::st_crs(src)$wkt

    xmin <- ext[1] - res
    ncols <- ceiling((ext[3] - xmin) / res)
    xmax <- xmin + res * ncols
    ymax <- ext[4] + res
    nrows <- ceiling((ymax - ext[2]) / res)
    ymin <- ymax - res * nrows
    gt <- c(xmin, res, 0, ymax, 0, -res)

    gdalraster::create(fmt, dstfile, ncols, nrows, nbands, dtName, options)
    ds <- new(GDALRaster, dstfile, read_only=FALSE)
    ds$setProjection(srs)
    ds$setGeoTransform(gt)
    if (!is.null(dstnodata)) {
        for (b in 1:nbands) {
            if (!ds$setNoDataValue(b, dstnodata))
                warning("Failed to set nodata value.", call.=FALSE)
        }
    }
    if (!is.null(init)) {
        message("Initializing destination raster...")
        for (b in 1:nbands) {
            ds$fillRaster(b, init, 0)
        }
    }

    ds$close()
    return(dstfile)
}

#' @rdname raster_desc
#' @export
rasterizePolygons <- function(dsn, layer, burn_value, rasterfile, src=NULL) {

    ## Deprecated, use gdalraster::rasterize()

    # dsn, layer - a polygon layer (non-overlapping polygons assumed)
    # burn_value - an integer, or field name from layer (integer attribute)
    # rasterfile - existing raster for output, make with rasterFromRaster()

    ds <- new(GDALRaster, rasterfile, read_only=FALSE)
    nrows <- ds$getRasterYSize()
    ncols <- ds$getRasterXSize()
    gt <- ds$getGeoTransform()
    xmin <- ds$bbox()[1]
    ymax <- ds$bbox()[4]

    if (is.null(src))
        src <- sf::st_read(dsn, layer, stringsAsFactors=FALSE, quiet=TRUE)
    else if (!is(src, "sf"))
        stop("'src' must be a polygon layer as 'sf' object")

    # a write function for the C++ rasterizer
    writeRaster <- function(yoff, xoff1, xoff2, burn_value, attrib_value) {
        a <- rep(burn_value, (xoff2-xoff1+1))
        ds$write(band=1, xoff1, yoff, (xoff2-xoff1+1), 1, a)
        return()
    }

    geom_col <- attr(src, "sf_column")
    geom_type <- sf::st_geometry_type(src, by_geometry = FALSE)
    if (!geom_type %in% c("POLYGON", "MULTIPOLYGON"))
        stop("geometry type must be POLYGON or MULTIPOLYGON", call. = FALSE)

    pb <- utils::txtProgressBar(min = 0, max = nrow(src))
    burn_this <- burn_value
    for (i in seq_len(nrow(src))) {
        if (!is.numeric(burn_value)) {
            # assume burn_value is a field name
            burn_this <- as.numeric(src[[burn_value]][i])
        }
        coords <- src[i, geom_col] |> sf::st_coordinates()
        parts <- integer(0)
        part_sizes <- integer(0)
        if (geom_type == "POLYGON") {
            parts <- unique(coords[, "L1"])
            for (j in seq_len(NROW(parts))) {
                part_sizes[j] <- nrow(coords[coords[, "L1"] == parts[j], ])
            }
        } else if (geom_type == "MULTIPOLYGON") {
            parts <- unique(coords[, c("L1", "L2")])
            for (j in seq_len(NROW(parts))) {
                part_sizes[j] <- nrow(coords[coords[, "L1"] == parts[j, 1] &
                                             coords[, "L2"] == parts[j, 2], ])
            }
        }

        grid_xs <- vapply(coords[, "X"],
                          getOffset,
                          0.0,
                          origin = xmin,
                          gt_pixel_size = gt[2])
        grid_ys <- vapply(coords[, "Y"],
                          getOffset,
                          0.0,
                          origin = ymax,
                          gt_pixel_size = gt[6])

        RasterizePolygon(ncols, nrows, part_sizes, grid_xs, grid_ys,
                         writeRaster, burn_this)

        utils::setTxtProgressBar(pb, i)
    }

    close(pb)
    ds$close()

    invisible()
}

#' @rdname raster_desc
#' @export
clipRaster <- function(dsn=NULL, layer=NULL, src=NULL,
                       srcfile, src_band=NULL,
                       dstfile, fmt=NULL, options=NULL, init=NULL,
                       dstnodata=NULL, maskByPolygons=TRUE) {
# Clip a larger raster to the extent of polygon layer.
# Polygon layer from dsn/layer, or src if already open as Spatial object.
# srcfile - source raster (big raster)
# src_band - source band(s) to clip, defaults to all bands in srcfile
# dstfile - destination raster (will be created)
# fmt - GDAL format string, if NULL will use format of srcfile if possible
# options - GDAL dataset creation options (driver-specific)
# init - initialize pixels to a background/nodata value
# dstnodata - value to set as designated nodata value on the destination raster
# Output extent will be set to maintain pixel alignment with src raster.
# Pixels in dst raster will be masked by polygons in the layer.

    if (is.null(fmt)) {
        fmt <- getGDALformat(dstfile)
        if (is.null(fmt)) {
            stop("use 'fmt' to specify a GDAL raster format code")
        }
    }

    if (is.null(src))
        src <- sf::st_read(dsn, layer, stringsAsFactors=FALSE, quiet=TRUE)
    else if (!is(src, "sf"))
        stop("'src' must be a polygon layer as 'sf' object")

    if (!is.null(init) && !maskByPolygons) {
        message("'init' value ignored when 'maskByPolygons' is FALSE")
    }

    if (fmt == "VRT") {
        ## handle VRT as a special case
        if (!is.null(src_band)) {
            message("individual band selection not supported for clip to VRT")
        }
        if (maskByPolygons) {
            message("'maskByPolygons' not available for clipping to VRT")
        }

        return(invisible(gdalraster::rasterToVRT(srcfile = srcfile,
                                     vrtfile = dstfile,
                                     subwindow = sf::st_bbox(src))))
    }

    # open the source raster
    ri <- rasterInfo(srcfile)
    src_ds <- new(GDALRaster, srcfile, read_only=TRUE)
    gt <- src_ds$getGeoTransform()
    xmin <- src_ds$bbox()[1]
    ymin <- src_ds$bbox()[2]
    xmax <- src_ds$bbox()[3]
    ymax <- src_ds$bbox()[4]
    cellsizeX <- gt[2]
    cellsizeY <- gt[6]

    bb <- sf::st_bbox(src)
    # bounding box should be inside the raster extent
    if (bb[1] < xmin || bb[3] > xmax ||
            bb[2] < ymin || bb[4] > ymax) {
        stop("polygon bounding box not completely within source raster extent")
    }

    # srcwin offsets
    xminOff <- floor(getOffset(bb[1], xmin, cellsizeX))
    ymaxOff <- floor(getOffset(bb[4], ymax, cellsizeY))
    xmaxOff <- ceiling(getOffset(bb[3], xmin, cellsizeX))
    yminOff <- ceiling(getOffset(bb[2], ymax, cellsizeY))

    # lay out the clip raster so it is pixel-aligned with src raster
    clip_ncols <- xmaxOff - xminOff
    clip_nrows <- yminOff - ymaxOff
    clip_xmin <- xmin + xminOff * cellsizeX
    clip_ymax <- ymax + ymaxOff * cellsizeY
    clip_gt <- c(clip_xmin, cellsizeX, 0, clip_ymax, 0, cellsizeY)

    if (is.null(src_band)) {
        nbands <- src_ds$getRasterCount()
        src_band <- 1:nbands
    } else {
        nbands <- length(src_band)
    }

    if (length(unique(ri$datatype)) > 1) {
        warning("multiple data types not supported", call.=FALSE)
    }
    dtName <- ri$datatype[src_band[1]]

    setRasterNodataValue <- TRUE
    if (is.null(dstnodata)) {
        if (any(ri$has_nodata_value[src_band])) {
            dstnodata <- ri$nodata_value[src_band]
        } else {
            dstnodata <- getDefaultNodata(dtName)
            setRasterNodataValue <- FALSE
        }
    }
    if (is.null(init)) init <- dstnodata

    srs <- src_ds$getProjectionRef()

    # create dst raster file
    gdalraster::create(fmt, dstfile, clip_ncols, clip_nrows, nbands,
                       dtName, options)
    dst_ds <- new(GDALRaster, dstfile, read_only=FALSE)
    dst_ds$setProjection(srs)
    dst_ds$setGeoTransform(clip_gt)
    if (setRasterNodataValue) {
        for (b in 1:nbands) {
            if (!dst_ds$setNoDataValue(b, dstnodata))
                warning("failed to set nodata value", call.=FALSE)
        }
    }

    # raster I/O function for the C++ rasterizer
    writeRaster <- function(yoff, xoff1, xoff2, burn_value, attrib_value) {
        for (b in 1:nbands) {
            a <- src_ds$read(band=src_band[b],
                             xoff=(xoff1+xminOff),
                             yoff=(yoff+ymaxOff),
                             xsize=(xoff2-xoff1+1),
                             ysize=1,
                             out_xsize=(xoff2-xoff1+1),
                             out_ysize=1)

            dst_ds$write(band=b,
                         xoff=xoff1,
                         yoff=yoff,
                         xsize=(xoff2-xoff1+1),
                         ysize=1,
                         rasterData=a)
        }
        return()
    }

    if (maskByPolygons) {
        message("initializing destination raster...")

        if (length(init) < nbands)
            init <- rep(init, nbands)
        for (b in 1:nbands)
            dst_ds$fillRaster(band=b, init[b], 0)

        # geom_col <- attr(src, "sf_column")
        geom_type <- sf::st_geometry_type(src, by_geometry = FALSE)
        if (!geom_type %in% c("POLYGON", "MULTIPOLYGON"))
            stop("geometry type must be POLYGON or MULTIPOLYGON", call. = FALSE)

        message("clipping to polygon layer...")
        coords <- sf::st_union(src) |> sf::st_coordinates()
        parts <- integer(0)
        part_sizes <- integer(0)
        if (geom_type == "POLYGON") {
            parts <- unique(coords[, "L1"])
            for (i in seq_len(NROW(parts))) {
                part_sizes[i] <- NROW(coords[coords[, "L1"] == parts[i], ])
            }
        } else if (geom_type == "MULTIPOLYGON") {
            parts <- unique(coords[, c("L1", "L2")])
            for (i in seq_len(NROW(parts))) {
                part_sizes[i] <- NROW(coords[coords[, "L1"] == parts[i, 1] &
                                             coords[, "L2"] == parts[i, 2], ])
            }
        }

        grid_xs <- vapply(coords[, "X"],
                          getOffset,
                          0.0,
                          origin = clip_xmin,
                          gt_pixel_size = clip_gt[2])
        grid_ys <- vapply(coords[, "Y"],
                          getOffset,
                          0.0,
                          origin = clip_ymax,
                          gt_pixel_size = clip_gt[6])

        RasterizePolygon(clip_ncols, clip_nrows, part_sizes, grid_xs, grid_ys,
                         writeRaster, 0)

    } else {
        message("clipping to polygon layer extent...")
        lapply(c(0:(clip_nrows-1)), writeRaster, xoff1=0, xoff2=clip_ncols-1, 0)
    }

    # copy band properties to the destination raster
    for (b in 1:nbands) {
        # description
        desc <- src_ds$getDescription(band = src_band[b])
        dst_ds$setDescription(band = b, desc)
        # unit type
        unit_type <- src_ds$getUnitType(band = src_band[b])
        dst_ds$setUnitType(band = b, unit_type)
        # scale
        scale_value <- src_ds$getScale(band = src_band[b])
        if (!is.na(scale_value))
            dst_ds$setScale(band = b, scale_value)
        # offset
        offset_value <- src_ds$getOffset(band = src_band[b])
        if (!is.na(offset_value))
            dst_ds$setOffset(band = b, offset_value)
        # color interp
        col_interp <- src_ds$getRasterColorInterp(band = src_band[b])
        dst_ds$setRasterColorInterp(band = b, col_interp)
        # color table
        col_tbl <- src_ds$getColorTable(band = src_band[b])
        pal_interp <- src_ds$getPaletteInterp(band = src_band[b])
        if (!is.null(col_tbl))
            dst_ds$setColorTable(band = b, col_tbl, pal_interp)
    }

    src_ds$close()
    dst_ds$close()
    
    message(paste("output written to:", dstfile))

    invisible(dstfile)
}

#' @rdname raster_desc
#' @export
recodeRaster <- function(srcfile, dstfile, lut, srcband=1, ...) {
#Recode a raster by values in a lookup table
#srcfile - filename of source raster
#lut - dataframe with two columns containing original value, new value
#dstfile - output raster file, will be created
#... additional arguments passed to rasterFromRaster (e.g., change data type)
# rasterFromRaster <- function(srcfile, dstfile, fmt=NULL, nbands=NULL,
#                              dtName=NULL, options=NULL, init=NULL,
#                              dstnodata=init) {
#Raster values not in the lookup table will be brought through unchanged.
#Raster data assumed to be integer.

    ri <- rasterInfo(srcfile)
    src_ds <- new(GDALRaster, srcfile, read_only=TRUE)
    nrows <- src_ds$getRasterYSize()
    ncols <- src_ds$getRasterXSize()

    #create the destination raster
    gdalraster::rasterFromRaster(srcfile, dstfile, nbands=1, ...)
    dst_ds <- new(GDALRaster, dstfile, read_only=FALSE)

    # row function
    process_row <- function(row) {
        a <- as.integer(src_ds$read(band=srcband,
                                    xoff = 0,
                                    yoff = row,
                                    xsize = ncols,
                                    ysize = 1,
                                    out_xsize = ncols,
                                    out_ysize = 1))
        a2 <- lut[,2][match(a, lut[,1])]
        a2 <- ifelse(is.na(a2), a, a2)
        dst_ds$write(band=1,
                     xoff = 0,
                     yoff = row,
                     xsize = ncols,
                     ysize = 1,
                     a2)
        utils::setTxtProgressBar(pb, row+1)
        return()
    }

    message("Recoding...")
    pb <- utils::txtProgressBar(min=0, max=nrows)
    lapply(0:(nrows-1), process_row)
    close(pb)

    message(paste("Output written to:", dstfile))
    dst_ds$close()
    src_ds$close()

    invisible(dstfile)
}

#' @rdname raster_desc
#' @export
pixelCount <- function(rasterfile, band = 1) {
    # Convenience function to get pixel counts from one raster.
    # Scans the whole raster.

    return(gdalraster::buildRAT(rasterfile,
                                band = band,
                                col_names = c("value", "count")))
}

#' @rdname raster_desc
#' @export
focalRaster <- function(srcfile, dstfile, w, fun=sum, na.rm=FALSE, ...,
                        fmt=NULL, dtName=NULL, options=NULL,
                        nodata_value=NULL, setRasterNodataValue=FALSE,
                        srcband=NULL) {

    # experimental

    ref <- rasterInfo(srcfile)
    if (is.null(ref)) stop(paste("Could not read raster info:", srcfile))
    nrows <- ref$ysize
    ncols <- ref$xsize
    if (is.null(dtName)) dtName=ref$datatype[1]
    if (is.null(nodata_value)) {
        nodata_value <- getDefaultNodata(dtName)
        if (is.null(nodata_value)) {
            stop("Invalid output data type (dtName).")
        }
    }
    nbands <- ifelse(is.null(srcband), ref$nbands, 1)

    # kernel info
    if (!is.matrix(w)) {
        stop("kernel must be a square matrix with odd-number dimensions.")
    }
    if (dim(w)[1] != dim(w)[2]) {
        stop("kernel must be a square matrix with odd-number dimensions.")
    }
    kernelsize <- dim(w)[1]
    if ((kernelsize %% 2) == 0) {
        stop("kernel must be a square matrix with odd-number dimensions.")
    }

    N <- as.integer( (kernelsize - 1) / 2 )

    if (kernelsize > nrows || kernelsize > ncols) {
        stop("kernel must be smaller than raster size")
    }

    # create the output raster
    dstnodata <- NULL
    if (setRasterNodataValue)
        dstnodata <- nodata_value
    gdalraster::rasterFromRaster(srcfile, dstfile, fmt=fmt, nbands=nbands,
                                 dtName=dtName, options=options,
                                 dstnodata=dstnodata)
    dst_ds <- new(GDALRaster, dstfile, read_only=FALSE)

    # source dataset
    src_ds <- new(GDALRaster, srcfile, read_only=TRUE)

    # raster input buffer for nrows = kernelsize
    # N marginal columns contain NA for when kernel is outside the raster
    rowdata <- matrix(NA_real_, nrow = kernelsize, ncol = ncols+2*N)
    # define columns of the data region in rowdata (columns inside the raster)
    rowdata.cols <- (1+N):(ncols+N)

    process_row <- function(row) {
        for (b in 1:nbands) {
            # start/end row numbers for raster input
            inrow.start <- row-N
            inrow.end <- row+N

            if (row == 0) {
                # fully populate the input buffer
                i <- 1
                for (this.row in inrow.start:inrow.end) {
                    if (this.row >= 0) {
                        rowdata[i,rowdata.cols] <<- src_ds$read(band = b,
                                                    xoff = 0,
                                                    yoff = this.row,
                                                    xsize = ncols,
                                                    ysize = 1,
                                                    out_xsize = ncols,
                                                    out_ysize = 1)
                    }
                    i <- i + 1
                }
            }
            else {
                # update the input buffer adding one new row
                for (i in 1:(kernelsize-1)) {
                    rowdata[i,] <<- rowdata[(i+1),]
                }
                if (inrow.end > (nrows-1)) {
                    # outside the raster
                    rowdata[kernelsize,] <<- NA_real_
                }
                else {
                    rowdata[kernelsize,rowdata.cols] <<- src_ds$read(band = b,
                                                            xoff = 0,
                                                            yoff = inrow.end,
                                                            xsize = ncols,
                                                            ysize = 1,
                                                            out_xsize = ncols,
                                                            out_ysize = 1)
                }
            }

            # move the kernel across rowdata and apply fun
            outrow <- vapply(1:ncols,
                             function(p, ...) {
                                 fun((rowdata[,p:(p+kernelsize-1)] * w), ...)
                             },
                             0, na.rm = na.rm, ...)
            outrow <- ifelse(is.na(outrow), nodata_value, outrow)

            # write a row of output
            dst_ds$write(band = b,
                         offx = 0,
                         offy = row,
                         xsize = ncols,
                         ysize = 1,
                         outrow)

            utils::setTxtProgressBar(pb, row+1)
            return()
        }
    }

    message("Calculating focal raster...")
    pb <- utils::txtProgressBar(min=0, max=nrows)
    lapply(0:(nrows-1), process_row)
    close(pb)

    message(paste("Output written to:", dstfile))
    dst_ds$close()
    src_ds$close()

    invisible(dstfile)
}

#' @rdname raster_desc
#' @export
zonalStats <- function(dsn=NULL, layer=NULL, src=NULL, attribute,
                       rasterfile, band = 1, lut=NULL, pixelfun=NULL,
                       na.rm=TRUE, ignoreValue=NULL, show_progress = FALSE) {
    ## zoneid, npixels, mean, min, max, sum, var, sd

    ds <- new(GDALRaster, rasterfile, read_only = TRUE)
    nrows <- ds$getRasterYSize()
    ncols <- ds$getRasterXSize()
    gt <- ds$getGeoTransform()
    xmin <- ds$bbox()[1]
    ymax <- ds$bbox()[4]

    if (is.null(src))
        src <- sf::st_read(dsn, layer, stringsAsFactors=FALSE, quiet=TRUE)
    else if (!is(src, "sf"))
        stop("'src' must be a polygon layer as 'sf' object")

    zoneid <- unique(as.character(src[[attribute]]))

    # list of RunningStats objects for the zones
    rs_list <- list()
    
    for (z in zoneid) {
        rs_list[[z]] <- new(RunningStats, na_rm_in=na.rm)
    }

    # raster I/O function for RasterizePolygon()
    readRaster <- function(yoff, xoff1, xoff2, burn_value, attrib_value) {
        a <- ds$read(band = band,
                     xoff = xoff1,
                     yoff = yoff,
                     xsize = ((xoff2-xoff1)+1),
                     ysize = 1,
                     out_xsize = ((xoff2-xoff1)+1),
                     out_ysize = 1)
        if (!is.null(ignoreValue))
            a <- a[!(a %in% ignoreValue)]
        if (!is.null(lut)) {
            a2 <- lut[,2][match(a, lut[,1])]
            a <- ifelse(is.na(a2), a, a2)
        }
        if (!is.null(pixelfun))
            a <- pixelfun(a)
        rs_list[[attrib_value]]$update(a)
        return()
    }

    geom_col <- attr(src, "sf_column")
    geom_type <- sf::st_geometry_type(src, by_geometry = FALSE)
    if (!geom_type %in% c("POLYGON", "MULTIPOLYGON"))
        stop("geometry type must be POLYGON or MULTIPOLYGON", call. = FALSE)

    if (show_progress)
        pb <- utils::txtProgressBar(min = 0, max = nrow(src))

    for (i in seq_len(nrow(src))) {
        this_attr <- as.character(src[[attribute]][i])
        coords <- src[i, geom_col] |> sf::st_coordinates()
        parts <- integer(0)
        part_sizes <- integer(0)
        if (geom_type == "POLYGON") {
            parts <- unique(coords[, "L1"])
            for (j in seq_len(NROW(parts))) {
                part_sizes[j] <- nrow(coords[coords[, "L1"] == parts[j], ])
            }
        } else if (geom_type == "MULTIPOLYGON") {
            parts <- unique(coords[, c("L1", "L2")])
            for (j in seq_len(NROW(parts))) {
                part_sizes[j] <- nrow(coords[coords[, "L1"] == parts[j, 1] &
                                             coords[, "L2"] == parts[j, 2], ])
            }
        }

        grid_xs <- vapply(coords[, "X"],
                          getOffset,
                          0.0,
                          origin = xmin,
                          gt_pixel_size = gt[2])
        grid_ys <- vapply(coords[, "Y"],
                          getOffset,
                          0.0,
                          origin = ymax,
                          gt_pixel_size = gt[6])

        RasterizePolygon(ncols, nrows, part_sizes, grid_xs, grid_ys,
                         readRaster, NA, this_attr)
        if (show_progress)
            utils::setTxtProgressBar(pb, i)
    }

    if (show_progress)
        close(pb)

    npixels <- rep(0, length(zoneid))
    zone.stats <- data.frame(zoneid, npixels, stringsAsFactors=FALSE)
    zone.stats$mean <- rep(NA_real_, length(zoneid))
    zone.stats$min <- rep(NA_real_, length(zoneid))
    zone.stats$max <- rep(NA_real_, length(zoneid))
    zone.stats$sum <- rep(NA_real_, length(zoneid))
    zone.stats$var <- rep(NA_real_, length(zoneid))
    zone.stats$sd <- rep(NA_real_, length(zoneid))
    for (z in zoneid) {
        zone.stats$npixels[zone.stats$zoneid==z] <- rs_list[[z]]$get_count()
        zone.stats$mean[zone.stats$zoneid==z] <- rs_list[[z]]$get_mean()
        zone.stats$min[zone.stats$zoneid==z] <- rs_list[[z]]$get_min()
        zone.stats$max[zone.stats$zoneid==z] <- rs_list[[z]]$get_max()
        zone.stats$sum[zone.stats$zoneid==z] <- rs_list[[z]]$get_sum()
        zone.stats$var[zone.stats$zoneid==z] <- rs_list[[z]]$get_var()
        zone.stats$sd[zone.stats$zoneid==z] <- rs_list[[z]]$get_sd()
    }

    ds$close()
    return(zone.stats)
}

#' @rdname raster_desc
#' @export
zonalMean <- function(dsn=NULL, layer=NULL, src=NULL, attribute,
                      rasterfile, band = 1,
                      lut=NULL, pixelfun=NULL, na.rm=TRUE, ...) {

    zone.stats <- zonalStats(dsn=dsn, layer=layer, src=src, attribute=attribute,
                             rasterfile=rasterfile, band=band,
                             lut=lut, pixelfun=pixelfun, na.rm=na.rm, ...)
    return(zone.stats[,1:3])
}

#' @rdname raster_desc
#' @export
zonalFreq <- function(dsn=NULL, layer=NULL, src=NULL, attribute,
                      rasterfile, band=1, aggfun=NULL, lut=NULL,
                      na.rm=FALSE, ignoreValue=NULL, show_progress = FALSE) {
    # aggfun is an aggregate function applied to the counts by zoneid,
    # like max to get the zonal most frequent value

    ds <- new(GDALRaster, rasterfile, read_only=TRUE)
    nrows <- ds$getRasterYSize()
    ncols <- ds$getRasterXSize()
    gt <- ds$getGeoTransform()
    xmin <- ds$bbox()[1]
    ymax <- ds$bbox()[4]

    if (is.null(src))
        src <- sf::st_read(dsn, layer, stringsAsFactors=FALSE, quiet=TRUE)
    else if (!is(src, "sf"))
        stop("'src' must be a polygon layer as 'sf' object")

    zoneid <- unique(as.character(src[[attribute]]))

    # CmbTable to count the unique combinations
    tbl <- new(CmbTable, keyLen = 2, varNames = c("idx", "value"))

    # raster I/O function for RasterizePolygon
    readRaster <- function(yoff, xoff1, xoff2, burn_value, attrib_value) {
        rowlength <- (xoff2-xoff1) + 1
        rowdata <- matrix(NA_integer_, nrow = 2, ncol = rowlength)
        rowdata[1,] <- rep(burn_value, rowlength)
        rowdata[2,] <- as.integer(ds$read(band=band,
                                          xoff = xoff1,
                                          yoff = yoff,
                                          xsize = rowlength,
                                          ysize = 1,
                                          out_xsize = rowlength,
                                          out_ysize = 1))
        if (!is.null(lut)) {
            a2 <- lut[,2][match(rowdata[2,], lut[,1])]
            rowdata[2,] <- ifelse(is.na(a2), rowdata[2,], a2)
        }
        tbl$updateFromMatrix(rowdata, 1)
        return()
    }

    geom_col <- attr(src, "sf_column")
    geom_type <- sf::st_geometry_type(src, by_geometry = FALSE)
    if (!geom_type %in% c("POLYGON", "MULTIPOLYGON"))
        stop("geometry type must be POLYGON or MULTIPOLYGON", call. = FALSE)

    if (show_progress)
        pb <- utils::txtProgressBar(min = 0, max = nrow(src))
    for (i in seq_len(nrow(src))) {
        this_attr <- as.character(src[[attribute]][i])
        this_attr_idx <- match(this_attr, zoneid)
        coords <- src[i, geom_col] |> sf::st_coordinates()
        parts <- integer(0)
        part_sizes <- integer(0)
        if (geom_type == "POLYGON") {
            parts <- unique(coords[, "L1"])
            for (j in seq_len(NROW(parts))) {
                part_sizes[j] <- nrow(coords[coords[, "L1"] == parts[j], ])
            }
        } else if (geom_type == "MULTIPOLYGON") {
            parts <- unique(coords[, c("L1", "L2")])
            for (j in seq_len(NROW(parts))) {
                part_sizes[j] <- nrow(coords[coords[, "L1"] == parts[j, 1] &
                                             coords[, "L2"] == parts[j, 2], ])
            }
        }

        grid_xs <- vapply(coords[, "X"],
                          getOffset,
                          0.0,
                          origin = xmin,
                          gt_pixel_size = gt[2])
        grid_ys <- vapply(coords[, "Y"],
                          getOffset,
                          0.0,
                          origin = ymax,
                          gt_pixel_size = gt[6])

        RasterizePolygon(ncols, nrows, part_sizes, grid_xs, grid_ys,
                         readRaster, this_attr_idx)

        if (show_progress)
            utils::setTxtProgressBar(pb, i)
    }

    if (show_progress)
        close(pb)

    # for CRAN check only:
    count <- NULL

    df_out <- tbl$asDataFrame()
    df_out$cmbid <- NULL
    df_out$zoneid <- zoneid[df_out$idx]
    df_out$idx <- NULL
    firstcols <- c("zoneid","value")
    df_out <- df_out[, c(firstcols, setdiff(names(df_out), firstcols))]
    if (na.rm) {
        df_out <- df_out[!is.na(df_out$value),]
    }
    if (!is.null(ignoreValue)) {
        df_out <- df_out[!(df_out$value %in% ignoreValue), ]
        #df_out <- subset(df_out, !(value %in% ignoreValue))
        #df_out <- df_out[(df_out$value != ignoreValue),]
    }
    if (!is.null(aggfun)) {
        df_agg <- aggregate(count ~ zoneid, df_out, aggfun)
        df_out <- merge(df_agg, df_out)
    } else {
        df_out <- transform(df_out,
                            zoneprop = ave(count,
                                           zoneid,
                                           FUN=function(x) round(x/sum(x), 9)))
    }

    ds$close()

    df_out <- df_out[with(df_out, order(zoneid, -count, value)), ]
    row.names(df_out) <- NULL
    return(df_out)
}

#' @rdname raster_desc
#' @export
zonalMajority <- function(dsn=NULL, layer=NULL, src = NULL, attribute,
                          rasterfile, band = 1, lut=NULL, ...) {

    return(zonalFreq(dsn=dsn, layer=layer, src=src, attribute=attribute,
                     rasterfile=rasterfile, band=band, aggfun=max,
                     lut=lut, ...))
}

#' @rdname raster_desc
#' @export
zonalMinority <- function(dsn=NULL, layer=NULL, src = NULL, attribute,
                          rasterfile, band = 1, lut=NULL, ...) {

    return(zonalFreq(dsn=dsn, layer=layer, src=src, attribute=attribute,
                     rasterfile=rasterfile, band=band, aggfun=min,
                     lut=lut, ...))
}

#' @rdname raster_desc
#' @export
zonalVariety <- function(dsn=NULL, layer=NULL, src = NULL, attribute,
                         rasterfile, band = 1, lut=NULL, ...) {

    zf <- zonalFreq(dsn=dsn, layer=layer, src=src, attribute=attribute,
                    rasterfile=rasterfile, band=band, lut=lut, ...)

    df_out <- aggregate(value ~ zoneid, zf, length)
    colnames(df_out)[2] <- "number_of_unique_values"
    return(df_out)
}

#' @rdname raster_desc
#' @export
zonalBayes <- function(dsn=NULL, layer=NULL, src=NULL, zoneidfld,
                       helperidfld = NULL, rasterfiles, prednames,
                       predfun, xy = FALSE, nMCMC = 100) {

# 'zoneidfld' - attribute of the polygon layer that identifies zones to predict
# on (values coerced to character).
# 'helperidfld' - optional attribute of the polygon layer that identifies helper
# polygon id (currently, integer IDs)
# If both 'zoneidfld' and 'helperidfld' are used, it is expected that the
# polygon layer resulted from an intersection or GIS union of the zone polygon
# layer and helper polygon layer.

    if (is.null(src))
        src <- sf::st_read(dsn, layer, stringsAsFactors=FALSE, quiet=TRUE)
    else if (!is(src, "sf"))
        stop("'src' must be a polygon layer as 'sf' object")

    if (length(rasterfiles) != length(prednames))
        stop("'length(rasterfiles)' must equal 'length(prednames)'",
                call. = FALSE)

    n_pred_columns <- length(rasterfiles)
    use_helper_polygons <- FALSE
    if (!is.null(helperidfld)) {
        use_helper_polygons <- TRUE
        n_pred_columns = n_pred_columns + 1
    }

    zoneid <- unique(as.character(src[[zoneidfld]]))

    # all predictor layers should have same extent and cell size
    ds <- new(GDALRaster, rasterfiles[1], read_only = TRUE)
    nrows <- ds$getRasterYSize()
    ncols <- ds$getRasterXSize()
    gt <- ds$getGeoTransform()
    xmin <- ds$bbox()[1]
    ymax <- ds$bbox()[4]
    cellsizeX <- ds$res()[1]
    cellsizeY <- ds$res()[2]
    ds$close()

    # list of raster datasets
    nraster <- length(rasterfiles)
    ds_list <- list()
    for (i in 1:nraster) {
        ds_list[[i]] <- new(GDALRaster, rasterfiles[i], read_only = TRUE)
        if (ds_list[[i]]$getRasterYSize() != nrows ||
                ds_list[[i]]$getRasterXSize() != ncols) {

            for (j in 1:i)
                ds_list[[j]]$close()

            message(rasterfiles[i])
            stop("all input rasters must have the same extent",
                 call. = FALSE)
        }
    }

    # list of zones, with list of nMCMC RunningStats objects per zone
    rs_list <- list()
    for (z in zoneid) {
        rs_list[[z]] <- list()
        for (i in 1:nMCMC) {
            rs_list[[z]][[i]] <- new(RunningStats, na_rm = TRUE)
        }
    }

    # raster I/O function for RasterizePolygon()
    readRaster <- function(yoff, xoff1, xoff2, burn_value, attrib_value) {
        x_len <- (xoff2 - xoff1) + 1
        m <- matrix(NA_integer_,
                    nrow = x_len,
                    ncol = n_pred_columns)

        colnames(m) <- c(prednames, helperidfld)
        for (i in 1:nraster) {
            m[, i] <- ds_list[[i]]$read(band = 1,
                                        xoff = xoff1,
                                        yoff = yoff,
                                        xsize = x_len,
                                        ysize = 1,
                                        out_xsize = x_len,
                                        out_ysize = 1)
        }
        if (use_helper_polygons)
            m[, nraster + 1] <- as.integer(burn_value) 
        
        if (xy) {
            m_xy <- matrix(NA_real_, nrow = x_len, ncol = 2)
            m_xy[, 1] <- seq(from = xoff1 + (cellsizeX / 2),
                             by = cellsizeX,
                             length.out = x_len)
            m_xy[, 2] <- rep_len(ymax - (cellsizeY / 2) - (cellsizeY * yoff),
                                 length.out = x_len)
        } else {
            xy <- NULL
        }

        # predicted values
        anyisna <- function(x) { any(is.na(x)) }
        na_rows <- apply(m, 1, anyisna)
        m <- m[!na_rows, , drop = FALSE]

        if (nrow(m) == 0)
            return()

        preds <- predfun(m, xy)

        if (NROW(preds) != nMCMC) 
            stop("fatal: ncol(preds) != nMCMC")

        for (i in seq_len(nMCMC)) {
            rs_list[[attrib_value]][[i]]$update(preds[i, ])
        }

        return()
    }

    # data processing
    geom_col <- attr(src, "sf_column")
    geom_type <- sf::st_geometry_type(src, by_geometry = FALSE)
    if (!geom_type %in% c("POLYGON", "MULTIPOLYGON"))
        stop("geometry type must be POLYGON or MULTIPOLYGON", call. = FALSE)

    pb <- utils::txtProgressBar(min = 0, max = nrow(src))

    for (i in seq_len(nrow(src))) {
        this_zoneid <- as.character(src[[zoneidfld]][i])
        this_helperid <- NA_integer_
        if (use_helper_polygons)
            this_helperid <- src[[helperidfld]][i]
        coords <- src[i, geom_col] |> sf::st_coordinates()
        parts <- numeric(0)
        part_sizes <- numeric(0)
        if (geom_type == "POLYGON") {
            parts <- unique(coords[, "L1"])
            for (j in seq_len(NROW(parts))) {
                part_sizes[j] <- nrow(coords[coords[, "L1"] == parts[j], ])
            }
        } else if (geom_type == "MULTIPOLYGON") {
            parts <- unique(coords[, c("L1", "L2")])
            for (j in seq_len(NROW(parts))) {
                part_sizes[j] <- nrow(coords[coords[, "L1"] == parts[j, 1] &
                                             coords[, "L2"] == parts[j, 2], ])
            }
        }

        grid_xs <- vapply(coords[, "X"],
                          getOffset,
                          0.0,
                          origin = xmin,
                          gt_pixel_size = gt[2])
        grid_ys <- vapply(coords[, "Y"],
                          getOffset,
                          0.0,
                          origin = ymax,
                          gt_pixel_size = gt[6])

        RasterizePolygon(ncols, nrows, part_sizes, grid_xs, grid_ys,
                         readRaster, this_helperid, this_zoneid)

        utils::setTxtProgressBar(pb, i)
    }

    close(pb)

    # output
    # data frame of zone ids and nMCMC columns of rs$get_mean() by zoneid
    zone.preds <- data.frame(zoneid, stringsAsFactors=FALSE)
    for (mcmc in seq_len(nMCMC)) {
        nm <- paste0("MCMC_", mcmc)
        zone.preds[[nm]] <- NA_real_
        for (z in zoneid) {
            zone.preds[zone.preds$zoneid == z, nm] <-
                    rs_list[[z]][[mcmc]]$get_mean()
        }
    }

    for (i in 1:nraster)
        ds_list[[i]]$close()

    return(zone.preds)
}
