# Chris Toney, chris.toney@usda.gov
# miscellaneous functions for processing vector data using package sf
# functions from raster_analysis.R required for layerFromRasterExtent()

## ptdata2sf
## sf2ptdata
## layerIntersection
## layerUnion
## tabulateIntersections
## selectByIntersects
## layerFromRasterExtent

#' @rdname internal_desc
#' @export
ptdata2sf <- function(ptdata, crs, coords=c(2,3), asSpatial=FALSE,
                      stringsAsFactors=FALSE) {
    # Convert a point dataframe containing id, x, y columns to sf spatial layer

    out <- st_as_sf(ptdata, crs=crs, stringsAsFactors=stringsAsFactors,
                    coords=coords, agr="identity")

    if (asSpatial) {
        return(sf::as_Spatial(out))
    } else {
        return(out)
    }
}

#' @rdname internal_desc
#' @export
sf2ptdata <- function(ptlayer, id_fld=1, stringsAsFactors=FALSE) {
    # Convert sf spatial layer to a point dataframe containing id, x, y columns

    if (canCoerce(ptlayer, "sf")) {
        ptlayer <- sf::st_as_sf(ptlayer, stringsAsFactors = stringsAsFactors)
    } else {
        stop("Cannot coerce layer object to sf.")
    }

    return(data.frame(ptlayer[, id_fld, drop = TRUE], st_coordinates(ptlayer),
                      stringsAsFactors = stringsAsFactors))
}

#' @rdname internal_desc
#' @export
layerIntersection <- function(layer1, layer2, asSpatial=FALSE,
                              dropLowerDimGeom=TRUE,
                              stringsAsFactors=FALSE) {
    # Convenience function for sf::st_intersection.
    # Adds option to drop lower dimension geometries that may result from the
    # intersetion, for example, points or lines that may result from
    # intersecting two polygon layers.

    if (canCoerce(layer1, "sf")) {
        layer1 <- sf::st_as_sf(layer1, stringsAsFactors = stringsAsFactors)
    } else {
        stop("Cannot coerce layer 1 object to sf.")
    }
    if (canCoerce(layer2, "sf")) {
        layer2 <- sf::st_as_sf(layer2, stringsAsFactors = stringsAsFactors)
    } else {
        stop("Cannot coerce layer 2 object to sf.")
    }

    int12 <- sf::st_intersection(layer1, layer2)
    if (dropLowerDimGeom) {
        dim <- min(max(st_dimension(layer1)), max(st_dimension(layer2)))
        int12 <- int12[sf::st_dimension(int12) == dim, ]
    }

    if (asSpatial) {
        return(sf::as_Spatial(int12))
    } else {
        return(sf::st_cast(int12))
    }
}

#' @rdname internal_desc
#' @export
layerUnion <- function(layer1, layer2, asSpatial=FALSE, dropLowerDimGeom=TRUE,
                       sfColName="geometry", stringsAsFactors=FALSE) {
    # Equivalent to the vector Union tool in ArcGIS or QGIS (www.qgis.org),
    # and OGR_L_Union function in the GDAL C API (www.gdal.org).

    if (canCoerce(layer1, "sf")) {
        layer1 <- sf::st_as_sf(layer1, stringsAsFactors = stringsAsFactors)
    } else {
        stop("Cannot coerce layer 1 object to sf.")
    }
    if (canCoerce(layer2, "sf")) {
        layer2 <- sf::st_as_sf(layer2, stringsAsFactors = stringsAsFactors)
    } else {
        stop("Cannot coerce layer 2 object to sf.")
    }

    # make sure sf_column name is the same in both layers
    i <- which(names(layer1) == attr(layer1, "sf_column"))
    if (length(i) == 1) {
        names(layer1)[i] <- sfColName
        sf::st_geometry(layer1) <- sfColName
    }

    i <- which(names(layer2) == attr(layer2, "sf_column"))
    if (length(i) == 1) {
        names(layer2)[i] <- sfColName
        sf::st_geometry(layer2) <- sfColName
    }

    int12 <- layerIntersection(layer1, layer2,
                               dropLowerDimGeom = dropLowerDimGeom)
    dif12 <- sf::st_difference(layer1, st_union(layer2))
    dif21 <- sf::st_difference(layer2, st_union(layer1))

    if (nrow(dif12) > 0) {
        col.diff1 <- setdiff(colnames(int12), colnames(dif12))
        dif12[, c(as.character(col.diff1))] <- NA
    }
    if (nrow(dif21) > 0) {
        col.diff2 <- setdiff(colnames(int12), colnames(dif21))
        dif21[, c(as.character(col.diff2))] <- NA
    }

    out <- rbind(int12, dif12, dif21)

    if (asSpatial) {
        return(sf::as_Spatial(out))
    } else {
        return(sf::st_cast(out))
    }
}

#' @rdname internal_desc
#' @export
tabulateIntersections <- function(layer1, layer1fld, layer2, layer2fld=NULL,
                                  stringsAsFactors=FALSE, withUnits=FALSE) {
    # for calculating polygon overlap, assumes layers are polygon in the same
    # projected CRS
    # area and percent of polygons in layer 1 overlapped by polygons in layer 2
    # if layer 2 field is NULL, all of the polygons in layer 2 will be treated
    # as a single polygon
    if (canCoerce(layer1, "sf")) {
        layer1 <- sf::st_as_sf(layer1, stringsAsFactors = stringsAsFactors)
    } else {
        stop("Cannot coerce layer 1 object to sf.")
    }
    if (canCoerce(layer2, "sf")) {
        layer2 <- sf::st_as_sf(layer2, stringsAsFactors = stringsAsFactors)
    } else {
        stop("Cannot coerce layer 2 object to sf.")
    }

    layer1sfcol <- attr(layer1, "sf_column")
    layer1agg <- aggregate(layer1[, c(layer1sfcol)],
                           by = list(layer1[,layer1fld, drop = TRUE]),
                           sum)
    names(layer1agg) <- c(layer1fld, "geometry")
    layer1agg$l1.area <- sf::st_area(layer1agg)
    if (!withUnits) {
        layer1agg$l1.area <- as.numeric(layer1agg$l1.area)
    }

    if (is.null(layer2fld)) {
        layer2agg <- sf::st_union(layer2)
    } else {
        layer2sfcol <- attr(layer2, "sf_column")
        layer2agg <- aggregate(layer2[, c(layer2sfcol)],
                               by = list(layer2[, layer2fld, drop = TRUE]),
                               sum)
        names(layer2agg) <- c(layer2fld, "geometry")
    }

    int12 <- sf::st_intersection(layer1agg, layer2agg)
    int12$int.area <- sf::st_area(int12)
    if (!withUnits) {
        int12$int.area <- as.numeric(int12$int.area)
    }

    if (is.null(layer2fld)) {
        out <- expand.grid(layer1agg[, layer1fld, drop = TRUE],
                           stringsAsFactors = FALSE)
        names(out) <- c(layer1fld)
    } else {
        out <- expand.grid(layer1agg[, layer1fld, drop = TRUE],
                           layer2agg[, layer2fld, drop = TRUE],
                           stringsAsFactors=FALSE)
        names(out) <- c(layer1fld, layer2fld)
    }

    out <- merge(out,
                 layer1agg[, c(layer1fld, "l1.area"), drop = TRUE])

    out <- merge(out,
                 int12[, c(layer1fld, layer2fld, "int.area"), drop = TRUE],
                 all.x = TRUE)

    out$int.pct <- round((out$int.area / out$l1.area) * 100, 3)

    return(out)
}

#' @rdname internal_desc
#' @export
selectByIntersects <- function(layer1, layer2, overlapThreshold=0,
                               thresholdAsPct=TRUE, asSpatial=FALSE,
                               stringsAsFactors=FALSE) {
    # Select features from layer1 that intersect features in layer2
    # Mainly for polygons. Same as layer1[layer2,] but with option to specify
    # overlap.
    # Selects only the polygons in layer1 that are overlapped by more than the
    # threshold value:
    #       overlapThreshold = threshold percent or absolute area
    # The default overlapThreshold = 0 excludes polygons in layer1 that touch
    # but do not overlap layer2.
    # overlapThreshold = -1 is equivalent to layer1[layer2,] (includes polygons
    # that touch).
    # thresholdAsPct = TRUE for overlapThreshold in percent, FALSE for absolute
    # area (CRS units).
    # Assumes both layers are in the same projected CRS.
    # Returns a subset of layer1 containing the selected features.
    # asSpatial = TRUE returns sp object, otherwise sf object

    if (canCoerce(layer1, "sf")) {
        layer1 <- sf::st_as_sf(layer1, stringsAsFactors=stringsAsFactors)
    } else {
        stop("Cannot coerce layer 1 object to sf.")
    }
    if (canCoerce(layer2, "sf")) {
        layer2 <- sf::st_as_sf(layer2, stringsAsFactors=stringsAsFactors)
    } else {
        stop("Cannot coerce layer 2 object to sf.")
    }

    out <- layer1[layer2,]
    if (nrow(out) == 0)
        return(NULL)

    if (overlapThreshold >= 0) {
        tmp.id <- seq_len(nrow(out))
        out <- sf::st_sf(data.frame(tmp.id, out))
        tab <- tabulateIntersections(out, "tmp.id", layer2)

        if (thresholdAsPct) {
            out <- out[tab$int.pct > overlapThreshold,]
        } else {
            out <- out[tab$int.area > overlapThreshold,]
        }
        out$tmp.id <- NULL
    }

    if (asSpatial) {
        return(sf::as_Spatial(out))
    } else {
        return(out)
    }
}

#' @rdname internal_desc
#' @export
layerFromRasterExtent <- function(rasterfile, asSpatial=FALSE) {
    ri <- rasterInfo(rasterfile)
    if (is.null(ri)) {
        stop("Cannot open raster file.")
    }

    b <- sf::st_bbox(c(xmin = ri$bbox[1], ymin = ri$bbox[2], xmax = ri$bbox[3],
                       ymax = ri$bbox[4]), crs = st_crs(ri$crs))
    g <- sf::st_as_sfc(b)
    out <- sf::st_sf(desc = "extent", g)

    if (asSpatial) {
        return(sf::as_Spatial(out))
    } else {
        return(sf::st_cast(out))
    }
}
