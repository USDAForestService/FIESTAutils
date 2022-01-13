#' Class \code{"Rcpp_CmbTable"}
#'
#' C++ program to combine raster files.
#'
#'
#' @name Rcpp_CmbTable-class
#' @docType class
#' @section Extends: Class \code{"\linkS4class{C++Object}"}, directly.
#'
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Chris Toney
#' @keywords classes
NULL





#' Class \code{"Rcpp_RunningStats"}
#'
#' C++ program to calculate mean and variance on a data stream.
#'
#'
#' @name Rcpp_RunningStats-class
#' @docType class
#' @section Extends: Class \code{"\linkS4class{C++Object}"}, directly.
#'
#' All reference classes extend and inherit methods from
#' \code{"\linkS4class{envRefClass}"}.
#' @author Chris Toney
#' @keywords classes
NULL




#' Reference tables - Code definitions.
#'
#' Table with variable codes (VALUE) and descriptions (MEANING).
#'
#'
#' @name ref_codes
#' @docType data
#' @format A dataframe with 7 columns, VARIABLE, VALUE, MEANING, COLORHEX,
#' GROUP, GROUPNM, GROUPHEX.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up tables.
#' @keywords datasets
NULL


#' Reference table - for generating tables.
#'
#' Table with row/column domain (VARNM) and their pretty names for table output
#' (TABLENM).
#'
#'
#' @name ref_domain
#' @docType data
#' @format A dataframe with 2 columns, VARNM and TABLENM.
#' @source FIA look-up table.
#' @keywords datasets
NULL


#' Reference table - for generating tables
#'
#' Data frame with variable names and descriptions
#'
#'
#' @name ref_estvar
#' @docType data
#' @format A data frame with 26 rows and 4 columns CATEGORY - Category of
#' estimation variable ESVARNM - Estimation variable in database ESTTITLE - A
#' title for estimation selections ESTFILTER - Filter statement for each
#' estimation selection
#' @source FIA look-up table
#' @keywords datasets
NULL


#' Reference table - state codes (STATECD).
#'
#' Table with state codes (VALUE), name (MEANING), abbreviation (ABBR), and
#' UNIT.
#'
#'
#' @name ref_statecd
#' @docType data
#' @format A dataframe with 4 columns, VALUE, MEANING, ABBR, UNIT.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source Imported from comma-delimited file.
#' @keywords datasets
NULL



#' SpatialPolygonsDataFrame with FIA state, unit, county codes and names
#'
#' Polygon feature class with state and county boundaries defined by Census
#' Bureau, including Federal Information Processing Standards (FIPS) codes. The
#' FIA Survey Unit code and name attributes (UNITCD, UNITNM) were appended to
#' dataset, with joining columns of STATECD and COUNTYCD.
#'
#' Derived from cb_2018_us_county_5m.  STATEFP was converted to numeric and
#' named STATECD COUNTYFP was converted to numeric and named COUNTYCD Lookup
#' table for FIA Research Station (REF_RESEARCH_STATION) was downloaded from
#' FIA DataMart on 20191105 (FIADB_1.6.1.00) and joined by STATECD. A lookup
#' table for UNITCD was created from plot data using unique STATECD, COUNTYCD,
#' UNITCD and joined to table.
#'
#' Converted to simple feature\cr Transformed CRS from longlat(EPSG:4269) to
#' Albers (EPSG:5070)\cr Saved to R object, with compression='xz'
#'
#'
#' @name stunitco
#' @docType data
#' @format A SpatialPolygonsDataFrame with 3233 features and 8 attributes RS -
#' FIA Research Station name RSCD - FIA Research Station code STATECD - FIPS
#' state code STATENM - FIPS state name STATEAB - FIPS state abbreviation
#' UNITCD - FIA survey unit code UNITNM - FIA survey unit name COUNTYCD - FIPS
#' county code COUNTYNM - FIPS county name
#' @source Downloaded from the United States Census Bureau on 2019 November 3,
#' format Esri Shapefile
#' (https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html)
#' Projection: Geographic (GCS_North_American_1983) EPSG: 4269
#' @keywords datasets
NULL
