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


#' Reference tables - NODATA Values.
#'
#' List of NODATA Values based on data type.
#'
#'
#' @name DEFAULT_NODATA
#' @docType data
#' @format A list of 6 components.
#' @source gdal values.
#' @keywords datasets
NULL


#' Reference tables - gdal data types.
#'
#' Table with gdal data type names.
#'
#'
#' @name GDT_NAMES
#' @docType data
#' @format A vector of 12 data type values.
#' @source gdal values.
#' @keywords datasets
NULL

#' Reference table - diameter 2-inch class codes (DIA).
#' 
#' Table with min (MIN), max (MAX), and 2-inch class diameter codes (MEANING).
#' 
#' 
#' @name ref_diacl2in
#' @docType data
#' @format A dataframe with 3 columns, MIN, MAX, and MEANING.
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source Imported from comma-delimited file.
#' @keywords datasets
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


#' Reference table - for generating estimates
#'
#' Data frame with variable names and descriptions
#'
#'
#' @name ref_estvar
#' @docType data
#' @format A data frame to use a reference for estimation 
#' variables and filters.
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



#' Reference table - List of RMRS plots that have fallen out of inventory
#' because they were not found or they were in the wrong place.
#'
#' Table with variable codes (VALUE) and descriptions (MEANING).
#'
#'
#' @name kindcd3old
#' @docType data
#' @format A dataframe 
#' @source FIA query.
#' SELECT bp.STATECD, bp.COUNTYCD, bp.PLOT_FIADB NEW_PLOT, bp.START_DATE NEW_START_DATE,
#'       	bp_old.COUNTYCD OLD_COUNTYCD, bp_old.PLOT_FIADB OLD_PLOT, 
#'	bp_old.END_DATE OLD_END_DATE, p.CN
#' FROM fs_nims_rmrs.NIMS_BASE_PLOT bp
#' JOIN fs_nims_rmrs.NIMS_BASE_PLOT bp_old on (bp.PREV_NBP_CN=bp_old.CN)
#' JOIN fs_nims_rmrs.NIMS_PLOT_RMRS_VW p on(p.NBP_CN=bp_old.CN)
#' WHERE p.KINDCD = 1
#' ORDER BY bp.STATECD, bp.COUNTYCD, bp_old.PLOT_FIADB"
#' @keywords datasets
NULL


#' Reference table - Metadata for cond default variables output from
#' DBgetPlots()
#' 
#' Data frame with variable names and descriptions
#' 
#' 
#' @name ref_cond
#' @docType data
#' @format A data frame with 61 rows and 3 columns VARIABLE - Variable in cond
#' data frame DESCRIPTION - Description of variable in cond data frame TABLE -
#' Table in database where variable originates or if derived
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up table
#' @keywords datasets
NULL


#' Reference table - Metadata for plt default variables output from
#' DBgetPlots()
#' 
#' Data frame with variable names and descriptions.
#' 
#' 
#' @name ref_plt
#' @docType data
#' @format A data frame with 43 rows and 3 columns VARIABLE - Variable in plt
#' data frame DESCRIPTION - Description of variable in plt data frame TABLE -
#' Table in database where variable originates or if derived
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up table
#' @keywords datasets
NULL


#' Reference table - Metadata for tree default variables output from
#' DBgetPlots()
#' 
#' Data frame with variable names and descriptions
#' 
#' 
#' @name ref_tree
#' @docType data
#' @format A data frame with 72 rows and 3 columns VARIABLE - Variable in tree
#' data frame DESCRIPTION - Description of variable in tree data frame TABLE -
#' Table in database where variable originates
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up table
#' @keywords datasets
NULL



#' Reference table - Metadata for shp_* default variables output from
#' DBgetPlots()
#' 
#' Data frame with variable names and descriptions
#' 
#' 
#' @name ref_shp
#' @docType data
#' @format A dataframe with 63 rows and 4 columns VARIABLE - Variable in plt
#' data frame DESCRIPTION - Description of variable in plt data frame TABLE -
#' Table in database where variable originates or if derived SHPEXPORT - Name
#' of variable for exported shapefile (<= 10 characters)
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source FIA look-up table
#' @keywords datasets
NULL


#' Reference table - Code definitions.
#'
#' Table with species information downloaded from datamart FIADB_REFERENCES, 
#' subset from REF_SPECIES TABLE.
#'
#'
#' @name ref_species
#' @docType data
#' @format A dataframe with 14 columns: SPCD, COMMON_NAME, GENUS, SPECIES, 
#' 		SPECIES_SYMBOL, E_SPGRCD, W_SPGRPCD, C_SPGRPCD, P_SPGRPCD, 
#'         MAJOR_SPGRPCD, JENKINS_TOTAL_B1, JENKINS_TOTAL_B2, 
#'         DRYWT_TO_GREENWT_CONERSION, SCIENTIFIC_NAME (GENUS + SPECIES).
#' @references O'Connell, B.M.; LaPoint, E.B.; Turner, J.A.; Ridley, T.; Boyer,
#' D.; Wilson, A.M.; Waddell, K.L.; Christensen, G.; Conkling, B.L. 2012. The
#' Forest Inventory and Analysis Database: Database Description and Users
#' Manual Version 5.1.2 for Phase 2. U.S. Department of Agriculture.
#' (http://fia.fs.fed.us/library/database-documentation/current/ver5-2012/FIADB_user
#' manual_5-1-2_p2_07_2012.pdf)
#' @source Imported from comma-delimited file.
#' @keywords datasets
NULL


#' Reference table - for generating tables.
#' 
#' Table conversion factors from English to metric units.
#' 
#' 
#' @name ref_conversion
#' @docType data
#' @format A dataframe with 4 columns: TYPE, ENGLISH, METRIC, CONVERSION.
#' @source Conversion table.
#' @keywords datasets
NULL


#' Reference table - Variable titles.
#' 
#' Table with variable name (VARNM) and associated title (TITLE).
#' 
#' 
#' @name ref_titles
#' @docType data
#' @format A dataframe with 2 columns, VARNM and TITLE.
#' @source Comma-delimited file.
#' @keywords datasets
NULL


#' Reference table - popType codes.
#' 
#' Table with population type (popType) and associated evaluation code (EVAL_TYP_CD).
#' 
#' 
#' @name ref_popType
#' @docType data
#' @format A dataframe with 2 columns, VARNM and TITLE.
#' @source Comma-delimited file.
#' @keywords datasets
NULL


