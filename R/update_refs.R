# download latest ref tables ---------------------------------------------------

ref_folder <- tempdir()
temp <- tempfile()
fn <- "https://apps.fs.usda.gov/fia/datamart/CSV/FIADB_REFERENCE.zip"
utils::download.file(fn, temp, mode="wb", quiet=TRUE)
utils::unzip(temp, exdir = ref_folder)

# load in most recent version of ref_codes 
# - assuming you are in the FIESTAutils directory
load(file = "data/ref_codes.rda")
old_ref_codes <- ref_codes

# to use for later checks
old_dim <- dim(ref_codes)
old_col_classes <- unlist(lapply(ref_codes, class))

# save version to archive
arcv_date <- format(Sys.Date(), "%Y%b")
fpath <- paste0("data/archive/ref_codes_", arcv_date, ".rda")
save(old_ref_codes, file = fpath)

# update function --------------------------------------------------------------

update_ref_codes <- function(variable,
                             old,
                             new,
                             extra_cond = NULL) {
  
  condition <- paste0("(old$VARIABLE != ", "'", variable, "'", ")", extra_cond)
  eval_cond <- eval(parse(text = condition))
  ref_codes_keep <- old[eval_cond, ]

  ref_codes_new <- rbind(ref_codes_keep, new)
  
  return(ref_codes_new)
  
}

# fortyp and fortypgrp ---------------------------------------------------------

REF_FORTYP <- read.csv(file.path(ref_folder, "REF_FOREST_TYPE.csv"))
REF_FORTYPGRP <- read.csv(file.path(ref_folder, "REF_FOREST_TYPE_GROUP.csv"))

ref_fortyp <- merge(REF_FORTYP[, c("VALUE", "MEANING", "TYPGRPCD")], 
                    REF_FORTYPGRP[, c("VALUE", "MEANING")],
                    by.x = "TYPGRPCD", by.y = "VALUE")

names(ref_fortyp) <- c("GROUPCD", "VALUE", "MEANING", "GROUPNM")

ref_codes_fortyp <- data.frame(VARIABLE="FORTYPCD",
                               ref_fortyp[, c("VALUE", "MEANING")],
                               COLORHEX=NA,
                               ref_fortyp[, c("GROUPCD", "GROUPNM")],
                               GROUPHEX=NA)

ref_codes_fortypgrp <- data.frame(VARIABLE="FORTYPGRPCD",
                                  REF_FORTYPGRP[, c("VALUE", "MEANING")],
                                  COLORHEX=NA,
                                  GROUPCD=NA,
                                  GROUPNM=NA,
                                  GROUPHEX=NA)

# species & speciesgrp ---------------------------------------------------------

REF_SPECIES <- read.csv(file.path(ref_folder, "REF_SPECIES.csv"))

ref_sp_vars <- c("SPCD", "COMMON_NAME", "GENUS", "SPECIES", "VARIETY", "SUBSPECIES",
                 "SPECIES_SYMBOL", "E_SPGRPCD", "W_SPGRPCD", "C_SPGRPCD", "P_SPGRPCD", 
                 "WOODLAND", "SFTWD_HRDWD", "MAJOR_SPGRPCD",  "DRYWT_TO_GREENWT_CONVERSION",
                 "CARBON_RATIO_LIVE", "CREATED_DATE")

ref_species <- REF_SPECIES[, ref_sp_vars] 
ref_species$SCIENTIFIC_NAME <- paste0(ref_species$GENUS, ref_species$SPECIES)

ref_species$CREATED_DATE = as.character(as.Date(ref_species$CREATED_DATE, format = "%Y-%m-%d"))

REF_SPECIES_GROUP <- read.csv(file.path(ref_folder, "REF_SPECIES_GROUP.csv"))
ref_spgrp <- REF_SPECIES_GROUP[, c("SPGRPCD", "NAME", "CLASS")]
ref_spgrp$CLASSNM <- ifelse(ref_spgrp$CLASS == "Softwood", 1, 2)

names(ref_spgrp) <- c("VALUE", "MEANING", "GROUPNM", "GROUPCD")

ref_codes_spgrp <- data.frame(VARIABLE="SPGRPCD",
                              ref_spgrp[, c("VALUE", "MEANING")],
                              COLORHEX=NA,
                              ref_spgrp[, c("GROUPCD", "GROUPNM")],
                              GROUPHEX=NA)

# owngrpcd ---------------------------------------------------------------------

REF_OWNGRPCD <- read.csv(file.path(ref_folder, "REF_OWNGRPCD.csv"))
owngrp <- REF_OWNGRPCD[, c("OWNGRPCD", "MEANING")]
names(owngrp) <- c("VALUE", "MEANING")

ref_codes_owngrp <- data.frame(VARIABLE="OWNGRPCD",
                               owngrp[, c("VALUE", "MEANING")],
                               COLORHEX=NA,
                               GROUPCD=NA,
                               GROUPNM=NA,
                               GROUPHEX=NA)

# statecd ----------------------------------------------------------------------

REF_RESEARCH_STATION <- read.csv(file.path(ref_folder, "REF_RESEARCH_STATION.csv"))

ref_statecd <- REF_RESEARCH_STATION[, c("STATECD", "STATE_NAME", "STATE_ABBR", "RSCD", "RS")]
names(ref_statecd) <- c("VALUE", "MEANING", "ABBR", "RSCD", "RS")

rs_lut <- c("RMRS" = "W",
            "PNWRS" = "W",
            "NCRS" = "E",
            "NERS" = "E",
            "SRS" = "E")

ref_statecd$REGION <- rs_lut[ref_statecd$RS]

ref_statecd$REGION_SPGRPCD <- ref_statecd$REGION

p_states <- c("Hawaii", "American Samoa", "Federated States of Micronesia",
              "Guam", "Marshall Islands", "Northern Mariana Islands", "Palau")
c_states <- c("Puerto Rico", "US Virgin Islands")

ref_statecd[ref_statecd$MEANING %in% p_states, "REGION_SPGRPCD"] <- "P"
ref_statecd[ref_statecd$MEANING %in% c_states, "REGION_SPGRPCD"] <- "C"

# survey unit ------------------------------------------------------------------

REF_UNIT <- read.csv(file.path(ref_folder, "REF_UNIT.csv"))
ref_unit <- REF_UNIT[, c("STATECD", "VALUE", "MEANING")]

ref_unit <- merge(ref_unit,
                  ref_statecd[ , c("VALUE", "MEANING")],
                  by.x = "STATECD",
                  by.y = "VALUE")

names(ref_unit) <- c("GROUPCD", "VALUE", "MEANING", "GROUPNM")

ref_codes_unit <- data.frame(VARIABLE="UNITCD",
                             ref_unit[, c("VALUE", "MEANING")],
                             COLORHEX=NA, 
                             ref_unit[ ,c("GROUPCD", "GROUPNM")],
                             GROUPHEX=NA)

# plant_dictionary -------------------------------------------------------------

REF_PLANT_DICTIONARY <- read.csv(file.path(ref_folder, "REF_PLANT_DICTIONARY.csv"))
ref_plant_dictionary <- REF_PLANT_DICTIONARY[ ,c("SYMBOL", "SCIENTIFIC_NAME")]


# update ref tables ------------------------------------------------------------

# ----- ref_codes -----

# want to keep existing "Other" row that seems to have been added manually at some point
ref_codes <- update_ref_codes("FORTYPCD", ref_codes, ref_codes_fortyp, " | (old$MEANING == 'Other')")
ref_codes <- update_ref_codes("FORTYPGRPCD", ref_codes, ref_codes_fortypgrp, " | (old$MEANING == 'Other')")

ref_codes <- update_ref_codes("SPGRPCD", ref_codes, ref_codes_spgrp)
ref_codes <- update_ref_codes("OWNGRPCD", ref_codes, ref_codes_owngrp)
ref_codes <- update_ref_codes("UNITCD", ref_codes, ref_codes_unit)

ref_codes <- ref_codes[order(ref_codes$VARIABLE), ]

# look at differences --------------------------------------------------------

# differences in unique VARIABLE values
diff_vars <- union(setdiff(unique(old_ref_codes$VARIABLE), unique(ref_codes$VARIABLE)),
                   setdiff(unique(ref_codes$VARIABLE), unique(old_ref_codes$VARIABLE)))

diff_vars

# differences in number of codes per variable (for variables that exist in both tables)

old_counts <- table(old_ref_codes[!old_ref_codes$VARIABLE %in% diff_vars, "VARIABLE"]) |>
  as.data.frame()

new_counts <- table(ref_codes[!ref_codes$VARIABLE %in% diff_vars, "VARIABLE"]) |>
  as.data.frame()

counts_compare <- merge(old_counts,
                        new_counts,
                        by = "Var1")

counts_compare[counts_compare$Freq.x != counts_compare$Freq.y, ]

# checks ---------------------------------------------------------------------

if (ncol(ref_codes) != old_dim[2]) {
  direction <- ifelse(ncol(ref_codes) < old_dim[2], "smaller", "larger")
  stop(paste0("Number of Columns of new `ref_codes` is ", direction, " than the previous version"))
}

if (nrow(ref_codes) > (1.5*old_dim[1])) {
  stop("Number of rows in new `ref_codes` is more than 50% larger than in the previous version")
}

new_col_classes <- unlist(lapply(ref_codes, class))
if (!identical(old_col_classes, new_col_classes)) {
  stop("The class of at least one column in the new version of `ref_codes` is different than in the previous version")
}

# save ref_codes ---------------------------------------------------------------

save(ref_codes, file = file.path("data", "ref_codes.rda"))

# ----- other ref_* -----

save(ref_species, file = file.path("data", "ref_species.rda"))
save(ref_statecd, file = file.path("data", "ref_statecd.rda"))
save(ref_plant_dictionary, file = file.path("data", "ref_plant_dictionary.rda"), compress = 'xz')

# cleanup ----------------------------------------------------------------------

unlink(temp)
unlink(paste0(ref_folder, "/*"))



