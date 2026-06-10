### Tabulation for Palau 2025 PHC ###
# Luis de la Rua ## Feb 2026 #

# SETTINGS ====================================================================

# Clean workspace
rm(list = ls())
gc()

source("setup.R")

getwd()
# Raw data directory
dd <- "C:/Users/luisr/SPC/SDD GIS - Documents/PopGIS/PopGIS3/Data/Palau/2025/"
tab <- "C:/Users/luisr/SPC/SDD GIS - Documents/PopGIS/PopGIS3/Data/Palau/2025/tables/"
nada <- "C:/Users/luisr/OneDrive - SPC/NADA/Palau/SPC_PLW_2025_PHC_v01_M/Data/Distribute/"

# 1.IMPORT AND PREPARE CENSUS DATASETS ========================================
## 1.1 Import Stata databases ----

stata_files <- list.files (nada,
                           pattern = "*.dta", full.names = T)
stata_files

## 1.2 Import datasets we are going to use ----
hous <- read_stata(stata_files[2])
pop <- read_stata(stata_files[4]) 

# 1.3 Get labels of the variables ----
view(get_labels(hous))
view(get_labels(pop))

# 1.4 Get variable labels ----

variables <- names(hous)
labels_list <- list()

for (variable in variables) {
  if (variable %in% names(hous)) {
    labels_list[[variable]] <- get_catlab(hous[[variable]])
  } else {
    warning(paste("Variable", variable, "not found in the dataset."))
  }
}

# Print the labels
for (variable in names(labels_list)) {
  if (!is.null(labels_list[[variable]])) {
    cat("Labels for variable", variable, ":\n")
    print(labels_list[[variable]])
    cat("\n")
  } else {
    cat("No labels found for variable", variable, "\n\n")
  }
}
labels_list

# 1.6 Export code books for hous and persons dataset ----
# 1. Extract Variable Labels (Question Text)
var_labels <- data.frame(
  Variable = names(hous),
  Description = var_label(hous) %>% as.character()
)

pop_var_labels <- data.frame(
  Variable = names(pop),
  Description = var_label(pop) %>% as.character()
)


# 2. HOUSING DATASET ==========================================================
## 2.1 Filter questionnaires we're keeping for the tabulation process ----
# Keep private and occuppied hhs
hh <- hous %>% 
  rename(hid = 	hamlet) %>% 
  filter(dwell_type %in% c(1,2)) %>% # dwell_type !=NA implies occupied
  mutate(hid = zap_labels(hid))

## 2.2 Create Codgeo table master EA list ----
# We will sort EA code out later when we get the originals
# We extract it from the Hamlet layer that we will connecting later on in PopGIS
ham_geo <- vect("C:/Users/luisr/SPC/SDD GIS - Documents/Census/2025/2025_PLW_PHC/layers/PLW_2025_PHC.gpkg", 
                layer = "PLW_2023HIES_HID_32653 — PLW_2023HIES_HID_4326")
codgeo <- ham_geo %>%
  as_tibble |> 
  select(hid)

test <- hh %>%
  group_by(hid) %>%
  summarise(total_hh = n(), .groups = "drop")
#### I HAVE TO INCLUDE THE TOTAL HHS 

## 2.3 Function to automate the PopGIS table generation =============================
# Define the function based on your exact workflow
process_popgis_tab <- function(data, backbone, var_name, rename_map, file_name) {
  
  # 1. Tabulate and Pivot
  tab_df <- data %>%
    filter(!is.na(!!sym(var_name))) %>%
    count(hid, !!sym(var_name)) %>%
    # Convert codes to character so they match the case_when keys
    mutate(!!sym(var_name) := as.character(zap_labels(!!sym(var_name)))) %>%
    pivot_wider(names_from = !!sym(var_name), values_from = n, values_fill = 0)
  
  # 2. Rename columns using your mapping
  tab_df <- tab_df %>%
    rename_with(~recode(., !!!rename_map), .cols = -hid)
  
  # 3. Join with Codgeo backbone (ensure 77 rows)
  final <- backbone %>%
    full_join(tab_df, by = "hid") %>%
    mutate(across(everything(), ~replace_na(., 0))) 
  
  # 4. Validation Checks
  if (sum(is.na(final)) != 0) stop(paste("NAS WRONG in", file_name))
  if (nrow(final) != 77) warning(paste("Row count is", nrow(final), "for", file_name))
  
  # 5. Summary Totals for console
  totals <- final %>%
    select(-hid) %>% 
    summarise(across(everything(), sum))
  
  cat("\n--- SUMMARY TOTALS:", file_name, "---\n")
  print(totals)
  
  # 6. Export
  write.xlsx(final, paste0(tab, file_name, ".xlsx"), sheetName = "hid")
}

## 2.4 Automate categories map production --------
get_gis_map <- function(vec, var_name = "unknown") {
  # 1. Extract raw labels from Stata metadata
  labs <- attr(vec, "labels")
  if (is.null(labs)) {
    warning(paste("No labels found for", var_name))
    return(NULL)
  }
  
  codes <- as.character(labs)
  raw_names <- names(labs)
  
  # 2. Clean the names (lowercase, no specials, spaces to underscores)
  clean_names <- raw_names %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", "") %>%
    str_trim() %>%
    str_replace_all("\\s+", "_") %>%
    str_sub(1, 10) %>%        # Truncate to 10
    str_replace("_$", "")     # Clean trailing underscore
  
  # 3. Handle Duplicates (Collision Detection)
  if (any(duplicated(clean_names))) {
    message(paste("! Collision detected in", var_name, "- resolving duplicates..."))
    clean_names <- make.unique(clean_names, sep = "_")
    # Ensure they are still <= 10 chars after adding suffixes
    clean_names <- str_sub(clean_names, 1, 10)
  }
  
  return(setNames(clean_names, codes))
}

## 2.5 Multi Select variables function and map names. -------

clean_multiselect_label <- function(label) {
  # 1. Split by : or ; and take the last part
  clean_text <- str_split(label, "[:;]")[[1]] %>% last() %>% str_trim()
  
  # 2. Clean for GIS (lowercase, alphanumeric, 10 chars)
  clean_text %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", "") %>%
    str_replace_all("\\s+", "_") %>%
    str_sub(1, 10) %>%
    str_replace("_$", "")
}

get_multiselect_map <- function(data, prefix) {
  # Find all columns belonging to this question
  cols <- names(data)[str_detect(names(data), paste0("^", prefix, "__"))]
  
  if (length(cols) == 0) return(NULL)
  
  # Extract the numbers after the __ (the codes)
  codes <- str_extract(cols, "(?<=__)\\d+$")
  
  # Extract the labels for these specific columns
  raw_labels <- map_chr(cols, ~as.character(var_label(data[[.x]])))
  
  # Clean the labels (removing prefix text before : or ;)
  clean_names <- map_chr(raw_labels, clean_multiselect_label)
  
  # Resolve collisions (duplicate 10-char names)
  if (any(duplicated(clean_names))) {
    clean_names <- make.unique(clean_names, sep = "_") %>% str_sub(1, 10)
  }
  
  return(setNames(clean_names, codes))
}

process_multiselect_popgis <-function(data, backbone, prefix, rename_map, file_name) {
  
  # Aggregate by EA
  tab_df <- data %>%
    group_by(hid) %>%
    summarise(
      across(starts_with(paste0(prefix, "__")), ~sum(. == 1, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Clean column names to be just the digits (to match the map keys)
  tab_df <- tab_df %>%
    rename_with(~str_extract(., "\\d+$"), starts_with(prefix))
  
  # Rename using the map
  tab_df <- tab_df %>%
    rename_with(~rename_map[.], .cols = any_of(names(rename_map)))
  
  # Final backbone join
  final <- backbone %>%
    full_join(tab_df, by = "hid") %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    relocate(total_hh, .after = hid)
  
  # Export
  write.xlsx(final, paste0(tab, file_name, ".xlsx"), sheetName = "hid")
  cat("Table", file_name, "tabulated successfully.\n")
}




## 2.6 TABLES =============================

### Table H1. Dwelling Type by Hamlet ----------------------------------------------

print_labels(hh$dwell_type)
cat_map <- get_gis_map(hh$dwell_type)
cat_map<- c(    "1" = "priv_hh",
                "2" = "barracks")

process_popgis_tab(hh, codgeo, "dwell_type", cat_map, "h1_dwell_type" )

### Table H2. Type of living quarter by Hamlet ---------------------------------------------
print_labels(hh$lquarters)
cat_map <- get_gis_map(hh$lquarters)
cat_map
cat_map<- c(    "1" = "fam_h_det",
                "2" = "fam_h_at",
                "3" = "build_19",
                "4" = "build_1019",
                "5" = "build_20",
                "7" = "priv_inst",
                "8" = "temp_str",
                "99" = "other")

process_popgis_tab(hh, codgeo, "lquarters", cat_map, "h2_lquarters" )

### Table H3. Floor type by Hamlet ---------------------------------------------
print_labels(hh$i3_floor)
cat_map <- get_gis_map(hh$i3_floor)
cat_map

process_popgis_tab(hh, codgeo, "i3_floor", cat_map, "i3_floor" )

### Table H4. Roof type by Hamlet ---------------------------------------------
print_labels(hh$i4_roof)
cat_map <- get_gis_map(hh$i4_roof)
cat_map

process_popgis_tab(hh, codgeo, "i4_roof", cat_map, "i4_roof" )

### Table H5. Walls type by Hamlet ---------------------------------------------
print_labels(hh$i5_material_walls)
cat_map <- get_gis_map(hh$i5_material_walls)
cat_map

process_popgis_tab(hh, codgeo, "i5_material_walls", cat_map, "i5_material_walls" )


### Table H6. Main Source of Drinking water by EA ---------------------------------------------
var_map <- get_multiselect_map(hh, "i6_drink_water") %>% print()
var_map["1"] <- "pub_pip_in"  
var_map["2"] <- "pub_pip_ot"  
print(var_map)

process_multiselect_popgis(hh, codgeo, "i6_drink_water", var_map, "i6_drink_water")

### Table H6a. Improved Sources of Drinking water by Hamlet ------------------
print_labels(hh$waterimpr)
cat_map <- get_gis_map(hh$waterimpr)
cat_map

process_popgis_tab(hh, codgeo, "waterimpr", cat_map, "i6a_waterimpr" )

### Table H7. Main Source of cooking water by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "i7_source_water") %>% print()
var_map["1"] <- "pub_pip_in"  
var_map["2"] <- "pub_pip_ot"  
var_map["7"] <- "own_tnk_in"
var_map["8"] <- "own_tnk_ot"
print(var_map)

process_multiselect_popgis(hh, codgeo, "i7_source_water", var_map, "i7_source_water")

### Table H7a. Cooking Water On Premises by Hamlet ------------------
print_labels(hh$waterprem)
cat_map <- get_gis_map(hh$waterprem)
cat_map

process_popgis_tab(hh, codgeo, "waterprem", cat_map, "i7a_waterprem" )

### Table H8. Toilet Facilities by Hamlet ------------------
print_labels(hh$i8_toilet_facility)
cat_map <- get_gis_map(hh$i8_toilet_facility)
cat_map
cat_map["1"] <- "flush_sew"  
cat_map["2"] <- "flush_sept"
cat_map["5"] <- "pl_slab"  
cat_map["6"] <- "pl_open"  
print(cat_map)
process_popgis_tab(hh, codgeo, "i8_toilet_facility", cat_map, "i8_toilet_facility" )

### Table H8a. Improved Sanitation by Hamlet ------------------
print_labels(hh$sanitationimpr)
cat_map <- get_gis_map(hh$sanitationimpr)
cat_map
process_popgis_tab(hh, codgeo, "sanitationimpr", cat_map, "i8a_sanitationimpr" )

### Table H8b. Shared Toilet by Hamlet ------------------
print_labels(hh$i8b_share_toilet)
cat_map <- get_gis_map(hh$i8b_share_toilet)
cat_map["1"] <- "shared"  
cat_map["2"] <- "not_shared"
cat_map

process_popgis_tab(hh, codgeo, "i8b_share_toilet", cat_map, "i8b_share_toilet" )

### Table H9. Main cooking fuel by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "i9_cook_fuel") %>% print()

process_multiselect_popgis(hh, codgeo, "i9_cook_fuel", var_map, "i9_cook_fuel")

### Table H10. Main source of electricity by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "i10_electricity") %>% print()
var_map["0"] <- "no_elect"

process_multiselect_popgis(hh, codgeo, "i10_electricity", var_map, "i10_electricity")