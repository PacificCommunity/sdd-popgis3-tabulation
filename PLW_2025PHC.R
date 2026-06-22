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

## 1.3 Get labels of the variables ----
view(get_labels(hous))
view(get_labels(pop))

## 1.4 Get variable labels ----

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

## 1.6 Export code books for hous and persons dataset ----
# Extract Variable Labels (Question Text)
var_labels <- data.frame(
  Variable = names(hous),
  Description = var_label(hous) %>% as.character()
)

pop_var_labels <- data.frame(
  Variable = names(pop),
  Description = var_label(pop) %>% as.character()
)

## 1.7 Merge Hamlet 15~ into 1501 for both datasets
pop <- pop |> 
  mutate(hamlet = if_else(hamlet %in% c(1502, 1503, 1504), 1501, hamlet))

hous <- hous |> 
  mutate(hamlet = if_else(hamlet %in% c(1502, 1503, 1504), 1501, hamlet))

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
ham_geo <- vect("C:/Users/luisr/SPC/SDD GIS - Documents/PopGIS/PopGIS3/GEO Layers/PLW/PLW_2025PHC_HID_4326.shp")
# Extract the data frame from the SpatVector first, then build the backbone
codgeo <- as.data.frame(ham_geo) %>%
  as_tibble() %>% 
  select(hid_2025) %>% 
  rename(hid = hid_2025) %>% 
  mutate(hid = as.numeric(hid)) %>%
  arrange(hid) |> 
  # Fulfilling your note: Include the total HHs
  left_join(
    hh %>%
      group_by(hid) %>%
      summarise(total_hh = n(), .groups = "drop"),
    by = "hid"
  ) %>%
  # Ensure any empty hamlets show 0 households instead of NA
  mutate(total_hh = replace_na(total_hh, 0))

message("backbone layer contains ", nrow(codgeo), " hamlets")

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
  
  # 3. Join with Codgeo backbone (ensure 78 rows)
  final <- backbone %>%
    full_join(tab_df, by = "hid") %>%
    mutate(across(everything(), ~replace_na(., 0))) 
  
  # 4. Validation Checks
  if (anyNA(final)) stop(paste("NAs detected in", file_name))
  if (nrow(final) != 77) warning(paste("Row count is", nrow(final), "for", file_name))
  
  # 5. Summary Totals for console
  totals <- final %>%
    select(-hid) %>% 
    summarise(across(everything(), sum))
  
  cat("\n--- SUMMARY TOTALS:", file_name, "---\n")
  print(totals)
  
  cat("\n--- FINAL TABLE PREVIEW:", file_name, "---\n")
  # Printing as a tibble with n=15 gives you a clean view of the columns and top rows
  print(as_tibble(final), n = 15) 
  cat("======================================================\n\n")
  
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

# Multiselect options tabulation 
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
  
  if (anyNA(final)) stop(paste("NAs detected in", file_name))
  if (nrow(final) != nrow(backbone)) warning(paste("Row count is", nrow(final), "for", file_name, "- expected", nrow(backbone)))
  
  # Summary Totals for console
  totals <- final %>%
    select(-hid, -total_hh) %>% # Exclude IDs and baseline household totals from the sum
    summarise(across(everything(), sum))
  
  cat("\n======================================================\n")
  cat("--- SUMMARY TOTALS:", file_name, "---\n")
  print(totals)
  
  # Print the actual table preview to the console
  cat("\n--- FINAL TABLE PREVIEW:", file_name, "---\n")
  print(as_tibble(final), n = 15)
  cat("======================================================\n\n")
  # ---------------------------------------------------------
  
  # Export
  write.xlsx(final, paste0(tab, file_name, ".xlsx"), sheetName = "hid")
  cat("Table", file_name, "tabulated successfully.\n")
}

## 2.7 Numeric variables function -------

process_numeric_popgis <- function(data, backbone, var_name, file_name) {
  
  # 1. Aggregate numeric variable by EA (hid)
  tab_df <- data %>%
    group_by(hid) %>%
    summarise(
      sum_val = sum(!!sym(var_name), na.rm = TRUE),
      avg_val = mean(!!sym(var_name), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Dynamically rename columns based on the input variable 
    # (e.g., tot_hhsize and avg_hhsize)
    rename(
      !!paste0("tot_", var_name) := sum_val,
      !!paste0("avg_", var_name) := avg_val
    ) %>%
    # Handle NaN that occurs if mean() is calculated on a hamlet with 0 households
    mutate(across(starts_with("avg_"), ~if_else(is.nan(.), 0, .)))
  
  # 2. Final backbone join
  final <- backbone %>%
    full_join(tab_df, by = "hid") %>%
    # Replace NAs with 0 for hamlets with no records
    mutate(across(everything(), ~replace_na(., 0))) %>%
    # Round averages to 1 decimal place for cleaner GIS mapping
    mutate(across(starts_with("avg_"), ~round(., 1))) %>%
    relocate(total_hh, .after = hid)
  
  # ---------------------------------------------------------
  # 3. Validation Checks
  if (anyNA(final)) stop(paste("NAs detected in", file_name))
  if (nrow(final) != nrow(backbone)) warning(paste("Row count is", nrow(final), "for", file_name, "- expected", nrow(backbone)))
  
  # 4. Summary Totals for console 
  # (Only summing the totals, because summing averages doesn't make statistical sense)
  totals <- final %>%
    select(starts_with("tot_")) %>% 
    summarise(across(everything(), sum))
  
  cat("\n======================================================\n")
  cat("--- SUMMARY TOTALS:", file_name, "---\n")
  print(totals)
  
  # 5. Print the actual table preview to the console
  cat("\n--- FINAL TABLE PREVIEW:", file_name, "---\n")
  print(as_tibble(final), n = 15)
  cat("======================================================\n\n")
  # ---------------------------------------------------------
  
  # 6. Export
  write.xlsx(final, paste0(tab, file_name, ".xlsx"), sheetName = "hid")
  cat("Table", file_name, "tabulated successfully.\n")
}

## 2.6 TABLES =============================

### Table H1. Dwelling Type by Hamlet ----------------------------------------------

print_labels(hh$dwell_type)
cat_map <- get_gis_map(hh$dwell_type)
cat_map <- c(    "1" = "priv_hh",
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

### Table H3. Household Size by Hamlet -----------------------------------------

process_numeric_popgis(hh, codgeo, "hhsize", "h3_hhsize" )

### Table H4. Sex of the Head of the Hosuehold
print_labels(hh$headsex)
cat_map <- get_gis_map(hh$headsex)


process_popgis_tab(hh, codgeo, "headsex", cat_map, "h4_headsex" )

### Table H5. Type of housing tenure by Hamlet ---------------------------------------------
print_labels(hh$tenure_type)
cat_map <- get_gis_map(hh$tenure_type)
cat_map

process_popgis_tab(hh, codgeo, "tenure_type", cat_map, "h5_tenure_type" )


### Table H6. Type of housing tenure by Hamlet ---------------------------------------------
print_labels(hh$tenure_type)
cat_map <- get_gis_map(hh$tenure_type)
cat_map

process_popgis_tab(hh, codgeo, "tenure_type", cat_map, "h6_tenure_type" )


### Table H7. Type of walls by Hamlet ---------------------------------------------
print_labels(hh$walls)
cat_map <- get_gis_map(hh$walls)
cat_map

process_popgis_tab(hh, codgeo, "walls", cat_map, "h7_walls" )


### Table H7a. Insulated walls by Hamlet ---------------------------------------------
print_labels(hh$insulated_walls)
cat_map <- get_gis_map(hh$insulated_walls)
cat_map <- c(    "1" = "yes",
                    "2" = "no",
                    "3" = "dunno")

process_popgis_tab(hh, codgeo, "insulated_walls", cat_map, "h7a_ins_walls" )

### Table H8. Type of roof by Hamlet ---------------------------------------------
print_labels(hh$roof)
cat_map <- get_gis_map(hh$roof)
cat_map

process_popgis_tab(hh, codgeo, "roof", cat_map, "h8_roof" )


### Table H8a. Insulated roof by Hamlet ---------------------------------------------
print_labels(hh$insulated_roof)
cat_map <- get_gis_map(hh$insulated_roof)
cat_map <- c(    "1" = "yes",
                 "2" = "no")

process_popgis_tab(hh, codgeo, "insulated_roof", cat_map, "h8a_ins_roof" )

### Table H9. Type of foundation by Hamlet ---------------------------------------------
print_labels(hh$foundation)
cat_map <- get_gis_map(hh$foundation)
cat_map

process_popgis_tab(hh, codgeo, "foundation", cat_map, "h9_foundation" )


### Table H10. Number of rooms by Hamlet -------------------------------------
hh <- hh %>%
  mutate(
    rooms_grouped = case_when(
      rooms == 1 ~ 1,
      rooms == 2 ~ 2,
      rooms >= 3 & rooms <= 4 ~ 3, # Groups 3 and 4 rooms together
      rooms >= 5 ~ 4,              # Caps the top end at 5+
      TRUE ~ 99                    # Catch-all for NAs, 0s, or missing data
    )
  )

# 2. Define the GIS map with your <= 10 character limit
cat_map_rooms <- c(
  "1" = "1_room",
  "2" = "2_rooms",
  "3" = "3_to_4_rm",
  "4" = "5_plus_rm",
  "99" = "unknown"
)

# 3. Process the table using your standard function
process_popgis_tab(hh, codgeo, "rooms_grouped", cat_map_rooms, "h10_rooms")


### Table H11. Number of bedrooms by Hamlet -------------------------------------
hh <- hh %>%
  mutate(
    rooms_grouped = case_when(
      bedrooms == 1 ~ 1,
      bedrooms == 2 ~ 2,
      bedrooms >= 3 & bedrooms <= 4 ~ 3, # Groups 3 and 4 rooms together
      bedrooms >= 5 ~ 4,              # Caps the top end at 5+
      TRUE ~ 99                    # Catch-all for NAs, 0s, or missing data
    )
  )

# 2. Define the GIS map with your <= 10 character limit
cat_map_rooms <- c(
  "1" = "1_room",
  "2" = "2_rooms",
  "3" = "3_to_4_rm",
  "4" = "5_plus_rm",
  "99" = "unknown"
)

# 3. Process the table using your standard function
process_popgis_tab(hh, codgeo, "rooms_grouped", cat_map_rooms, "h11_bedrooms")


### Table H12. Main Drinking Water by Hamlet ---------------------------------------------
print_labels(hh$drink_water)
cat_map <- get_gis_map(hh$drink_water)


process_popgis_tab(hh, codgeo, "drink_water", cat_map, "h12_drink_water" )

### Table H13. Main Cook Water by Hamlet ---------------------------------------------
print_labels(hh$cook_water)
cat_map <- get_gis_map(hh$cook_water)


process_popgis_tab(hh, codgeo, "cook_water", cat_map, "h13_cook_water" )

### Table H14. Hot Water by Hamlet ---------------------------------------------
print_labels(hh$hotwater)
cat_map <- get_gis_map(hh$hotwater)
cat_map

process_popgis_tab(hh, codgeo, "hotwater", cat_map, "h14_hotwater" )


### Table H15. Type of Energy for water heater by Hamlet ---------------------------------------------
print_labels(hh$energy_type)
cat_map <- get_gis_map(hh$energy_type)
cat_map

process_popgis_tab(hh, codgeo, "energy_type", cat_map, "h15_energy_type" )

### Table H16. Bathub or shower in the household by Hamlet ---------------------------------------------
print_labels(hh$bath_shower)
cat_map <- get_gis_map(hh$bath_shower)
cat_map

process_popgis_tab(hh, codgeo, "bath_shower", cat_map, "h16_bath_shower" )

### Table H17. Sink in the household by Hamlet ---------------------------------------------
print_labels(hh$sink)
cat_map <- get_gis_map(hh$sink)
cat_map

process_popgis_tab(hh, codgeo, "sink", cat_map, "h17_sink" )


### Table H18. Electricity type by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "electricity") %>% print()

process_multiselect_popgis(hh, codgeo, "electricity", var_map, "h18_electricity")


### Table H19. Main source of lighting by Hamlet ---------------------------------------------
print_labels(hh$lighting)
cat_map <- get_gis_map(hh$lighting)
cat_map

process_popgis_tab(hh, codgeo, "lighting", cat_map, "h19_lighting" )

### Table H20. Main cooking fuel by Hamlet ---------------------------------------------
print_labels(hh$cook_fuel)
cat_map <- get_gis_map(hh$cook_fuel)
cat_map

process_popgis_tab(hh, codgeo, "cook_fuel", cat_map, "h20_cook_fuel" )

### Table H21. Main cooking place by Hamlet ---------------------------------------------
print_labels(hh$cook_place)
cat_map <- get_gis_map(hh$cook_place)
cat_map

process_popgis_tab(hh, codgeo, "cook_place", cat_map, "h21_cook_place" )


### Table H22. Main toilet facility by Hamlet ---------------------------------------------
print_labels(hh$toilet)
cat_map <- get_gis_map(hh$toilet)
cat_map

process_popgis_tab(hh, codgeo, "toilet", cat_map, "h22_toilet" )

### Table H22a. Household with shared toilet facility by Hamlet ---------------------------------------------
print_labels(hh$share_toilet)
cat_map <- get_gis_map(hh$share_toilet)
cat_map

process_popgis_tab(hh, codgeo, "share_toilet", cat_map, "h22a_sh_toilet" )

### Table H23. Main waste disposal by Hamlet ---------------------------------------------
print_labels(hh$waste_disp)
cat_map <- get_gis_map(hh$waste_disp)
cat_map

process_popgis_tab(hh, codgeo, "waste_disp", cat_map, "h23_waste_disp" ) 

### Table H24. Household appliances by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "hhld_appliance") %>% print()

process_multiselect_popgis(hh, codgeo, "hhld_appliance", var_map, "h24_appliance")


### Table H25. Household with solar panel by Hamlet ---------------------------------------------
print_labels(hh$solar)
cat_map <- get_gis_map(hh$solar)
cat_map

process_popgis_tab(hh, codgeo, "solar", cat_map, "h25_solar" ) 

### Table H26. Household with aircon by Hamlet ---------------------------------------------
print_labels(hh$aircon)
cat_map <- get_gis_map(hh$aircon)
cat_map

process_popgis_tab(hh, codgeo, "aircon", cat_map, "h26_aircon" ) 

### Table H27. Household transport items by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "transport") %>% print()

process_multiselect_popgis(hh, codgeo, "transport", var_map, "h27_transport")

### Table H28. Household with internet by Hamlet ---------------------------------------------
print_labels(hh$internet)
cat_map <- get_gis_map(hh$internet)
cat_map

process_popgis_tab(hh, codgeo, "internet", cat_map, "h28_internet" ) 


### Table H29. Household by agriculture activity by Hamlet ---------------------------------------------
print_labels(hh$agriculture)
cat_map <- get_gis_map(hh$agriculture)
cat_map

process_popgis_tab(hh, codgeo, "agriculture", cat_map, "h29_agriculture" ) 

### Table H30. Type if crop and land by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "land_crop") %>% print()
var_map[2] <- "crop_othpl"

process_multiselect_popgis(hh, codgeo, "land_crop", var_map, "h30_landcrop")

### Table H31. Household own crops by Hamlet ---------------------------------------------
print_labels(hh$own_crops)
cat_map <- get_gis_map(hh$own_crops)
cat_map

process_popgis_tab(hh, codgeo, "own_crops", cat_map, "h31_own_crops" ) 

### Table H32. Household own fruits by Hamlet ---------------------------------------------
print_labels(hh$own_fruits)
cat_map <- get_gis_map(hh$own_fruits)
cat_map

process_popgis_tab(hh, codgeo, "own_fruits", cat_map, "h32_own_fruits" ) 

### Table H33. Type livestock by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "livestock") %>% print()

process_multiselect_popgis(hh, codgeo, "livestock", var_map, "h33_livestock")


### Table H34. Type aquaculture by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "aquaculture") %>% print()

process_multiselect_popgis(hh, codgeo, "aquaculture", var_map, "h34_aquaculture")

### Table H35. Household by purpose for agriculture by Hamlet ---------------------------------------------
print_labels(hh$agr_purpose)
cat_map <- get_gis_map(hh$agr_purpose)
cat_map

process_popgis_tab(hh, codgeo, "agr_purpose", cat_map, "h35_agr_purpose" ) 

### Table H36. Household Forst or wooded land by Hamlet ---------------------------------------------
print_labels(hh$forest)
cat_map <- get_gis_map(hh$forest)
cat_map

process_popgis_tab(hh, codgeo, "forest", cat_map, "h36_forest" ) 

### Table H36. Household Forest or wooded land by Hamlet ---------------------------------------------
print_labels(hh$forest)
cat_map <- get_gis_map(hh$forest)
cat_map

process_popgis_tab(hh, codgeo, "forest", cat_map, "h36_forest" ) 

### Table H37. Household member fishing by Hamlet ---------------------------------------------
print_labels(hh$fishing)
cat_map <- get_gis_map(hh$fishing)
cat_map

process_popgis_tab(hh, codgeo, "fishing", cat_map, "h37_fishing" ) 

### Table H38. Household purpose for fishing by Hamlet ---------------------------------------------
print_labels(hh$fish_purpose)
cat_map <- get_gis_map(hh$fish_purpose)
cat_map

process_popgis_tab(hh, codgeo, "fish_purpose", cat_map, "h38_fish_purpose" ) 

### Table H39. Type of fishing method by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "fish_method") %>% print()

process_multiselect_popgis(hh, codgeo, "fish_method", var_map, "h39_fish_method")

### Table H40. Type of fishing mlocation by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "fish_location") %>% print()

process_multiselect_popgis(hh, codgeo, "fish_location", var_map, "h40_fish_location")

### Table H41. Type of fish  by Hamlet ---------------------------------------------
var_map <- get_multiselect_map(hh, "fish_type") %>% print()

process_multiselect_popgis(hh, codgeo, "fish_type", var_map, "h41_fish_type")




# 3. PERSON DATASET =============================================================

## 3.1 Prepare dataset ------ 
# Keep Private households
# Keep private and occuppied hhs
pop <- pop %>% 
  rename(hid = 	hamlet) %>% 
  filter(dwell_type %in% c(1,2)) %>% # dwell_type !=NA implies occupied
  mutate(hid = zap_labels(hid))

## 2.2 Create Codgeo table master EA list ----
# We will sort EA code out later when we get the originals
# We extract it from the Hamlet layer that we will connecting later on in PopGIS
ham_geo <- vect("C:/Users/luisr/SPC/SDD GIS - Documents/PopGIS/PopGIS3/GEO Layers/PLW/PLW_2025PHC_HID_4326.shp")
# Extract the data frame from the SpatVector first, then build the backbone
codgeo <- as.data.frame(ham_geo) %>%
  as_tibble() %>% 
  select(hid_2025) %>% 
  rename(hid = hid_2025) %>% 
  mutate(hid = as.numeric(hid)) %>%
  arrange(hid) |> 
  # Fulfilling your note: Include the total pop
  left_join(
    pop %>%
      group_by(hid) %>%
      summarise(t_pop = n(),
                m_pop = sum(sex == 1, na.rm = TRUE),   
                f_pop = sum(sex == 2, na.rm = TRUE),
                .groups = "drop"),
    by = "hid"
  ) %>%
  # Ensure any empty hamlets show 0 households instead of NA
  mutate(across(c(t_pop, m_pop, f_pop), ~replace_na(., 0)))

message("backbone layer contains ", nrow(codgeo), " hamlets")

sum(codgeo$totpop)



variables <- names(pop)
labels_list <- list()

for (variable in variables) {
  if (variable %in% names(pop)) {
    labels_list[[variable]] <- get_catlab(pop[[variable]])
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

## 3.2 Define functions -------

get_pop_map <- function(vec, var_name = "unknown") {
  # 1. Extract labels from Stata/Server metadata
  labs <- attr(vec, "labels")
  if (is.null(labs)) {
    warning(paste("No labels found for", var_name))
    return(NULL)
  }
  
  codes <- as.character(labs)
  raw_names <- names(labs)
  
  # 2. Clean the names with the split-and-truncate logic
  clean_names <- map_chr(raw_names, function(x) {
    # Split by : or ; and take the last part (the category)
    clean <- str_split(x, "[:;]")[[1]] %>% last() %>% str_trim()
    
    clean %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_replace_all("\\s+", "_") %>%
      str_sub(1, 8) %>%        # Max 8 chars to allow for "t_", "m_", "f_"
      str_replace("_$", "")
  })
  
  # 3. Handle Duplicates
  if (any(duplicated(clean_names))) {
    message(paste("! Collision in", var_name, "- resolving duplicates..."))
    clean_names <- make.unique(clean_names, sep = "_") %>% str_sub(1, 10)
  }
  
  return(setNames(clean_names, codes))
}

process_pop_sex_tab <- function(data, backbone, var_name, file_name) {
  
  # 1. Generate the base GIS map
  base_map <- get_pop_map(data[[var_name]], var_name)
  if (is.null(base_map)) stop(paste("Metadata missing for:", var_name))
  
  # 2. Prepare long data
  tab_long <- data %>%
    filter(!is.na(!!sym(var_name)), !is.na(sex)) %>%
    mutate(sex_prefix = case_when(sex == 1 ~ "m", sex == 2 ~ "f"))
  
  # 3. Aggregation
  counts_all <- bind_rows(
    tab_long %>% count(hid, sex_prefix, !!sym(var_name)) %>% rename(prefix = sex_prefix),
    tab_long %>% count(hid, !!sym(var_name)) %>% mutate(prefix = "t")
  ) %>%
    mutate(col_key = paste0(prefix, "_", !!sym(var_name)))
  
  # 4. Pivot Wide
  p_table <- counts_all %>%
    select(hid, col_key, n) %>%
    pivot_wider(names_from = col_key, values_from = n, values_fill = 0)
  
  # 5. CREATE AND APPLY MAP IMMEDIATELY
  # This ensures p_table has the real names (t_kiribati, etc.) before we proceed
  final_rename_map <- c(
    setNames(paste0("t_", base_map), paste0("t_", names(base_map))),
    setNames(paste0("m_", base_map), paste0("m_", names(base_map))),
    setNames(paste0("f_", base_map), paste0("f_", names(base_map)))
  )
  
  # Apply renaming here
  p_table <- p_table %>%
    rename_with(~final_rename_map[.], .cols = any_of(names(final_rename_map)))
  
  # 6. Final Assembly with Backbone
  final_df <- backbone %>%
    left_join(p_table, by = "hid") %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    # Calculate Totals using the NEW names
    mutate(
      t_pop = rowSums(select(., starts_with("t_")), na.rm = TRUE),
      m_pop = rowSums(select(., starts_with("m_")), na.rm = TRUE),
      f_pop = rowSums(select(., starts_with("f_")), na.rm = TRUE)
    ) %>%
    relocate(t_pop, m_pop, f_pop, .after = hid)
  
  # --- VERIFICATION BLOCK (Using assigned names) ---
  # This sums every numeric column in the final table
  cat("\n==========================================\n")
  cat("VERIFICATION TOTALS FOR:", file_name, "\n")
  cat("==========================================\n")
  
  check_totals <- final_df %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    pivot_longer(everything(), names_to = "Indicator", values_to = "National_Total")
  
  print(as.data.frame(check_totals))
  cat("==========================================\n\n")
  
  # 7. Export
  write.xlsx(final_df, 
             file = paste0(tab, file_name, ".xlsx"), 
             sheetName = "hid", 
             rowNames = FALSE, 
             overwrite = TRUE)
}


## 3.3 POPPULATION TABLES ------------------------------------------------------
### Table P1. Population by 5–year age group by sex ----
get_pop_map(pop$age_grp5) %>% print()
print_labels(pop$age_grp5)

process_pop_sex_tab(
  data = pop, 
  backbone = codgeo, 
  var_name = "age_grp5", 
  file_name = "p1_age_5yrbands"
)

### Table P2. Population by Ethnic Group and by Sex ----

get_pop_map(pop$ethnicity) %>% print()

process_pop_sex_tab(
  data = pop, 
  backbone = codgeo, 
  var_name = "ethnicity", 
  file_name = "p2_ethnicity"
)

### Table P3. Population by citizenship and by Sex ----- 

cat_map <- get_pop_map(pop$citizenship) %>% print()

process_pop_sex_tab(
  data = pop, 
  backbone = codgeo, 
  var_name = "citizenship", 
  file_name = "p3_citizen"
)

### Table P4. Population by relationship with head of the household and by Sex -----  
cat_map <- get_pop_map(pop$relat) %>% print()

process_pop_sex_tab(
  data = pop, 
  backbone = codgeo, 
  var_name = "relat", 
  file_name = "p4_relat"
)



### Table P5. Population 15 years and Over by Sex and Marital Status ----
# define population 15+
pop15 <- pop %>% 
  filter(age > 14)

get_pop_map(pop15$mstatus) %>% print()

process_pop_sex_tab(
  data = pop15, 
  backbone = codgeo, 
  var_name = "mstatus", 
  file_name = "p4_marital_status"
)


### Table P6a. Population by Individual classified as disabled (WG statistic, Min. 1/6 of Q ==3 or 4) ----
pop5 <- pop %>%
  filter(age > 4)

# 
# process_pop_sex_tab(
#   data = pop5, 
#   backbone = codgeo, 
#   var_name = "wg_disabled", 
#   file_name = "p6a_wg_disabled"
# )

### Table P6a. Population 5 years old and over by Difficulty in Seeing and by Sex ----
get_pop_map(pop$seeing) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "seeing", 
  file_name = "p6a_seeing"
)

### Table P6b. Population 5 years old and over by Difficulty in Hearing and by Sex ----
get_pop_map(pop$hearing) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "hearing", 
  file_name = "p6b_hearing"
)

### Table P6c. Population 5 years old and over by Difficulty in Mobility and by Sex ----
get_pop_map(pop$walking) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "walking", 
  file_name = "p6c_mobility"
)

### Table P6d. Population 5 years old and over by Difficulty in Remembering and by Sex ----
get_pop_map(pop$remembering) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "remembering", 
  file_name = "p6d_memory"
)

### Table P6e. Population 5 years old and over by Difficulty in Selfcare and by Sex ----
get_pop_map(pop$selfcare) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "selfcare", 
  file_name = "p6e_sefcare"
)

### Table P6f. Population 5 years old and over by Difficulty in Communication and by Sex ----
get_pop_map(pop$communication) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "communication", 
  file_name = "p6f_communication"
)

### Table P7a. Population 5 years old and over by Some Difficulty (cut-off) ----
get_pop_map(pop$some_disab) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "some_disab", 
  file_name = "p7a_some_disab"
)


