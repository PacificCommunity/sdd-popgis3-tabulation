### Tabulation for Marshall Islands 2021 PHC ###
# Luis de la Rua ## Feb 2026 #

# SETTINGS ====================================================================

# Clean workspace
rm(list = ls())
gc()

source("setup.R")

getwd()
# Raw data directory
dd <- "C:/Users/luisr/SPC/SDD GIS - Documents/PopGIS/PopGIS3/Data/RMI/2021 PHC/"
tab <- "C:/Users/luisr/SPC/SDD GIS - Documents/PopGIS/PopGIS3/Data/RMI/2021 PHC/tables/"
nada <- "C:/Users/luisr/OneDrive - SPC/NADA/Republic of Marshall Islands/SPC_MHL_2021_PHC_v01_M/Data/Original/Final edit"

# 1.IMPORT AND PREPARE CENSUS DATASETS ========================================
## 1.1 Import Stata databases ----

stata_files <- list.files (nada,
                           pattern = "*.dta", full.names = T)
stata_files

## 1.2 Import datasets we are going to use ----
hous <- read_stata(stata_files[2])
pop <- read_stata(stata_files[3]) 

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

# 2. HOUSING DATASET ==========================================================
## 2.1 Filter questionnaires we're keeping for the tabulation process ----
# Keep private and occuppied hhs
hh <- hous %>% 
  rename(aid = Atoll_island) %>% 
  filter(hhtype == 1, occupancyStatus == 1, refusal == 0) %>%
  mutate(aid = zap_labels(aid))

## 2.2 Create Codgeo table master EA list ----
# We will sort EA code out later when we get the originals
codgeo <- hh %>%
  group_by(aid) %>%
  summarise(cnt = n(), .groups = "drop") %>%
  select(aid)

## 2.3 TABLES =============================

# Function to automate the PopGIS table generation =============================
# Define the function based on your exact workflow
process_popgis_tab <- function(data, backbone, var_name, rename_map, file_name) {
  
  # 1. Tabulate and Pivot
  tab_df <- data %>%
    filter(!is.na(!!sym(var_name))) %>%
    count(aid, !!sym(var_name)) %>%
    # Convert codes to character so they match the case_when keys
    mutate(!!sym(var_name) := as.character(zap_labels(!!sym(var_name)))) %>%
    pivot_wider(names_from = !!sym(var_name), values_from = n, values_fill = 0)
  
  # 2. Rename columns using your mapping
  tab_df <- tab_df %>%
    rename_with(~recode(., !!!rename_map), .cols = -aid)
  
  # 3. Join with Codgeo backbone (ensure 77 rows)
  final <- backbone %>%
    left_join(tab_df, by = "aid") %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    # Calculate Total HH
    mutate(tot_hh = rowSums(select(., -aid), na.rm = TRUE)) %>%
    relocate(tot_hh, .after = aid)
  
  # 4. Validation Checks
  if (sum(is.na(final)) != 0) stop(paste("NAS WRONG in", file_name))
  if (nrow(final) != 77) warning(paste("Row count is", nrow(final), "for", file_name))
  
  # 5. Summary Totals for console
  totals <- final %>%
    select(-aid) %>% 
    summarise(across(everything(), sum))
  
  cat("\n--- SUMMARY TOTALS:", file_name, "---\n")
  print(totals)
  
  # 6. Export
  write.xlsx(final, paste0(tab, file_name, ".xlsx"), sheetName = "aid")
}

## Automate categories map production --------
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

### Table H1. Dwelling Type by EA ----------------------------------------------

print_labels(hh$i1_building)
cat_map <- get_gis_map(hh$i1_building)
cat_map
cat_map <- c(    "1" = "detach",
                "2" = "mult",
                "3" = "attach",
                "4" = "apart",
                "5" = "shop",
                "6" = "lodge",
                "99" = "oth",
                "100" = "boat")

process_popgis_tab(hh, codgeo, "i1_building", cat_map, "h1_lq_type" )

### Table H2. Housing tenure by EA ---------------------------------------------
print_labels(hh$i2_type)
cat_map <- get_gis_map(hh$i2_type)
cat_map
cat_map<- c(    "1" = "loan",
                "2" = "free",
                "3" = "rented",
                "4" = "pub_rent",
                "5" = "no_pay",
                "6" = "oarr_owner",
                "99" = "other")

process_popgis_tab(hh, codgeo, "i2_type", cat_map, "i2_type" )

### Table H3. Floor type by EA ---------------------------------------------
print_labels(hh$i3_floor)
cat_map <- get_gis_map(hh$i3_floor)
cat_map

process_popgis_tab(hh, codgeo, "i3_floor", cat_map, "i3_floor" )

### Table H4. Roof type by EA ---------------------------------------------
print_labels(hh$i4_roof)
cat_map <- get_gis_map(hh$i4_roof)
cat_map

process_popgis_tab(hh, codgeo, "i4_roof", cat_map, "i4_roof" )

### Table H5. Walls type by EA ---------------------------------------------
print_labels(hh$i5_material_walls)
cat_map <- get_gis_map(hh$i5_material_walls)
cat_map

process_popgis_tab(hh, codgeo, "i5_material_walls", cat_map, "i5_material_walls" )


### Table H6. Walls type by EA ---------------------------------------------
print_labels(hh$i5_material_walls)
cat_map <- get_gis_map(hh$i5_material_walls)
cat_map

process_popgis_tab(hh, codgeo, "i5_material_walls", cat_map, "i5_material_walls" )
i5_material_walls