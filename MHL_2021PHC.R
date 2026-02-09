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
  summarise(total_hh = n(), .groups = "drop")
  

## 2.3 Function to automate the PopGIS table generation =============================
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
    mutate(across(everything(), ~replace_na(., 0))) 
  
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
    group_by(aid) %>%
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
    left_join(tab_df, by = "aid") %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    relocate(total_hh, .after = aid)
  
  # Export
  write.xlsx(final, paste0(tab, file_name, ".xlsx"), sheetName = "aid")
  cat("Table", file_name, "tabulated successfully.\n")
}


## 2.6 TABLES =============================

### Table H1. Dwelling Type by Atoll ----------------------------------------------

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

process_popgis_tab(hh, codgeo, "i1_building", cat_map, "i1_building" )

### Table H2. Housing tenure by Atoll ---------------------------------------------
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

### Table H3. Floor type by Atoll ---------------------------------------------
print_labels(hh$i3_floor)
cat_map <- get_gis_map(hh$i3_floor)
cat_map

process_popgis_tab(hh, codgeo, "i3_floor", cat_map, "i3_floor" )

### Table H4. Roof type by Atoll ---------------------------------------------
print_labels(hh$i4_roof)
cat_map <- get_gis_map(hh$i4_roof)
cat_map

process_popgis_tab(hh, codgeo, "i4_roof", cat_map, "i4_roof" )

### Table H5. Walls type by Atoll ---------------------------------------------
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

### Table H6a. HH by Improved Sources of Drinking water by Atoll ------------------
print_labels(hh$waterimpr)
cat_map <- get_gis_map(hh$waterimpr)
cat_map

process_popgis_tab(hh, codgeo, "waterimpr", cat_map, "i6a_waterimpr" )

### Table H7. Main Source of cooking water by Atoll ---------------------------------------------
var_map <- get_multiselect_map(hh, "i7_source_water") %>% print()
var_map["1"] <- "pub_pip_in"  
var_map["2"] <- "pub_pip_ot"  
var_map["7"] <- "own_tnk_in"
var_map["8"] <- "own_tnk_ot"
print(var_map)

process_multiselect_popgis(hh, codgeo, "i7_source_water", var_map, "i7_source_water")

### Table H7a. HH by Cooking Water On Premises by Atoll ------------------
print_labels(hh$waterprem)
cat_map <- get_gis_map(hh$waterprem)
cat_map

process_popgis_tab(hh, codgeo, "waterprem", cat_map, "i7a_waterprem" )

### Table H8. HH by Toilet Facilities by Atoll ------------------
print_labels(hh$i8_toilet_facility)
cat_map <- get_gis_map(hh$i8_toilet_facility)
cat_map
cat_map["1"] <- "flush_sew"  
cat_map["2"] <- "flush_sept"
cat_map["5"] <- "pl_slab"  
cat_map["6"] <- "pl_open"  
print(cat_map)
process_popgis_tab(hh, codgeo, "i8_toilet_facility", cat_map, "i8_toilet_facility" )

### Table H8a. HH by Improved Sanitation by Atoll ------------------
print_labels(hh$sanitationimpr)
cat_map <- get_gis_map(hh$sanitationimpr)
cat_map

process_popgis_tab(hh, codgeo, "sanitationimpr", cat_map, "i8a_sanitationimpr" )

### Table H8b. HH by Shared Toilet by Atoll ------------------
print_labels(hh$i8b_share_toilet)
cat_map <- get_gis_map(hh$i8b_share_toilet)
cat_map

process_popgis_tab(hh, codgeo, "i8b_share_toilet", cat_map, "i8b_share_toilet" )

### Table H9. Main cooking fuel by Atoll ---------------------------------------------
var_map <- get_multiselect_map(hh, "i9_cook_fuel") %>% print()

process_multiselect_popgis(hh, codgeo, "i9_cook_fuel", var_map, "i9_cook_fuel")

### Table H10. Main source of electricity by Atoll ---------------------------------------------
var_map <- get_multiselect_map(hh, "i10_electricity") %>% print()

process_multiselect_popgis(hh, codgeo, "i10_electricity", var_map, "i10_electricity")

### Table H11. Main source of lighting by Atoll ---------------------------------------------
var_map <- get_multiselect_map(hh, "i11_lighting") %>% print()

process_multiselect_popgis(hh, codgeo, "i11_lighting", var_map, "i11_lighting")

### Table H12. Type of Waste Disposal by Atoll ---------------------------------------------
var_map <- get_multiselect_map(hh, "i12_waste_disp") %>% print()
var_map["1"] <- "pers_pub"  
var_map["2"] <- "pers_yrslf" 
print(var_map)
process_multiselect_popgis(hh, codgeo, "i12_waste_disp", var_map, "i12_waste_disp")

### Table H13. Household goods by Atoll ------------------------------------------
var_map <- get_multiselect_map(hh, "i13_hhld_goods") %>% print()

process_multiselect_popgis(hh, codgeo, "i13_hhld_goods", var_map, "i13_hhld_goods")

### Table H13g. Household by Access to Internet by Atoll --------------------------------
print_labels(hh$i13g_internet)
cat_map <- get_gis_map(hh$i13g_internet)
cat_map

process_popgis_tab(hh, codgeo, "i13g_internet", cat_map, "i13g_internet" )


### Table H14. Household by Main source of income by Atoll --------------------------------
var_map <- get_multiselect_map(hh, "i15a_hh_income") %>% print()
var_map["4"] <- "rent_ll"  
var_map["5"] <- "rent_hl" 
print(var_map)
process_multiselect_popgis(hh, codgeo, "i15a_hh_income", var_map, "i15a_hh_income")

### Table H17a. HH by environment problem by Atoll -------------------------------
var_map <- get_multiselect_map(hh, "i17a_environ_problem") %>% print()

process_multiselect_popgis(hh, codgeo, "i17a_environ_problem", var_map, "i17a_environ_problem")

### Table H17b. Environment problem limit of income by Atoll -------------------------------
print_labels(hh$I17b_limit_income)
cat_map <- get_gis_map(hh$I17b_limit_income)
cat_map

process_popgis_tab(hh, codgeo, "I17b_limit_income", cat_map, "I17b_limit_income" )

### Table H17c. Environment problem cause to move by Atoll -------------------------------
print_labels(hh$I17c_cause_2move)
cat_map <- get_gis_map(hh$I17c_cause_2move)
cat_map

process_popgis_tab(hh, codgeo, "I17c_cause_2move", cat_map, "I17c_cause_2move" )

### Table H17d. Community affected  in the last 10 years by Atoll -------------------------------
var_map <- get_multiselect_map(hh, "I17d_affected_10yrs") %>% print()

process_multiselect_popgis(hh, codgeo, "I17d_affected_10yrs", var_map, "I17d_affected_10yrs")


### Table H18. HH by Agriculture activity by Atoll -------------------------------
var_map <- get_multiselect_map(hh, "j1_agricuture") %>% print()

process_multiselect_popgis(hh, codgeo, "j1_agricuture", var_map, "j1_agricuture")

### Table H18a. Main Purpose Growing crops by Atoll ------------------------------------
print_labels(hh$j1a_growcrops)
cat_map <- get_gis_map(hh$j1a_growcrops)
cat_map

process_popgis_tab(hh, codgeo, "j1a_growcrops", cat_map, "j1a_growcrops" )

### Table H18b. Main Purpose fishing by Atoll ------------------------------------
print_labels(hh$j1b_fishing)
cat_map <- get_gis_map(hh$j1b_fishing)
cat_map

process_popgis_tab(hh, codgeo, "j1b_fishing", cat_map, "j1b_fishing" )

### Table H18c. Main Purpose Raising livestock by Atoll ------------------------------------
print_labels(hh$j1c_livestock)
cat_map <- get_gis_map(hh$j1c_livestock)
cat_map

process_popgis_tab(hh, codgeo, "j1c_livestock", cat_map, "j1c_livestock" )

### Table H18d. Main Purpose Freshwater aquaculture by Atoll ------------------------------------
print_labels(hh$j1d_freshwater_aqua)
cat_map <- get_gis_map(hh$j1d_freshwater_aqua)
cat_map

process_popgis_tab(hh, codgeo, "j1d_freshwater_aqua", cat_map, "j1d_freshwater_aqua")


### Table H18e. Main Purpose Marine aquaculture by Atoll ------------------------------------
print_labels(hh$j1e_marine_aqua)
cat_map <- get_gis_map(hh$j1e_marine_aqua)
cat_map

process_popgis_tab(hh, codgeo, "j1e_marine_aqua", cat_map, "j1e_marine_aqua")

### Table H18f. Main Purpose Forestry by Atoll ------------------------------------
print_labels(hh$j1f_forestry)
cat_map <- get_gis_map(hh$j1f_forestry)
cat_map

process_popgis_tab(hh, codgeo, "j1f_forestry", cat_map, "j1f_forestry")

### Table H18g. Main Purpose Handicraft by Atoll ------------------------------------
print_labels(hh$j1g_handicraft)
cat_map <- get_gis_map(hh$j1g_handicraft)
cat_map

process_popgis_tab(hh, codgeo, "j1g_handicraft", cat_map, "j1g_handicraft")

### Table H18h. Main Purpose Hunting by Atoll ------------------------------------
print_labels(hh$j1g_hunting)
cat_map <- get_gis_map(hh$j1g_hunting)
cat_map

process_popgis_tab(hh, codgeo, "j1g_hunting", cat_map, "j1g_hunting")


# 3. PERSON DATASET =============================================================

## 3.1 Prepare dataset ------ 
# Keep Private households
pop <- pop %>% 
  filter(hhtype == 1) %>% 
  rename(aid = Atoll_island)

print_labels(pop$hhtype)


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
    filter(!is.na(!!sym(var_name)), !is.na(r2_sex)) %>%
    mutate(sex_prefix = case_when(r2_sex == 1 ~ "m", r2_sex == 2 ~ "f"))
  
  # 3. Aggregation
  counts_all <- bind_rows(
    tab_long %>% count(aid, sex_prefix, !!sym(var_name)) %>% rename(prefix = sex_prefix),
    tab_long %>% count(aid, !!sym(var_name)) %>% mutate(prefix = "t")
  ) %>%
    mutate(col_key = paste0(prefix, "_", !!sym(var_name)))
  
  # 4. Pivot Wide
  p_table <- counts_all %>%
    select(aid, col_key, n) %>%
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
    left_join(p_table, by = "aid") %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    # Calculate Totals using the NEW names
    mutate(
      t_pop = rowSums(select(., starts_with("t_")), na.rm = TRUE),
      m_pop = rowSums(select(., starts_with("m_")), na.rm = TRUE),
      f_pop = rowSums(select(., starts_with("f_")), na.rm = TRUE)
    ) %>%
    relocate(t_pop, m_pop, f_pop, .after = aid)
  
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
             sheetName = "aid", 
             rowNames = FALSE, 
             overwrite = TRUE)
}


## 3.3 POPPULATION TABLES ------------------------------------------------------
### Table P1. Population by 5–year age group by sex ----
get_pop_map(pop$ilo_age_5yrbands) %>% print()
print_labels(pop$ilo_age_5yrbands)

process_pop_sex_tab(
  data = pop, 
  backbone = codgeo, 
  var_name = "ilo_age_5yrbands", 
  file_name = "p1_ilo_age_5yrbands"
)

### Table P2. Population by Ethnic Group and by Sex ----

get_pop_map(pop$c1_ethnic) %>% print()

process_pop_sex_tab(
  data = pop, 
  backbone = codgeo, 
  var_name = "c1_ethnic", 
  file_name = "p2_ethnic"
)

### Table P3. Population by citizenship and by Sex ----- 

cat_map <- get_pop_map(pop$c1_ethnic) %>% print()

process_pop_sex_tab(
  data = pop, 
  backbone = codgeo, 
  var_name = "c3_citizen", 
  file_name = "p3_citizen"
)


### Table P4. Population 15 years and Over by Sex and Marital Status ----
# define population 15+
pop15 <- pop %>% 
  filter(r3_age > 14)


get_pop_map(pop$c4_marital_status) %>% print()

process_pop_sex_tab(
  data = pop15, 
  backbone = codgeo, 
  var_name = "c4_marital_status", 
  file_name = "p4_marital_status"
)

### Table P5.  Population by Relationship to Head of Household and by Sex -----

get_pop_map(pop$r4_relat) %>% print()

process_pop_sex_tab(
  data = pop, 
  backbone = codgeo, 
  var_name = "r4_relat", 
  file_name = "p5_relat"
)

### Table P6a. Population by Individual classified as disbaled (WG statistic, Min. 1/6 of Q ==3 or 4) ----
pop5 <- pop %>% 
  filter(r3_age > 4)
get_pop_map(pop$d1_seeing) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "wg_disabled", 
  file_name = "p6a_wg_disabled"
)

### Table 6b. Population 5 years old and over by Difficulty in Seeing and by Sex ----
get_pop_map(pop$d1_seeing) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "d1_seeing", 
  file_name = "p6b_seeing"
)

### Table 6c. Population 5 years old and over by Difficulty in Hearing and by Sex ----
get_pop_map(pop$d2_hearing) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "d2_hearing", 
  file_name = "p6c_hearing"
)

### Table 6d. Population 5 years old and over by Difficulty in Mobility and by Sex ----
get_pop_map(pop$d3_mobility) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "d3_mobility", 
  file_name = "p6d_mobility"
)

### Table 6e. Population 5 years old and over by Difficulty in Remembering and by Sex ----
get_pop_map(pop$d4_memory) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "d4_memory", 
  file_name = "p6e_memory"
)

### Table 6f. Population 5 years old and over by Difficulty in Selfcare and by Sex ----
get_pop_map(pop$d5_sefcare) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "d5_sefcare", 
  file_name = "p6f_sefcare"
)

### Table 6g. Population 5 years old and over by Difficulty in Communication and by Sex ----
get_pop_map(pop$d6_communication) %>% print()

process_pop_sex_tab(
  data = pop5, 
  backbone = codgeo, 
  var_name = "d6_communication", 
  file_name = "p6g_communication"
)

### Table 7. Population by Urban/Rural by 5–year age group by internet access ---- 
pop10 <- pop %>% 
  filter(r3_age > 9)

### Table 7. Population by Urban/Rural by 5–year age group by internet access ---- 
pop10 <- pop %>% 
  filter(r3_age > 9)

get_pop_map(pop$h1_internet_access) %>% print()

process_pop_sex_tab(
  data = pop10, 
  backbone = codgeo, 
  var_name = "h1_internet_access", 
  file_name = "p7_internet"
)

### Table 7a. Population by place of internet access ---- 

get_pop_map(pop$h2_location) %>% print()

process_pop_sex_tab(
  data = pop10, 
  backbone = codgeo, 
  var_name = "h2_location", 
  file_name = "p7a_int_place"
)

### Table 8. Population by own mobile phone ---- 

get_pop_map(pop$h3_mobile_phone) %>% print()

process_pop_sex_tab(
  data = pop10, 
  backbone = codgeo, 
  var_name = "h3_mobile_phone", 
  file_name = "p8_mobile_phone"
)

### Table 9. Population by main activity ---- 

get_pop_map(pop$lf1) %>% print()

process_pop_sex_tab(
  data = pop15, 
  backbone = codgeo, 
  var_name = "lf1", 
  file_name = "p9_main_act"
)

### Table 10. Population by main activity ---- 

get_pop_map(pop$ilo_lfs) %>% print()

process_pop_sex_tab(
  data = pop15, 
  backbone = codgeo, 
  var_name = "ilo_lfs", 
  file_name = "p10_lfstatus"
)

### Table 11. Population by Degree of Labour market attachment by Atol ---- 

get_pop_map(pop$ilo_olf_dlma) %>% print()

process_pop_sex_tab(
  data = pop15, 
  backbone = codgeo, 
  var_name = "ilo_olf_dlma", 
  file_name = "p11_olf_dlma"
)

### Table 12. Population by Status in employment (ICSE 93) – Main job by Atol ---- 

get_pop_map(pop$ilo_job1_ste_icse93) %>% print()

process_pop_sex_tab(
  data = pop15, 
  backbone = codgeo, 
  var_name = "ilo_job1_ste_icse93", 
  file_name = "p12_ilo_job1_ste"
)
