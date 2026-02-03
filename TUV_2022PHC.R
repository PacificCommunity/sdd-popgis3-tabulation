### Tabulation for Tuvalu 2022 PHC ###
# Luis de la Rua ## May 2024 #

# SETTINGS ====================================================================

source("setup.R")

getwd()
# Raw data directory
dd <- "C:/Users/luisr/SPC/SDD GIS - Documents/PopGIS/PopGIS3/Data/Tuvalu/2022dataset/"
tab <- "C:/Users/luisr/SPC/SDD GIS - Documents/PopGIS/PopGIS3/Data/Tuvalu/2022dataset/tables"


# 1.IMPORT AND PREPARE CENSUS DATASETS ========================================
# 1.1 Import Stata databases ----

stata_files <- list.files (paste0(dd,"StataV2"),
                           pattern = "*.dta", full.names = T)
stata_files

# 1.2 Import datasets we are going to use ----
hous <- read_stata(stata_files[15])
pop <- read_stata(stata_files[16]) # ask Toga what dataset we should use either ILO or the other

# 1.3 Generate full EA codes table to make sure that all our tables have the same size ----
eatable <- as.data.frame(table(hous$ea)) %>% 
  rename(ea = Var1) %>% 
  select(ea)

# 1.4 Get labels of the variables ----
view(get_labels(hous))
view(get_labels(pop))

# 1.5 Get variable labels ----


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

# 2. HOUSING DATASET ==========================================================
# Keep Private and Occupied households
hous1 <- hous %>% 
  filter(hh_type == 1 & occupancy == 1)

get_catlab(hous1$hh_type)
table(hous1$hh_type)

# Table H1. Total Number of Private Households Type of Living Quarters ----
df <- hous1 %>% 
  group_by(ea, lquarters) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = lquarters, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) %>% # Exclude 'ea' column and select all other columns
  rename(ea2022 = ea)
# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("h_detach" , "h_attach" , "buil_app" , "buil_hh" , "att_shop")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea2022) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h1_lquarter_22.xlsx"))

# Table H2. Total Number of Private Households by Living Tenure ----
df <- hous1 %>% 
  group_by(ea, tenure) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = tenure, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("own" , "rent" , "empl" , "fam")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h2_tenure_22.xlsx"))


# Table H3. Total Number of Private Households by Rental Arrangement ----
df <- hous1 %>% 
  group_by(ea, rent) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = rent, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("gov" , "kaup" , "coop_pp" , "priv_ind" , "org" )
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h3_rent_22.xlsx"))

# Table H4. Total Number of Private Households by House Construction ----
df <- hous1 %>% 
  group_by(ea, main_construct) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = main_construct, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("conc" , "timb" , "local_mat" , "comb" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h4_mconst_22.xlsx"))

# Table H6. Total Number of Private Households by Material of Floor ----
df <- hous1 %>% 
  group_by(ea, floor) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = floor, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("sand" , "gravel" , "wood" , "coco" , "parquet" , "tiles" , "cement" , "carpet" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h6_floor_22.xlsx"))

# Table H7. Total Number of Private Households by Material of Roof ----
df <- hous1 %>% 
  group_by(ea, roof) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = roof, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("coco" , "pand" , "wood" , "metal" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h7_roof_22.xlsx"))

# Table H8. Total Number of Private Households by Type of Metal Roof ----
df <- hous1 %>% 
  group_by(ea, roof_type) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = roof_type, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("gable" , "monop" , "flat" , "hipcon" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h8_roof_type_22.xlsx"))


# Table H9. Total Number of Private Households by Material of Wall----
df <- hous1 %>% 
  group_by(ea, wall) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = wall, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(., -ea))) 

# Get numeric column names sorted numerically
numeric_col_names <- names(df)[!names(df) %in% c("ea", "tot_hh")]
numeric_col_names <- sort(as.numeric(numeric_col_names), na.last = TRUE)
numeric_col_names <- as.character(numeric_col_names)

# Reorder columns: 'ea', 'tot_hh', and then the rest in numeric order
df <- df %>% 
    select(ea, tot_hh, all_of(numeric_col_names))

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("coco" , "lapa" , "nowall" , "plyw" , "hardflex" , "reu_wood" , "cement" , "cem_blk" , "wplank" , "pvc" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h9_wall_22.xlsx"))

# Table H10. Total Number of Private Households by Number of Rooms ----

df <- hous1 %>% 
  group_by(ea, rooms) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = rooms, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("r_1" , "r_2" , "r_3" , "r_4" , "r_5" , "r_6" , "r_7" , "r_8")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h10_rooms_22.xlsx"))

# Table H12. Total Number of Private Households by Main Water Source ----

df <- hous1 %>% 
  group_by(ea, watersource) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = watersource, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("tpiped" , "tpopout" , "tcom" , "cneig" , "ttruck" , "bwater" , "wkiosk" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h12_dwater_22.xlsx"))

# Table H13a. Total Number of Private Households by Water Safety ----

df <- hous1 %>% 
  group_by(ea, water_safe) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = water_safe, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no" , "t_dknow")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h13a_watersaf_22.xlsx"))

# Table H13b. Total Number of Private Households by Water Acceptability ----

df <- hous1 %>% 
  group_by(ea, water_accept) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = water_accept, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no" , "t_dknow")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h13b_wateracc_22.xlsx"))

# Table H13c. Total Number of Private Households by Water Sufficiency ----

df <- hous1 %>% 
  group_by(ea, sufficient_water) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = sufficient_water, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no" , "t_dknow")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h13a_watersuff_22.xlsx"))

# Table H14. Total Number of Private Households by Months of Water Shortages ----

df <- hous1 %>% 
  select(ea,starts_with("month_shortage")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("month_shortage"), sum, na.rm = TRUE)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  rename_with(~ gsub("month_shortage_", "ms", .x), starts_with("month_shortage")) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h13a_watershort_22.xlsx"))

# Table H15. Total Number of Private Households by Main Water Source for Cooking and Handwashing ----

df <- hous1 %>% 
  group_by(ea, cook_water) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = cook_water, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("tpiped" , "tpopout" , "tcom" , "cneig" , "ttruck" , "bwater" , "wkiosk" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h15_cwat_22.xlsx"))

# Table H16. Total Number of Private Households by Main Toilet Facility ----
df <- hous1 %>% 
  group_by(ea, toilet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = toilet, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(., -ea))) 

# Get numeric column names sorted numerically
numeric_col_names <- names(df)[!names(df) %in% c("ea", "tot_hh")]
numeric_col_names <- sort(as.numeric(numeric_col_names), na.last = TRUE)
numeric_col_names <- as.character(numeric_col_names)

# Reorder columns: 'ea', 'tot_hh', and then the rest in numeric order
df <- df %>% 
  select(ea, tot_hh, all_of(numeric_col_names))

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("fsept" , "fpit" , "felse" , "fdknow" , "vip" , "pitlat_sl" , "pitlat_op" , "pitlat_oc" , "comp" , "nofac" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h16_toilet_22.xlsx"))

# Table H17. Total Number of Private Households by Toilet Sharing ----

df <- hous1 %>% 
  group_by(ea, share_toilet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = share_toilet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h17_shtoilet_22.xlsx"))

# Table H18. Total Number of Private Households by Main Source of Cooking Energy ----

df <- hous1 %>% 
  group_by(ea, cook) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = cook, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("elec" , "lpg" , "kero" , "wood" , "coco" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h18_cooken_22.xlsx"))

# Table H19. Total Number of Private Households by Main Source of Lighting ----
df <- hous1 %>% 
  group_by(ea, lighting) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = lighting, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("pgrid" , "solpan" , "sollamp" , "kerlamp" , "lpglamp" , "privgen" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h19_light_22.xlsx"))

# Table H20. Total Number of Private Households by Use of Energy Sources ----


df <- hous1 %>% 
  select(ea,starts_with("energy_source")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("energy_source"), sum, na.rm = TRUE)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  rename_with(~ gsub("energy_source_", "es", .x), starts_with("energy_source_")) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h20_ensour_22.xlsx"))

# Table H21. Total Number of Private Households by Main Method of Waste Disposal ----

df <- hous1 %>% 
  group_by(ea, waste_disp) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = waste_disp, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("coll" , "authsites" , "rec" , "burn" , "compost" , "bury" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h21_waste_22.xlsx"))

# Table H22a. Total Number of Private Households by Waste Disposal Service Ratings ----
df <- hous1 %>% 
  group_by(ea, trash_service) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = trash_service, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("fsat" , "sat" , "neut" , "disap" , "cdisap")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h22a_trash_ser_22.xlsx"))

# Table H22b. Total Number of Private Households by Trash Services Needs Improvement ----

df <- hous1 %>% 
  group_by(ea, trash_improve) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = trash_improve, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("timecol" , "bins" , "cserv" , "dumploc" , "oth")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h22b_trash_imp_22.xlsx"))

# Table H22c. Total Number of Private Households by Trash Bin Ownership ----

df <- hous1 %>% 
  group_by(ea, trash_bin) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = trash_bin, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h22c_trash_bin_22.xlsx"))

# Table H23a. Total Number of Private Households by Effect of Natural Disasters and Erosion - King Tide ----

df <- hous1 %>% 
  group_by(ea, king_tide) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = king_tide, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h23a_disa_kt_22.xlsx"))


# Table H23b. Total Number of Private Households by Effect of Natural Disasters and Erosion - Storm Surge ----

df <- hous1 %>% 
  group_by(ea, storm_surge) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = storm_surge, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h23b_disa_ss_22.xlsx"))

# Table H23c. Total Number of Private Households by Effect of Natural Disasters and Erosion -Other Natural Disaster ----

df <- hous1 %>% 
  group_by(ea, natural_disaster) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = natural_disaster, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h23c_disa_od_22.xlsx"))

# Table H23d. Total Number of Private Households by Effect of Natural Disasters and Erosion - Erosion ----

df <- hous1 %>% 
  group_by(ea, erosion) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = erosion, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h23d_disa_er_22.xlsx"))

# Table H24a. Total Number of Private Households by Communication - Land Line ----
df <- hous1 %>% 
  group_by(ea, landline) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = landline, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h24_landline_22.xlsx"))

# Table H24b. Total Number of Private Households by Communication - Internet Access ----
df <- hous1 %>% 
  group_by(ea, internet_access) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = internet_access, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h24_intacc_22.xlsx"))

# Table H24c. Total Number of Private Households by Communication - Type of Internet Connection ----

df <- hous1 %>% 
  select(ea,starts_with("internet_type__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("internet_type__"), sum, na.rm = TRUE)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  rename_with(~ gsub("internet_type__", "it", .x), starts_with("internet_type__")) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h24_typ_int_22.xlsx"))

# Table H24d. Total Number of Private Households by Communication - Cable TV ----
df <- hous1 %>% 
  group_by(ea, cable_tv) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = cable_tv, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h24_cable_22.xlsx"))

# Table H24e. Total Number of Private Households by Communication - Online Subscription ----
df <- hous1 %>% 
  group_by(ea, online_subscription) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = online_subscription, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h24_subs_22.xlsx"))

# Table H25. Total Number of Private Households by Assets Owned ----

df <- hous1 %>% 
  select(ea,starts_with("assets__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("assets__"), sum, na.rm = TRUE)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  rename_with(~ gsub("assets__", "as", .x), starts_with("assets__")) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h25_asset_22.xlsx"))

# Table H27. Total Number of Private Households by Transport Ownership

df <- hous1 %>% 
  select(ea,starts_with("h13b1__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("h13b1__"), sum, na.rm = TRUE)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  rename_with(~ gsub("h13b1__", "tr", .x), starts_with("h13b1__")) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h27_transp_22.xlsx"))

# Table H29. Total Number of Private Households by Island by Livestock Ownership ----

df <- hous1 %>% 
  select(ea,starts_with("h1801__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("h1801__"), sum, na.rm = TRUE)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  rename_with(~ gsub("h1801__", "lv", .x), starts_with("h1801__")) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h29_lvstk_22.xlsx"))

# Table H31. Total Number of Private Households by Remittance Received ----
rem <- read_stata(stata_files[4])
df <- hous1 %>% 
  select(ea,starts_with("h1501")) %>% 
  group_by(ea) %>% 
  mutate(t_yes = if_else(h1501==1,1,0),
         t_no = ifelse(h1501 ==2,1,0)) %>% 
  summarize(across(c(t_yes, t_no), sum, na.rm = TRUE)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) 

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h31_remit_22.xlsx"))

# Table H34. Total Number of Private Households Growing Vegetables by Type of Vegetable ### DOESNT WORK!!! ----
# agr <- read_stata(stata_files[8])
# easimp <- hous1 %>% 
#   select(ea, interview__key)
# agr <- merge(agr, easimp, by="interview__key")
# 
# df <- agr %>% 
#   select(ea,h1906r__id) %>% 
#   group_by(ea, h1906r__id) %>% 
#   summarize(count = n(), .groups = 'drop') %>% 
#   pivot_wider(names_from = h1906r__id, values_from = count, values_fill = list(count = 0)) %>% 
#   mutate(tot_hh = rowSums(select(.,-ea)))
# 
# # Get numeric column names sorted numerically
# numeric_col_names <- names(df)[!names(df) %in% c("ea", "tot_hh")]
# numeric_col_names <- sort(as.numeric(numeric_col_names), na.last = TRUE)
# numeric_col_names <- as.character(numeric_col_names)
# 
# # Reorder columns: 'ea', 'tot_hh', and then the rest in numeric order
# df <- df %>% 
#   select(ea, tot_hh, all_of(numeric_col_names))
# 
# # rename variables NEED TO CHANGE MANUALLY
# lab_var <- c("ccumb" , "ccab" , "bpepp"  , "tspin" , "cpepp" , "corn" , "eplang" , "lett" , "onion" , "tom" , "pumk" , "oth")
# names(df)[c(-1,-2)] <- lab_var
# names(df)
# 
# # Number of columns totals to compare with published tables
# ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
# ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
# totals <- df %>%
#   select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
#   summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
# totals
# 
# # Export
# write_xlsx(df,paste0(tab,"h34_veg_22.xlsx"))

# Table H35. Total Number of Private Households Growing Crops by Root Crops Grown in Parcels ---- tabulation works but numbers dont match with tables
# crop <- read_stata(stata_files[6])
# easimp <- hous1 %>% 
#   select(ea, interview__key)
# crop <- merge(crop, easimp, by="interview__key")
# 
# df <- crop %>% 
#   select(ea,h1911r__id) %>% 
#   group_by(ea, h1911r__id) %>% 
#   summarize(count = n(), .groups = 'drop') %>% 
#   pivot_wider(names_from = h1911r__id, values_from = count, values_fill = list(count = 0)) %>% 
#   mutate(tot_hh = rowSums(select(.,-ea)))
# 
# # Get numeric column names sorted numerically
# numeric_col_names <- names(df)[!names(df) %in% c("ea", "tot_hh")]
# numeric_col_names <- sort(as.numeric(numeric_col_names), na.last = TRUE)
# numeric_col_names <- as.character(numeric_col_names)
# 
# # Reorder columns: 'ea', 'tot_hh', and then the rest in numeric order
# df <- df %>% 
#   select(ea, tot_hh, all_of(numeric_col_names))
# 
# # rename variables NEED TO CHANGE MANUALLY
# lab_var <- c("staro" , "taro" , "cassava" , "felo" , "lauk" , "spot" , "oth")
# names(df)[c(-1,-2)] <- lab_var
# names(df)
# 
# # Number of columns totals to compare with published tables
# ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
# ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
# totals <- df %>%
#   select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
#   summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
# totals
# 
# # Export
# write_xlsx(df,paste0(tab,"h35_crop_22.xlsx"))

# Table H36. Total Number of Private Households Growing Fruits by Fruits Grown in Parcels ---- tabulation works but numbers dont match with tables
# frt <- read_stata(stata_files[7])
# easimp <- hous1 %>%
#   select(ea, interview__key)
# frt <- merge(frt, easimp, by="interview__key")
# 
# df <- frt %>%
#   select(ea,h1921r__id) %>%
#   group_by(ea, h1921r__id) %>%
#   summarize(count = n(), .groups = 'drop') %>%
#   pivot_wider(names_from = h1921r__id, values_from = count, values_fill = list(count = 0)) %>%
#   mutate(tot_hh = rowSums(select(.,-ea)))
# 
# # Get numeric column names sorted numerically
# numeric_col_names <- names(df)[!names(df) %in% c("ea", "tot_hh")]
# numeric_col_names <- sort(as.numeric(numeric_col_names), na.last = TRUE)
# numeric_col_names <- as.character(numeric_col_names)
# 
# # Reorder columns: 'ea', 'tot_hh', and then the rest in numeric order
# df <- df %>%
#   select(ea, tot_hh, all_of(numeric_col_names))
# 
# # rename variables NEED TO CHANGE MANUALLY
# lab_var <- c("ycoco" , "mcoco" , "banana" , "bfruit" , "pand" , "lime" , "paw" , "melon" , "toddy" , "oth")
# names(df)[c(-1,-2)] <- lab_var
# names(df)
# 
# # Number of columns totals to compare with published tables
# ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!")
# ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
# totals <- df %>%
#   select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
#   summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
# totals
# 
# # Export
# write_xlsx(df,paste0(tab,"h36_fruit_22.xlsx"))

# Table H37a. Total Number of Private Households Engaged in Fishing ----

df <- hous1 %>% 
  group_by(ea, h2001) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = h2001, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!")
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h37a_fishing_22.xlsx"))

# Table H37b. Total Number of Private Households Engaged in Fishing by Fishing Methods ----
tot_hh <- hous1 %>% 
  group_by(ea, h2001) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = h2001, values_from = count, values_fill = list(count = 0)) %>% 
  select(ea, `1`) %>% 
  rename(tot_hh = `1`)
sum(tot_hh$tot_hh)

df <- hous1 %>% 
  select(ea,starts_with("h2003__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("h2003__"), sum, na.rm = TRUE)) %>% 
  merge(., tot_hh, by='ea') %>% 
  rename_with(~ gsub("h2003__", "fm", .x), starts_with("h2003__")) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h37b_fishing_22.xlsx"))

# Table H39a. Total Number of Private Households Engaged in Handicrafts ----

df <- hous1 %>% 
  group_by(ea, handicraft) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = handicraft, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!")
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h39a_hcraft_22.xlsx"))

# Table H39b. Total Number of Private Households Engaged in Handicrafts Types of Handicraft ----

tot_hh <- hous1 %>% 
  group_by(ea,handicraft) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = handicraft, values_from = count, values_fill = list(count = 0)) %>% 
  select(ea, `1`) %>% 
  rename(tot_hh = `1`)
sum(tot_hh$tot_hh)

df <- hous1 %>% 
  select(ea,starts_with("handicraft_type__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("handicraft_type__"), sum, na.rm = TRUE)) %>% 
  merge(., tot_hh, by='ea') %>% 
  rename_with(~ gsub("handicraft_type__", "hc", .x), starts_with("handicraft_type__")) %>% 
  select(ea, tot_hh, hc1:hc12) 

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h39b_hcraft_22.xlsx"))

# Table H40. Total Number of Private Households Engaged in Processed Foods ----

tot_hh <- hous1 %>% 
  group_by(ea,handicraft) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = handicraft, values_from = count, values_fill = list(count = 0)) %>% 
  select(ea, `1`) %>% 
  rename(tot_hh = `1`)
sum(tot_hh$tot_hh)

df <- hous1 %>% 
  select(ea,starts_with("handicraft_type__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("handicraft_type__"), sum, na.rm = TRUE)) %>% 
  merge(., tot_hh, by='ea') %>% 
  rename_with(~ gsub("handicraft_type__", "hc", .x), starts_with("handicraft_type__")) %>% 
  select(ea, tot_hh, hc13:hc20) 

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h40_pfood_22.xlsx"))

# Table H41. Total Number of Private Households with Water Tank Access ----

df <- hous1 %>% 
  group_by(ea, watertank) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = watertank, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(tot_hh = rowSums(select(.,-ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything()) # Exclude 'ea' column and select all other columns

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t_yes" , "t_no")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!")
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h41_wtank_22.xlsx"))

# Table H41a. Total Number of Private Households with Water Tank Access - Number of Water tanks ----
tot_hh<- hous1 %>% 
  group_by(ea, watertank) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = watertank, values_from = count, values_fill = list(count = 0)) %>% 
  select(ea, `1`) %>% 
  rename(tot_hh=`1`) # Exclude 'ea' column and select all other columns
sum(tot_hh$tot_hh)

wt <- read_stata(stata_files[1])
easimp <- hous1 %>%
  select(ea, interview__key)
wt <- merge(wt, easimp, by="interview__key")

df <- wt %>%
  select(ea,interview__key, tank_roster__id) %>%
  group_by(ea,interview__key) %>%
  summarize(num = max(tank_roster__id), .groups = 'drop') %>% # get the max number of tanks from rooster
  group_by(ea,num) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = num, values_from = count, values_fill = list(count = 0)) %>% 
  merge(.,tot_hh, by='ea') %>% 
  select(order(colnames(.))) %>% 
  select(ea, tot_hh, everything())

# rename variables NEED TO CHANGE MANUALLY
lab_var <- c("t1" , "t2" , "t3" , "t4" , "t5")
names(df)[c(-1,-2)] <- lab_var
names(df)

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!")
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

# Export
write_xlsx(df,paste0(tab,"h41a_wtanknum_22.xlsx"))


# 3. POPULATION DATASET ==========================================================

# Create 5 years age groups
pop <- pop %>%
  mutate(age_grp5 = cut(age, 
                        breaks = seq(0, 95, by = 5), 
                        right = FALSE, ,
                        labels = c('0',	'5',	'10',	'15',	'20',	'25',	'30',	'35',	'40',
                                   '45',	'50',	'55',	'60',	'65',	'70',	'75','80', '85', '90')
                        ))
table(pop$age_grp5)

# Keep Private households
pop1 <- pop %>% 
  filter(hh_type == 1)
get_catlab(pop$hh_type)
get_catlab(pop1$hh_type)

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


# Table P3a. Table 3. Total Population by 5-Year Age Group De-jure ----
# Total
dft <- pop %>% 
  filter(de_jure_count ==1) %>% 
  select(ea,sex,age_grp5) %>% 
  group_by(ea,age_grp5) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = age_grp5, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, pop, everything()) %>% 
  relocate(`5`, .after = `0`) %>% 
  rename_with(~ paste0("t_", .), -1)

# Female
dff <- pop %>% 
  filter(de_jure_count ==1, sex ==2) %>% 
  select(ea,sex,age_grp5) %>% 
  group_by(ea,age_grp5) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = age_grp5, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, pop, everything()) %>% 
  relocate(`5`, .after = `0`) %>% 
  rename_with(~ paste0("f_", .), -1)

# Male
dfm <- pop %>% 
  filter(de_jure_count ==1, sex ==1) %>% 
  select(ea,sex,age_grp5) %>% 
  group_by(ea,age_grp5) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = age_grp5, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, pop, everything()) %>% 
  relocate(`5`, .after = `0`) %>% 
  rename_with(~ paste0("m_", .), -1)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')

# review upped age groups
df <- df %>% 
  mutate(t_75 = t_75 + t_80 + t_85 + t_90,
         f_75 = f_75 + f_80 + f_85 + f_90,
         m_75 = m_75 + m_80 + m_85) %>% 
  select(-c( t_80, t_85, t_90,f_80, f_85, f_90, m_80, m_85))

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p3a_5ag_dj_22.xlsx"))


# Table P3b. Table 3. Total Population by 5-Year Age Group De-facto ----

# Total
dft <- pop %>% 
  filter(de_facto_count ==1) %>% 
  select(ea,sex,age_grp5) %>% 
  group_by(ea,age_grp5) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = age_grp5, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, pop, everything()) %>% 
  relocate(`5`, .after = `0`) %>% 
  rename_with(~ paste0("t_", .), -1)

# Female
dff <- pop %>% 
  filter(de_facto_count ==1, sex ==2) %>% 
  select(ea,sex,age_grp5) %>% 
  group_by(ea,age_grp5) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = age_grp5, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, pop, everything()) %>% 
  relocate(`5`, .after = `0`) %>% 
  rename_with(~ paste0("f_", .), -1)

# Male
dfm <- pop %>% 
  filter(de_facto_count ==1, sex ==1) %>% 
  select(ea,sex,age_grp5) %>% 
  group_by(ea,age_grp5) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = age_grp5, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, pop, everything()) %>% 
  relocate(`5`, .after = `0`) %>% 
  rename_with(~ paste0("m_", .), -1)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')

# review upped age groups
df <- df %>% 
  mutate(t_75 = t_75 + t_80 + t_85 + t_90,
         f_75 = f_75 + f_80 + f_85 + f_90,
         m_75 = m_75 + m_80 + m_85) %>% 
  select(-c( t_80, t_85, t_90,f_80, f_85, f_90, m_80, m_85))

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p3b_5ag_df_22.xlsx"))

# Table 4. Resident Population de Facto by 5-Year Age Group by Sex ----

# filter residents for the rest of the tabulations
popr <- pop %>%
  filter( df_resident == 1)

table(popr$df_resident)  
table(popr$de_facto_count)
sum(popr$df_resident, na.rm = T)

# Total
dft <- popr %>% 
  select(ea,age_grp5) %>% 
  group_by(ea,age_grp5) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = age_grp5, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, pop, everything()) %>% 
  relocate(`5`, .after = `0`) %>% 
  rename_with(~ paste0("t_", .), -1)

# Female
dff <- popr %>% 
  filter( sex ==2) %>% 
  select(ea,sex,age_grp5) %>% 
  group_by(ea,age_grp5) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = age_grp5, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, pop, everything()) %>% 
  relocate(`5`, .after = `0`) %>% 
  rename_with(~ paste0("f_", .), -1)

# Male
dfm <- popr %>% 
  filter( sex ==1) %>% 
  select(ea,sex,age_grp5) %>% 
  group_by(ea,age_grp5) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = age_grp5, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(colnames(.))) %>% 
  select(ea, pop, everything()) %>% 
  relocate(`5`, .after = `0`) %>% 
  rename_with(~ paste0("m_", .), -1)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')

# review upped age groups
df <- df %>% 
  mutate(t_75 = t_75 + t_80 + t_85 + t_90,
         f_75 = f_75 + f_80 + f_85 + f_90,
         m_75 = m_75 + m_80 + m_85) %>% 
  select(-c( t_80, t_85, t_90,f_80, f_85, f_90, m_80, m_85))

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p3a_5ag_df_22.xlsx"))

# Table 6. Resident Population by Religion by Sex ----
# Total
dft <- popr %>% 
  select(ea,religion) %>% 
  group_by(ea,religion) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = religion, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_ekt" , "t_sda" , "t_jeh" , "t_bah" , "t_berthren" , "t_aog" , "t_cath" , "t_lds" , "t_oth" , "t_non" , "t_ref" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter( sex ==2) %>% 
  select(ea,religion) %>% 
  group_by(ea,religion) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = religion, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_ekt" , "f_sda" , "f_jeh" , "f_bah" , "f_berthren" , "f_aog" , "f_cath" , "f_lds" , "f_oth" , "f_non" , "f_ref" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter( sex ==1) %>% 
  select(ea,religion) %>% 
  group_by(ea,religion) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = religion, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_ekt" , "m_sda" , "m_jeh" , "m_bah" , "m_berthren" , "m_aog" , "m_cath" , "m_lds" , "m_oth" , "m_non" , "m_ref" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p6_religion_22.xlsx"))

# Table 7. Resident Population 15 Years and Above by Marital Status by Sex ----

# Total
dft <- popr %>% 
  filter(age>14) %>% 
  select(ea,mstatus) %>% 
  group_by(ea,mstatus) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = mstatus, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_single" , "t_married" , "t_divor" , "t_widowed" , "t_common" , "t_unknown" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(age>14, sex ==2) %>% 
  select(ea,mstatus) %>% 
  group_by(ea,mstatus) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = mstatus, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_single" , "f_married" , "f_divor" , "f_widowed" , "f_common" , "f_unknown" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(age>14, sex ==1) %>% 
  select(ea,mstatus) %>% 
  group_by(ea,mstatus) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = mstatus, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_single" , "m_married" , "m_divor" , "m_widowed" , "m_common" , "m_unknown" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p7_marital_22.xlsx"))

# Table 9. Resident Population by Relationship with Head of the Household by Sex ----

# Total
dft <- popr %>% 
  select(ea,relat) %>% 
  group_by(ea,relat) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = relat, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_head" , "t_spouse" , "t_son" , "t_adopt" , "t_brother" , "t_father" , "t_gchild" , "t_gpar" , "t_aunt" , "t_neph" , "t_cousin" , "t_oth" , "t_unrel" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2) %>% 
  select(ea,relat) %>% 
  group_by(ea,relat) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = relat, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_head" , "f_spouse" , "f_son" , "f_adopt" , "f_brother" , "f_father" , "f_gchild" , "f_gpar" , "f_aunt" , "f_neph" , "f_cousin" , "f_oth" , "f_unrel" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1) %>% 
  select(ea,relat) %>% 
  group_by(ea,relat) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = relat, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_head" , "m_spouse" , "m_son" , "m_adopt" , "m_brother" , "m_father" , "m_gchild" , "m_gpar" , "m_aunt" , "m_neph" , "m_cousin" , "m_oth" , "m_unrel" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p9_hhrelat_22.xlsx"))

# Table 10. Resident Population by Sex by Nationality ----

# Total
dft <- popr %>% 
  select(ea,nationality) %>% 
  group_by(ea,nationality) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = nationality, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_tuv" , "t_fji" , "t_sam" , "t_kir" , "t_ton" , "t_othpi" , "t_oth" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2) %>% 
  select(ea,nationality) %>% 
  group_by(ea,nationality) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = nationality, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_tuv" , "f_fji" , "f_sam" , "f_kir" , "f_ton" , "f_othpi" , "f_oth" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1) %>% 
  select(ea,nationality) %>% 
  group_by(ea,nationality) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = nationality, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_tuv" , "m_fji" , "m_sam" , "m_kir" , "m_ton" , "m_othpi" , "m_oth" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p10_nation_22.xlsx"))

# Table 12. Resident Population by Sex by Ethnicity----

# Total
dft <- popr %>% 
  select(ea,ethnic) %>% 
  group_by(ea,ethnic) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ethnic, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_tuv" , "t_tuvkir" , "t_tuvother" , "t_oth" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2) %>% 
  select(ea,ethnic) %>% 
  group_by(ea,ethnic) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ethnic, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_tuv" , "f_tuvkir" , "f_tuvother" , "f_oth" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1) %>% 
  select(ea,ethnic) %>% 
  group_by(ea,ethnic) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ethnic, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_tuv" , "m_tuvkir" , "m_tuvother" , "m_oth" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p12_ethn_22.xlsx"))

# Table 19a. Resident Population by Disability Domains - Seeing  ----

# Total
dft <- popr %>% 
  filter(age >4) %>% 
  select(ea,seeing) %>% 
  group_by(ea,seeing) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = seeing, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_no" , "t_yes_some" , "t_yes_alot" , "t_cannot" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age >4) %>% 
  select(ea,seeing) %>% 
  group_by(ea,seeing) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = seeing, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_no" , "f_yes_some" , "f_yes_alot" , "f_cannot" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age >4) %>% 
  select(ea,seeing) %>% 
  group_by(ea,seeing) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = seeing, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_no" , "m_yes_some" , "m_yes_alot" , "m_cannot" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p19a_seeing_22.xlsx"))

# Table 19b. Resident Population by Disability Domains - Hearing ----
# Total
dft <- popr %>% 
  filter(age >4) %>% 
  select(ea,hearing) %>% 
  group_by(ea,hearing) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = hearing, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_no" , "t_yes_some" , "t_yes_alot" , "t_cannot" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age >4) %>% 
  select(ea,hearing) %>% 
  group_by(ea,hearing) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = hearing, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_no" , "f_yes_some" , "f_yes_alot" , "f_cannot" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age >4) %>% 
  select(ea,hearing) %>% 
  group_by(ea,hearing) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = hearing, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_no" , "m_yes_some" , "m_yes_alot" , "m_cannot" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p19b_hearing_22.xlsx"))

# Table 19c. Resident Population by Disability Domains - Walking ----
dft <- popr %>% 
  filter(age >4) %>% 
  select(ea,walking) %>% 
  group_by(ea,walking) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = walking, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_no" , "t_yes_some" , "t_yes_alot" , "t_cannot" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age >4) %>% 
  select(ea,walking) %>% 
  group_by(ea,walking) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = walking, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_no" , "f_yes_some" , "f_yes_alot" , "f_cannot" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age >4) %>% 
  select(ea,walking) %>% 
  group_by(ea,walking) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = walking, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_no" , "m_yes_some" , "m_yes_alot" , "m_cannot" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p19c_walking_22.xlsx"))

# Table 19d. Resident Population by Disability Domains - Remembering ----
dft <- popr %>% 
  filter(age >4) %>% 
  select(ea,remembering) %>% 
  group_by(ea,remembering) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = remembering, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_no" , "t_yes_some" , "t_yes_alot" , "t_cannot" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age >4) %>% 
  select(ea,remembering) %>% 
  group_by(ea,remembering) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = remembering, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_no" , "f_yes_some" , "f_yes_alot" , "f_cannot" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age >4) %>% 
  select(ea,remembering) %>% 
  group_by(ea,remembering) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = remembering, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_no" , "m_yes_some" , "m_yes_alot" , "m_cannot" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p19d_remembering_22.xlsx"))

# Table 19e. Resident Population by Disability Domains - Selfcare  ----
dft <- popr %>% 
  filter(age >4) %>% 
  select(ea,selfcare) %>% 
  group_by(ea,selfcare) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = selfcare, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_no" , "t_yes_some" , "t_yes_alot" , "t_cannot" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age >4) %>% 
  select(ea,selfcare) %>% 
  group_by(ea,selfcare) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = selfcare, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_no" , "f_yes_some" , "f_yes_alot" , "f_cannot" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age >4) %>% 
  select(ea,selfcare) %>% 
  group_by(ea,selfcare) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = selfcare, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_no" , "m_yes_some" , "m_yes_alot" , "m_cannot" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p19e_selfcaring_22.xlsx"))

# Table 19f. Resident Population by Disability Domains - Communicating  ----
dft <- popr %>% 
  filter(age >4) %>% 
  select(ea,communicating) %>% 
  group_by(ea,communicating) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = communicating, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_no" , "t_yes_some" , "t_yes_alot" , "t_cannot" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age >4) %>% 
  select(ea,communicating) %>% 
  group_by(ea,communicating) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = communicating, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_no" , "f_yes_some" , "f_yes_alot" , "f_cannot" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age >4) %>% 
  select(ea,communicating) %>% 
  group_by(ea,communicating) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = communicating, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_no" , "m_yes_some" , "m_yes_alot" , "m_cannot" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p19f_comm_22.xlsx"))

# Table 29. Resident Population by Sex by Ciguatera Food ----

dft <- popr %>% 
  select(ea,ciguatera) %>% 
  group_by(ea,ciguatera) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ciguatera, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2) %>% 
  select(ea,ciguatera) %>% 
  group_by(ea,ciguatera) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ciguatera, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1) %>% 
  select(ea,ciguatera) %>% 
  group_by(ea,ciguatera) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ciguatera, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_yes" , "m_no" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p29_ciguat_22.xlsx"))


# Table 29. Resident Population by Sex by Ciguatera Food - Frequency ----

dft <- popr %>% 
  select(ea,ciguatera_freq) %>% 
  group_by(ea,ciguatera_freq) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ciguatera_freq, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -c(ea,`NA`)))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1) %>% 
  select(-t_NA) 

lab_var <- c("t_once" , "t_twice" , "t_thrice" , "t_3times")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2) %>% 
  select(ea,ciguatera_freq) %>% 
  group_by(ea,ciguatera_freq) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ciguatera_freq, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -c(ea,`NA`)))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)%>% 
  select(-f_NA) 

lab_var <- c("f_once" , "f_twice" , "f_thrice" , "f_3times")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1) %>% 
  select(ea,ciguatera_freq) %>% 
  group_by(ea,ciguatera_freq) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ciguatera_freq, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(.,, -c(ea,`NA`)))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)%>% 
  select(-m_NA) 

lab_var <- c("m_once" , "m_twice" , "m_thrice" , "m_3times")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p29_ciguatfreq_22.xlsx"))

# Table 43a. Resident Population by Sex by Ever Attended (2+)  ----
dft <- popr %>% 
  filter(age>2) %>% 
  select(ea,ever_attended) %>% 
  group_by(ea,ever_attended) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ever_attended, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no" , "t_dknw" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>2) %>% 
  select(ea,ever_attended) %>% 
  group_by(ea,ever_attended) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ever_attended, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no" , "f_dknw" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>2) %>% 
  select(ea,ever_attended) %>% 
  group_by(ea,ever_attended) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = ever_attended, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_yes" , "m_no" , "m_dknw" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p43_everattend_22.xlsx"))
# Table 43b. Resident Population by Sex by Current Attending ----

dft <- popr %>% 
  filter(age>2, ever_attended ==1) %>% 
  select(ea,current_attend) %>% 
  group_by(ea,current_attend) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = current_attend, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no" , "t_dknw" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>2, ever_attended ==1) %>% 
  select(ea,current_attend) %>% 
  group_by(ea,current_attend) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = current_attend, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no" , "f_dknw" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>2, ever_attended ==1) %>% 
  select(ea,current_attend) %>% 
  group_by(ea,current_attend) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = current_attend, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_yes" , "m_no" , "m_dknw" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p43_currattend_22.xlsx"))

# Table 44. Resident Population by Age Group by Sex by Grade Completed ----

dft <- popr %>% 
  filter(age>2, ever_attended ==1) %>% 
  select(ea,grade_completed) %>% 
  group_by(ea,grade_completed) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = grade_completed, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_oms" , "t_nolev" , "t_ps1" , "t_ps2" , "t_ps3" , "t_y1" , "t_y2" , "t_y3" , "t_y4" , "t_y5" , "t_y6" , "t_y7" , "t_y8" , "t_y9" , "t_y10" , "t_y11" , "t_y12" , "t_y13" , "t_voc" , "t_tert")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>2, ever_attended ==1) %>% 
  select(ea,grade_completed) %>% 
  group_by(ea,grade_completed) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = grade_completed, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_oms" , "f_nolev" , "f_ps1" , "f_ps2" , "f_ps3" , "f_y1" , "f_y2" , "f_y3" , "f_y4" , "f_y5" , "f_y6" , "f_y7" , "f_y8" , "f_y9" , "f_y10" , "f_y11" , "f_y12" , "f_y13" , "f_voc" , "f_tert")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>2, ever_attended ==1) %>% 
  select(ea,grade_completed) %>% 
  group_by(ea,grade_completed) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = grade_completed, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_oms" , "m_nolev" , "m_ps1" , "m_ps2" , "m_ps3" , "m_y1" , "m_y2" , "m_y3" , "m_y4" , "m_y5" , "m_y6" , "m_y7" , "m_y8" , "m_y9" , "m_y10" , "m_y11" , "m_y12" , "m_y13" , "m_voc" , "m_tert")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p44_gradecomp_22.xlsx"))

# Table 46. Resident Population by Age Group by Sex by Highest Qualification Achieved ----


dft <- popr %>% 
  filter(age>2, ever_attended ==1) %>% 
  select(ea,qualification) %>% 
  group_by(ea,qualification) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = qualification, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_noqu" , "t_psl" , "t_ctcl" , "t_colcert" , "t_f5" , "t_f6" , "t_f7" , "t_vocat" , "t_marit" , "t_trade" , "t_teach" , "t_nurs" , "t_othcert" , "t_dip" , "t_deg" , "t_mast" , "t_othqu")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>2, ever_attended ==1) %>% 
  select(ea,qualification) %>% 
  group_by(ea,qualification) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = qualification, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_noqu" , "f_psl" , "f_ctcl" , "f_colcert" , "f_f5" , "f_f6" , "f_f7" , "f_vocat" , "f_marit" , "f_trade" , "f_teach" , "f_nurs" , "f_othcert" , "f_dip" , "f_deg" , "f_mast" , "f_othqu")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>2, ever_attended ==1) %>% 
  select(ea,qualification) %>% 
  group_by(ea,qualification) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = qualification, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c("m_noqu" , "m_psl" , "m_ctcl" , "m_colcert" , "m_f5" , "m_f6" , "m_f7" , "m_vocat" , "m_marit" , "m_trade" , "m_teach" , "m_nurs" , "m_othcert" , "m_dip" , "m_deg" , "m_mast" , "m_othqu")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p45_levachiev_22.xlsx"))

# Table 48. Resident Population by Age Group by Sex by Pre-School Attendance Before ----
dft <- popr %>% 
  filter(age>2, ever_attended ==1) %>% 
  select(ea,preschool) %>% 
  group_by(ea,preschool) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = preschool, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>2, ever_attended ==1) %>% 
  select(ea,preschool) %>% 
  group_by(ea,preschool) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = preschool, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>2, ever_attended ==1) %>% 
  select(ea,preschool) %>% 
  group_by(ea,preschool) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = preschool, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c( "m_yes" , "m_no")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p48_preschool_22.xlsx"))

# Table 49a. Resident Population by Island by Sex by Litearacy in Tuvaluan ----
dft <- popr %>% 
  filter(age>4) %>% 
  select(ea,tuvaluan_literate) %>% 
  group_by(ea,tuvaluan_literate) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = tuvaluan_literate, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no" , "t_ref" , "t_dknow" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>4) %>% 
  select(ea,tuvaluan_literate) %>% 
  group_by(ea,tuvaluan_literate) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = tuvaluan_literate, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no" , "f_ref" , "f_dknow" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>4) %>% 
  select(ea,tuvaluan_literate) %>% 
  group_by(ea,tuvaluan_literate) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = tuvaluan_literate, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c( "m_yes" , "m_no" , "m_ref" , "m_dknow" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p49a_littuv_22.xlsx"))
# Table 49b. Resident Population by Island by Sex by Litearacy in Nuian ----

dft <- popr %>% 
  filter(age>4) %>% 
  select(ea,nuian_literate ) %>% 
  group_by(ea,nuian_literate) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = nuian_literate, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no" , "t_ref" , "t_dknow" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>4) %>% 
  select(ea,nuian_literate) %>% 
  group_by(ea,nuian_literate) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = nuian_literate, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no" , "f_ref" , "f_dknow" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>4) %>% 
  select(ea,nuian_literate) %>% 
  group_by(ea,nuian_literate) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = nuian_literate, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c( "m_yes" , "m_no" , "m_ref" , "m_dknow" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p49b_litniu_22.xlsx"))
# Table 49c. Resident Population by Island by Sex by Litearacy in English ----

dft <- popr %>% 
  filter(age>4) %>% 
  select(ea,english_literate ) %>% 
  group_by(ea,english_literate) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = english_literate, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no" , "t_ref" , "t_dknow" , "t_ns")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>4) %>% 
  select(ea,english_literate) %>% 
  group_by(ea,english_literate) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = english_literate, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no" , "f_ref" , "f_dknow" , "f_ns")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>4) %>% 
  select(ea,english_literate) %>% 
  group_by(ea,english_literate) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = english_literate, values_from = count, values_fill = list(count = 0)) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c( "m_yes" , "m_no" , "m_ref" , "m_dknow" , "m_ns")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p49c_liten_22.xlsx"))

# Table 51a. Resident Population by Sex by Access to Internet ----

dft <- popr %>% 
  filter(age>9) %>% 
  select(ea,access_internet ) %>% 
  group_by(ea,access_internet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = access_internet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no" )
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>9) %>% 
  select(ea,access_internet) %>% 
  group_by(ea,access_internet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = access_internet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no" )
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>9) %>% 
  select(ea,access_internet) %>% 
  group_by(ea,access_internet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = access_internet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c( "m_yes" , "m_no" )
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p50_accint_22.xlsx"))

# Table 51b. Resident Population by Sex by Place of Internet ----
pop <- popr %>% 
  filter(age>9) %>% 
  select(ea,access_internet ) %>% 
  group_by(ea,access_internet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = access_internet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, `1`) %>% 
  rename(t_pop = `1`)


dft <- popr %>% 
  filter(age>9) %>%
  select(ea,starts_with("internet_location__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("internet_location__"), sum, na.rm = TRUE)) %>%
  merge(., pop, by='ea') %>% 
  rename_with(~ gsub("internet_location__", "t_il", .x), starts_with("internet_location__")) %>% 
  select(ea, t_pop, t_il1:t_il6)


# Female
pop <- popr %>% 
  filter(age>9, sex ==2) %>% 
  select(ea,access_internet ) %>% 
  group_by(ea,access_internet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = access_internet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, `1`) %>% 
  rename(f_pop = `1`)


dff <- popr %>% 
  filter(age>9, sex ==2) %>%
  select(ea,starts_with("internet_location__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("internet_location__"), sum, na.rm = TRUE)) %>%
  merge(., pop, by='ea') %>% 
  rename_with(~ gsub("internet_location__", "f_il", .x), starts_with("internet_location__")) %>% 
  select(ea, f_pop, f_il1:f_il6)

# Male
pop <- popr %>% 
  filter(age>9, sex ==1) %>% 
  select(ea,access_internet ) %>% 
  group_by(ea,access_internet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = access_internet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, `1`) %>% 
  rename(m_pop = `1`)


dfm <- popr %>% 
  filter(age>9, sex ==1) %>%
  select(ea,starts_with("internet_location__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("internet_location__"), sum, na.rm = TRUE)) %>%
  merge(., pop, by='ea') %>% 
  rename_with(~ gsub("internet_location__", "m_il", .x), starts_with("internet_location__")) %>% 
  select(ea, m_pop, m_il1:m_il6)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals



write_xlsx(df,paste0(tab,"p50_intplace_22.xlsx"))

# Table 55. Resident Population by Sex by Purpose of Using Internet ----
dft <- popr %>% 
  filter(age>9) %>%
  select(ea,starts_with("internet_purpose__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("internet_purpose__"), sum, na.rm = TRUE)) %>%
  rename_with(~ gsub("internet_purpose__", "t_iu", .x), starts_with("internet_purpose__")) %>% 
  select(ea, t_iu1:t_iu16)


# Female
dff <- popr %>% 
  filter(age>9, sex ==2) %>%
  select(ea,starts_with("internet_purpose__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("internet_purpose__"), sum, na.rm = TRUE)) %>%
  rename_with(~ gsub("internet_purpose__", "f_iu", .x), starts_with("internet_purpose__")) %>% 
  select(ea, f_iu1:f_iu16)

# Male
dfm <- popr %>% 
  filter(age>9, sex ==1) %>%
  select(ea,starts_with("internet_purpose__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("internet_purpose__"), sum, na.rm = TRUE)) %>%
  rename_with(~ gsub("internet_purpose__", "m_iu", .x), starts_with("internet_purpose__")) %>% 
  select(ea, m_iu1:m_iu16)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p55_intuse_22.xlsx"))


# Table 57a. Resident Population by Sex by Use of Mobile ----

dft <- popr %>% 
  filter(age>9) %>% 
  select(ea,use_mobile ) %>% 
  group_by(ea,use_mobile) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = use_mobile, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>9) %>% 
  select(ea,use_mobile) %>% 
  group_by(ea,use_mobile) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = use_mobile, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>9) %>% 
  select(ea,use_mobile) %>% 
  group_by(ea,use_mobile) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = use_mobile, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c( "m_yes" , "m_no")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p57_mobuse_22.xlsx"))


# Table 57b. Resident Population by Sex by Ownership of Mobile ----
dft <- popr %>% 
  filter(age>9) %>% 
  select(ea,own_mobile ) %>% 
  group_by(ea,own_mobile) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = own_mobile, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>9) %>% 
  select(ea,own_mobile) %>% 
  group_by(ea,own_mobile) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = own_mobile, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>9) %>% 
  select(ea,own_mobile) %>% 
  group_by(ea,own_mobile) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = own_mobile, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c( "m_yes" , "m_no")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')

# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p57_mobown_22.xlsx"))

# Table 57c. Resident Population by Sex by Ownership of Tablets ----
dft <- popr %>% 
  filter(age>9) %>% 
  select(ea,own_tablet ) %>% 
  group_by(ea,own_tablet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = own_tablet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>9) %>% 
  select(ea,own_tablet) %>% 
  group_by(ea,own_tablet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = own_tablet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>9) %>% 
  select(ea,own_tablet) %>% 
  group_by(ea,own_tablet) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = own_tablet, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c( "m_yes" , "m_no")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p57_tabown_22.xlsx"))

# Table 57d. Resident Population by Sex by Ownership of Laptop ----

dft <- popr %>% 
  filter(age>9) %>% 
  select(ea,own_laptop ) %>% 
  group_by(ea,own_laptop) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = own_laptop, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("t_", .), -1)

lab_var <- c("t_yes" , "t_no")
names(dft)[c(-1,-2)] <- lab_var
names(dft)

# Female
dff <- popr %>% 
  filter(sex ==2, age>9) %>% 
  select(ea,own_laptop) %>% 
  group_by(ea,own_laptop) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = own_laptop, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea))) %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("f_", .), -1)

lab_var <- c("f_yes" , "f_no")
names(dff)[c(-1,-2)] <- lab_var
names(dff)

# Male
dfm <- popr %>% 
  filter(sex ==1, age>9) %>% 
  select(ea,own_laptop) %>% 
  group_by(ea,own_laptop) %>% 
  summarize(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = own_laptop, values_from = count, values_fill = list(count = 0)) %>% 
  select(-`NA`) %>% 
  mutate(pop = rowSums(select(., -ea)))  %>% 
  select(order(as.numeric(colnames(.)))) %>% 
  select(ea, pop, everything()) %>% 
  rename_with(~ paste0("m_", .), -1)

lab_var <- c( "m_yes" , "m_no")
names(dfm)[c(-1,-2)] <- lab_var
names(dfm)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p57_ltopown_22.xlsx"))

# Table 59. Resident Population by Island by Sex by Alcohol/Tobacco Consumption ----
pop <- popr %>% 
  filter(age>14) %>% 
  select(ea,sex) %>% 
  group_by(ea,sex) %>% 
  summarize(count = n(),.groups = 'drop') %>% 
  pivot_wider(names_from = sex, values_from =count, values_fill = list(count = 0)) %>% 
  mutate(t_pop = `1` + `2`) %>% 
  rename(m_pop = `1`,
         f_pop = `2`)

dft <- popr %>% 
  filter(age>14) %>%
  select(ea,starts_with("p601__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("p601__"), sum, na.rm = TRUE)) %>%
  rename_with(~ gsub("p601__", "t_con", .x), starts_with("p601__")) %>% 
  select(ea, t_con1:t_con9)


# Female
dff <- popr %>% 
  filter(age>14, sex ==2) %>%
  select(ea,starts_with("p601__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("p601__"), sum, na.rm = TRUE)) %>%
  rename_with(~ gsub("p601__", "f_con", .x), starts_with("p601__")) %>% 
  select(ea, f_con1:f_con9)

# Male
dfm <- popr %>% 
  filter(age>14, sex ==1) %>%
  select(ea,starts_with("p601__")) %>% 
  group_by(ea) %>% 
  summarize(across(starts_with("p601__"), sum, na.rm = TRUE)) %>%
  rename_with(~ gsub("p601__", "m_con", .x), starts_with("p601__")) %>% 
  select(ea, m_con1:m_con9)

# Merge and export
df <- merge(dft, dff, by = 'ea')
df <- merge(df, dfm, by = 'ea')
df <- merge(df,pop, by = 'ea')


# Number of columns totals to compare with published tables
ifelse(nrow(df) == 77, "nrow ok","NROW NOT OK!!") 
ifelse(sum(is.na(df)) == 0, "nas ok", "NAS WRONG!!")
totals <- df %>%
  select(-ea) %>%  # Exclude the 'id' column if it's not numeric and not needed
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
totals

write_xlsx(df,paste0(tab,"p59_consum_22.xlsx"))

# 4. RENAME TAB NAMES ON EXCEL FILES =========
library(openxlsx)
files <- list.files(path = tab, pattern = "\\.xlsx$", full.names= T)

# loop to replace ea by ea2022
for (file in files) {
  data <- read.xlsx(file)
  if("ea" %in% colnames(data)){
    colnames(data)[colnames(data)=="ea"] <- "ea2022"
    write.xlsx(data,file = file, overwrite = T)
  }
}

# loop the sheetname change
for (file in files) {
  wb <- loadWorkbook(file)
  sheets(wb)
  new_sn <- "ea2022"
  names(wb)[1] <- new_sn
  
  saveWorkbook(wb,file,overwrite =  T)
  rm(wb)
}

