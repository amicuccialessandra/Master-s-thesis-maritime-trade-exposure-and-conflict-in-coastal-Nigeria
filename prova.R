#admin2 level 
# --- 0. Load Libraries ---
# It's good practice to load all necessary libraries at the beginning of your script.
# install.packages(c("sf", "terra", "exactextractr", "httr", "rvest", "xml2", "data.table", "readxl", "dplyr", "lubridate", "fixest"))
library(sf)
library(terra)
library(exactextractr)
library(httr)
library(rvest)
library(xml2)
library(data.table)
library(readxl)   # For reading .xlsx files
library(dplyr)    # For data manipulation (e.g., mutate, select, filter, rename)
library(lubridate) # For working with dates (e.g., year())
library(fixest)   # For feols regression

# --- 1. Data Import and Initial Spatial Preparation (NIGERIA ADMIN BOUNDARIES) ---

# Read the main Nigeria administrative level 2 shapefile
nig <- read_sf(paste0(getwd(), "/nga_polnda_adm2_1m_salb.shp"))

# Transform 'nig' to the Projected Coordinate System (PCS) for accurate area/distance calculations
# EPSG:26393 (Minna, Nigeria - Grid) is suitable for Nigeria.
# Ensure 'nig' is transformed early for consistent spatial operations.
nig <- nig %>%
  st_transform("+init=EPSG:26393")

shp <- shp %>%
  st_transform("+init=EPSG:26393")

#creating a grid
grid <- st_make_grid(nig, cellsize = 100000) # takes shp and returns a grid

# We need to remove the part of squares not perfectly overlapping with shp
grid <- grid %>% st_intersection(nig) # overlapping squares, missing values


# Compute the area of cells
# Remember: we can make this calculation because we are using a Projected Coordinate System
area <- st_area(grid)
area <- as.numeric(area); area
to_rm <- which(area <= 7e+08); to_rm
# Drop smallest cells
grid
grid <- grid[-to_rm]
grid

# In order to match the grid with other shapefiles,
# we need the grid to be a shapefile
grid <- st_as_sf(grid)
grid
# We can also assign an id to each cell
grid <- grid %>% 
  mutate(FIP = 1:n())


# Adjust path to point to the unzipped .shp file
shapefile_path <- "ports.shp"

# Read shapefile
ports_data <- st_read(shapefile_path)

ports_data <- st_transform(ports_data, "+init=EPSG:26393")
ports_data <- st_transform(ports_data, st_crs(nig))


nigeria_boundary <- st_boundary(st_union(nig))

# Trova i poligoni admin2 che toccano questo confine esterno
coastal_admin2 <- nig[st_touches(nig, nigeria_boundary, sparse = FALSE)[,1], ]

coastal_ADM2_NAME <- c(
  "Badagry", "Ojo", "Amuwo-Odofin", "Eti-Osa", "Lagos Island", "Ibeju/Lekki", "Epe", 
  "Ogun Waterside", "Ilaje", "Bekwara", "Ese-Odo", "Warri South", "Warri South West", 
  "Warri North", "Burutu", "Bomadi", "Brass", "Southern Ijaw", "Ekeremor", "Kolokuma/Opokuma", 
  "Bonny", "Okrika", "Degema", "Andoni", "Akuku Toru", "Asari-Toru", "Opobo/Nkoro", 
  "Eastern Obolo", "Ibeno", "Mbo", "Oron", "Okobo", "Udung Uko", "Calabar South", "Calabar-Municipal",
  "Akpabuyo"
)

# Supponiamo che la colonna corretta sia ADM2_NAME
coastal_admin2 <- nig %>%
  filter(ADM2_NAME %in% coastal_ADM2_NAME)


# --- 2. Define Master Coastal Administrative Units ---
# This is crucial for consistency. We'll use the spatial join method as the master definition.

# First, define coastal_ADM1 (states)
coastal_ADM1_NAME <- c(
  "Lagos", "Ogun", "Ondo", "Delta", "Bayelsa", "Rivers", "Akwa Ibom", "Cross River", "Abia"
)
coastal_admin1 <- nig %>%
  filter(ADM1_NAME %in% coastal_ADM1_NAME)

admin2_in_coastal_admin1 <- st_join(nig, coastal_admin1, join = st_within, left = FALSE)

coastal_admin2_master <- admin2_in_coastal_admin1

if ("ADM2_NAME.x" %in% names(coastal_admin2_master)) {
  coastal_admin2_master <- coastal_admin2_master %>%
    
    rename(ADM2_NAME = ADM2_NAME.x)
}


#buffer porti 
convert_to_decimal <- function(deg, min, dec, hemi) {
  coord <- deg + min/60 + dec/3600
  # Usa ifelse per vettori
  coord <- ifelse(hemi %in% c("S", "W"), -coord, coord)
  return(coord)
}

ports_data <- ports_data %>%
  mutate(
    LAT_dec = convert_to_decimal(LAT_DEG, LAT_MIN, LAT_DD, LAT_HEMI),
    LON_dec = convert_to_decimal(LONG_DEG, LONG_MIN, LNG_DD, LONG_HEMI)
  )

# Crea sf object
ports_sf <- st_as_sf(ports_data, coords = c("LON_dec", "LAT_dec"), crs = 4326)

# Trasforma in CRS metrico (qui EPSG 32631 è UTM zona 31N, cambia se necessario)
ports_proj <- st_transform(ports_sf, 32631)

# Ora puoi creare il buffer
ports_buffer <- st_buffer(ports_proj, dist = 1000000)  # buffer di 1000km

# Assicurati che il buffer sia valido
ports_buffer <- st_make_valid(ports_buffer)

# Se admin2_in_coastal_admin1 è in un CRS diverso, trasformalo
admin2_proj <- st_transform(admin2_in_coastal_admin1, st_crs(ports_buffer))


coastal_buffer_admin2 <- admin2_proj %>%
  filter(st_intersects(admin2_proj, ports_buffer, sparse = FALSE)[,1])

# Calcola la distanza minima da ogni admin2 ai porti
dist_ports <- st_distance(coastal_buffer_admin2, ports_proj)
coastal_buffer_admin2$dist_min_port <- apply(as.matrix(dist_ports), 1, min)



#conflict
conflict <- read_excel("Cartel2.xlsx")
rapacity_sub_events <- c(
  "Looting/property destruction", 
  "Abduction/forced disappearance", 
  "Armed clash", 
  "Attack", 
  "Shelling/artillery/missile attack", 
  "Sexual violence"  # spesso associata a violenza predatoria
)


non_rapacity_sub_events <- c(
  "Peaceful protest", 
  "Violent demonstration", 
  "Protest with intervention", 
  "Agreement", 
  "Change to group/activity", 
  "Disrupted weapons use", 
  "Arrests", 
  "Headquarters or base established", 
  "Non-violent transfer of territory", 
  "Non-state actor overtakes territory", 
  "Government regains territory", 
  "Other"  # da valutare caso per caso
)


conflict_non_rapacity <- conflict %>%
  filter(
    sub_event_type %in% non_rapacity_sub_events |
      grepl("loot|steal|raid|resource|gold|oil|land|plunder|livestock|pasture|farm|grazing", 
            tolower(notes))
  )


conflict_non_rapacity_clean <- conflict_non_rapacity[complete.cases(conflict_non_rapacity$longitude, conflict_non_rapacity$latitude), ]

# Count the number of removed lines
rows_removed <- nrow(conflict_non_rapacity) - nrow(conflict_non_rapacity_clean)
print(paste("Righe rimosse:", rows_removed))

conflict_sf <- st_as_sf(conflict_non_rapacity_clean, coords = c("longitude", "latitude"), crs = st_crs(nig))

# Ensure both are in the same CRS
grid_transformed <- st_transform(grid, "+init=EPSG:26393")
conflict_sf_transformed <- st_transform(conflict_sf, "+init=EPSG:26393")

conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393)

st_crs(conflict_sf) <- 4326  # Set this to the correct original CRS if known

# Step 2: Re-transform conflict_sf to EPSG:26393
conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393)


# Step 3: Perform spatial join and summarize
conflict_sf1 <- st_join(grid_transformed, conflict_sf_transformed) %>%
  group_by(FIP, admin2, year) %>%
  summarise(n_conflicts = n(), .groups = 'drop')

# Check the resulting data
head(conflict_sf1)
conflict_sf_transformed1 <- conflict_sf_transformed %>%
  mutate(conflict_id = row_number())  # Create a unique identifier

# Perform the spatial join and ensure conflict_id is retained

conflict_sf3 <- st_join(grid_transformed, conflict_sf_transformed) %>% 
  group_by(FIP, year, event_type,fatalities,event_id_cnty) %>% 
  summarise(n_conflicts = n())




coastal_admin2 <- st_transform(admin2_in_coastal_admin1, crs = st_crs(conflict_sf))
within_matrix <- st_within(conflict_sf, coastal_admin2, sparse = FALSE)
# within_matrix è una matrice: righe = punti, colonne = poligoni costieri

# Per ogni punto, verifica se è dentro almeno un poligono
inside_any <- apply(within_matrix, 1, any)

# Ora puoi filtrare i punti
conflict_coastal <- conflict_sf[inside_any, ]


#problema con i conflitti

conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393) # Use the same CRS as your grid and panel

# Now, use your coastal_admin2_master for the spatial filter
# Ensure coastal_admin2_master is also in crs 26393
coastal_admin2_master_proj <- st_transform(coastal_admin2_master, crs = 26393)

# Perform the spatial join to get conflicts within the master coastal ADM2s
# Using st_intersects for points within polygons, or st_within for stricter containment
conflict_coastal <- st_join(conflict_sf_transformed, coastal_admin2_master_proj, join = st_intersects, left = FALSE)

coastal_buffer_admin2$export_proxy <- 1 / (coastal_buffer_admin2$dist_min_port + 1)
coastal_buffer_admin2 <- coastal_buffer_admin2 %>%
  mutate(
    dist_min_norm = (dist_min_port - min(dist_min_port, na.rm = TRUE)) / (max(dist_min_port, na.rm = TRUE) - min(dist_min_port, na.rm = TRUE)),
    nightlight_norm = (nightlight_mean - min(nightlight_mean, na.rm = TRUE)) / (max(nightlight_mean, na.rm = TRUE) - min(nightlight_mean, na.rm = TRUE)),
    export_proxy = ifelse(is.na(export_proxy), 0, export_proxy),  # se l’hai già calcolato
    export_norm = (export_proxy - min(export_proxy, na.rm = TRUE)) / (max(export_proxy, na.rm = TRUE) - min(export_proxy, na.rm = TRUE))
  )


#LSCI come proxy temporale dell’export nigeriano per tutto il tuo periodo d’analisi.
LSCI<- read_excel("LSCI.xlsx")
LSCI <- as.data.table(LSCI)
LSCI <- LSCI %>%
  filter(Economy_Label == "Nigeria")


cols_quarterly <- names(LSCI)[-1]

# 3. Pivot long: da wide a long (colonna trimestre + valore)
LSCI_long <- melt(LSCI, measure.vars = cols_quarterly,
                  variable.name = "Quarter", value.name = "LSCI_value")

# 4. Pulizia nomi trimestre e conversione in date
parse_quarter_date <- function(colname) {
  # Estrai "May  2009" da "May  2009_Index_Average..."
  date_part <- sub("_Index.*", "", colname)
  
  # Normalizza spazi e rimuovi punti
  date_part <- gsub("\\s+", " ", date_part)
  date_part <- gsub("\\.", "", date_part)
  
  # Dividi in mese + anno
  parts <- strsplit(date_part, " ")[[1]]
  month_str <- parts[1]
  year_str <- parts[2]
  
  # Mappa mesi abbreviati a numeri
  month_map <- c(
    Jan = 1, Feb = 2, Mar = 3, Apr = 4,
    May = 5, Jun = 6, Jul = 7, Aug = 8,
    Sep = 9, Oct = 10, Nov = 11, Dec = 12
  )
  
  # Controllo se mese esiste
  if (!month_str %in% names(month_map)) return(NA)
  
  month_num <- month_map[[month_str]]
  
  # Costruisci data stringa "YYYY-MM-15"
  date_str <- sprintf("%s-%02d-15", year_str, month_num)
  as.Date(date_str)
}

dates <- sapply(cols_quarterly, parse_quarter_date)

LSCI_long <- melt(LSCI,
                  id.vars = "Economy_Label",
                  measure.vars = cols_quarterly,
                  variable.name = "quarter_label",
                  value.name = "LSCI_value")
LSCI_long[, date := as.Date(dates[quarter_label])]

LSCI_long[, year := year(date)]
LSCI_long[, quarter := quarter(date)]
LSCI_long[, LSCI_value := as.numeric(LSCI_value)]  # assicura che sia numerico
LSCI_annual <- LSCI_long[, .(LSCI_annual = mean(LSCI_value, na.rm = TRUE)), by = year]



install.packages(c("sf", "terra", "exactextractr", "httr", "rvest", "xml2"))

library(sf)
library(terra)
library(exactextractr)
library(httr)
library(rvest)
library(xml2)
library(data.table)

# Crea cartella per i dati
dir.create("nightlights_data", showWarnings = FALSE)

# 2. Proietta lo shapefile (se serve) per farlo combaciare con i raster
nig <- st_transform(nig, crs = "EPSG:4326")

# 3. Elenco dei raster scaricati
raster_files <- list.files("nightlights/", pattern = "\\.tif$", full.names = TRUE)
r <- rast("nightlights/Harmonized_DN_NTL_2009_calDMSP.tif") 
nig <- st_transform(nig, crs = crs(r))

# Convertilo in vect per terra
nig_vect <- vect(nig)

# Ora croppa e maschera
r_crop <- crop(r, nig_vect)
r_mask <- mask(r_crop, nig_vect)

plot(r_mask)

# 4. Crea cartella output
dir.create("nightlights_nigeria", showWarnings = FALSE)

# 5. Loop per ritagliare ogni raster
for (file in raster_files) {
  r <- rast(file)
  r_crop <- crop(r, vect(nig))
  r_mask <- mask(r_crop, vect(nig))
  plot(r_mask)  # se vuoi vedere
  year <- gsub("[^0-9]", "", basename(file))  # estrae l'anno dal nome
  out_name <- paste0("nightlights_nigeria/nightlight_nigeria_", year, ".tif")
  
  writeRaster(r_mask, out_name, overwrite = TRUE)
  cat("Salvato:", out_name, "\n")
}

coastal_admin2 <- st_transform(coastal_admin2, crs = "EPSG:4326")

# Convertilo in vect per terra
coastal_vect <- vect(coastal_admin2)

# Elenco dei raster nightlights
raster_files <- list.files("nightlights/", pattern = "\\.tif$", full.names = TRUE)

# Crea cartella output
dir.create("nightlights_coastal", showWarnings = FALSE)

for (file in raster_files) {
  r <- rast(file)
  
  # Proietta coastal_vect al CRS del raster (se necessario)
  coastal_vect_proj <- project(coastal_vect, crs(r))
  
  # Croppa e maschera usando solo l’area costiera
  r_crop <- crop(r, coastal_vect_proj)
  r_mask <- mask(r_crop, coastal_vect_proj)
  
  # Visualizza (opzionale)
  plot(r_mask, main = paste("Nightlights coast -", basename(file)))
  
  # Estrai l'anno dal nome del file
  year <- gsub("[^0-9]", "", basename(file))
  
  # Salva il raster mascherato solo sulla costa
  out_name <- paste0("nightlights_coastal/nightlight_coastal_", year, ".tif")
  writeRaster(r_mask, out_name, overwrite = TRUE)
  
  cat("Salvato:", out_name, "\n")
}

nightlight_stats <- terra::extract(r_mask, vect(admin2_in_coastal_admin1), fun=mean, na.rm=TRUE)

# Aggiungi al dataframe admin2_costieri
admin2_in_coastal_admin1$nightlight_mean <- nightlight_stats[,2]
#normalizzo
admin2_in_coastal_admin1$nightlight_norm <- (admin2_in_coastal_admin1$nightlight_mean - min(admin2_in_coastal_admin1$nightlight_mean, na.rm=TRUE)) / 
  (max(admin2_in_coastal_admin1$nightlight_mean, na.rm=TRUE) - min(admin2_in_coastal_admin1$nightlight_mean, na.rm=TRUE))


ContPortThroughput<- read_excel("US.ContPortThroughput_20250723_173714.xlsx")
ContPortThroughput <- as.data.table(ContPortThroughput)
ContPortThroughput <- ContPortThroughput %>%
  filter(Economy_Label == "Nigeria")

cols_teu <- grep("^20(0[0-9]|1[0-9])_TEU_Twenty_foot_Equivalent_Unit_Value$", names(ContPortThroughput), value = TRUE)
cols_to_keep <- c("Economy_Label", cols_teu)
ContPortThroughput <- ContPortThroughput[, ..cols_to_keep]
ContPortThroughput[, (cols_teu) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = cols_teu]
# Usa melt per passare da wide a long
ContPortThroughput_long <- melt(
  ContPortThroughput,
  id.vars = "Economy_Label",
  measure.vars = patterns("^20(0[0-9]|1[0-9])_TEU_Twenty_foot_Equivalent_Unit_Value$"),
  variable.name = "year",
  value.name = "teu"
)

# La colonna year sarà un fattore numerico, converti in anno reale
ContPortThroughput_long[, year := as.integer(gsub("_.*", "", names(ContPortThroughput)[
  grep("^20(0[0-9]|1[0-9])_TEU_Twenty_foot_Equivalent_Unit_Value$", names(ContPortThroughput))
][year]))]

setorder(ContPortThroughput_long, year)

ContPortThroughput_long[, shock := c(NA, diff(log(teu)))]


nightlight_list <- list()

for (file in raster_files) {
  r <- rast(file)
  year <- as.integer(gsub("[^0-9]", "", basename(file)))
  
  nightlight_vals <- terra::extract(r, vect(admin2_in_coastal_admin1), fun=mean, na.rm=TRUE)[,2]
  
  tmp_dt <- data.table(
    ADM2_ID = admin2_in_coastal_admin1$ADM2_CODE.x,
    year = year,
    nightlight_mean = nightlight_vals
  )
  
  nightlight_list[[as.character(year)]] <- tmp_dt
}

nightlight_dt <- rbindlist(nightlight_list)

# 3. Distanza porti (assumendo costante nel tempo)
dist_ports_dt <- data.table(
  ADM2_ID = coastal_buffer_admin2$ADM2_CODE.x,
  dist_min_port = coastal_buffer_admin2$dist_min_port
)



# 5. Ora devi unire questi dati per creare il dataframe finale

# Per semplicità, uniamo nightlight e distanza (quest'ultima fissa)
df_admin2_year <- merge(nightlight_dt, dist_ports_dt, by = "ADM2_ID", all.x = TRUE)

final_df <- merge(df_admin2_year, LSCI_annual, by = "year", all.x = TRUE)



exposure_list <- list()

for (t in LSCI_annual$year) {
  exposure_list[[as.character(t)]] <- coastal_buffer_admin2 %>%
    mutate(
      year = t,
      Exposure = LSCI_annual[year == t, LSCI_annual] * (
        (1/3 * dist_min_norm) +
          (1/3 * nightlight_norm) +
          (1/3 * export_norm)
      )
    )
}

exposure_df <- bind_rows(exposure_list)


#checks 
identical(exposure_df$ADM2_NAME.x, exposure_df$ADM2_NAME.y)
identical(exposure_df$CNTRY_NAME.x, exposure_df$CNTRY_NAME.y)
identical(exposure_df$CNTRY_CODE.x, exposure_df$CNTRY_CODE.y)
identical(exposure_df$ADM1_NAME.x, exposure_df$ADM1_NAME.y)
identical(exposure_df$ADM1_CODE.x, exposure_df$ADM1_CODE.y)
identical(exposure_df$ADM2_CODE.x, exposure_df$ADM2_CODE.y)
identical(exposure_df$HRname.x, exposure_df$HRname.y)
identical(exposure_df$HRpcode.x, exposure_df$HRpcode.y)
identical(exposure_df$HRparent.x, exposure_df$HRparent.y)
identical(exposure_df$Shape_Leng.x, exposure_df$Shape_Leng.y)
identical(exposure_df$Shape_Area.x, exposure_df$Shape_Area.y)
identical(exposure_df$ADM2_New.x, exposure_df$ADM2_New.y)




# Tieni solo le colonne .x e tutte le variabili nuove
setDT(exposure_df)
cols_to_keep <- grep("\\.x$", colnames(exposure_df), value = TRUE)
cols_to_keep <- c(cols_to_keep, "geometry", "nightlight_mean", "nightlight_norm",
                  "dist_min_port", "export_proxy", "dist_min_norm", 
                  "export_norm", "year", "Exposure")

# Rinomina le .x rimuovendo il suffisso
clean_names <- gsub("\\.x$", "", cols_to_keep)
df_clean <- exposure_df[, ..cols_to_keep]
setnames(df_clean, old = cols_to_keep, new = clean_names)


panel_data <- merge(exposure_df, ContPortThroughput_long[, .(year, shock)], 
                    by = "year", all.x = TRUE)
#cols_to_keep1 <- grep("\\.x$", colnames(panel_data), value = TRUE)
#cols_to_keep1 <- c(cols_to_keep1, "geometry", "nightlight_mean", "nightlight_norm",
#"dist_min_port", "export_proxy", "dist_min_norm", 
#"export_norm", "year", "Exposure", "shock")
#clean_names1 <- gsub("\\.x$", "", cols_to_keep1)
#df_clean1 <- panel_data[, ..cols_to_keep]
#setnames(df_clean1, old = cols_to_keep1, new = clean_names1)
panel_data[, exposure_shock := Exposure * shock]

identical(conflict_coastal$admin2, panel_data$ADM2_NAME.x)






# Make sure your 'panel_data' is built using the cleaned ADM2 names.
# After df_clean is created from exposure_df, ensure its ADM2_NAME column is the one used.
# Let's ensure 'panel_data' also contains ADM2_CODE as a unique identifier from the master.
# It seems you are using ADM2_CODE.x in nightlight_dt. Let's stick to ADM2_CODE.
# Make sure coastal_admin2_master has ADM2_CODE.

# Before constructing panel_data, ensure df_clean is the one you continue with
# and that it contains the necessary identifying columns from coastal_admin2_master
# (e.g., ADM2_CODE and ADM2_NAME).

# Example of ensuring ADM2_CODE for merging:
df_clean <- coastal_buffer_admin2 %>%
   dplyr::select(ADM2_CODE.x, ADM2_NAME.x, geometry, dist_min_port, nightlight_mean, export_proxy) %>%
   mutate(
    dist_min_norm = (dist_min_port - min(dist_min_port, na.rm = TRUE)) / (max(dist_min_port, na.rm = TRUE) - min(dist_min_port, na.rm = TRUE)),
    nightlight_norm = (nightlight_mean - min(nightlight_mean, na.rm = TRUE)) / (max(nightlight_mean, na.rm = TRUE) - min(nightlight_mean, na.rm = TRUE)),
    export_proxy = ifelse(is.na(export_proxy), 0, export_proxy),
     export_norm = (export_proxy - min(export_proxy, na.rm = TRUE)) / (max(export_proxy, na.rm = TRUE) - min(export_proxy, na.rm = TRUE))
  )

# Then in the exposure_list loop, use these normalized columns.
exposure_list[[as.character(t)]] <- df_clean %>% # Use df_clean as base
   mutate(
    year = t,
     Exposure = LSCI_annual[year == t, LSCI_annual] * (
       (1/3 * dist_min_norm) +
       (1/3 * nightlight_norm) +
      (1/3 * export_norm)
    )
   )
exposure_df <- bind_rows(exposure_list) # this exposure_df should now be clean from .x, .y suffixes if built carefully

# Your current df_clean is created *after* exposure_df is bound.
# Let's refine the process to keep names consistent from the start.

# It seems `ADM2_CODE.x` is coming from `admin2_in_coastal_admin1` in `nightlight_dt`.
# Let's make sure the `ADM2_NAME` column is always consistent in name.

# The `panel_data` merge line:
panel_data <- merge(exposure_df, ContPortThroughput_long[, .(year, shock)], by = "year", all.x = TRUE)
# Here, `exposure_df` still contains `ADM2_NAME.x` if the `identical()` checks were against `panel_data$ADM2_NAME.x`.
# The `setDT(exposure_df)` and `setnames(df_clean, ...)` happens *after* `exposure_df` is created.
# You need to apply `df_clean` to `panel_data` construction.

# Revised:
# First, create exposure_df with clean names from the start (or clean it right after creation)
# Use a common identifier like ADM2_CODE alongside ADM2_NAME

# Assuming coastal_buffer_admin2 is what you use for base attributes
# Let's add ADM2_CODE to it and make sure it's kept.
final_df_clean <- final_df %>%
  dplyr::select(ADM2_CODE, ADM2_NAME, dist_min_port, nightlight_mean, nightlight_norm, export_proxy, dist_min_norm, export_norm) %>%
  rename(ADM2_CODE = ADM2_CODE.x, ADM2_NAME = ADM2_NAME.x)

exposure_list <- list()
for (t in LSCI_annual$year) {
  exposure_list[[as.character(t)]] <- coastal_buffer_admin2_clean %>% # Use the cleaned base
    mutate(
      year = t,
      Exposure = LSCI_annual[year == t, LSCI_annual] * (
        (1/3 * dist_min_norm) +
          (1/3 * nightlight_norm) +
          (1/3 * export_norm)
      )
    )
}
exposure_df_cleaned <- bind_rows(exposure_list)
setDT(exposure_df_cleaned) # If you want to continue with data.table

# Now, `panel_data` merge:
panel_data <- merge(exposure_df_cleaned, ContPortThroughput_long[, .(year, shock)],
                    by = "year", all.x = TRUE)

panel_data[, exposure_shock := Exposure * shock]

# At this point, panel_data should have clean 'ADM2_NAME' (not .x or .y)
# And conflict_coastal should also have a clean 'ADM2_NAME' from Step 3.

# Check if the unique sets of names are the same, regardless of order or original data type
all(sort(unique(as.character(conflict_coastal$ADM2_NAME))) == sort(unique(as.character(panel_data$ADM2_NAME.x))))

model <- feols(n_conflicts ~ exposure_shock | ADM2_NAME + year, data = final_regression_data)

summary(model)


model1 <- feols(n_conflicts ~ exposure_shock, data = final_regression_data)

summary(model1)
