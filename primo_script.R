setwd("C:/Documenti")
remove(list = ls())
library(terra)
library(exactextractr)
library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)
library(haven)
library(fixest)
library(modelsummary)
library(labelled)
library(SPEI)
library(conleyreg)
library(raster)
library(readxl)
library(ncdf4)
library("RNetCDF")
library(patchwork)
library(sf)
library(fixest)
#step 1: shapefile
shp <- read_sf(paste0(getwd(), "/TM_WORLD_BORDERS-0.3.shp"))
shp <- st_make_valid(shp)
shp <- shp %>% 
  mutate(id = 1:n(),
         NAME = tolower(NAME))
# Load names of Subsaharan countries and select them in shp
load("afr_names.rda")
sub_shp <- subset(shp, ISO3 %in% afr_names$iso3c)
# Subset the sub_shp object to include only Nigeria (ISO3 code "NGA")
shp <- shp %>% filter(NAME %in% "nigeria")
#admin2 level 
nig <- read_sf(paste0(getwd(), "/nga_polnda_adm2_1m_salb.shp"))
shp <- shp %>%
  st_transform("+init=EPSG:26393")
nig <- nig %>%
  st_transform("+init=EPSG:26393")
#creating a grid
grid <- st_make_grid(nig, cellsize = 100000) # takes shp and returns a grid
# We need to remove the part of squares not perfectly overlapping with shp
grid <- grid %>% st_intersection(nig) # overlapping squares, missing values
# Compute the area of cells
area <- st_area(grid)
area <- as.numeric(area); area
to_rm <- which(area <= 7e+08); to_rm
# Drop smallest cells
grid <- grid[-to_rm]
# In order to match the grid with other shapefiles, we need the grid to be a 
shapefile
grid <- st_as_sf(grid)
# We can also assign an id to each cell
grid <- grid %>% 
  mutate(FIP = 1:n())
# Adjust path to point to the unzipped .shp file
shapefile_path <- "ports.shp"
# Read shapefile
ports_data <- st_read(shapefile_path)
ports_data <- st_transform(ports_data, "+init=EPSG:26393")
ports_data <- st_transform(ports_data, st_crs(nig))
# Plot
ggplot() +
  geom_sf(data = nig, fill = "gray90", color = "black") +  # Admin2 map
  geom_sf(data = grid, fill = NA, color = "black", linewidth = 0.2) +  # Grid 
overlay
  geom_sf(data = ports_data, color = "red", size = 2) +  # Ports as red dots
  theme_minimal() +
  ggtitle("Nigerian Ports with Admin2 Boundaries and Grid")
#add the Onne port, absent in the dataset
cols_no_geom <- setdiff(names(ports_data), "geometry")
nuovo_porto <- as.data.frame(matrix(NA, nrow = 1, ncol = length(cols_no_geom)))
names(nuovo_porto) <- cols_no_geom
nuovo_porto$MAIN_PORT_ <- "ONNE"
nuovo_porto$LNG_DD <- 7.0361
nuovo_porto$LAT_DD <- 4.7444
nuovo_porto_sf <- st_as_sf(
  nuovo_porto,
  coords = c("LNG_DD", "LAT_DD"),
  crs = 4326,
  remove = FALSE
)
st_geometry(nuovo_porto_sf) <- st_sfc(st_point(c(7.0361, 4.7444)), crs = 4326)
nuovo_porto_sf <- st_transform(nuovo_porto_sf, st_crs(ports_data))
#Avoid duplicates
if ("MAIN_PORT_" %in% names(ports_data)) {
  ports_data <- ports_data %>% filter(is.na(MAIN_PORT_) | MAIN_PORT_ != "ONNE")
}
add_missing_cols <- function(x, cols) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    for (m in missing) x[[m]] <- NA
  }
  x[, cols]
}
common_cols <- union(names(ports_data), names(nuovo_porto_sf))
ports_data_aligned     <- add_missing_cols(ports_data, common_cols)
nuovo_porto_sf_aligned <- add_missing_cols(nuovo_porto_sf, common_cols)
ports_data <- rbind(ports_data_aligned, nuovo_porto_sf_aligned)
#plot again
ggplot() +
  geom_sf(data = nig, fill = "gray90", color = "black") +  # Admin2 map
  geom_sf(data = grid, fill = NA, color = "black", linewidth = 0.2) +  # Grid 
overlay
  geom_sf(data = ports_data, color = "red", size = 2) +  # Ports as red dots
  theme_minimal() +
  ggtitle("Nigerian Ports with Admin2 Boundaries and Grid")
nigeria_boundary <- st_boundary(st_union(nig))
#Coastal ADMIN
coastal_ADM1_NAME <- c(
  "Lagos", "Ogun", "Ondo", "Delta", "Bayelsa", "Rivers", "Akwa lbom", "Cross 
River", "Abia"
)
coastal_admin1 <- nig %>%
  filter(ADM1_NAME %in% coastal_ADM1_NAME)
admin2_in_coastal_admin1 <- st_join(nig, coastal_admin1, join = st_within, left 
= FALSE)
#merge
admin2_in_coastal_admin1 <- st_join(nig, coastal_admin1, join = st_within, left 
= FALSE)
coastal_admin2_master <- admin2_in_coastal_admin1
#Cleaning
if ("ADM2_NAME.x" %in% names(coastal_admin2_master)) {
  coastal_admin2_master <- coastal_admin2_master %>% dplyr::rename(ADM2_NAME = 
ADM2_NAME.x)
}
if ("ADM2_CODE.x" %in% names(coastal_admin2_master)) {
  coastal_admin2_master <- coastal_admin2_master %>% dplyr::rename(ADM2_CODE = 
ADM2_CODE.x)
}
admin2_in_coastal_admin1 <- coastal_admin2_master
#buffer 
convert_to_decimal <- function(deg, min, dec, hemi) {
  coord <- deg + min/60 + dec/3600
  coord <- ifelse(hemi %in% c("S", "W"), -coord, coord)
  return(coord)
}
ports_data <- ports_data %>%
  mutate(
    LAT_dec = convert_to_decimal(LAT_DEG, LAT_MIN, LAT_DD, LAT_HEMI),
    LON_dec = convert_to_decimal(LONG_DEG, LONG_MIN, LNG_DD, LONG_HEMI)
  )
#sf object
ports_sf <- st_as_sf(ports_data, coords = c("LON_dec", "LAT_dec"), crs = 4326)
ports_proj <- st_transform(ports_sf, 32631)
ports_buffer <- st_buffer(ports_proj, dist = 1000000)  # buffer di 1000km
ports_buffer <- st_make_valid(ports_buffer)
admin2_proj <- st_transform(admin2_in_coastal_admin1, st_crs(ports_buffer))
coastal_buffer_admin2 <- admin2_proj %>%
  filter(st_intersects(admin2_proj, ports_buffer, sparse = FALSE)[,1])
#minimum distance
dist_ports <- st_distance(coastal_buffer_admin2, ports_proj)
coastal_buffer_admin2$dist_min_port <- apply(as.matrix(dist_ports), 1, min)/1000
####CONFLICT####
conflict <- read_excel("Cartel2.xlsx")
rapacity_sub_events <- c(
  "Looting/property destruction", 
  "Abduction/forced disappearance", 
  "Armed clash", 
  "Attack", 
  "Shelling/artillery/missile attack", 
  "Sexual violence"
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
  "Other"
)
conflict_non_rapacity <- conflict %>%
  filter(
    sub_event_type %in% non_rapacity_sub_events |
      grepl("loot|steal|raid|resource|gold|oil|land|plunder|livestock|pasture|
farm|grazing", 
            tolower(notes))
  )
conflict_non_rapacity_clean <- 
conflict_non_rapacity[complete.cases(conflict_non_rapacity$longitude, 
conflict_non_rapacity$latitude), ]
# Count the number of removed lines
rows_removed <- nrow(conflict_non_rapacity) - nrow(conflict_non_rapacity_clean)
conflict_sf <- st_as_sf(conflict_non_rapacity_clean, coords = c("longitude", 
"latitude"), crs = st_crs(nig))
# Ensure both are in the same CRS
grid_transformed <- st_transform(grid, "+init=EPSG:26393")
conflict_sf_transformed <- st_transform(conflict_sf, "+init=EPSG:26393")
conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393)
# Step 2: Re-transform conflict_sf to EPSG:26393
conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393)
# Step 3: Perform spatial join and summarize
conflict_sf1 <- st_join(grid_transformed, conflict_sf_transformed) %>%
  group_by(FIP, admin2, year) %>%
  summarise(n_conflicts = n(), .groups = 'drop')
conflict_sf_transformed1 <- conflict_sf_transformed %>%
  mutate(conflict_id = row_number())
conflict_sf3 <- st_join(grid_transformed, conflict_sf_transformed) %>% 
  group_by(FIP, year, event_type,fatalities,event_id_cnty) %>% 
  summarise(n_conflicts = n())
st_crs(conflict_sf) <- 4326
conflict_sf <- st_transform(conflict_sf, st_crs(nig))
coastal_admin2 <- st_transform(admin2_in_coastal_admin1, crs = 
st_crs(conflict_sf))
within_matrix <- st_within(conflict_sf, coastal_admin2, sparse = FALSE)
#VERIFY
inside_any <- apply(within_matrix, 1, any)
#filter
conflict_coastal <- conflict_sf[inside_any, ]
# Perform the spatial join to get conflicts within the master coastal ADM2s
coastal_admin2_master_proj <- st_transform(coastal_admin2_master, crs = 4326)
coastal_admin2_master_proj <- st_transform(coastal_admin2_master, crs = 26393)
st_crs(conflict_sf_transformed) <- 4326
conflict_sf_transformed <- st_transform(conflict_sf_transformed, 4326)
conflict_sf_transformed <- st_transform(conflict_sf_transformed, 26393)
# Using st_intersects for points within polygons, or st_within for stricter 
containment
conflict_coastal <- st_join(conflict_sf_transformed, coastal_admin2_master_proj, 
join = st_intersects, left = FALSE)
conflict_sf <- sf::st_transform(conflict_sf, sf::st_crs(coastal_admin2_master))
##Join spaziale
ev_join <- sf::st_join(conflict_sf, coastal_admin2_master, join = sf::st_within, 
left = FALSE)
ev_df   <- as.data.frame(sf::st_drop_geometry(ev_join))
adm2_vec  <- 
unique(as.character(as.data.frame(sf::st_drop_geometry(coastal_admin2_master))
$ADM2_NAME))
years_vec <- sort(unique(ev_df$year))   # oppure: years_vec <- seq(2009, 2019)
counts_raw <- aggregate(list(n_conflicts = ev_df$year),
                        by = list(ADM2_NAME = ev_df$ADM2_NAME,
                                  year      = ev_df$year),
                        FUN = length)
#panel
panel <- merge(data.frame(ADM2_NAME = adm2_vec, stringsAsFactors = FALSE),
               data.frame(year = years_vec), all = TRUE)
conflicts_adm2_year <- merge(panel, counts_raw, by = c("ADM2_NAME","year"), 
all.x = TRUE)
conflicts_adm2_year$n_conflicts[is.na(conflicts_adm2_year$n_conflicts)] <- 0L
###NIGHTLIGHTS####
dir.create("nightlights_data", showWarnings = FALSE)
nig <- st_transform(nig, crs = "EPSG:4326")
#list
raster_files <- list.files("nightlights/", pattern = "\\.tif$", full.names = 
TRUE)
r <- rast("nightlights/Harmonized_DN_NTL_2009_calDMSP.tif") 
nig <- st_transform(nig, crs = crs(r))
nig_vect <- vect(nig)
r_crop <- crop(r, nig_vect)
r_mask <- mask(r_crop, nig_vect)
#output folder
dir.create("nightlights_nigeria", showWarnings = FALSE)
# 5. Loop for each raster
for (file in raster_files) {
  r <- rast(file)
  r_crop <- crop(r, vect(nig))
  r_mask <- mask(r_crop, vect(nig))
  plot(r_mask)
  year <- gsub("[^0-9]", "", basename(file))
  out_name <- paste0("nightlights_nigeria/nightlight_nigeria_", year, ".tif")
  writeRaster(r_mask, out_name, overwrite = TRUE)
  cat("Salvato:", out_name, "\n")
}
coastal_admin2 <- st_transform(coastal_admin2, crs = "EPSG:4326")
coastal_vect <- vect(coastal_admin2)
# list
raster_files <- list.files("nightlights/", pattern = "\\.tif$", full.names = 
TRUE)
# output for coastal
dir.create("nightlights_coastal", showWarnings = FALSE)
for (file in raster_files) {
  r <- rast(file)
  coastal_vect_proj <- project(coastal_vect, crs(r))
  r_crop <- crop(r, coastal_vect_proj)
  r_mask <- mask(r_crop, coastal_vect_proj)
  plot(r_mask)
  year <- gsub("[^0-9]", "", basename(file))
  out_name <- paste0("nightlights_coastal/nightlight_coastal_", year, ".tif")
  writeRaster(r_mask, out_name, overwrite = TRUE)
  cat("Salvato:", out_name, "\n")
}
nightlight_stats <- terra::extract(r_mask, vect(admin2_in_coastal_admin1), 
fun=mean, na.rm=TRUE)
# add admin2 coastal
admin2_in_coastal_admin1$nightlight_mean <- nightlight_stats[,2]
admin2_in_coastal_admin1$nightlight_norm <- 
(admin2_in_coastal_admin1$nightlight_mean - 
min(admin2_in_coastal_admin1$nightlight_mean, na.rm=TRUE)) / 
  (max(admin2_in_coastal_admin1$nightlight_mean, na.rm=TRUE) - 
min(admin2_in_coastal_admin1$nightlight_mean, na.rm=TRUE))
####SHOCK Container Port Throughput
ContPortThroughput<- read_excel("US.ContPortThroughput_20250723_173714.xlsx")
ContPortThroughput <- as.data.table(ContPortThroughput)
ContPortThroughput <- ContPortThroughput %>%
  filter(Economy_Label == "Nigeria")
cols_teu <- grep("^20(0[0-9]|1[0-9])_TEU_Twenty_foot_Equivalent_Unit_Value$", 
names(ContPortThroughput), value = TRUE)
cols_to_keep <- c("Economy_Label", cols_teu)
ContPortThroughput <- ContPortThroughput[, ..cols_to_keep]
ContPortThroughput[, (cols_teu) := lapply(.SD, function(x) 
as.numeric(as.character(x))), .SDcols = cols_teu]
ContPortThroughput_long <- melt(
  ContPortThroughput,
  id.vars = "Economy_Label",
  measure.vars = patterns("^20(0[0-9]|1[0
9])_TEU_Twenty_foot_Equivalent_Unit_Value$"),
  variable.name = "year",
  value.name = "teu"
)
ContPortThroughput_long[, year := as.integer(gsub("_.*", "", 
names(ContPortThroughput)[
  grep("^20(0[0-9]|1[0-9])_TEU_Twenty_foot_Equivalent_Unit_Value$", 
names(ContPortThroughput))
][year]))]
setorder(ContPortThroughput_long, year)
ContPortThroughput_long[, shock := c(NA, diff(log(teu)))]
####LSCI#####
LSCI<- read_excel("LSCI.xlsx")
LSCI <- as.data.table(LSCI)
LSCI <- LSCI %>%
  filter(Economy_Label == "Nigeria")
cols_quarterly <- names(LSCI)[-1]
LSCI_long <- melt(LSCI, measure.vars = cols_quarterly,
                  variable.name = "Quarter", value.name = "LSCI_value")
#cleaning
parse_quarter_date <- function(colname) {
  date_part <- sub("_Index.*", "", colname)
  date_part <- gsub("\\s+", " ", date_part)
  date_part <- gsub("\\.", "", date_part)
  parts <- strsplit(date_part, " ")[[1]]
  month_str <- parts[1]
  year_str <- parts[2]
  month_map <- c(
    Jan = 1, Feb = 2, Mar = 3, Apr = 4,
    May = 5, Jun = 6, Jul = 7, Aug = 8,
    Sep = 9, Oct = 10, Nov = 11, Dec = 12
  )
  if (!month_str %in% names(month_map)) return(NA)
  month_num <- month_map[[month_str]]
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
LSCI_annual <- LSCI_long[, .(LSCI_annual = mean(LSCI_value, na.rm = TRUE)), by = 
year]
###Constructing the index####
nightlight_list <- list()
for (file in raster_files) {
  r <- rast(file)
  year <- as.integer(gsub("[^0-9]", "", basename(file)))
  nightlight_vals <- terra::extract(r, vect(admin2_in_coastal_admin1), fun=mean, 
na.rm=TRUE)[,2]
  tmp_dt <- data.table(
    ADM2_CODE = admin2_in_coastal_admin1$ADM2_CODE,
    ADM2_NAME = admin2_in_coastal_admin1$ADM2_NAME,
    year = year,
    nightlight_mean = nightlight_vals
  )
  nightlight_list[[as.character(year)]] <- tmp_dt
}
nightlight_dt <- rbindlist(nightlight_list)
#first merge
coastal_buffer_admin2 <- merge(nightlight_dt, coastal_buffer_admin2 , by = 
"ADM2_CODE", all.x = TRUE)
coastal_buffer_admin2[, ADM2_NAME.y := NULL]
final_df <- merge(coastal_buffer_admin2, LSCI_annual, by = "year", all.x = TRUE)
#normalize distance
final_df$export_proxy <- 1 / (final_df$dist_min_port + 1)
#skeleton of panel data
years <- unique(LSCI_annual$year)
adm2s <- unique(coastal_admin2_master$ADM2_NAME)
panel_skeleton <- expand.grid(ADM2_NAME = adm2s, year = years)
print(paste("Scheletro creato. Numero di righe:", nrow(panel_skeleton)))
final_regression_data <- merge(panel_skeleton, LSCI_annual, by = "year", all.x = 
TRUE)
print(paste("Dopo unione con LSCI_annual. Numero di righe:", 
nrow(final_regression_data)))
final_regression_data <- merge(final_regression_data, ContPortThroughput_long[, 
c("year", "shock")], by = "year", all.x = TRUE)
print(paste("Dopo unione con ContPortThroughput_long. Numero di righe:", 
nrow(final_regression_data)))
final_regression_data <- merge(final_regression_data, nightlight_dt, by = 
c("ADM2_NAME", "year"), all.x = TRUE)
print(paste("Dopo unione con nightlight_annual. Numero di righe:", 
nrow(final_regression_data)))
final_regression_data <- merge(final_regression_data, conflicts_aggregated, by = 
c("ADM2_NAME", "year"), all.x = TRUE)
print(paste("Dopo unione con conflicts_aggregated. Numero di righe:", 
nrow(final_regression_data)))
setDT(final_df)
norm_vals <- final_df |> 
  dplyr::select(ADM2_NAME, dist_min_norm, dist_min_port) |> 
  dplyr::distinct()
norm_vals[, names(norm_vals)[grepl("\\.y$", names(norm_vals))] := NULL]
x_cols <- names(norm_vals)[grepl("\\.x$", names(norm_vals))]
setnames(norm_vals, old = x_cols, new = gsub("\\.x$", "", x_cols))
# adding coastal buffer admin2
setDT(coastal_buffer_admin2)
coastal_buffer_admin2[, names(coastal_buffer_admin2)[grepl("\\.y$", 
names(coastal_buffer_admin2))] := NULL]
coastal_buffer_admin2 <- coastal_buffer_admin2 |> 
  left_join(norm_vals, by = "ADM2_NAME")
x_cols <- names(coastal_buffer_admin2)[grepl("\\.x$", 
names(coastal_buffer_admin2))]
setnames(coastal_buffer_admin2, old = x_cols, new = gsub("\\.x$", "", x_cols))
dist_one <- as.data.table(st_drop_geometry(coastal_buffer_admin2))[
  , .(dist_min_port = min(as.numeric(dist_min_port), na.rm = TRUE),
      dist_min_norm = first(dist_min_norm)), by = ADM2_NAME]
final_regression_data <- dist_one[final_regression_data, on = "ADM2_NAME"]
print(paste("Dopo unione con i dati geografici. Numero di righe:", 
nrow(final_regression_data)))
setDT(final_regression_data)
# adding epsilon to avoid division for 0 
epsilon <- 0.000001
final_regression_data[is.na(n_conflicts), n_conflicts := 0]
final_regression_data <- final_regression_data %>%
  mutate(
    indice2 = (1 / (dist_min_norm + epsilon)) * shock * lsci_value
  )
###index only for main ports ####
main_ports <- c("LAGOS", "TIN CAN ISLAND", "PORT HARCOURT", 
#filtering
                "ONNE", "WARRI", "CALABAR")
ports_main_sf <- ports_sf %>%
  filter(MAIN_PORT_ %in% main_ports)
ports_main_proj <- st_transform(ports_main_sf, 32631)
# Buffer
ports_buffer_main <- st_buffer(ports_main_proj, dist = 1000000)  
ports_buffer_main <- st_make_valid(ports_buffer_main)
admin2_proj_main <- st_transform(admin2_in_coastal_admin1, 
st_crs(ports_buffer_main))
coastal_buffer_admin2_main <- admin2_proj_main %>%
  filter(st_intersects(admin2_proj_main, ports_buffer_main, sparse = FALSE)[,1])
dist_ports_main <- st_distance(coastal_buffer_admin2_main, ports_main_proj)
coastal_buffer_admin2_main$dist_min_port_main <- 
apply(as.matrix(dist_ports_main), 1, min)/1000
coastal_admin2_master_main <- merge(
  coastal_admin2_master,
  st_drop_geometry(coastal_buffer_admin2_main[, c("ADM2_NAME", 
"dist_min_port_main")]),
  by = "ADM2_NAME",
  all.x = TRUE
)
#normalizing distances
coastal_admin2_master_main <- coastal_admin2_master_main %>%
  mutate(
    dist_min_norm_main = (dist_min_port_main - min(dist_min_port_main, na.rm = 
TRUE)) /
      (max(dist_min_port_main, na.rm = TRUE) - min(dist_min_port_main, na.rm = 
TRUE))
  )
coastal_buffer_admin2_main <- merge(
  nightlight_dt, 
  coastal_buffer_admin2_main, 
  by = "ADM2_CODE", 
  all.x = TRUE
)
coastal_buffer_admin2_main[, ADM2_NAME.y := NULL]
final_df_main <- merge(coastal_buffer_admin2_main, LSCI_annual, by = "year", 
all.x = TRUE)
final_df_main$export_proxy_main <- 1 / (final_df_main$dist_min_port_main + 1)
final_df_main <- final_df_main %>%
  mutate(
    dist_min_norm_main = (dist_min_port_main - min(dist_min_port_main, na.rm = 
TRUE)) /
      (max(dist_min_port_main, na.rm = TRUE) - min(dist_min_port_main, na.rm = 
TRUE)),
    export_proxy_main  = ifelse(is.na(export_proxy_main), 0, export_proxy_main)
  )
panel_list_main <- list()
for (t in LSCI_annual$year) {
  lsci_t  <- LSCI_annual[year == t, lsci_value][1]
  shock_t <- ContPortThroughput_long[year == t, shock][1]
  if (is.na(shock_t)) shock_t <- 0
  tmp <- final_df_main %>%
    mutate(
      year      = t,
      shock     = shock_t,
      Exposure_main = lsci_t * shock_t * dist_min_norm_main,
      indice2_main  = lsci_t * shock_t * export_proxy_main
    )
  panel_list_main[[as.character(t)]] <- tmp
}
panel_data_main <- bind_rows(panel_list_main)
setDT(panel_data_main)
panel_data_main[, names(panel_data_main)[grepl("\\.y$", names(panel_data_main))] 
:= NULL]
x_cols_main <- names(panel_data_main)[grepl("\\.x$", names(panel_data_main))]
setnames(panel_data_main, old = x_cols_main, new = gsub("\\.x$", "", 
x_cols_main))
years <- unique(LSCI_annual$year)
adm2s <- unique(coastal_admin2_master_main$ADM2_NAME)  # <-- usa la versione 
_main
panel_skeleton_main <- expand.grid(ADM2_NAME = adm2s, year = years)
final_regression_data_main <- merge(panel_skeleton_main, LSCI_annual, by = 
"year", all.x = TRUE)
final_regression_data_main <- merge(final_regression_data_main, 
ContPortThroughput_long[, c("year", "shock")], by = "year", all.x = TRUE)
final_regression_data_main <- merge(final_regression_data_main, nightlight_dt, 
by = c("ADM2_NAME", "year"), all.x = TRUE)
final_regression_data_main <- merge(final_regression_data_main, 
conflicts_aggregated, by = c("ADM2_NAME", "year"), all.x = TRUE)
final_regression_data_main <- merge(final_regression_data_main, 
st_drop_geometry(coastal_admin2_master_main[, c("ADM2_NAME", 
"dist_min_port_main", "dist_min_norm_main")]), by = "ADM2_NAME", all.x = TRUE)
setDT(final_regression_data_main)
final_regression_data_main[is.na(n_conflicts), n_conflicts := 0]
epsilon <- 1e-6
final_regression_data_main <- final_regression_data_main %>%
  mutate(
    indice2_main =( (1 / (dist_min_norm_main + epsilon)) * shock * 
lsci_value)/100000,
    indice_main  =          (    dist_min_norm_main          ) * shock * 
lsci_value
  )
# creation of index for falsification test: remove the main ports 
main_ports <- c("LAGOS", "TIN CAN ISLAND", "PORT HARCOURT", 
                "ONNE", "WARRI", "CALABAR")
ports_nonmain_sf <- ports_sf %>%
  filter(!MAIN_PORT_ %in% main_ports)
#control
stopifnot(nrow(ports_nonmain_sf) > 0)
ports_nonmain_proj <- st_transform(ports_nonmain_sf, 32631)
admin2_proj_all    <- st_transform(admin2_in_coastal_admin1, 
st_crs(ports_nonmain_proj))
#min distance
dist_ports_nonmain <- st_distance(admin2_proj_all, ports_nonmain_proj)  # in 
metri
dist_min_port_nonmain_km <- apply(as.matrix(dist_ports_nonmain), 1, min) / 1000
admin2_with_nonmain <- admin2_proj_all %>%
  mutate(dist_min_port_nonmain = dist_min_port_nonmain_km)
coastal_admin2_master_nonmain <- merge(
  coastal_admin2_master,
  st_drop_geometry(admin2_with_nonmain[, c("ADM2_NAME", 
"dist_min_port_nonmain")]),
  by = "ADM2_NAME",
  all.x = TRUE
)
coastal_admin2_master_nonmain <- coastal_admin2_master_nonmain %>%
  mutate(
    dist_min_norm_nonmain = (dist_min_port_nonmain - min(dist_min_port_nonmain, 
na.rm = TRUE)) /
      (max(dist_min_port_nonmain, na.rm = TRUE) - min(dist_min_port_nonmain, 
na.rm = TRUE)),
    export_proxy_nonmain = 1 / (dist_min_port_nonmain + 1)  # stessa logica 
dell'export_proxy_main
  )
#panel
final_df_nonmain <- merge(
  nightlight_dt,
  st_drop_geometry(coastal_admin2_master_nonmain[, 
c("ADM2_NAME","dist_min_port_nonmain","dist_min_norm_nonmain","export_proxy_nonm
ain")]),
  by = "ADM2_NAME",
  all.x = TRUE
)
final_df_nonmain <- merge(final_df_nonmain, LSCI_annual, by = "year", all.x = 
TRUE)
panel_list_nonmain <- list()
for (t in LSCI_annual$year) {
  lsci_t  <- LSCI_annual[year == t, lsci_value][1]
  shock_t <- ContPortThroughput_long[year == t, shock][1]
  if (is.na(shock_t)) shock_t <- 0
  tmp <- final_df_nonmain %>%
    mutate(
      year               = t,
      shock              = shock_t,
      Exposure_nonmain   = lsci_t * shock_t * dist_min_norm_nonmain,
      indice2_nonmain    = lsci_t * shock_t * export_proxy_nonmain
    )
  panel_list_nonmain[[as.character(t)]] <- tmp
}
panel_data_nonmain <- bind_rows(panel_list_nonmain)
setDT(panel_data_nonmain)
panel_data_nonmain[, names(panel_data_nonmain)[grepl("\\.y$", 
names(panel_data_nonmain))] := NULL]
x_cols_nonmain <- names(panel_data_nonmain)[grepl("\\.x$", 
names(panel_data_nonmain))]
setnames(panel_data_nonmain, old = x_cols_nonmain, new = gsub("\\.x$", "", 
x_cols_nonmain))
#index
years <- unique(LSCI_annual$year)
adm2s  <- unique(coastal_admin2_master_nonmain$ADM2_NAME)
panel_skeleton_nonmain <- expand.grid(ADM2_NAME = adm2s, year = years)
final_regression_data_nonmain <- merge(panel_skeleton_nonmain, LSCI_annual, by = 
"year", all.x = TRUE)
final_regression_data_nonmain <- merge(final_regression_data_nonmain, 
ContPortThroughput_long[, c("year","shock")], by = "year", all.x = TRUE)
final_regression_data_nonmain <- merge(final_regression_data_nonmain, 
nightlight_dt, by = c("ADM2_NAME","year"), all.x = TRUE)
final_regression_data_nonmain <- merge(final_regression_data_nonmain, 
conflicts_aggregated, by = c("ADM2_NAME","year"), all.x = TRUE)
final_regression_data_nonmain <- merge(
  final_regression_data_nonmain,
  st_drop_geometry(coastal_admin2_master_nonmain[, 
c("ADM2_NAME","dist_min_port_nonmain","dist_min_norm_nonmain","export_proxy_nonm
ain")]),
  by = "ADM2_NAME", all.x = TRUE
)
setDT(final_regression_data_nonmain)
final_regression_data_nonmain[is.na(n_conflicts), n_conflicts := 0]
# Coerenza con la definizione usata per i MAIN
epsilon <- 1e-6
#LOG nightlights for both index 
#trade ports 
final_regression_data_main <- final_regression_data_main %>%
  mutate(log_nightlight = log(nightlight_mean + 1))  # +1 per evitare log(0)
#dummy high
final_regression_data_main <- final_regression_data_main %>%
  group_by(year) %>%
  mutate(q3_log_nl = quantile(log_nightlight, 0.75, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(high_nl = ifelse(log_nightlight > q3_log_nl, 1, 0))
#interaction with index
final_regression_data_main <- final_regression_data_main %>%
  mutate(
    indice2_highNL = indice2_main * high_nl,
    indice2_lowNL  = indice2_main * (1 - high_nl)
  )
#non trade ports
final_regression_data_nonmain <- final_regression_data_nonmain %>%
  mutate(
    indice2_nonmain = lsci_t * shock_t * export_proxy_nonmain
  )
final_regression_data_nonmain <- final_regression_data_nonmain %>%
  mutate(
    indice2_nonmain = indice2_nonmain
  )
final_regression_data_nonmain <- final_regression_data_nonmain %>%
  mutate(log_nightlight = log(nightlight_mean + 1)) 
final_regression_data_nonmain <- final_regression_data_nonmain %>%
  group_by(year) %>%
  mutate(q3_log_nl = quantile(log_nightlight, 0.75, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    high_nl = ifelse(log_nightlight > q3_log_nl, 1, 0),
    indice2_highNL_nonmain = indice2_nonmain * high_nl,
    indice2_lowNL_nonmain  = indice2_nonmain * (1 - high_nl)
  )
###regression####
#trade port 
#baseline model 
model_base <- feols(
  n_conflicts ~ indice2_main + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main
)
summary(model_base)
#interaction with high and low nightlights
model_interaction2 <- feols(
  n_conflicts ~ indice2_highNL + indice2_lowNL + log_nightlight | ADM2_NAME + 
year,
  data = final_regression_data_main
)
summary(model_interaction2)
##falsification test
#baseline model
model_base_nonmain <- feols(
  n_conflicts ~ indice2_nonmain + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_nonmain, cluster = ~ ADM2_NAME
)
model_interaction2_nonmain <- feols(
  n_conflicts ~ indice2_highNL_nonmain + indice2_lowNL_nonmain + log_nightlight 
| ADM2_NAME + year,
  data = final_regression_data_nonmain, cluster = ~ ADM2_NAME
)
####Heterogeneity####
#geographical 
adm <- coastal_admin2_master_main %>%
  st_make_valid() %>%
  st_transform(32631) %>%                           # WGS84 / UTM 31N (metrico)
  mutate(centroid_m = st_point_on_surface(geometry))%>% # nessun warning qui
  st_transform(4326) %>%                             # torna a lon/lat
  mutate(
    lon = st_coordinates(centroid_m)[,1],
    lat = st_coordinates(centroid_m)[,2]
  )
# 2) Nord/Sud "coast-aware"
fit_coast <- lm(lat ~ poly(lon, 2, raw = TRUE), data = adm)   # curva lat~lon
adm <- adm %>% mutate(
  lat_pred = predict(fit_coast, newdata = data.frame(lon = lon)),
  macro_NS = if_else(lat > lat_pred, "Nord", "Sud")
)
# 3) Est/Ovest
lon_med <- median(adm$lon, na.rm = TRUE)
adm <- adm %>% mutate(macro_EW = if_else(lon <= lon_med, "Ovest", "Est"))
#check rapido
print(table(adm$macro_NS, adm$macro_EW))
# 4) Join with the panel
adm_u <- adm %>% st_drop_geometry() %>%
  dplyr::select(ADM2_CODE, macro_NS, macro_EW) %>%
  mutate(ADM2_CODE = as.character(ADM2_CODE))
final_regression_data_main <- final_regression_data_main %>%
  mutate(ADM2_CODE = as.character(ADM2_CODE)) %>%
  dplyr::select(-matches("^macro_"), -matches("^indice2_(nord|sud|est|ovest)$"), 
everything()) %>%
  left_join(adm_u, by = "ADM2_CODE")
#check
print(table(final_regression_data_main$macro_NS, 
final_regression_data_main$macro_EW))
#regression
mod_NS <- feols(
  n_conflicts ~ i(macro_NS, indice2_main) + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main, cluster = ~ ADM2_NAME
)
mod_EW <- feols(
  n_conflicts ~ i(macro_EW, indice2_main) + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main, cluster = ~ ADM2_NAME
)
summary(mod_NS); summary(mod_EW)
#sectorial
chem_first <- c("LAGOS", "TIN CAN ISLAND", "WARRI")         
veg_first <- c("PORT HARCOURT", "ONNE", "CALABAR")        
port_meta <- tibble::tibble(
  MAIN_PORT_ = c(veg_first, chem_first),
  port_sector = c(rep("Vegetables-first", length(veg_first)),
                  rep("Chemicals-first", length(chem_first)))
)
ports_tagged <- ports_main_proj %>%
  left_join(port_meta, by = "MAIN_PORT_")
adm2_cent <- coastal_admin2_master_main %>%
  st_transform(st_crs(ports_tagged)) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  dplyr::select(ADM2_NAME, geometry = centroid)
nearest_id <- st_nearest_feature(adm2_cent, ports_tagged)
adm2_port <- adm2_cent %>%
  mutate(
    nearest_port = ports_tagged$MAIN_PORT_[nearest_id],
    port_sector  = ports_tagged$port_sector[nearest_id]
  ) %>%
  st_drop_geometry()
#join with the panel
df_sector <- final_regression_data_main %>%
  left_join(adm2_port[, c("ADM2_NAME","nearest_port","port_sector")], by = 
"ADM2_NAME") %>%
  mutate(
    port_sector = ifelse(is.na(port_sector), "Unknown", port_sector)
  )
m_sector <- feols(
  n_conflicts ~ i(port_sector, indice2_main) + log_nightlight |
    ADM2_NAME + year,
  data = df_sector,
  cluster = ~ ADM2_NAME   # resta coerente con ADM2 come vuoi tu
)
summary(m_sector)
##trade flow 
import_ports <- c("ONNE", "CALABAR")
export_ports <- c("LAGOS", "TIN CAN ISLAND", "PORT HARCOURT", "WARRI")
port_flow <- tibble::tibble(
  MAIN_PORT_ = c(import_ports, export_ports),
  flow_group = c(rep("Import-oriented", length(import_ports)),
)
                 rep("Export-oriented", length(export_ports)))
ports_tagged2 <- ports_main_proj %>% left_join(port_flow, by = "MAIN_PORT_")
nearest_id2 <- st_nearest_feature(adm2_cent, ports_tagged2)
adm2_flow <- adm2_cent %>%
  mutate(flow_group = ports_tagged2$flow_group[nearest_id2]) %>%
  st_drop_geometry()
df_flow <- final_regression_data_main %>%
  left_join(adm2_flow[, c("ADM2_NAME","flow_group")], by = "ADM2_NAME")
m_flow <- feols(
  n_conflicts ~ i(flow_group, indice2_main) + log_nightlight |
    ADM2_NAME + year,
  data = df_flow, cluster = ~ ADM2_NAME
)
summary(m_flow)
##tribes 
tribes <- read_sf("tribalmap.shp"); tribes
st_crs(tribes)
st_crs(nig)
tribes_wgs84 <- sf::st_transform(tribes, sf::st_crs(admin2_in_coastal_admin1))
nt <- st_intersection(
  st_make_valid(tribes_wgs84),
  st_make_valid(admin2_in_coastal_admin1)
)
# weight the tribal units
nt <- nt %>%
  mutate(
    area_int = as.numeric(st_area(geometry))
  )
stopifnot(all(c("ADM2_NAME") %in% names(nt)))
tribe_var <- "TRIBE_CODE"
#tribal shares
tribal_shares <- nt %>%
  st_drop_geometry() %>%
  group_by(ADM2_NAME, !!sym(tribe_var)) %>%
  summarise(area = sum(area_int, na.rm = TRUE), .groups = "drop") %>%
  group_by(ADM2_NAME) %>%
  mutate(
    area_tot = sum(area, na.rm = TRUE),
    share    = ifelse(area_tot > 0, area / area_tot, NA_real_)
  ) %>%
  ungroup()
# indexes
tribal_index_admin2 <- tribal_shares %>%
  group_by(ADM2_NAME) %>%
  summarise(
    # quota della tribù dominante
    tribal_share_dom = max(share, na.rm = TRUE),
    # fractionalization = 1 - sum(s_i^2)
    tribal_frac      = 1 - sum(share^2, na.rm = TRUE),
    .groups = "drop"
  )
#regression
df <- final_regression_data_main %>%
  left_join(tribal_index_admin2, by = "ADM2_NAME") %>%
  mutate(
    # interazioni dell'Index con tribù in Low-NL e High-NL
    idxL_dom = indice2_lowNL  * tribal_share_dom,
    idxH_dom = indice2_highNL * tribal_share_dom,
    idxL_frac = indice2_lowNL  * tribal_frac,
    idxH_frac = indice2_highNL * tribal_frac
  )
# --- 3) Modello con dominance: eterogeneità sia in Low che in High -----------
m_tr_dom <- feols(
  n_conflicts ~ indice2_lowNL + indice2_highNL + idxL_dom + idxH_dom + 
log_nightlight |
    ADM2_NAME + year,
  data = df, cluster = ~ ADM2_NAME
)
# --- 4) Modello con fractionalization: idem ----------------------------------
m_tr_frac <- feols(
  n_conflicts ~ indice2_lowNL + indice2_highNL + idxL_frac + idxH_frac + 
log_nightlight |
    ADM2_NAME + year,
  data = df, cluster = ~ ADM2_NAME
)
summary(m_tr_dom)
summary(m_tr_frac)
#water 
r <- rast(paste0(getwd(), "/wat/faowat00.tif"))
nig <- st_read("nga_polnda_adm2_1m_salb.shp")
nig <- st_transform(nig, crs(r))
r_crop <- crop(r, vect(nig))
r_mask <- mask(r_crop, vect(nig))
#mean
vals <- terra::extract(r_mask, vect(nig), fun=mean, na.rm=TRUE)
# dataframe
theme1_dt <- data.table(
  ADM2_NAME = nig$ADM2_NAME,
  water_avail = vals[,2]
)
#merge with panel
final_regression_data_main <- merge(
  final_regression_data_main,
  theme1_dt,
  by="ADM2_NAME",
  all.x=TRUE
)
#regression
df_water <- final_regression_data_main %>%
  mutate(high_water = ifelse(water_avail > median(water_avail, na.rm=TRUE), 1, 
0))
df_water <- final_regression_data_main %>%
  mutate(
    high_water = ifelse(
      water_avail > median(water_avail, na.rm = TRUE), 1, 0
    )
  )
q <- unique(quantile(final_regression_data_main$water_avail, probs = 
c(.333,.666), na.rm=TRUE))
df_water <- final_regression_data_main %>%
  mutate(
    water_terc = ntile(water_avail, 3),   # 1 = basso, 2 = medio, 3 = alto
    water_terc = factor(water_terc, labels = c("Bassa","Media","Alta"))
  )
m_water_terc <- feols(
  n_conflicts ~ indice2_main * i(water_terc, ref = "Bassa") + log_nightlight |
    ADM2_NAME + year,
  data = df_water,
  cluster = ~ADM2_NAME
)
summary(m_water_terc)
