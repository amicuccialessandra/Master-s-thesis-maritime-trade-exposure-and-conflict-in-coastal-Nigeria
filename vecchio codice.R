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



afr_names

sub_shp <- subset(shp, ISO3 %in% afr_names$iso3c)

# Subset the sub_shp object to include only Nigeria (ISO3 code "NGA")

shp <- shp %>% filter(NAME %in% "nigeria")



st_crs(shp)



#admin2 level

nig <- read_sf(paste0(getwd(), "/nga_polnda_adm2_1m_salb.shp"))



st_crs(nig)

# Plot the map of Nigeria

ggplot() + geom_sf(data = shp)



ggplot() + geom_sf(data = shp %>%
                     
                     st_transform("+init=EPSG:26393"))

shp <- shp %>%
  
  st_transform("+init=EPSG:26393")



#plot at admin2 level

ggplot() + geom_sf(data = nig)



ggplot() + geom_sf(data = nig %>%
                     
                     st_transform("+init=EPSG:26393"))

nig <- nig %>%
  
  st_transform("+init=EPSG:26393")



#creating a grid

grid <- st_make_grid(nig, cellsize = 100000) # takes shp and returns a grid



ggplot() + geom_sf(data = nig) + geom_sf(data = grid, fill = NA)



# We need to remove the part of squares not perfectly overlapping with shp

grid <- grid %>% st_intersection(nig) # overlapping squares, missing values

ggplot() + geom_sf(data = nig) + geom_sf(data = grid, fill = NA)



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



# Plot using base R

plot(ports_data)



# Or use ggplot2 for a nicer map

ggplot(data = ports_data) +
  
  geom_sf() +
  
  theme_minimal() +
  
  ggtitle("Nigeria Ports Map")



ports_data <- st_transform(ports_data, "+init=EPSG:26393")

ports_data <- st_transform(ports_data, st_crs(nig))



# Plot everything

ggplot() +
  
  geom_sf(data = nig, fill = "gray90", color = "black") + # Admin2 map
  
  geom_sf(data = grid, fill = NA, color = "black", linewidth = 0.2) + # Grid overlay
  
  geom_sf(data = ports_data, color = "red", size = 2) + # Ports as red dots
  
  theme_minimal() +
  
  ggtitle("Nigerian Ports with Admin2 Boundaries and Grid")





nigeria_boundary <- st_boundary(st_union(nig))



# Trova i poligoni admin2 che toccano questo confine esterno

coastal_admin2 <- nig[st_touches(nig, nigeria_boundary, sparse = FALSE)[,1], ]



# Visualizza

ggplot() +
  
  geom_sf(data = nig, fill = "gray90") +
  
  geom_sf(data = coastal_admin2, fill = "lightblue") +
  
  ggtitle("Admin2 costieri in Nigeria")



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



# Visualizza

ggplot() +
  
  geom_sf(data = nig, fill = "gray90") +
  
  geom_sf(data = coastal_admin2, fill = "lightblue") +
  
  theme_minimal() +
  
  ggtitle("Admin2 Costieri")



coastal_ADM1_NAME <- c(
  
  "Lagos", "Ogun", "Ondo", "Delta", "Bayelsa", "Rivers", "Akwa lbom", "Cross River", "Abia"
  
)



# Supponiamo che la colonna corretta sia ADM2_NAME

coastal_admin1 <- nig %>%
  
  filter(ADM1_NAME %in% coastal_ADM1_NAME)



# Visualizza

ggplot() +
  
  geom_sf(data = nig, fill = "gray90") +
  
  geom_sf(data = coastal_admin1, fill = "lightblue") +
  
  theme_minimal() +
  
  ggtitle("Admin2 Costieri")





admin2_in_coastal_admin1 <- st_join(nig, coastal_admin1, join = st_within, left = FALSE)



# 'left = FALSE' tiene solo i record di nig che effettivamente cadono dentro coastal_admin1



# 3. Visualizza

ggplot() +
  
  geom_sf(data = nig, fill = "gray90") +
  
  geom_sf(data = admin2_in_coastal_admin1, fill = "lightblue") +
  
  theme_minimal() +
  
  ggtitle("Admin2 dentro Admin1 costieri")



ggplot() +
  
  geom_sf(data = admin2_in_coastal_admin1, fill = "gray90", color = "black") + # Admin2 map
  
  geom_sf(data = ports_data, color = "red", size = 2) + # Ports as red dots
  
  theme_minimal() +
  
  ggtitle("Nigerian Ports with Admin2 Boundaries and Grid")



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

ports_buffer <- st_buffer(ports_proj, dist = 1000000) # buffer di 1000km



# Assicurati che il buffer sia valido

ports_buffer <- st_make_valid(ports_buffer)



# Se admin2_in_coastal_admin1 è in un CRS diverso, trasformalo

admin2_proj <- st_transform(admin2_in_coastal_admin1, st_crs(ports_buffer))





coastal_buffer_admin2 <- admin2_proj %>%
  
  filter(st_intersects(admin2_proj, ports_buffer, sparse = FALSE)[,1])



# Calcola la distanza minima da ogni admin2 ai porti

dist_ports <- st_distance(coastal_buffer_admin2, ports_proj)

coastal_buffer_admin2$dist_min_port <- apply(as.matrix(dist_ports), 1, min)



ggplot() +
  
  geom_sf(data = coastal_buffer_admin2, aes(fill = dist_min_port), color = "black", size = 0.2) +
  
  scale_fill_viridis_c(option = "plasma", name = "Distanza min. porto (m)") +
  
  labs(title = "Distanza minima tra aree amministrative costiere e porti",
       
       subtitle = "Buffer di 10000 km intorno ai porti",
       
       caption = "Fonte dati: admin2 e porti") +
  
  theme_minimal()





#conflict

conflict <- read_excel("Cartel2.xlsx")

rapacity_sub_events <- c(
  
  "Looting/property destruction",
  
  "Abduction/forced disappearance",
  
  "Armed clash",
  
  "Attack",
  
  "Shelling/artillery/missile attack",
  
  "Sexual violence" # spesso associata a violenza predatoria
  
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
  
  "Other" # da valutare caso per caso
  
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



# Check the CRS to ensure they match

print(st_crs(grid_transformed))

print(st_crs(conflict_sf_transformed))



# Check a few entries from conflict_sf_transformed

head(conflict_sf_transformed)



# Check the bounding box of both datasets to ensure overlap

print(st_bbox(grid_transformed))

print(st_bbox(conflict_sf_transformed))



# Print the original CRS of conflict_sf

print(st_crs(conflict_sf))



conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393)



# Verify the bounding box after transformation

print(st_bbox(conflict_sf_transformed))



print(st_crs(conflict_sf))



# If the original CRS is not identified correctly, set it to the known CRS (e.g., EPSG:4326)



st_crs(conflict_sf) <- 4326 # Set this to the correct original CRS if known



# Step 2: Re-transform conflict_sf to EPSG:26393

conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393)

# Verify the bounding box after transformation

print(st_bbox(conflict_sf_transformed))



# Step 3: Perform spatial join and summarize

conflict_sf1 <- st_join(grid_transformed, conflict_sf_transformed) %>%
  
  group_by(FIP, year) %>%
  
  summarise(n_conflicts = n(), .groups = 'drop')



# Check the resulting data

head(conflict_sf1)

conflict_sf_transformed1 <- conflict_sf_transformed %>%
  
  mutate(conflict_id = row_number()) # Create a unique identifier



# Perform the spatial join and ensure conflict_id is retained



conflict_sf3 <- st_join(grid_transformed, conflict_sf_transformed) %>%
  
  group_by(FIP, year, event_type,fatalities,event_id_cnty) %>%
  
  summarise(n_conflicts = n())



#plotting conflict

pc_2009 <- ggplot()+
  
  geom_sf(data = nig, fill = NA)+
  
  geom_sf(data = conflict_sf%>%filter(year==2009))+
  
  labs(title = "Conflict in Nigeria - 2009") +
  
  theme_minimal()



pc_2012 <- ggplot()+
  
  geom_sf(data = nig, fill = NA)+
  
  geom_sf(data = conflict_sf%>%filter(year==2012))+
  
  labs(title = "Conflict in Nigeria - 2012") +
  
  theme_minimal()

pc_2015 <-ggplot()+
  
  geom_sf(data = nig, fill = NA)+
  
  geom_sf(data = conflict_sf%>%filter(year==2015))+
  
  labs(title = "Conflict in Nigeria - 2015") +
  
  theme_minimal()

pc_2019 <- ggplot()+
  
  geom_sf(data = nig, fill = NA)+
  
  geom_sf(data = conflict_sf%>%filter(year==2019))+
  
  labs(title = "Conflict in Nigeria - 2019") +
  
  theme_minimal()



p_combined4 <- pc_2009+ pc_2012 + pc_2015 + pc_2019 +
  
  plot_layout(ncol = 2, nrow = 2)

p_combined4







coastal_admin2 <- st_transform(admin2_in_coastal_admin1, crs = st_crs(conflict_sf))

within_matrix <- st_within(conflict_sf, coastal_admin2, sparse = FALSE)

# within_matrix è una matrice: righe = punti, colonne = poligoni costieri



# Per ogni punto, verifica se è dentro almeno un poligono

inside_any <- apply(within_matrix, 1, any)



# Ora puoi filtrare i punti

conflict_coastal <- conflict_sf[inside_any, ]





ggplot() +
  
  geom_sf(data = coastal_admin2, fill = "lightblue", color = "black") + # Poligoni costieri
  
  geom_sf(data = conflict_coastal, color = "red", size = 1, alpha = 0.7) + # Punti conflitti
  
  theme_minimal() +
  
  ggtitle("Conflitti nella zona costiera")

pc_2009 <- ggplot()+
  
  geom_sf(data = coastal_admin2, fill = NA)+
  
  geom_sf(data = conflict_coastal%>%filter(year==2009), color = "red", size = 1, alpha = 0.7)+
  
  labs(title = "Conflict in Nigeria - 2009") +
  
  theme_minimal()



pc_2012 <- ggplot()+
  
  geom_sf(data = coastal_admin2, fill = NA)+
  
  geom_sf(data = conflict_coastal%>%filter(year==2012), color = "red", size = 1, alpha = 0.7)+
  
  labs(title = "Conflict in Nigeria - 2012") +
  
  theme_minimal()

pc_2015 <-ggplot()+
  
  geom_sf(data = coastal_admin2, fill = NA)+
  
  geom_sf(data = conflict_coastal%>%filter(year==2015), color = "red", size = 1, alpha = 0.7)+
  
  labs(title = "Conflict in Nigeria - 2015") +
  
  theme_minimal()

pc_2019 <- ggplot()+
  
  geom_sf(data = coastal_admin2, fill = NA)+
  
  geom_sf(data = conflict_coastal%>%filter(year==2019), color = "red", size = 1, alpha = 0.7)+
  
  labs(title = "Conflict in Nigeria - 2019") +
  
  theme_minimal()



p_combined <- pc_2009+ pc_2012 + pc_2015 + pc_2019 +
  
  plot_layout(ncol = 2, nrow = 2)

p_combined



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



library(terra)

library(sf)



# 1. Carica shapefile Nigeria



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
  
  plot(r_mask) # se vuoi vedere
  
  year <- gsub("[^0-9]", "", basename(file)) # estrae l'anno dal nome
  
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
  
  # Estrai "May 2009" da "May 2009_Index_Average..."
  
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

LSCI_long[, LSCI_value := as.numeric(LSCI_value)] # assicura che sia numerico

LSCI_annual <- LSCI_long[, .(LSCI_annual = mean(LSCI_value, na.rm = TRUE)), by = year]



nightlight_list <- list()



for (file in raster_files) {
  
  r <- rast(file)
  
  year <- as.integer(gsub("[^0-9]", "", basename(file)))
  
  
  nightlight_vals <- terra::extract(r, vect(admin2_in_coastal_admin1), fun=mean, na.rm=TRUE)[,2]
  
  
  tmp_dt <- data.table(
    
    ADM2_ID = admin2_in_coastal_admin1$ADM2_CODE.x,
    ADM2_NAME = admin2_in_coastal_admin1$ADM2_NAME.x,
    
    year = year,
    
    nightlight_mean = nightlight_vals
    
  )
  
  
  nightlight_list[[as.character(year)]] <- tmp_dt
  
}



nightlight_dt <- rbindlist(nightlight_list)



# 3. Distanza porti (assumendo costante nel tempo)

dist_ports_dt <- data.table(
  
  ADM2_ID = coastal_buffer_admin2$ADM2_CODE.x,
  ADM2_NAME = admin2_in_coastal_admin1$ADM2_NAME.x,
  dist_min_port = coastal_buffer_admin2$dist_min_port
  
)







# 5. Ora devi unire questi dati per creare il dataframe finale



# Per semplicità, uniamo nightlight e distanza (quest'ultima fissa)

df_admin2_year <- merge(nightlight_dt, dist_ports_dt, by = "ADM2_ID", all.x = TRUE)



final_df <- merge(df_admin2_year, LSCI_annual, by = "year", all.x = TRUE)





library(dplyr)


final_df$export_proxy <- 1 / (final_df$dist_min_port + 1)
final_df <- final_df %>% mutate %>%
  
  mutate(
    
    dist_min_norm = (dist_min_port - min(dist_min_port, na.rm = TRUE)) / (max(dist_min_port, na.rm = TRUE) - min(dist_min_port, na.rm = TRUE)),
    
    nightlight_norm = (nightlight_mean - min(nightlight_mean, na.rm = TRUE)) / (max(nightlight_mean, na.rm = TRUE) - min(nightlight_mean, na.rm = TRUE)),
    
    export_proxy = ifelse(is.na(export_proxy), 0, export_proxy), # se l’hai già calcolato
    
    export_norm = (export_proxy - min(export_proxy, na.rm = TRUE)) / (max(export_proxy, na.rm = TRUE) - min(export_proxy, na.rm = TRUE))
    
  )
