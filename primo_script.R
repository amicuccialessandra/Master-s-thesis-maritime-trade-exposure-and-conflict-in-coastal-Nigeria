setwd("C:/Users/ale_a/OneDrive/Documenti/Tesi")

remove(list = ls())
library(data.table)
library(patchwork)
library(terra)
library(readxl)
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

dir.create("data", showWarnings = FALSE)

nig <- readRDS("data/nig.rds")
coastal_admin2_master <- readRDS("data/coastal_admin2_master.rds")
grid <- readRDS("data/grid.rds")
ports_data <- readRDS("data/ports_data.rds")
nightlight_dt <- readRDS("data/nightlight_dt.rds")
conflicts_aggregated <- readRDS("data/conflicts_aggregated.rds")
LSCI_annual <- readRDS("data/LSCI_annual.rds")
ContPortThroughput_long <- readRDS("data/ContPortThroughput_long.rds")
final_regression_data <- readRDS("data/final_regression_data.rds")



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

# Plot the map of Nigeria
ggplot() + geom_sf(data = shp %>%
                     st_transform("+init=EPSG:26393"))
shp <- shp %>%
  st_transform("+init=EPSG:26393")

#plot at admin2 level
nigeria <- ggplot() + geom_sf(data = nig %>%
                     st_transform("+init=EPSG:26393")) +
          labs(title = "Map of Nigeria")
  
nig <- nig %>%
  st_transform("+init=EPSG:26393")

ggsave("nig.pdf", # Save as PDF
       plot = nigeria,
       width = 10,                 # Width in inches
       height = 8,                 # Height in inches
       dpi = 300)                  # DPI is less critical for vector graphics, but good practice

#creating a grid
grid <- st_make_grid(nig, cellsize = 100000) # takes shp and returns a grid

ggplot() + geom_sf(data = nig) + geom_sf(data = grid, fill = NA)

# We need to remove the part of squares not perfectly overlapping with shp
grid <- grid %>% st_intersection(nig) # overlapping squares, missing values
nigeria_map <- ggplot() + geom_sf(data = nig) + geom_sf(data = grid, fill = NA)
ggsave("nigeria_map.pdf", # Save as PDF
       plot = nigeria_map,
       width = 10,                 # Width in inches
       height = 8,                 # Height in inches
       dpi = 300)                  # DPI is less critical for vector graphics, but good practice
# Compute the area of cells
# Remember: we can make this calculation because we are using a Projected Coordinate System
area <- st_area(grid)
area <- as.numeric(area); area
to_rm <- which(area <= 7e+08); to_rm
# Drop smallest cells
grid <- grid[-to_rm]

# In order to match the grid with other shapefiles,
# we need the grid to be a shapefile
grid <- st_as_sf(grid)
# We can also assign an id to each cell
grid <- grid %>% 
  mutate(FIP = 1:n())


# Adjust path to point to the unzipped .shp file
shapefile_path <- "ports.shp"

# Read shapefile
ports_data <- st_read(shapefile_path)

# Or use ggplot2 for a nicer map
ggplot(data = ports_data) +
  geom_sf() +
  theme_minimal() +
  ggtitle("Nigeria Ports Map")

ports_data <- st_transform(ports_data, "+init=EPSG:26393")
ports_data <- st_transform(ports_data, st_crs(nig))

# Plot everything
ggplot() +
  geom_sf(data = nig, fill = "gray90", color = "black") +  # Admin2 map
  geom_sf(data = grid, fill = NA, color = "black", linewidth = 0.2) +  # Grid overlay
  geom_sf(data = ports_data, color = "red", size = 2) +  # Ports as red dots
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
Admin2_cost <- ggplot() +
  geom_sf(data = nig, fill = "gray90") +
  geom_sf(data = admin2_in_coastal_admin1, fill = "lightblue") +
  theme_minimal() +
  ggtitle("Coastal Admin2")

ggsave("Admin2_cost.pdf", # Save as PDF
       plot = Admin2_cost,
       width = 10,                 # Width in inches
       height = 8,                 # Height in inches
       dpi = 300)                  # DPI is less critical for vector graphics, but good practice


port <- ggplot() +
  geom_sf(data = admin2_in_coastal_admin1, fill = "gray90", color = "grey") +  # Admin2 map
  geom_sf(data = ports_data, color = "red", size = 2) +  # Ports as red dots
  theme_minimal() +
  ggtitle("Nigerian Ports with Admin2 Boundaries and Grid")

ggsave("Nigerian Ports with Admin2 Boundaries and Grid.pdf", # Save as PDF
       plot = port,
       width = 10,                 # Width in inches
       height = 8,                 # Height in inches
       dpi = 300)                  # DPI is less critical for vector graphics, but good practice
####AAAAAAAAAAAAAAA#####
admin2_in_coastal_admin1 <- st_join(nig, coastal_admin1, join = st_within, left = FALSE)

coastal_admin2_master <- admin2_in_coastal_admin1

# Pulisci i nomi delle colonne in coastal_admin2_master (e quindi admin2_in_coastal_admin1)
if ("ADM2_NAME.x" %in% names(coastal_admin2_master)) {
  coastal_admin2_master <- coastal_admin2_master %>% dplyr::rename(ADM2_NAME = ADM2_NAME.x)
}

if ("ADM2_CODE.x" %in% names(coastal_admin2_master)) {
  coastal_admin2_master <- coastal_admin2_master %>% dplyr::rename(ADM2_CODE = ADM2_CODE.x)
}


# Ora, assicurati che admin2_in_coastal_admin1 sia la versione pulita per i passaggi successivi
admin2_in_coastal_admin1 <- coastal_admin2_master

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

dist_port_p <- ggplot() +
  geom_sf(data = coastal_buffer_admin2, aes(fill = dist_min_port), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Distance (m)") +
  labs(title = "Distance between coastal administrative areas and ports",
       subtitle = "10,000 km buffer around ports") +
  theme_minimal()
ggsave("distanza_porti_mappa.pdf", # Save as PDF
       plot = dist_port_p,
       width = 10,                 # Width in inches
       height = 8,                 # Height in inches
       dpi = 300)                  # DPI is less critical for vector graphics, but good practice

dist_port_p
