setwd("C:/Users/ale_a/OneDrive/Documenti/Tesi")

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
ggplot() +
  geom_sf(data = nig, fill = "gray90") +
  geom_sf(data = admin2_in_coastal_admin1, fill = "lightblue") +
  theme_minimal() +
  ggtitle("Admin2 dentro Admin1 costieri")

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

st_crs(conflict_sf) <- 4326  # Set this to the correct original CRS if known

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
  mutate(conflict_id = row_number())  # Create a unique identifier

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
  geom_sf(data = coastal_admin2, fill = "lightblue", color = "black") +  # Poligoni costieri
  geom_sf(data = conflict_coastal, color = "red", size = 1, alpha = 0.7) +  # Punti conflitti
  theme_minimal() +
  ggtitle("Conflitti nella zona costiera")
