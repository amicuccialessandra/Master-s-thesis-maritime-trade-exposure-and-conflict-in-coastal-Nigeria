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
    sub_event_type %in% non_rapacity_sub_events)


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



# Step 2: Re-transform conflict_sf to EPSG:26393
conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393)
# Verify the bounding box after transformation
print(st_bbox(conflict_sf_transformed))

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

# 1. Assegna il CRS corretto originale, cioè lat/lon (WGS84)
st_crs(conflict_sf) <- 4326

# 2. Ora trasforma le coordinate nel CRS di 'nig' (EPSG:26393)
conflict_sf <- st_transform(conflict_sf, st_crs(nig))
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
ggsave("conflict_cost.pdf", # Save as PDF
       plot = p_combined,
       width = 5,                 # Width in inches
       height = 8,                 # Height in inches
       dpi = 300)                  # DPI is less critical for vector graphics, but good practice


# Perform the spatial join to get conflicts within the master coastal ADM2s
coastal_admin2_master_proj <- st_transform(coastal_admin2_master, crs = 4326)
coastal_admin2_master_proj <- st_transform(coastal_admin2_master, crs = 26393)
st_crs(conflict_sf_transformed) <- 4326
conflict_sf_transformed <- st_transform(conflict_sf_transformed, 4326)
conflict_sf_transformed <- st_transform(conflict_sf_transformed, 26393)
# Using st_intersects for points within polygons, or st_within for stricter containment

conflict_coastal <- st_join(conflict_sf_transformed, coastal_admin2_master_proj, join = st_intersects, left = FALSE)

# 2) Porto i poligoni ADM2 in un CRS coerente.
#    Se il tuo shapefile è in EPSG:26393 (Minna / Nigeria East Belt) lo riconosco così:
st_crs(coastal_admin2_master)  # deve dare EPSG:26393
#    Per semplicità, lavoro tutto in 4326:
coastal_admin2_wgs <- st_transform(coastal_admin2_master, 4326)

# (Opzionale ma consigliato) Rendo le geometrie valide
conflict_sf <- st_make_valid(conflict_sf)
coastal_admin2_wgs <- st_make_valid(coastal_admin2_wgs)

# 3) Controlli veloci: stessi CRS e bbox sensato
st_crs(conflict_sf); st_crs(coastal_admin2_wgs)
st_bbox(conflict_sf); st_bbox(coastal_admin2_wgs)

# 4) Join spaziale: prendo SOLO i conflitti dentro gli ADM2 costieri
#    Meglio 'st_within' (punti dentro poligoni) che 'st_intersects'
conflict_coastal <- st_join(conflict_sf, coastal_admin2_wgs, join = st_within, left = FALSE)

# 5) Verifica
nrow(conflict_coastal)
head(conflict_coastal)
