library(terra)
library(sf)
library(dplyr)
library(data.table)

# 1. Carica raster acqua
r <- rast(paste0(getwd(), "/wat/faowat00.tif"))

# 2. Carica shapefile Nigeria ADM2
nig <- st_read("nga_polnda_adm2_1m_salb.shp")
nig <- st_transform(nig, crs(r))

# 3. Ritaglia e maschera
r_crop <- crop(r, vect(nig))
r_mask <- mask(r_crop, vect(nig))

# 4. Estrai media per ADM2
vals <- terra::extract(r_mask, vect(nig), fun=mean, na.rm=TRUE)

# 5. Crea dataframe
theme1_dt <- data.table(
  ADM2_NAME = nig$ADM2_NAME,
  water_avail = vals[,2]
)

# 6. Unisci al pannello
final_regression_data_main <- merge(
  final_regression_data_main,
  theme1_dt,
  by="ADM2_NAME",
  all.x=TRUE
)

# 7. Usa nei modelli
m_water <- feols(
  n_conflicts ~ indice2_main + log_nightlight + water_avail |  year,
  data=final_regression_data_main
)
summary(m_water)

m_water2 <- feols(
  n_conflicts ~ indice2_highNL_main + indice2_lowNL_main + log_nightlight + water_avail | year,
  data = final_regression_data_main
)

summary(m_water2)
df_water <- final_regression_data_main %>%
  mutate(high_water = ifelse(water_avail > median(water_avail, na.rm=TRUE), 1, 0))

m_water_het <- feols(
  n_conflicts ~ indice2_main * i(high_water) + log_nightlight | ADM2_NAME + year,
  data = df_water
)
summary(m_water_het)


df_water <- final_regression_data_main %>%
  mutate(
    high_water = ifelse(
      water_avail > median(water_avail, na.rm = TRUE), 1, 0
    )
  )

m_water_het <- feols(
  n_conflicts ~ indice2_main * i(high_water) + log_nightlight | ADM2_NAME + year,
  data = df_water,
  cluster = ~ADM2_NAME
)

summary(m_water_het)

df_water <- final_regression_data_main %>%
  mutate(
    high_water = ifelse(water_avail > median(water_avail, na.rm = TRUE), 1, 0)
  )

m_water_het <- feols(
  n_conflicts ~ indice2_main * i(high_water) + log_nightlight | ADM2_NAME + year,
  data = df_water,
  cluster = ~ADM2_NAME
)

summary(m_water_het)

df_water <- final_regression_data_main %>%
  mutate(
    high_water = ifelse(water_avail > median(water_avail, na.rm = TRUE), 1, 0)
  )

m_water_het <- feols(
  n_conflicts ~ indice2_main * i(high_water, ref = 0) + log_nightlight |
    ADM2_NAME + year,
  data = df_water,
  cluster = ~ADM2_NAME
)

summary(m_water_het)


q <- unique(quantile(final_regression_data_main$water_avail, probs = c(.333,.666), na.rm=TRUE))

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
 
