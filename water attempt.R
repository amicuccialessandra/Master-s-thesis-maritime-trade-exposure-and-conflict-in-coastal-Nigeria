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
  n_conflicts ~ indice2_highNL + indice2_lowNL + log_nightlight + water_avail | year,
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
 
# Standardizza per confrontabilità
final_regression_data_main <- final_regression_data_main %>%
  mutate(water_z = as.numeric(scale(water_avail)))



food_gdp <- read_excel("GDP_WB.xlsx")
gdp_nigeria <- food_gdp %>%
  filter(
    `Country Name` == "Nigeria",
    `Indicator Name` == "GDP (current US$)" # Assicurati che il nome dell'indicatore sia esattamente questo
  )

data <- read_excel("wb_appendice.xlsx")

# Filtra per la Nigeria e trasforma i dati in formato 'long'
data_long <- data %>%
  filter(`Country Name` == "Nigeria") %>%
  # Seleziona le colonne di interesse
  dplyr::select(`Series Name`, `2009 [YR2009]`:`2019 [YR2019]`) %>%
  # Trasforma i dati da wide a long
  pivot_longer(
    cols = `2009 [YR2009]`:`2019 [YR2019]`,
    names_to = "year_raw",
    values_to = "value"
  ) %>%
  # Estrai l'anno e converti in numerico, rimuovendo il testo tra parentesi quadre
  mutate(
    year = as.numeric(gsub(" \\[YR.*", "", year_raw)),
    value = as.numeric(value)
  ) %>%
  # Rimuovi la colonna non più necessaria 'year_raw'
  dplyr::select(-year_raw)

data_wide <- data_long %>%
  pivot_wider(
    names_from = `Series Name`,
    values_from = value
  )

oil_rents_per_year <- data_long %>%
  filter(`Series Name` == "Oil rents (% of GDP)") %>%
  rename(oil_rents_pct_gdp = value)

# Seleziona solo le rendite di gas naturale
gas_rents_per_year <- data_long %>%
  filter(`Series Name` == "Natural gas rents (% of GDP)") %>%
  rename(gas_rents_pct_gdp = value)

migration <- read_excel("migration(1).xlsx")
#from 1974 to 2019 net migration
#for the school missing values more costant from 1999 
#aggregated
migration <- migration %>%
  rename(
    year = Time,  # Rename Time column to year
    Agriculture_forestry_and_fishing = "Agriculture, forestry, and fishing, value added (% of GDP) [NV.AGR.TOTL.ZS]",
    net_migration = "Net migration [SM.POP.NETM]"# Rename long column name
  )
migration_1 <- migration %>%  dplyr::select(year, net_migration)
wgi_data_modified <- read_excel("wgi_data_modified.xlsx")
wgi_data_modified$year <- as.numeric(wgi_data_modified$year)
# Hai già: oil_rents_per_year, gas_rents_per_year, data_wide (GDP ecc.), migration_1, wgi_data_modified

# (a) Costruisci un dataframe "year-level" con tutto
year_controls <- list(
  oil_rents_per_year %>% dplyr::select(year, oil_rents_pct_gdp),
  gas_rents_per_year %>% dplyr::select(year, gas_rents_pct_gdp),
  migration_1 %>% dplyr::select(year, net_migration),
  wgi_data_modified %>% dplyr::select(
    year,
    ControlofCorruption, RuleofLaw, RegulatoryQuality,
    GovernmentEffectiveness, PoliticalStabilityNoViolence, VoiceandAccountability
  )
) %>% Reduce(function(x,y) dplyr::left_join(x,y, by="year"), .) %>%
  dplyr::distinct(year, .keep_all = TRUE)   # evita duplicati per anno

# b) Rendi numeriche le WGI (se non lo fossero)
wgi_cols <- c(
  "ControlofCorruption","RuleofLaw","RegulatoryQuality",
  "GovernmentEffectiveness","PoliticalStabilityNoViolence","VoiceandAccountability"
)
year_controls <- year_controls %>%
  mutate(
    across(all_of(wgi_cols), ~ suppressWarnings(as.numeric(.x)))
  )

# c) Indice sintetico di governance (media degli z-score)
year_controls <- year_controls %>%
  mutate(
    across(all_of(wgi_cols), ~ as.numeric(scale(.x)), .names = "z_{col}")
  ) %>%
  rowwise() %>%
  mutate(
    governance_index = mean(c_across(starts_with("z_")), na.rm = TRUE)
  ) %>%
  ungroup()

# ---- 2) Unisci al pannello ----
final_regression_data_main <- final_regression_data_main %>%
  left_join(year_controls, by = "year")

# ---- 3) Regressione senza FE di anno (con FE ADM2) + controlli macro annuali ----
#    (Versione con le sei componenti WGI esplicite)
m_noYearFE <- feols(
  n_conflicts ~ indice2_main + log_nightlight +
    oil_rents_pct_gdp + gas_rents_pct_gdp + net_migration +
    ControlofCorruption + RuleofLaw + RegulatoryQuality +
    GovernmentEffectiveness + PoliticalStabilityNoViolence + VoiceandAccountability
  | ADM2_NAME,
  data = final_regression_data_main,
  cluster = ~ ADM2_NAME
)

summary(m_noYearFE)

m_noYearFE_wgiIndex <- feols(
  n_conflicts ~ indice2_main + log_nightlight +
    oil_rents_pct_gdp + gas_rents_pct_gdp + net_migration +
    governance_index
  | ADM2_NAME,
  data = final_regression_data_main,
  cluster = ~ ADM2_NAME
)

summary(m_noYearFE_wgiIndex)
