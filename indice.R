nightlight_list <- list()

for (file in raster_files) {
  r <- rast(file)
  year <- as.integer(gsub("[^0-9]", "", basename(file)))
  
  nightlight_vals <- terra::extract(r, vect(admin2_in_coastal_admin1), fun=mean, na.rm=TRUE)[,2]
  
  tmp_dt <- data.table(
    ADM2_CODE = admin2_in_coastal_admin1$ADM2_CODE,
    ADM2_NAME = admin2_in_coastal_admin1$ADM2_NAME,
    year = year,
    nightlight_mean = nightlight_vals
  )
  
  nightlight_list[[as.character(year)]] <- tmp_dt
}

nightlight_dt <- rbindlist(nightlight_list)

# 3. Distanza porti (assumendo costante nel tempo)
#dist_ports_dt <- data.table(
#  ADM2_CODE = coastal_buffer_admin2$ADM2_CODE.x,
#  ADM2_NAME = admin2_in_coastal_admin1$ADM2_NAME.x,
#  dist_min_port = coastal_buffer_admin2$dist_min_port
#)



# 5. Ora devi unire questi dati per creare il dataframe finale

# Per semplicità, uniamo nightlight e distanza (quest'ultima fissa)
coastal_buffer_admin2 <- merge(nightlight_dt, coastal_buffer_admin2 , by = "ADM2_CODE", all.x = TRUE)
coastal_buffer_admin2[, ADM2_NAME.y := NULL]
final_df <- merge(coastal_buffer_admin2, LSCI_annual, by = "year", all.x = TRUE)




###INDICE AGGIORNATO 

# Normalizza la distanza per sicurezza (opzionale, se vuoi comparabilità)
final_df$export_proxy <- 1 / (final_df$dist_min_port + 1)

final_df <- final_df %>%
  mutate(
    dist_min_norm = (dist_min_port - min(dist_min_port, na.rm = TRUE)) /
      (max(dist_min_port, na.rm = TRUE) - min(dist_min_port, na.rm = TRUE)),
    export_proxy  = ifelse(is.na(export_proxy), 0, export_proxy)
  )

# -----------------------
# Costruisci pannello: replica ogni ADM2 per tutti gli anni
# -----------------------
panel_list <- list()
for (t in LSCI_annual$year) {
  
  # Prendi i valori dell'anno t
  lsci_t  <- LSCI_annual[year == t, lsci_value][1]
  shock_t <- ContPortThroughput_long[year == t, shock][1]
  
  if(is.na(shock_t)) {
    warning(paste("Shock mancante per anno", t, "- imposto 0"))
    shock_t <- 0
  }
  
  tmp <- final_df %>%
    mutate(
      year     = t,
      shock    = shock_t,
      Exposure = lsci_t * shock_t * dist_min_norm,
      indice2  = lsci_t * shock_t * export_proxy
    )
  panel_list[[as.character(t)]] <- tmp
}

panel_data <- bind_rows(panel_list)

# Pulisci nomi colonne
setDT(panel_data)
panel_data[, names(panel_data)[grepl("\\.y$", names(panel_data))] := NULL]
x_cols <- names(panel_data)[grepl("\\.x$", names(panel_data))]
setnames(panel_data, old = x_cols, new = gsub("\\.x$", "", x_cols))

# -----------------------
# Unisci i conflitti
# -----------------------
conflicts_aggregated <- conflict_coastal[, .(n_conflicts = .N), by = .(ADM2_NAME, year)]

final_regression_data <- merge(
  panel_data,
  conflicts_aggregated,
  by = c("ADM2_NAME", "year"),
  all.x = TRUE
)

# Sostituisci NA con 0
final_regression_data[is.na(n_conflicts), n_conflicts := 0]

# -----------------------
# Regressioni
# -----------------------
model_exp <- feols(n_conflicts ~ Exposure | ADM2_NAME + year, data = final_regression_data)
summary(model_exp)

model_ind2 <- feols(n_conflicts ~ indice2 | ADM2_NAME + year, data = final_regression_data)
summary(model_ind2)



model6<- feols(
  n_conflicts ~ indice2 | ADM2_NAME + year, 
  data = final_regression_data
)
summary(model6)
final_regression_data <- final_regression_data %>% mutate(indice2 = 1/dist_min_norm * shock * lsci_value) 

model7<- feols(
  I(n_conflicts>0) ~ indice2 | ADM2_NAME + year, 
  data = final_regression_data
)
summary(model7)

model8<- feols(
  I(n_conflicts>0) ~ indice2 + nightlight_norm | ADM2_NAME + year, 
  data = final_regression_data
)
summary(model8)


# Step 1: Crea uno scheletro del pannello
years <- unique(LSCI_annual$year)
adm2s <- unique(coastal_admin2_master$ADM2_NAME)
panel_skeleton <- expand.grid(ADM2_NAME = adm2s, year = years)
print(paste("Scheletro creato. Numero di righe:", nrow(panel_skeleton)))
final_regression_data <- merge(panel_skeleton, LSCI_annual, by = "year", all.x = TRUE)
print(paste("Dopo unione con LSCI_annual. Numero di righe:", nrow(final_regression_data)))
final_regression_data <- merge(final_regression_data, ContPortThroughput_long[, c("year", "shock")], by = "year", all.x = TRUE)
print(paste("Dopo unione con ContPortThroughput_long. Numero di righe:", nrow(final_regression_data)))
final_regression_data <- merge(final_regression_data, nightlight_annual, by = c("ADM2_NAME", "year"), all.x = TRUE)
print(paste("Dopo unione con nightlight_annual. Numero di righe:", nrow(final_regression_data)))
final_regression_data <- merge(final_regression_data, conflicts_aggregated, by = c("ADM2_NAME", "year"), all.x = TRUE)
print(paste("Dopo unione con conflicts_aggregated. Numero di righe:", nrow(final_regression_data)))
final_regression_data <- merge(final_regression_data, st_drop_geometry(coastal_admin2_master[, c("ADM2_NAME", "dist_min_port", "dist_min_norm")]), by = "ADM2_NAME", all.x = TRUE)
print(paste("Dopo unione con i dati geografici. Numero di righe:", nrow(final_regression_data)))
setDT(final_regression_data)

# Step 2: Calcola il tuo nuovo indice e pulisci i dati
# Aggiungi un valore piccolo a dist_min_norm per evitare la divisione per zero
epsilon <- 0.000001
final_regression_data[is.na(n_conflicts), n_conflicts := 0]
final_regression_data <- final_regression_data %>%
  mutate(
    indice2 = (1 / (dist_min_norm + epsilon)) * shock * lsci_value
  )

# Step 3: Verifica le osservazioni valide per la regressione e aggiusta il modello
# Manteniamo tutte le variabili. feols gestirà l'eliminazione dei NA.
model6 <- feols(n_conflicts ~ indice2 | ADM2_NAME + year, data = final_regression_data)
summary(model6)

model9 <- feols(n_conflicts ~ indice2 + nightlight_norm | ADM2_NAME + year, data = final_regression_data)
summary(model9) 
final_regression_data <- final_regression_data %>%
  mutate(
    indice = dist_min_norm * shock * lsci_value
  )
model7 <- feols(n_conflicts  ~ indice + nightlight_norm| ADM2_NAME + year, data = final_regression_data)
summary(model7)
