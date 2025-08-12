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

final_df$export_proxy <- 1 / (final_df$dist_min_port + 1)
final_df <- final_df %>% mutate %>%
  
  mutate(
    
    dist_min_norm = (dist_min_port - min(dist_min_port, na.rm = TRUE)) / (max(dist_min_port, na.rm = TRUE) - min(dist_min_port, na.rm = TRUE)),
    
    nightlight_norm = (nightlight_mean - min(nightlight_mean, na.rm = TRUE)) / (max(nightlight_mean, na.rm = TRUE) - min(nightlight_mean, na.rm = TRUE)),
    
    export_proxy = ifelse(is.na(export_proxy), 0, export_proxy), # se l’hai già calcolato
    
    export_norm = (export_proxy - min(export_proxy, na.rm = TRUE)) / (max(export_proxy, na.rm = TRUE) - min(export_proxy, na.rm = TRUE))
    
  )

exposure_list <- list()
setnames(LSCI_annual, "LSCI_annual", "lsci_value")

for (t in LSCI_annual$year) {
  # Estrai il valore annuale fuori da mutate()
  lsci_t <- LSCI_annual[year == t, lsci_value]
  
  exposure_list[[as.character(t)]] <- final_df %>%
    mutate(
      year = t,
      Exposure = lsci_t * (
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
identical(exposure_df$HRname.x, exposure_df$HRname.y)
identical(exposure_df$HRpcode.x, exposure_df$HRpcode.y)
identical(exposure_df$HRparent.x, exposure_df$HRparent.y)
identical(exposure_df$Shape_Leng.x, exposure_df$Shape_Leng.y)
identical(exposure_df$Shape_Area.x, exposure_df$Shape_Area.y)
identical(exposure_df$ADM2_New.x, exposure_df$ADM2_New.y)




# Tieni solo le colonne .x e tutte le variabili nuove
setDT(exposure_df)

# 1. Elimina tutte le colonne .y
exposure_df[, names(exposure_df)[grepl("\\.y$", names(exposure_df))] := NULL]

# 2. Rinomina tutte le colonne che finiscono con .x (rimuove il suffisso)
x_cols <- names(exposure_df)[grepl("\\.x$", names(exposure_df))]
setnames(exposure_df, old = x_cols, new = gsub("\\.x$", "", x_cols))



panel_data <- merge(exposure_df, ContPortThroughput_long[, .(year, shock)], 
                    by = "year", all.x = TRUE)
# 1. Elimina tutte le colonne .y
panel_data[, names(panel_data)[grepl("\\.y$", names(panel_data))] := NULL]

# 2. Rinomina tutte le colonne che finiscono con .x (rimuove il suffisso)
x_cols <- names(panel_data)[grepl("\\.x$", names(panel_data))]
setnames(panel_data, old = x_cols, new = gsub("\\.x$", "", x_cols))
panel_data[, exposure_shock := Exposure * shock]


# Ensure conflict_coastal is a data.table (if not already)
setDT(conflict_coastal)


# Aggregate conflict_coastal to get n_conflicts per ADM2 and year
# Make sure 'ADM2_NAME' exists and is correctly named in conflict_coastal
# This assumes 'ADM2_NAME' comes from the spatial join with coastal_admin2_master
conflicts_aggregated <- conflict_coastal[, .(n_conflicts = .N), by = .(ADM2_NAME, year)]

final_regression_data <- merge(
  panel_data,
  conflicts_aggregated,
  by = c("ADM2_NAME", "year"), # Keys to merge on
  all.x = TRUE               # Keep all rows from panel_data, fill n_conflicts with NA if no match
)

# Replace NA conflicts with 0, as no conflicts is a meaningful zero count
final_regression_data[is.na(n_conflicts), n_conflicts := 0]


model <- feols(n_conflicts ~ exposure_shock | ADM2_NAME + year, data = final_regression_data)
summary(model)


model1 <- feols(n_conflicts ~ exposure_shock, data = final_regression_data)

summary(model1)

saveRDS(nig, file = "data/nig.rds")
saveRDS(coastal_admin2_master, file = "data/coastal_admin2_master.rds")
saveRDS(grid, file = "data/grid.rds")
saveRDS(ports_data, file = "data/ports_data.rds")
saveRDS(nightlight_dt, file = "data/nightlight_dt.rds")
saveRDS(conflicts_aggregated, file = "data/conflicts_aggregated.rds")
saveRDS(LSCI_annual, file = "data/LSCI_annual.rds")
saveRDS(ContPortThroughput_long, file = "data/ContPortThroughput_long.rds")
saveRDS(final_regression_data, file = "data/final_regression_data.rds")
