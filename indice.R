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
colonna_estratta <- conflict_coastal[["admin2"]]
colonna_estratta<-unique(colonna_estratta)
df_colonna <- data.frame(colonna_estratta)

write_xlsx(df_colonna, path = "colonna_estratta.xlsx")
library(writexl)
colonna_estratta1 <- panel_data[["ADM2_NAME.x"]]
colonna_estratta1<-unique(colonna_estratta1)
df_colonna1 <- data.frame(colonna_estratta1)
write_xlsx(df_colonna1, path = "colonna_estratta1.xlsx")
model <- feols(n_conflict ~ exposure_shock | ADM2_NAME + year, data = panel_data)
