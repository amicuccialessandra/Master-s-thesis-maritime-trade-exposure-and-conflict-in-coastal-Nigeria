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
  plot(r_mask)  # se vuoi vedere
  year <- gsub("[^0-9]", "", basename(file))  # estrae l'anno dal nome
  out_name <- paste0("nightlights_nigeria/nightlight_nigeria_", year, ".tif")
  
  writeRaster(r_mask, out_name, overwrite = TRUE)
  cat("Salvato:", out_name, "\n")
}

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
