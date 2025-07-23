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
  # Estrai "May  2009" da "May  2009_Index_Average..."
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
LSCI_long[, LSCI_value := as.numeric(LSCI_value)]  # assicura che sia numerico
LSCI_annual <- LSCI_long[, .(LSCI_annual = mean(LSCI_value, na.rm = TRUE)), by = year]
