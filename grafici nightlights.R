nightlight_avg_per_year_mean <- nightlight_dt %>%
  group_by(year) %>%
  summarise(avg_nightlight_mean = mean(nightlight_mean, na.rm = TRUE), .groups = 'drop')

ggplot(data = nightlight_avg_per_year_mean, aes(x = year, y = avg_nightlight_mean)) +
  geom_line(color = "steelblue", size = 1) + # Linea per la serie storica
  geom_point(color = "darkblue", size = 2) + # Punti per ogni anno
  labs(
    title = "Average trend in night-time lighting in coastal areas (absolute values)",
    x = "Year",
    y = "Average trend in night-time lighting"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# 2. Plot per la media delle luci notturne (valori normalizzati)

# Calcola la media annuale delle luci notturne normalizzate su tutte le ADM2
# Assicurati che final_df contenga nightlight_norm per tutte le ADM2 e anni
nightlight_avg_per_year_norm <- final_df %>%
  group_by(year) %>%
  summarise(avg_nightlight_norm = mean(nightlight_norm, na.rm = TRUE), .groups = 'drop')

ggplot(data = nightlight_avg_per_year_norm, aes(x = year, y = avg_nightlight_norm)) +
  geom_line(color = "steelblue", size = 1) + # Linea per la serie storica
  geom_point(color = "darkblue", size = 2) + # Punti per ogni anno
  labs(
    title = "Normalised Average Trend of Night Lights in Coastal Areas",
    x = "Year",
    y = "Normalised Average Trend of Night Lights"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



total_conflicts_per_year <- final_regression_data %>%
  group_by(year) %>%
  summarise(total_n_conflicts = sum(n_conflicts, na.rm = TRUE), .groups = 'drop')

# 2. Crea il grafico della serie storica
ggplot(data = total_conflicts_per_year, aes(x = year, y = total_n_conflicts)) +
  geom_line(color = "firebrick", size = 1) + # Linea per la serie storica
  geom_point(color = "darkred", size = 2) + # Punti per ogni anno
  labs(
    title = "Total Conflicts in Nigeria's Coastal Areas (by Year)",
    x = "Year",
    y = "Total Conflicts"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




library(dplyr)
library(ggplot2)
library(tidyr) # Per la funzione pivot_longer o gather

# 1. Unisci i due dataframe aggregati per anno
combined_trends <- nightlight_avg_per_year_mean %>%
  full_join(total_conflicts_per_year, by = "year")

# 2. Trasforma il dataframe in formato "lungo"
# Questo è necessario per poter usare facet_wrap con le due variabili
combined_trends_long <- combined_trends %>%
  pivot_longer(
    cols = c(avg_nightlight_mean, total_n_conflicts), # Le colonne che vuoi "meltare"
    names_to = "variable",                             # Nome della nuova colonna per i nomi delle variabili
    values_to = "value"                                # Nome della nuova colonna per i valori
  ) %>%
  # Aggiungi un'etichetta più leggibile per i pannelli
  mutate(
    variable_label = case_when(
      variable == "avg_nightlight_mean" ~ "Average Night Lights",
      variable == "total_n_conflicts"   ~ "Total Conflicts",
      TRUE ~ variable # Cattura qualsiasi altra variabile non prevista
    )
  )

# 3. Crea il grafico a pannelli (faceted plot)
ggplot(data = combined_trends_long, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1) + # Colora le linee in base alla variabile
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 1) + # scales="free_y" permette assi Y diversi
  labs(
    title = "Trends in Conflict and Night Lights in Nigeria's Coastal Areas",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none" # La legenda non è necessaria con facet_wrap
  ) +
  scale_color_manual(values = c("avg_nightlight_mean" = "steelblue", "total_n_conflicts" = "firebrick")) # Colori personalizzati



# 1. Normalizza entrambe le serie temporali su una scala 0-1
# Funzione di normalizzazione (min-max scaling)
normalize_0_1 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

combined_trends_normalized <- combined_trends %>%
  mutate(
    avg_nightlight_mean_norm_overall = normalize_0_1(avg_nightlight_mean),
    total_n_conflicts_norm_overall = normalize_0_1(total_n_conflicts)
  )

# 2. Trasforma il dataframe normalizzato in formato "lungo"
combined_trends_norm_long <- combined_trends_normalized %>%
  pivot_longer(
    cols = c(avg_nightlight_mean_norm_overall, total_n_conflicts_norm_overall),
    names_to = "variable",
    values_to = "value_norm"
  ) %>%
  mutate(
    variable_label = case_when(
      variable == "avg_nightlight_mean_norm_overall" ~ "Luci Notturne Medie (Normalizzate)",
      variable == "total_n_conflicts_norm_overall"   ~ "Conflitti Totali (Normalizzati)",
      TRUE ~ variable
    )
  )

# 3. Crea il grafico su un singolo asse Y
ggplot(data = combined_trends_norm_long, aes(x = year, y = value_norm, color = variable_label)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Andamento Relativo di Conflitti e Luci Notturne nelle Aree Costiere",
    x = "Anno",
    y = "Valore Normalizzato (0-1)",
    color = "Variabile" # Legenda per identificare le linee
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c(
    "Luci Notturne Medie (Normalizzate)" = "steelblue",
    "Conflitti Totali (Normalizzati)" = "firebrick"
  ))


ggplot(data = LSCI_annual, aes(x = year, y = lsci_value)) +
  geom_line(color = "firebrick", size = 1) + # Linea per la serie storica
  geom_point(color = "darkred", size = 2) + # Punti per ogni anno
  labs(
    title = "LSCI Nigeria (per Anno)",
    x = "Anno",
    y = "LSCI"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


combined_trends_lsci <- LSCI_annual %>%
  full_join(total_conflicts_per_year, by = "year")

# 2. Trasforma il dataframe in formato "lungo"
combined_trends_lsci_long <- combined_trends_lsci %>%
  pivot_longer(
    cols = c(lsci_value, total_n_conflicts),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable_label = case_when(
      variable == "LSCI_annual"       ~ "Indice LSCI (Nazionale)",
      variable == "total_n_conflicts" ~ "Conflitti Totali",
      TRUE ~ variable
    )
  )

# 3. Crea il grafico a pannelli
ggplot(data = combined_trends_lsci_long, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 1) +
  labs(
    title = "Andamento dei Conflitti e dell'Indice LSCI della Nigeria",
    x = "Anno",
    y = "Valore"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_color_manual(values = c("LSCI_annual" = "darkgreen", "total_n_conflicts" = "firebrick"))


food_gdp <- read_excel("GDP_WB.xlsx")
gdp_nigeria <- food_gdp %>%
  filter(
    `Country Name` == "Nigeria",
    `Indicator Name` == "GDP (current US$)" # Assicurati che il nome dell'indicatore sia esattamente questo
  )

# 2. Seleziona solo le colonne di interesse (anni) e Country Code
# (Le altre colonne come Country Name e Indicator Name ora sono ridondanti)
gdp_nigeria <- gdp_nigeria %>%
  dplyr::filter(
    `Country Name` == "Nigeria",
    `Indicator Name` == "GDP (current US$)" # Controlla il nome esatto dell'indicatore nel tuo file
  )

# 2. Seleziona le colonne di interesse e trasforma il dataframe da wide a long
gdp_nigeria_long <- gdp_nigeria %>%
  # Seleziona esplicitamente tutte le colonne degli anni
  tidyr::pivot_longer(
    # Qui specifichiamo l'intervallo di colonne da "1960" a "2024"
    # Poiché sono in ordine e formattate come stringhe, questo funziona
    cols = "1960":"2019",
    names_to = "year",
    values_to = "gdp_value"
  ) %>%
  dplyr::mutate(
    year = as.numeric(year),
    gdp_value = as.numeric(gdp_value)
  ) %>%
  dplyr::filter(year >= 2009)

# Ora gdp_nigeria_long dovrebbe contenere i dati corretti.
# Controlla la testa del nuovo dataframe per confermare.
head(gdp_nigeria_long)


ggplot(data = gdp_nigeria_long, aes(x = year, y = gdp_value )) +
  geom_line(color = "firebrick", size = 1) + # Linea per la serie storica
  geom_point(color = "darkred", size = 2) + # Punti per ogni anno
  labs(
    title = "GDP della Nigeria (per Anno)",
    x = "Anno",
    y = "GDP"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


data <- read_excel("wb_appendice.xlsx")

# Filtra per la Nigeria e trasforma i dati in formato 'long'
data_long <- data %>%
  filter(`Country Name` == "Nigeria") %>%
  # Seleziona le colonne di interesse
  select(`Series Name`, `2009 [YR2009]`:`2019 [YR2019]`) %>%
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
  select(-year_raw)

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


combined_trends_oil <- nightlight_avg_per_year_mean %>%
  full_join(oil_rents_per_year, by = "year")

# Trasforma i dati in formato 'long'
combined_trends_oil_long <- combined_trends_oil %>%
  select(year, avg_nightlight_mean, oil_rents_pct_gdp) %>%
  pivot_longer(
    cols = c(avg_nightlight_mean, oil_rents_pct_gdp),
    names_to = "variable",
    values_to = "value"
  )

# Crea il grafico a pannelli (faceted plot)
ggplot(data = combined_trends_oil_long, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Confronto tra Trend di Luci Notturne e Rendite Petrolifere",
    x = "Anno",
    y = "Valore"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_color_manual(values = c("avg_nightlight_mean" = "steelblue", "oil_rents_pct_gdp" = "black"))


combined_trends_df <- total_conflicts_per_year

# Unisci il dataset delle luci notturne
combined_trends_df <- combined_trends_df %>%
  full_join(nightlight_avg_per_year_mean, by = "year")

# Unisci il dataset del PIL
combined_trends_df <- combined_trends_df %>%
  full_join(gdp_nigeria_long, by = "year")

# Unisci il dataset dell'indice LSCI
combined_trends_df <- combined_trends_df %>%
  full_join(LSCI_annual, by = "year")

# Unisci il dataset del World Bank in formato wide
combined_trends_df <- combined_trends_df %>%
  full_join(data_wide, by = "year")

combined_long <- combined_trends_df %>%
  # Seleziona le colonne con i nomi corretti (in questo caso con .x)
  select(year, total_n_conflicts, `Oil rents (% of GDP).x`, `Natural gas rents (% of GDP).x`, `avg_nightlight_mean.x`) %>%
  pivot_longer(
    cols = c(total_n_conflicts, `Oil rents (% of GDP).x`, `Natural gas rents (% of GDP).x`, `avg_nightlight_mean.x`),
    names_to = "variable",
    values_to = "value"
  )


# Crea un grafico a pannelli per confrontare le variabili
ggplot(data = combined_long, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Confronto tra Trend di Conflitti, Luci Notturne e Rendite Petrolifere",
    x = "Anno",
    y = "Valore"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")



data_wb_wide_pulito <- data_wb_wide %>%
  rename(
    gdp_growth_pct = `GDP growth (annual %)`,
    oil_rents_pct_gdp = `Oil rents (% of GDP)`,
    gas_rents_pct_gdp = `Natural gas rents (% of GDP)`,
    urban_growth_pct = `Urban population growth (annual %)`
    
  ) %>%
 
  select(
    year, gdp_growth_pct, urban_growth_pct)
combined_trends_df <- combined_trends_df %>%
  full_join(data_wb_wide_pulito, by = "year")


df_for_plotting <- combined_trends_df %>%
  select(
    year,
    total_n_conflicts,
    gdp_value = `gdp_value.x`, # O .y, a seconda di quale preferisci
    gdp_growth_pct,
    urban_growth_pct, 
    nightlight_avg_mean = `avg_nightlight_mean.x` #
  )
  
  # 2. Trasforma il dataframe in formato "lungo" per la visualizzazione
combined_long <- df_for_plotting %>%
pivot_longer(
    cols = c(gdp_growth_pct, gdp_value, urban_growth_pct, nightlight_avg_mean), # Le variabili da confrontare
    names_to = "variable",                                         # Nome della nuova colonna per le etichette
    values_to = "value"                                            # Nome della nuova colonna per i valori
  )

# 3. Crea il grafico a pannelli
ggplot(data = combined_long, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Confronto dei trend: nightlights, Crescita PIL e PIL",
    x = "Anno",
    y = "Valore"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_color_manual(values = c(
    "nightlight_avg_mean" = "firebrick",
    "gdp_growth_pct" = "darkgreen",
    "gdp_value" = "purple", 
    "urban_growth_pct" = "orange"
  ))


#####TOTAL CONFLICT 
# 1. Carica i pacchetti necessari
# Se non li hai ancora installati, esegui:
# install.packages(c("readxl", "dplyr", "ggplot2"))
library(readxl)
library(dplyr)
library(ggplot2)

# 2. Leggi il tuo file Excel
# Assicurati che il file "Cartel2.xlsx" si trovi nella stessa directory dello script.
# 4. Categorizza i conflitti in "Rapacity" e "Non-Rapacity"
# Utilizziamo `case_when` per assegnare le categorie.
conflicts <- conflict %>%
  mutate(
    conflict_type = case_when(
      (sub_event_type %in% rapacity_sub_events |
         grepl("loot|steal|raid|resource|gold|oil|land|plunder|livestock|pasture|farm|grazing",
               tolower(notes))) ~ "Rapacity",
      sub_event_type %in% non_rapacity_sub_events ~ "Non-Rapacity",
      TRUE ~ NA_character_ # Assegna NA a tutti gli altri casi
    )
  )

# 5. Prepara i dati per il grafico del trend
# Raggruppa i dati per anno e tipo di conflitto, contando il numero di eventi.
conflict_trend <- conflicts %>%
  filter(!is.na(year) & !is.na(conflict_type)  & year >= 2009 & year <= 2019) %>%
  group_by(year, conflict_type) %>%
  summarise(
    n_conflicts = n(),
    .groups = 'drop'
  )

# 6. Crea il grafico a linee
# Utilizziamo `geom_line` per mostrare il trend e `geom_point` per evidenziare i punti dati.
p <- ggplot(conflict_trend, aes(x = year, y = n_conflicts, color = conflict_type)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "Trend Temporale dei Conflitti",
    subtitle = "Numero di eventi per tipo di conflitto dal 1997 al 2024",
    x = "Anno",
    y = "Numero di Conflitti",
    color = "Tipo di Conflitto"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Rapacity" = "red", "Non-Rapacity" = "blue")) +
  theme(legend.position = "bottom")

# Stampa il grafico
print(p)


#1. Carica i pacchetti necessari
# Se non li hai ancora installati, esegui:
# install.packages(c("terra", "sf", "dplyr", "ggplot2"))
library(terra)
library(sf)
library(dplyr)
library(ggplot2)

# 2. Imposta le cartelle di input e output
raster_dir <- "nightlights/"
output_dir <- "nightlights_nigeria/"

# Assicurati che la cartella di output esista
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# 4. Elenco dei file raster e filtro per l'intervallo 2009-2019
raster_files <- list.files(raster_dir, pattern = "\\.tif$", full.names = TRUE)

# Crea un dataframe vuoto per i risultati
nigeria_nightlights_data <- data.frame(year = numeric(), mean_nightlights = numeric())

# 5. Loop per calcolare la media annuale
# In questo loop viene calcolata la media dei pixel per l'intera Nigeria.
for (file in raster_files) {
  year <- as.numeric(gsub("[^0-9]", "", basename(file)))
  
  # Filtra per l'intervallo di anni desiderato
  if (year >= 2009 & year <= 2019) {
    cat("Elaborazione anno:", year, "\n")
    
    # Carica il raster
    r <- rast(file)
    
    # Proietta 'nig_vect' al CRS del raster
    nig_vect_proj <- project(nig_vect, crs(r))
    
    # Croppa e maschera usando l’intera Nigeria
    r_crop <- crop(r, nig_vect_proj)
    r_mask <- mask(r_crop, nig_vect_proj)
    
    # Calcola la media dell'area mascherata.
    # Per questo esempio, assumiamo che 'r_mask' sia già calcolato.
    # Se hai i dati 'nig_vect', decommenta le righe sopra.
    mean_value <- global(r_mask, fun = "mean", na.rm = TRUE)
    
    # Calcola la media dell'intero raster per dimostrazione
    mean_value <- global(r, fun = "mean", na.rm = TRUE)
    
    # Aggiungi i dati al dataframe
    nigeria_nightlights_data <- rbind(nigeria_nightlights_data, data.frame(year = year, mean_nightlights = mean_value$mean))
  }
}

# 6. Normalizza i dati
# Normalizziamo i valori dividendo ogni media annuale per la media del primo anno (2009).
# Questo ci mostra la variazione percentuale relativa rispetto all'anno base.
baseline_value <- nigeria_nightlights_data %>% filter(year == 2009) %>% pull(mean_nightlights)

nigeria_nightlights_data <- nigeria_nightlights_data %>%
  mutate(normalized_mean = mean_nightlights / baseline_value)

# 7. Crea il grafico a linee per il trend normalizzato
p <- ggplot(nigeria_nightlights_data, aes(x = year, y = normalized_mean)) +
  geom_line(size = 1, color = "darkblue") +
  geom_point(size = 2, color = "darkblue") +
  labs(
    title = "Trend Normalizzato della Luminosità Notturna (Nigeria)",
    subtitle = "Variazione relativa rispetto all'anno base 2009",
    x = "Anno",
    y = "Valore Normalizzato (2009 = 1)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2009, 2019, by = 1))

# Stampa il grafico
print(p)



combined_trends_df_1 <- conflict_trend %>%
  full_join(nigeria_nightlights_data, by = "year") %>%
  full_join(gdp_nigeria_long, by = "year") %>%
  full_join(LSCI_annual, by = "year") %>%
  full_join(data_wb_wide_pulito, by = "year")

# Pulisci il dataframe unito, seleziona solo le colonne richieste
# e rinomina le colonne per chiarezza
df_for_plotting_1 <- combined_trends_df_1 %>%
  select(
    year,
    n_conflicts,
    normalized_mean,
    gdp_value,
    gdp_growth_pct,
    urban_growth_pct
  )

# Converti il dataframe in formato "lungo" per la visualizzazione con facet_wrap
combined_long_1 <- df_for_plotting_1 %>%
  pivot_longer(
    cols = c(n_conflicts, normalized_mean, gdp_value, gdp_growth_pct, urban_growth_pct),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable_label = case_when(
      variable == "n_conflicts" ~ "Conflitti Totali",
      variable == "normalized_mean" ~ "Luminosità Notturna Media",
      variable == "gdp_growth_pct" ~ "Crescita PIL (%)",
      variable == "gdp_value" ~ "Valore PIL",
      variable == "urban_growth_pct" ~ "Crescita Urbana (%)",
      TRUE ~ variable
    )
  )

# --- Sezione 3: Crea il grafico a pannelli per confrontare le variabili ---

# Crea il grafico a pannelli (faceted plot)
ggplot(data = combined_long, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 2) +
  labs(
    title = "Confronto dei Trend in Nigeria (2009-2019)",
    x = "Anno",
    y = "Valore"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_color_manual(values = c(
    "total_n_conflicts" = "#FF5733",
    "avg_nightlight_mean" = "#3371FF",
    "gdp_growth_pct" = "#33FF57",
    "gdp_value" = "#A533FF",
    "urban_growth_pct" = "#FFC433"
  ))

conflict_trend <- conflict %>%
  mutate(
    conflict_type = case_when(
      (sub_event_type %in% rapacity_sub_events |
         grepl("loot|steal|raid|resource|gold|oil|land|plunder|livestock|pasture|farm|grazing",
               tolower(notes))) ~ "Rapacity",
      sub_event_type %in% non_rapacity_sub_events ~ "Non_Rapacity",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(year) & !is.na(conflict_type) & year >= 2009 & year <= 2019) %>%
  group_by(year, conflict_type) %>%
  summarise(
    n_conflicts = n(),
    .groups = 'drop'
  ) %>%
  # Questo passaggio cruciale trasforma il dataframe per avere una colonna per ogni tipo di conflitto
  pivot_wider(names_from = conflict_type, values_from = n_conflicts, names_prefix = "n_conflicts_")

combined_trends_df <- conflict_trend %>%
  full_join(nigeria_nightlights_data, by = "year") %>%
  full_join(gdp_nigeria_long, by = "year") %>%
  full_join(LSCI_annual, by = "year") %>%
  full_join(data_wb_wide_pulito, by = "year")

# Seleziona le colonne richieste e rinominale per chiarezza
df_for_plotting <- combined_trends_df %>%
  select(
    year,
    n_conflicts_Rapacity,
    n_conflicts_Non_Rapacity,
    normalized_mean,
    gdp_value,
    gdp_growth_pct,
    urban_growth_pct
  )

# Converti il dataframe in formato "lungo"
combined_long <- df_for_plotting %>%
  pivot_longer(
    cols = c(n_conflicts_Rapacity, n_conflicts_Non_Rapacity, normalized_mean, gdp_value, gdp_growth_pct, urban_growth_pct),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable_label = case_when(
      variable == "n_conflicts_Rapacity" ~ "Conflitti (Rapacity)",
      variable == "n_conflicts_Non_Rapacity" ~ "Conflitti (Non-Rapacity)",
      variable == "normalized_mean" ~ "Luminosità Notturna Media",
      variable == "gdp_growth_pct" ~ "Crescita PIL (%)",
      variable == "gdp_value" ~ "Valore PIL",
      variable == "urban_growth_pct" ~ "Crescita Urbana (%)",
      TRUE ~ variable
    )
  )

# --- Sezione 3: Crea il grafico a pannelli per confrontare le variabili ---

# Crea il grafico a pannelli (faceted plot)
ggplot(data = combined_long, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 2) +
  labs(
    title = "Confronto dei Trend in Nigeria (2009-2019)",
    x = "Anno",
    y = "Valore"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  scale_color_manual(values = c(
    "n_conflicts_Rapacity" = "#FF5733",
    "n_conflicts_Non_Rapacity" = "#3371FF",
    "normalized_mean" = "#33FF57",
    "gdp_value" = "#A533FF",
    "gdp_growth_pct" = "#FFC433",
    "urban_growth_pct" = "#FF33B5"
  ))

