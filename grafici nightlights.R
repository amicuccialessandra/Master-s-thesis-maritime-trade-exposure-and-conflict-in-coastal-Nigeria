nightlight_avg_per_year_mean <- nightlight_dt %>%
  group_by(year) %>%
  summarise(avg_nightlight_mean = mean(nightlight_mean, na.rm = TRUE), .groups = 'drop')

ggplot(data = nightlight_avg_per_year_mean, aes(x = year, y = avg_nightlight_mean)) +
  geom_line(color = "steelblue", size = 1) + # Linea per la serie storica
  geom_point(color = "darkblue", size = 2) + # Punti per ogni anno
  labs(
    title = "Andamento Medio delle Luci Notturne nelle Aree Costiere (Valori Assoluti)",
    x = "Anno",
    y = "Luminosità Notturna Media"
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
    title = "Andamento Medio Normalizzato delle Luci Notturne nelle Aree Costiere",
    x = "Anno",
    y = "Luminosità Notturna Media (Normalizzata)"
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
    title = "Conflitti Totali Nelle Aree Costiere della Nigeria (per Anno)",
    x = "Anno",
    y = "Numero Totale di Conflitti"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




library(dplyr)
library(ggplot2)
library(tidyr) # Per la funzione pivot_longer o gather

# --- Assicurati di aver calcolato questi dataframe dal passaggio precedente ---
# nightlight_avg_per_year_mean <- nightlight_dt %>%
#   group_by(year) %>%
#   summarise(avg_nightlight_mean = mean(nightlight_mean, na.rm = TRUE), .groups = 'drop')

# total_conflicts_per_year <- final_regression_data %>%
#   group_by(year) %>%
#   summarise(total_n_conflicts = sum(n_conflicts, na.rm = TRUE), .groups = 'drop')
# --------------------------------------------------------------------------

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
      variable == "avg_nightlight_mean" ~ "Luci Notturne Medie",
      variable == "total_n_conflicts"   ~ "Conflitti Totali",
      TRUE ~ variable # Cattura qualsiasi altra variabile non prevista
    )
  )

# 3. Crea il grafico a pannelli (faceted plot)
ggplot(data = combined_trends_long, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1) + # Colora le linee in base alla variabile
  geom_point(aes(color = variable), size = 2) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 1) + # scales="free_y" permette assi Y diversi
  labs(
    title = "Andamento dei Conflitti e delle Luci Notturne nelle Aree Costiere della Nigeria",
    x = "Anno",
    y = "Valore"
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
gdp_nigeria <- gdp_data %>%
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
