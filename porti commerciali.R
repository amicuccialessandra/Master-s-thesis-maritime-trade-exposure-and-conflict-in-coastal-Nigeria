# lista dei porti "principali"
# -----------------------
# PORTI PRINCIPALI
# -----------------------
main_ports <- c("LAGOS", "TIN CAN ISLAND", "PORT HARCOURT", 
                "ONNE", "WARRI", "CALABAR")

# Filtro solo i porti principali
ports_main_sf <- ports_sf %>%
  filter(MAIN_PORT_ %in% main_ports)
ports_main_proj <- st_transform(ports_main_sf, 32631)

# Buffer intorno ai porti principali
ports_buffer_main <- st_buffer(ports_main_proj, dist = 1000000)  
ports_buffer_main <- st_make_valid(ports_buffer_main)

# Trasforma admin2 nel CRS dei porti
admin2_proj_main <- st_transform(admin2_in_coastal_admin1, st_crs(ports_buffer_main))

# Prendo solo gli admin2 dentro al buffer dei porti principali
coastal_buffer_admin2_main <- admin2_proj_main %>%
  filter(st_intersects(admin2_proj_main, ports_buffer_main, sparse = FALSE)[,1])

# Calcolo distanza minima dagli admin2 ai porti principali
dist_ports_main <- st_distance(coastal_buffer_admin2_main, ports_main_proj)
coastal_buffer_admin2_main$dist_min_port_main <- apply(as.matrix(dist_ports_main), 1, min)

# Creo un nuovo coastal_admin2_master_main con distanza "main"
coastal_admin2_master_main <- merge(
  coastal_admin2_master,
  st_drop_geometry(coastal_buffer_admin2_main[, c("ADM2_NAME", "dist_min_port_main")]),
  by = "ADM2_NAME",
  all.x = TRUE
)

# Normalizzo la distanza principale
coastal_admin2_master_main <- coastal_admin2_master_main %>%
  mutate(
    dist_min_norm_main = (dist_min_port_main - min(dist_min_port_main, na.rm = TRUE)) /
      (max(dist_min_port_main, na.rm = TRUE) - min(dist_min_port_main, na.rm = TRUE))
  )

# -----------------------
# MAPPA SOLO PORTI PRINCIPALI
# -----------------------
dist_port_main_plot <- ggplot() +
  geom_sf(data = coastal_buffer_admin2_main, aes(fill = dist_min_port_main), color = "black", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", name = "Distance (m)") +
  labs(title = "Distance between coastal administrative areas and main ports",
       subtitle = "1,000 km buffer around main ports") +
  theme_minimal()

ggsave("distanza_porti_main_mappa.pdf", 
       plot = dist_port_main_plot,
       width = 10, height = 8, dpi = 300)

dist_port_main_plot



coastal_buffer_admin2_main <- merge(
  nightlight_dt, 
  coastal_buffer_admin2_main,   # <-- la versione filtrata sui porti principali
  by = "ADM2_CODE", 
  all.x = TRUE
)
coastal_buffer_admin2_main[, ADM2_NAME.y := NULL]

final_df_main <- merge(coastal_buffer_admin2_main, LSCI_annual, by = "year", all.x = TRUE)

# Normalizza la distanza per sicurezza
final_df_main$export_proxy_main <- 1 / (final_df_main$dist_min_port_main + 1)

final_df_main <- final_df_main %>%
  mutate(
    dist_min_norm_main = (dist_min_port_main - min(dist_min_port_main, na.rm = TRUE)) /
      (max(dist_min_port_main, na.rm = TRUE) - min(dist_min_port_main, na.rm = TRUE)),
    export_proxy_main  = ifelse(is.na(export_proxy_main), 0, export_proxy_main)
  )
panel_list_main <- list()
for (t in LSCI_annual$year) {
  lsci_t  <- LSCI_annual[year == t, lsci_value][1]
  shock_t <- ContPortThroughput_long[year == t, shock][1]
  
  if (is.na(shock_t)) shock_t <- 0
  
  tmp <- final_df_main %>%
    mutate(
      year      = t,
      shock     = shock_t,
      Exposure_main = lsci_t * shock_t * dist_min_norm_main,
      indice2_main  = lsci_t * shock_t * export_proxy_main
    )
  panel_list_main[[as.character(t)]] <- tmp
}

panel_data_main <- bind_rows(panel_list_main)
setDT(panel_data_main)
panel_data_main[, names(panel_data_main)[grepl("\\.y$", names(panel_data_main))] := NULL]
x_cols_main <- names(panel_data_main)[grepl("\\.x$", names(panel_data_main))]
setnames(panel_data_main, old = x_cols_main, new = gsub("\\.x$", "", x_cols_main))

years <- unique(LSCI_annual$year)
adm2s <- unique(coastal_admin2_master_main$ADM2_NAME)  # <-- usa la versione _main
panel_skeleton_main <- expand.grid(ADM2_NAME = adm2s, year = years)

final_regression_data_main <- merge(panel_skeleton_main, LSCI_annual, by = "year", all.x = TRUE)
final_regression_data_main <- merge(final_regression_data_main, ContPortThroughput_long[, c("year", "shock")], by = "year", all.x = TRUE)
final_regression_data_main <- merge(final_regression_data_main, nightlight_annual, by = c("ADM2_NAME", "year"), all.x = TRUE)
final_regression_data_main <- merge(final_regression_data_main, conflicts_aggregated, by = c("ADM2_NAME", "year"), all.x = TRUE)
final_regression_data_main <- merge(final_regression_data_main, st_drop_geometry(coastal_admin2_master_main[, c("ADM2_NAME", "dist_min_port_main", "dist_min_norm_main")]), by = "ADM2_NAME", all.x = TRUE)
setDT(final_regression_data_main)
final_regression_data_main[is.na(n_conflicts), n_conflicts := 0]


epsilon <- 1e-6
final_regression_data_main <- final_regression_data_main %>%
  mutate(
    indice2_main = (1 / (dist_min_norm_main + epsilon)) * shock * lsci_value,
    indice_main  = dist_min_norm_main * shock * lsci_value
  )

# Regressioni
model6_main <- feols(n_conflicts ~ indice2_main | ADM2_NAME + year, data = final_regression_data_main)
summary(model6_main)

model9_main <- feols(n_conflicts ~ indice2_main + nightlight_norm | ADM2_NAME + year, data = final_regression_data_main)
summary(model9_main)

model7_main <- feols(n_conflicts ~ indice_main + nightlight_norm | ADM2_NAME + year, data = final_regression_data_main)
summary(model7_main)


final_regression_data_main <- final_regression_data_main %>%
  group_by(year) %>%
  mutate(med_nl = median(nightlight_norm, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(high_nl = ifelse(nightlight_norm > med_nl, 1, 0))

final_regression_data_main <- final_regression_data_main %>%
  mutate(
    indice2_highNL_main = indice2_main * high_nl
  )
model_interaction_main <- feols(
  n_conflicts ~ indice2_main * high_nl + nightlight_norm | ADM2_NAME + year, 
  data = final_regression_data_main
)
summary(model_interaction_main)

final_regression_data_main <- final_regression_data_main %>%
  group_by(year) %>%
  mutate(q3_nl = quantile(nightlight_norm, probs = 0.75, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(high_nl = ifelse(nightlight_norm > q3_nl, 1, 0)) %>%
  mutate(
    indice2_highNL_main = indice2_main * high_nl,
    indice2_lowNL_main  = indice2_main * (1 - high_nl)
  )
model_interaction1_main <- feols(
  n_conflicts ~ indice2_main * high_nl + nightlight_norm | ADM2_NAME + year,
  data = final_regression_data_main
)
summary(model_interaction1_main)

model_interaction2_main <- feols(
  n_conflicts ~ indice2_highNL_main + indice2_lowNL_main | ADM2_NAME + year,
  data = final_regression_data_main
)
summary(model_interaction2_main)


# ---------------------------
# 1. Log dei nightlights
# ---------------------------
final_regression_data <- final_regression_data %>%
  mutate(log_nightlight = log(nightlight_mean + 1))  # +1 per evitare log(0)

# ---------------------------
# 2. Creazione dummy "high_nl" per ogni anno (basata sul log)
# ---------------------------
final_regression_data <- final_regression_data %>%
  group_by(year) %>%
  mutate(q3_log_nl = quantile(log_nightlight, 0.75, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(high_nl = ifelse(log_nightlight > q3_log_nl, 1, 0))

# ---------------------------
# 3. Creazione interazioni con indice2
# ---------------------------
final_regression_data <- final_regression_data %>%
  mutate(
    indice2_highNL = indice2 * high_nl,
    indice2_lowNL  = indice2 * (1 - high_nl)
  )

# ---------------------------
# 4. Regressioni
# ---------------------------

# Modello con interazione esplicita
model_interaction <- feols(
  n_conflicts ~ indice2 * high_nl + log_nightlight | ADM2_NAME + year,
  data = final_regression_data
)
summary(model_interaction)

# Modello con interazioni separate
model_interaction2 <- feols(
  n_conflicts ~ indice2_highNL + indice2_lowNL + log_nightlight | ADM2_NAME + year,
  data = final_regression_data
)
summary(model_interaction2)

# Modello base con solo indice2 e log nightlights
model_base <- feols(
  n_conflicts ~ indice2 + log_nightlight | ADM2_NAME + year,
  data = final_regression_data
)
summary(model_base)


# ---------------------------
# 1. Log dei nightlights
# ---------------------------
final_regression_data_main <- final_regression_data_main %>%
  mutate(log_nightlight = log(nightlight_mean + 1))  # +1 per evitare log(0)

# ---------------------------
# 2. Creazione dummy "high_nl" per ogni anno (basata sul log)
# ---------------------------
final_regression_data_main <- final_regression_data_main %>%
  group_by(year) %>%
  mutate(q3_log_nl = quantile(log_nightlight, 0.75, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(high_nl = ifelse(log_nightlight > q3_log_nl, 1, 0))

# ---------------------------
# 3. Creazione interazioni con indice2
# ---------------------------
final_regression_data_main <- final_regression_data_main %>%
  mutate(
    indice2_highNL = indice2 * high_nl,
    indice2_lowNL  = indice2 * (1 - high_nl)
  )

# ---------------------------
# 4. Regressioni
# ---------------------------

# Modello con interazione esplicita
model_interaction_main <- feols(
  n_conflicts ~ indice2_main * high_nl + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main
)
summary(model_interaction_main)

# Modello con interazioni separate
model_interaction2 <- feols(
  n_conflicts ~ indice2_highNL_main + indice2_lowNL_main + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main
)
summary(model_interaction2)

# Modello base con solo indice2 e log nightlights
model_base <- feols(
  n_conflicts ~ indice2_main + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main
)
summary(model_base)



library(ggplot2)
library(dplyr)
library(broom)

# Crea lista modelli
models <- list(
  Base = model_base,
  Interaction = model_interaction_main,
  Separate = model_interaction2_main
)

# Trasforma in tidy dataframe
coef_df <- bind_rows(
  lapply(names(models), function(name) {
    broom::tidy(models[[name]]) %>%
      mutate(Model = name)
  })
)

# Filtra solo variabili di interesse
coef_df <- coef_df %>%
  filter(term %in% c("indice2_main", "indice2_highNL_main", "indice2_lowNL_main", 
                     "log_nightlight", "high_nl", "indice2_main:high_nl"))

# Grafico
ggplot(coef_df, aes(x = term, y = estimate, color = Model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                width = 0.2, position = position_dodge(width = 0.7)) +
  coord_flip() +
  labs(x = "Variabili", y = "Coefficiente stimato",
       title = "Confronto coefficienti tra modelli",
       subtitle = "Barre: intervalli di errore standard") +
  theme_minimal(base_size = 14)
