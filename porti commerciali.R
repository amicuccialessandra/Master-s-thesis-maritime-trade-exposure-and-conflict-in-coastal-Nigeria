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

col_names <- colnames(ports_main_proj)
col_names <- col_names[col_names != "geometry"]

# Crea un data frame vuoto con tutte le colonne (tranne geometry)
nuovo_porto <- as.data.frame(matrix(NA, nrow = 1, ncol = length(col_names)))
colnames(nuovo_porto) <- col_names

# Inserisci i dati che conosci
nuovo_porto$MAIN_PORT_ <- "ONNE"
nuovo_porto$LAT_DD <- 4.7444
nuovo_porto$LNG_DD <- 7.0361

# Converti in sf con la colonna geometry
nuovo_porto_sf <- st_as_sf(nuovo_porto, coords = c("LNG_DD","LAT_DD"), crs = 4326)

# Trasforma CRS come ports_main_proj
nuovo_porto_proj <- st_transform(nuovo_porto_sf, st_crs(ports_main_proj))

# Ora uniamo: selezioniamo solo le stesse colonne di ports_main_proj + geometry
ports_main_proj <- rbind(
  ports_main_proj[, colnames(nuovo_porto_proj)],
  nuovo_porto_proj
)

# Controllo finale
ports_main_proj
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
port2 <- ggplot() +
  geom_sf(data = coastal_buffer_admin2_main, fill = "gray90", color = "grey") +  # Admin2 map
  geom_sf(data = ports_main_proj, color = "red", size = 2) +  # Ports as red dots
  theme_minimal() +
  ggtitle("Nigerian Ports with Admin2 Boundaries and Grid")

port2
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



#eterogeneità 
# Step 1: definisci dummy pre/post 2015
final_regression_data_main <- final_regression_data_main %>%
  mutate(post2015 = ifelse(year > 2015, 1, 0),
         pre2015  = 1 - post2015)

# Step 2: dummy high/low nightlights basata sul log già presente
final_regression_data_main <- final_regression_data_main %>%
  group_by(year) %>%
  mutate(q3_log_nl = quantile(nightlight_mean, 0.75, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    high_nl = ifelse(nightlight_mean > q3_log_nl, 1, 0),
    low_nl  = 1 - high_nl
  )

# Step 3: crea le interazioni con high/low e pre/post 2015
final_regression_data_main <- final_regression_data_main %>%
  mutate(
    indice_high_pre  = indice2_main * high_nl * pre2015,
    indice_high_post = indice2_main * high_nl * post2015,
    indice_low_pre   = indice2_main * low_nl  * pre2015,
    indice_low_post  = indice2_main * low_nl  * post2015
  )

# Step 4: regressione con tutte le interazioni
model_hetero <- feols(
  n_conflicts ~ indice_high_pre + indice_high_post + 
    indice_low_pre + indice_low_post + nightlight_mean |
    ADM2_NAME + year,
  data = final_regression_data_main
)

summary(model_hetero)


model_hetero2 <- feols(
  n_conflicts ~ indice_high_pre + indice_high_post + 
    indice2_lowNL_main + nightlight_mean |
    ADM2_NAME + year,
  data = final_regression_data_main
)

summary(model_hetero2)

model_hetero3 <- feols(
  n_conflicts ~ indice2_highNL_main + 
    indice_low_pre + indice_low_post + nightlight_mean |
    ADM2_NAME + year,
  data = final_regression_data_main
)

summary(model_hetero3)


#porti 
library(dplyr)
library(ggplot2)
library(sf)

# Supponiamo di avere ADM2 costieri in coastal_admin2_master_main
admin2_costieri <- coastal_admin2_master_main

# Calcolo il centroide di ogni ADM2
admin2_costieri <- admin2_costieri %>%
  mutate(centroid = st_centroid(geometry),
         lat = st_coordinates(centroid)[,2])  # prendo la latitudine

lat_terzili <- quantile(admin2_costieri$lat, probs = c(1/2), na.rm = TRUE)  # mediana come cut-off

admin2_costieri <- admin2_costieri %>%
  mutate(macro_region = case_when(
    lat <= lat_terzili[1] ~ "Sud",
    lat > lat_terzili[1]  ~ "Nord"
  ))

# Controllo
table(admin2_costieri$macro_region)

# Plottiamo le ADM2 costiere Nord/Sud colorate
ggplot(admin2_costieri) +
  geom_sf(aes(fill = macro_region), color = "black", size = 0.2) +
  scale_fill_manual(values = c("Nord" = "red", "Sud" = "blue")) +
  labs(title = "Macro-regioni ADM2 costiere (Nord e Sud)",
       fill = "Macro-regione") +
  theme_minimal()

# 1. Uniamo la macro-regione Nord/Sud ai dati per la regressione
final_regression_data_main <- final_regression_data_main %>%
  left_join(
    st_drop_geometry(admin2_costieri[, c("ADM2_NAME", "macro_region")]),
    by = "ADM2_NAME"
  )

# Controllo
table(final_regression_data_main$macro_region)

# 2. Creiamo interazioni per eterogeneità Nord/Sud
final_regression_data_main <- final_regression_data_main %>%
  mutate(
    indice2_nord = ifelse(macro_region == "Nord", indice2_main, 0),
    indice2_sud  = ifelse(macro_region == "Sud", indice2_main, 0)
  )

# 3. Esempio di regressione con eterogeneità Nord/Sud --> SIGNIFICATIVO 
model_hetero <- feols(
  n_conflicts ~ indice2_nord + indice2_sud + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main
)
summary(model_hetero)

# 4. Interazione esplicita (opzionale)
model_hetero_interaction <- feols(
  n_conflicts ~ indice2_main * macro_region + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main
)
summary(model_hetero_interaction)

model_nordsud <- feols(
  n_conflicts ~ indice2_main * macro_region + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main
)
model_nordsud

final_regression_data_main <- final_regression_data_main %>%
  mutate(
    dist_group = case_when(
      dist_min_port_main <= quantile(dist_min_port_main, 0.33, na.rm = TRUE) ~ "Vicino porto",
      dist_min_port_main <= quantile(dist_min_port_main, 0.66, na.rm = TRUE) ~ "Medio",
      TRUE ~ "Lontano"
    )
  )

table(final_regression_data_main$dist_group)

model_distance <- feols(
  n_conflicts ~ indice2_main * dist_group + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main
)

summary(model_distance)


cutoff <- median(final_regression_data_main$dist_min_port_main, na.rm = TRUE)

final_regression_data_main <- final_regression_data_main %>%
  mutate(
    dist_group2 = ifelse(dist_min_port_main <= cutoff, "Vicino", "Lontano")
  )

# Controllo distribuzione
table(final_regression_data_main$dist_group2)

# 2. Stima del modello: Vicino vs Lontano
model_dist2 <- feols(
  n_conflicts ~ indice2_main * dist_group2 + log_nightlight | ADM2_NAME + year,
  data = final_regression_data_main
)

summary(model_dist2)

library(broom)

# A) assegna a ogni ADM2 il porto principale più vicino
adm2_cent <- coastal_admin2_master_main |>
  st_transform(st_crs(ports_main_proj)) |>
  mutate(centroid = st_centroid(geometry)) |>
  dplyr::select(ADM2_NAME, geometry = centroid)

nearest_id <- st_nearest_feature(adm2_cent, ports_main_proj)
adm2_nn <- adm2_cent |>
  mutate(nearest_port = ports_main_proj$MAIN_PORT_[nearest_id]) |>
  st_drop_geometry()

# B) raggruppa porti in Ovest/Est (modifica qui se vuoi una tripartizione)
west_ports <- c("LAGOS","TIN CAN ISLAND","WARRI")
east_ports <- c("PORT HARCOURT","ONNE","CALABAR")

adm2_nn <- adm2_nn |>
  mutate(port_group = case_when(
    nearest_port %in% west_ports ~ "Ovest",
    nearest_port %in% east_ports ~ "Est",
    TRUE ~ "Altro"
  ))

# C) unisci a final_regression_data_main
dfp <- final_regression_data_main |>
  left_join(adm2_nn[,c("ADM2_NAME","nearest_port","port_group")], by="ADM2_NAME") |>
  mutate(port_group = ifelse(is.na(port_group),"Altro",port_group))

# D) regressione: interazione con gruppo porto
m_port <- feols(
  n_conflicts ~ indice2_main * i(port_group) + log_nightlight | ADM2_NAME + year,
  data = dfp
)
summary(m_port)

# E) coef-plot compatto
coef_df <- broom::tidy(m_port)
ggplot(coef_df %>% filter(grepl("indice2_main", term)),
       aes(x=term, y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.1)+
  coord_flip()+
  labs(title="Indice × gruppo porto (Ovest/Est)", x="", y="Coeff. (±95% CI)")+
  theme_minimal()

library(splines)

dfd <- final_regression_data_main |>
  mutate(dist_km = dist_min_port_main/1000)

# A) Splines sulla distanza: effetto di indice che varia con la distanza
m_spline <- feols(
  n_conflicts ~ log_nightlight + indice2_main:bs(dist_km, df=3) | ADM2_NAME + year,
  data = dfd
)
summary(m_spline)

# B) Terzili (più leggibile in figura)
q <- quantile(dfd$dist_min_port_main, probs = c(.333,.666), na.rm=TRUE)
dfd <- dfd |>
  mutate(dist_terc = cut(dist_min_port_main,
                         breaks=c(-Inf, q[1], q[2], Inf),
                         labels=c("Vicino","Medio","Lontano")))
m_terc <- feols(
  n_conflicts ~ indice2_main * i(dist_terc) + log_nightlight | ADM2_NAME + year,
  data=dfd
)
summary(m_terc)

# Coef-plot terzili
coef_df2 <- broom::tidy(m_terc)
ggplot(coef_df2 %>% filter(grepl("indice2_main", term)),
       aes(x=term, y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.1)+
  coord_flip()+ theme_minimal()+
  labs(title="Indice × Terzili distanza dal porto", x="", y="Coeff. (±95% CI)")


dfs <- final_regression_data_main |>
  mutate(
    expo_spatial = 1/(dist_min_port_main + 1),
    rel = pmax(pmin(year - 2015, 5), -5)
    # cap da -3 a +3
  )

m_es <- feols(
  n_conflicts ~ log_nightlight + expo_spatial:i(rel, ref=-1) | ADM2_NAME + year,
  data=dfs
)
summary(m_es)

# Estrai serie dinamica
es_df <- broom::tidy(m_es) %>% filter(grepl("expo_spatial:i\\(rel", term))
es_df$k <- as.integer(sub(".*rel, ref = -1\\)\\)([-0-9]+).*", "\\1", es_df$term))

ggplot(es_df, aes(x=k, y=estimate))+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_point()+
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.1)+
  theme_minimal()+
  labs(title="Event study: esposizione spaziale × (t−2015)",
       x="Anni da 2015 (baseline = -1)", y="Coeff. (±95% CI)")


dll <- final_regression_data_main |>
  arrange(ADM2_NAME, year) |>
  group_by(ADM2_NAME) |>
  mutate(ind_l2 = dplyr::lag(indice2_main,2),
         ind_l1 = dplyr::lag(indice2_main,1),
         ind_f1 = dplyr::lead(indice2_main,1)) |>
  ungroup()

m_ll <- feols(
  n_conflicts ~ ind_l2 + ind_l1 + indice2_main + ind_f1 + log_nightlight |
    ADM2_NAME + year,
   data=dll
)
summary(m_ll)


LAT_CUTOFF <- 8.5

adm2_lat <- coastal_admin2_master_main |>
  st_transform(4326) |>
  mutate(lat = st_coordinates(st_centroid(geometry))[,2]) |>
  st_drop_geometry() |>
  transmute(ADM2_NAME, macro_ns = ifelse(lat >= LAT_CUTOFF, "Nord", "Sud"))

df_ns <- final_regression_data_main |>
  left_join(adm2_lat, by="ADM2_NAME")

m_ns <- feols(
  n_conflicts ~ indice2_main * i(macro_ns) + log_nightlight | ADM2_NAME + year,
  data=df_ns
)
summary(m_ns)

# Plot
coef_ns <- broom::tidy(m_ns) %>% filter(grepl("indice2_main", term))
ggplot(coef_ns, aes(x=term, y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error), width=.1)+
  coord_flip()+ theme_minimal()+
  labs(title=paste0("Indice × Nord/Sud (cut @ ", LAT_CUTOFF, "°N)"),
       x="", y="Coeff. (±95% CI)")

final_regression_data_main <- final_regression_data_main%>%
  mutate(period = ifelse(year <= 2015, "Pre", "Post"))
# 0) esposizione spaziale fissa e periodi
dfs <- final_regression_data_main |>
  dplyr::mutate(
    expo_spatial = 1/(dist_min_port_main + 1),
    period = ifelse(year <= 2015, "Pre", "Post")
  )

m_prepost_idx <- feols(
  n_conflicts ~ indice2_main * i(period, ref = "Pre") + log_nightlight |
    ADM2_NAME + year,
  data = final_regression_data_main,
  cluster = ~ ADM2_NAME
)
summary(m_prepost_idx)
wald(m_prepost_idx, "indice2_main:periodPost = 0")

dll <- final_regression_data_main |>
  arrange(ADM2_NAME, year) |>
  group_by(ADM2_NAME) |>
  mutate(ind_l1 = dplyr::lag(indice2_main,1),
         ind_l2 = dplyr::lag(indice2_main,2),
         ind_f1 = dplyr::lead(indice2_main,1)) |>
  ungroup()

m_ll <- feols(n_conflicts ~ ind_l2 + ind_l1 + indice2_main + ind_f1 + log_nightlight |
                ADM2_NAME + year,
              data = dll, cluster = ~ ADM2_NAME)
m_ll

##in base a trade industries 
library(sf); library(dplyr)

# === INPUT attesi ===
# ports_main_proj: sf con una colonna identificativa del porto, es. MAIN_PORT_
# coastal_admin2_master_main: sf ADM2 costieri (con ADM2_NAME, ADM1_NAME, geometry)
# final_regression_data_main: pannello ADM2×anno (con ADM2_NAME, year, indice2_main, log_nightlight, ecc.)

# A) Mappa dei profili settoriali dei porti  (modifica i nomi per allinearli ai tuoi)
chem_first <- c("LAGOS", "TIN CAN ISLAND", "WARRI")          # Vegetables > Chemicals
veg_first <- c("PORT HARCOURT", "ONNE", "CALABAR")         # Chemicals > Vegetables

port_meta <- tibble::tibble(
  MAIN_PORT_ = c(veg_first, chem_first),
  port_sector = c(rep("Vegetables-first", length(veg_first)),
                  rep("Chemicals-first", length(chem_first)))
)

# B) Associa la meta ai porti e trova per ogni ADM2 il porto principale più vicino
ports_tagged <- ports_main_proj %>%
  left_join(port_meta, by = "MAIN_PORT_")

adm2_cent <- coastal_admin2_master_main %>%
  st_transform(st_crs(ports_tagged)) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  dplyr::select(ADM2_NAME, geometry = centroid)

nearest_id <- st_nearest_feature(adm2_cent, ports_tagged)

adm2_port <- adm2_cent %>%
  mutate(
    nearest_port = ports_tagged$MAIN_PORT_[nearest_id],
    port_sector  = ports_tagged$port_sector[nearest_id]
  ) %>%
  st_drop_geometry()

# C) Porta `port_sector` dentro al tuo pannello
df_sector <- final_regression_data_main %>%
  left_join(adm2_port[, c("ADM2_NAME","nearest_port","port_sector")], by = "ADM2_NAME") %>%
  mutate(
    port_sector = ifelse(is.na(port_sector), "Unknown", port_sector)
  )

m_sector <- feols(
  n_conflicts ~ indice2_main * i(port_sector, ref = "Vegetables-first") + log_nightlight |
    ADM2_NAME + year,
  data = df_sector,
  cluster = ~ ADM2_NAME   # resta coerente con ADM2 come vuoi tu
)
summary(m_sector)

##import/exp
# Esempio: aggiorna i nomi con i tuoi
import_ports <- c("ONNE", "CALABAR")
export_ports <- c("LAGOS", "TIN CAN ISLAND", "PORT HARCOURT", "WARRI")

port_flow <- tibble::tibble(
  MAIN_PORT_ = c(import_ports, export_ports),
  flow_group = c(rep("Import-oriented", length(import_ports)),
                 rep("Export-oriented", length(export_ports)))
)

ports_tagged2 <- ports_main_proj %>% left_join(port_flow, by = "MAIN_PORT_")

nearest_id2 <- st_nearest_feature(adm2_cent, ports_tagged2)
adm2_flow <- adm2_cent %>%
  mutate(flow_group = ports_tagged2$flow_group[nearest_id2]) %>%
  st_drop_geometry()

df_flow <- final_regression_data_main %>%
  left_join(adm2_flow[, c("ADM2_NAME","flow_group")], by = "ADM2_NAME")

m_flow <- feols(
  n_conflicts ~ indice2_main * i(flow_group, ref = "Export-oriented") + log_nightlight |
    ADM2_NAME + year,
  data = df_flow, cluster = ~ ADM2_NAME
)
summary(m_flow)

port_flow_map <- tibble::tibble(
  MAIN_PORT_ = c(import_ports, export_ports),
  flow_group = c(rep("Import", length(import_ports)),
                 rep("Export", length(export_ports)))
)

# 2) Tagga i porti e trova per ogni ADM2 il porto principale più vicino
ports_tagged <- ports_main_proj %>%
  left_join(port_flow_map, by = "MAIN_PORT_")

adm2_cent <- coastal_admin2_master_main %>%
  st_transform(st_crs(ports_tagged)) %>%
  mutate(centroid = st_centroid(geometry)) %>%
  dplyr::select(ADM2_NAME, geometry = centroid)

nearest_id <- st_nearest_feature(adm2_cent, ports_tagged)

adm2_flow <- adm2_cent %>%
  mutate(
    nearest_port = ports_tagged$MAIN_PORT_[nearest_id],
    flow_group   = ports_tagged$flow_group[nearest_id]
  ) %>%
  st_drop_geometry()

# 3) Porta flow_group nel pannello e imposta baseline = "Export"
df_flow <- final_regression_data_main %>%
  left_join(adm2_flow[, c("ADM2_NAME","nearest_port","flow_group")], by = "ADM2_NAME") %>%
  mutate(
    flow_group = ifelse(is.na(flow_group), "Export", flow_group),  # fallback prudente
    flow_group = factor(flow_group, levels = c("Export","Import")) # baseline: Export
  )



# 5) Stime

## 5a) Interazione semplice: effetto dell'indice che cambia tra Export vs Import
m_flow_int <- feols(
  n_conflicts ~ indice2_main * i(flow_group, ref = "Export") + log_nightlight |
    ADM2_NAME + year,
  data = df_flow,
  cluster = ~ ADM2_NAME   # oppure ~ ADM1_NAME se preferisci
)
summary(m_flow_int)

m_flow_int_dist <- feols(
  n_conflicts ~ indice2_main * i(flow_group, ref = "Export") +
    log_nightlight + log(1 + dist_min_port_main) |
    ADM2_NAME + year,
  data = df_flow,
  cluster = ~ ADM2_NAME
)
summary(m_flow_int_dist)

## 5c) (Facoltativo) Stime separate per gruppo (controllo robustezza)
m_export <- feols(
  n_conflicts ~ indice2_main + log_nightlight | ADM2_NAME + year,
  data = subset(df_flow, flow_group == "Export"),
  cluster = ~ ADM2_NAME
)
m_import <- feols(
  n_conflicts ~ indice2_main + log_nightlight | ADM2_NAME + year,
  data = subset(df_flow, flow_group == "Import"),
  cluster = ~ ADM2_NAME
)
summary(m_export); summary(m_import)


##tribes 
tribes <- read_sf("tribalmap.shp"); tribes
st_crs(tribes)
st_crs(nig)
tribes_wgs84 <- sf::st_transform(tribes, sf::st_crs(nig))
nt <- st_intersection(tribes_wgs84, nig)
ggplot() +
  geom_sf(data = nig) +
  geom_sf(data = nt, color = "red")

agri_land <- read_excel("FAOSTAT_data_en_4-16-2024- Agricultural land used 2.xlsx")
#constant value but aggregated 
#1 when land is used for agriculture and 0 otherwise 
agri_land$Item <- ifelse(agri_land$Item == "Agricultural land", 1, 0)

# Example code continuation
library(dplyr)

agri <- agri_land %>%
  filter(Item == 1, Element == "Area") %>%   # tieni solo "Agricultural land"
  dplyr::select(year = Year, agri_land_thousand_ha = Value) %>%
  arrange(year)
