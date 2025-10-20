conflict <- read_excel("Cartel2.xlsx")
rapacity_sub_events <- c(
  "Looting/property destruction", 
  "Abduction/forced disappearance", 
  "Armed clash", 
  "Attack", 
  "Shelling/artillery/missile attack", 
  "Sexual violence"  # spesso associata a violenza predatoria
)


non_rapacity_sub_events <- c(
  "Peaceful protest", 
  "Violent demonstration", 
  "Protest with intervention", 
  "Agreement", 
  "Change to group/activity", 
  "Disrupted weapons use", 
  "Arrests", 
  "Headquarters or base established", 
  "Non-violent transfer of territory", 
  "Non-state actor overtakes territory", 
  "Government regains territory", 
  "Other"  # da valutare caso per caso
)


conflict_non_rapacity <- conflict %>%
  filter(
    sub_event_type %in% non_rapacity_sub_events |
      grepl("loot|steal|raid|resource|gold|oil|land|plunder|livestock|pasture|farm|grazing", 
            tolower(notes))
  )


conflict_non_rapacity_clean <- conflict_non_rapacity[complete.cases(conflict_non_rapacity$longitude, conflict_non_rapacity$latitude), ]

# Count the number of removed lines
rows_removed <- nrow(conflict_non_rapacity) - nrow(conflict_non_rapacity_clean)
print(paste("Righe rimosse:", rows_removed))

conflict_sf <- st_as_sf(conflict_non_rapacity_clean, coords = c("longitude", "latitude"), crs = st_crs(nig))

# Ensure both are in the same CRS
grid_transformed <- st_transform(grid, "+init=EPSG:26393")
conflict_sf_transformed <- st_transform(conflict_sf, "+init=EPSG:26393")

# Check the CRS to ensure they match
print(st_crs(grid_transformed))
print(st_crs(conflict_sf_transformed))

# Check a few entries from conflict_sf_transformed
head(conflict_sf_transformed)

# Check the bounding box of both datasets to ensure overlap
print(st_bbox(grid_transformed))
print(st_bbox(conflict_sf_transformed))

# Print the original CRS of conflict_sf
print(st_crs(conflict_sf))

conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393)

# Verify the bounding box after transformation
print(st_bbox(conflict_sf_transformed))

print(st_crs(conflict_sf))



# Step 2: Re-transform conflict_sf to EPSG:26393
conflict_sf_transformed <- st_transform(conflict_sf, crs = 26393)
# Verify the bounding box after transformation
print(st_bbox(conflict_sf_transformed))

# Step 3: Perform spatial join and summarize
conflict_sf1 <- st_join(grid_transformed, conflict_sf_transformed) %>%
  group_by(FIP, admin2, year) %>%
  summarise(n_conflicts = n(), .groups = 'drop')

# Check the resulting data
head(conflict_sf1)
conflict_sf_transformed1 <- conflict_sf_transformed %>%
  mutate(conflict_id = row_number())  # Create a unique identifier

# Perform the spatial join and ensure conflict_id is retained

conflict_sf3 <- st_join(grid_transformed, conflict_sf_transformed) %>% 
  group_by(FIP, year, event_type,fatalities,event_id_cnty) %>% 
  summarise(n_conflicts = n())

# 1. Assegna il CRS corretto originale, cioè lat/lon (WGS84)
st_crs(conflict_sf) <- 4326

# 2. Ora trasforma le coordinate nel CRS di 'nig' (EPSG:26393)
conflict_sf <- st_transform(conflict_sf, st_crs(nig))
#plotting conflict 
pc_2009 <- ggplot()+
  geom_sf(data = nig, fill = NA)+
  geom_sf(data = conflict_sf%>%filter(year==2009))+
  labs(title = "Conflict in Nigeria - 2009") +  
  theme_minimal()

pc_2012 <- ggplot()+
  geom_sf(data = nig, fill = NA)+
  geom_sf(data = conflict_sf%>%filter(year==2012))+
  labs(title = "Conflict in Nigeria - 2012") +  
  theme_minimal()
pc_2015 <-ggplot()+
  geom_sf(data = nig, fill = NA)+
  geom_sf(data = conflict_sf%>%filter(year==2015))+
  labs(title = "Conflict in Nigeria - 2015") +  
  theme_minimal()
pc_2019 <- ggplot()+
  geom_sf(data = nig, fill = NA)+
  geom_sf(data = conflict_sf%>%filter(year==2019))+
  labs(title = "Conflict in Nigeria - 2019") +  
  theme_minimal()

p_combined4 <- pc_2009+ pc_2012 + pc_2015 + pc_2019 + 
  plot_layout(ncol = 2, nrow = 2) 
p_combined4



coastal_admin2 <- st_transform(admin2_in_coastal_admin1, crs = st_crs(conflict_sf))
within_matrix <- st_within(conflict_sf, coastal_admin2, sparse = FALSE)
# within_matrix è una matrice: righe = punti, colonne = poligoni costieri

# Per ogni punto, verifica se è dentro almeno un poligono
inside_any <- apply(within_matrix, 1, any)

# Ora puoi filtrare i punti
conflict_coastal <- conflict_sf[inside_any, ]

ggplot() +
  geom_sf(data = coastal_admin2, fill = "lightblue", color = "black") +  # Poligoni costieri
  geom_sf(data = conflict_coastal, color = "red", size = 1, alpha = 0.7) +  # Punti conflitti
  theme_minimal() +
  ggtitle("Conflitti nella zona costiera")
pc_2009 <- ggplot()+
  geom_sf(data = coastal_admin2, fill = NA)+
  geom_sf(data = conflict_coastal%>%filter(year==2009), color = "red", size = 1, alpha = 0.7)+
  labs(title = "Conflict in Nigeria - 2009") +  
  theme_minimal()

pc_2012 <- ggplot()+
  geom_sf(data = coastal_admin2, fill = NA)+
  geom_sf(data = conflict_coastal%>%filter(year==2012), color = "red", size = 1, alpha = 0.7)+
  labs(title = "Conflict in Nigeria - 2012") +  
  theme_minimal()
pc_2015 <-ggplot()+
  geom_sf(data = coastal_admin2, fill = NA)+
  geom_sf(data = conflict_coastal%>%filter(year==2015), color = "red", size = 1, alpha = 0.7)+
  labs(title = "Conflict in Nigeria - 2015") +  
  theme_minimal()
pc_2019 <- ggplot()+
  geom_sf(data = coastal_admin2, fill = NA)+
  geom_sf(data = conflict_coastal%>%filter(year==2019), color = "red", size = 1, alpha = 0.7)+
  labs(title = "Conflict in Nigeria - 2019") +  
  theme_minimal()

p_combined <- pc_2009+ pc_2012 + pc_2015 + pc_2019 + 
  plot_layout(ncol = 2, nrow = 2) 
p_combined
ggsave("conflict_cost.pdf", # Save as PDF
       plot = p_combined,
       width = 5,                 # Width in inches
       height = 8,                 # Height in inches
       dpi = 300)                  # DPI is less critical for vector graphics, but good practice


# Perform the spatial join to get conflicts within the master coastal ADM2s
coastal_admin2_master_proj <- st_transform(coastal_admin2_master, crs = 4326)
coastal_admin2_master_proj <- st_transform(coastal_admin2_master, crs = 26393)
st_crs(conflict_sf_transformed) <- 4326
conflict_sf_transformed <- st_transform(conflict_sf_transformed, 4326)
conflict_sf_transformed <- st_transform(conflict_sf_transformed, 26393)
# Using st_intersects for points within polygons, or st_within for stricter containment

conflict_coastal <- st_join(conflict_sf_transformed, coastal_admin2_master_proj, join = st_intersects, left = FALSE)

## Requisiti:
## - conflict_sf: punti degli eventi, ha almeno 'year'
## - coastal_admin2_master: poligoni ADM2 costieri, ha almeno 'ADM2_NAME'
## - Stesso CRS (se no scommenta la riga sotto)
conflict_sf <- sf::st_transform(conflict_sf, sf::st_crs(coastal_admin2_master))

## 1) Join spaziale: assegna ogni evento a un ADM2
ev_join <- sf::st_join(conflict_sf, coastal_admin2_master, join = sf::st_within, left = FALSE)
ev_df   <- as.data.frame(sf::st_drop_geometry(ev_join))

## 2) Vettori universo (tutti gli ADM2 e tutti gli anni del periodo)
adm2_vec  <- unique(as.character(as.data.frame(sf::st_drop_geometry(coastal_admin2_master))$ADM2_NAME))
years_vec <- sort(unique(ev_df$year))   # oppure: years_vec <- seq(2009, 2019)

## 3) Conteggi per ADM2 × anno (solo eventi osservati)
counts_raw <- aggregate(list(n_conflicts = ev_df$year),
                        by = list(ADM2_NAME = ev_df$ADM2_NAME,
                                  year      = ev_df$year),
                        FUN = length)

## 4) Pannello completo ADM2 × anno (riempi gli zeri dove mancano eventi)
panel <- merge(data.frame(ADM2_NAME = adm2_vec, stringsAsFactors = FALSE),
               data.frame(year = years_vec), all = TRUE)
conflicts_adm2_year <- merge(panel, counts_raw, by = c("ADM2_NAME","year"), all.x = TRUE)
conflicts_adm2_year$n_conflicts[is.na(conflicts_adm2_year$n_conflicts)] <- 0L

## --- STATISTICHE DESCRITTIVE ---

## 5) Complessive (tutto il pannello)
x <- conflicts_adm2_year$n_conflicts
mean_x   <- mean(x); median_x <- median(x); sd_x <- sd(x)
min_x    <- min(x);  max_x    <- max(x);    share0 <- mean(x == 0)

cat("\n--- Descriptive stats (ADM2-year, overall) ---\n")
cat(sprintf("Mean: %.3f\nMedian: %.3f\nStd. Dev.: %.3f\nMin: %d\nMax: %d\nShare of zeros: %.3f\n",
            mean_x, median_x, sd_x, min_x, max_x, share0))

## 6) Per anno (utile per tabella LaTeX con una riga per anno)
by_year_fun <- function(v, yr) tapply(v, yr, function(z) c(Mean=mean(z), Median=median(z),
                                                           SD=sd(z), Min=min(z), Max=max(z),
                                                           ShareZeros=mean(z==0)))
by_year_mat <- do.call(rbind, by_year_fun(conflicts_adm2_year$n_conflicts, conflicts_adm2_year$year))
by_year_mat <- round(by_year_mat, 3)

cat("\n--- Descriptive stats by year (rows = year) ---\n")
print(by_year_mat)

## --- TESTI (numeri inseriti automaticamente) ---

cat("\n--- Text for Table Z ---\n")
cat(sprintf(paste0(
  "Table Z summarizes the distribution of conflict events across ADM2-years. ",
  "The outcome is highly skewed: while the average ADM2-year experiences fewer than %.1f events, ",
  "some units record more than %d, reflecting the localized and episodic nature of conflict.\n"),
  mean_x, max_x))

cat("\n--- Text for Figure W ---\n")
cat(sprintf(paste0(
  "Figure W depicts the distribution of conflict counts across ADM2-years. ",
  "Most observations register no events (share of zeros ≈ %.2f), ",
  "while a small number of ADM2s account for a disproportionate share of violent episodes.\n"),
  share0))

## --- GRAFICI (CON ANNI IN ASCISSA) ---

## 7) Serie storica: totale eventi per anno (somma su tutti gli ADM2)
tot_per_year <- aggregate(n_conflicts ~ year, data = conflicts_adm2_year, sum)

plot(tot_per_year$year, tot_per_year$n_conflicts, type = "b", pch = 16,
     xlab = "Year", ylab = "Total conflict events",
     main = "Total conflict events per year (sum over ADM2)")

## 8) Boxplot della distribuzione per anno (ADM2-year)
##    (mostra la dispersione tra ADM2 per ciascun anno)
boxplot(n_conflicts ~ year, data = conflicts_adm2_year,
        xlab = "Year", ylab = "Conflicts per ADM2-year",
        main = "Distribution of conflicts per ADM2-year by year", outline = FALSE)

## 9) Istogramma complessivo (tutti gli anni insieme, bin = 1)
brks <- seq(-0.5, max_x + 0.5, by = 1)
hist(x, breaks = brks, main = "Histogram: conflicts per ADM2-year (all years)",
     xlab = "Number of events", ylab = "Count of ADM2-years")

## 10) (Opzionale) Densità solo per positivi
xp <- x[x > 0]
if (length(xp) > 10) {
  d <- density(xp)
  plot(d, main = "Kernel density of positive conflict counts", xlab = "Events (>0)")
}

# ----------------------------------------------
# Rapacity vs Non-Rapacity: Panels, Descriptives & Figures
# ----------------------------------------------
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(scales)
library(kableExtra)
library(stringr)
library(patchwork)

# ---- 0) Inputs (your vectors) ----
rapacity_sub_events <- c(
  "Looting/property destruction", 
  "Abduction/forced disappearance", 
  "Armed clash", 
  "Attack", 
  "Shelling/artillery/missile attack", 
  "Sexual violence"
)

non_rapacity_sub_events <- c(
  "Peaceful protest", 
  "Violent demonstration", 
  "Protest with intervention", 
  "Agreement", 
  "Change to group/activity", 
  "Disrupted weapons use", 
  "Arrests", 
  "Headquarters or base established", 
  "Non-violent transfer of territory", 
  "Non-state actor overtakes territory", 
  "Government regains territory", 
  "Other"
)

# ---- 1) Harmonize CRS and classify events ----
# Ensure both layers share the same CRS
if (st_crs(conflict_sf)$epsg != st_crs(coastal_admin2_master)$epsg) {
  conflict_sf <- st_transform(conflict_sf, st_crs(coastal_admin2_master))
}

# Keep only Rapacity / Non-Rapacity; drop the rest to keep types clean and comparable
events_labeled <- conflict_sf |>
  mutate(Type = case_when(
    sub_event_type %in% rapacity_sub_events ~ "Rapacity",
    sub_event_type %in% non_rapacity_sub_events ~ "Non-Rapacity",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(Type))

# ---- 2) Assign ADM2 and aggregate to ADM2 × year × Type ----
# - strict containment; switch to st_intersects if needed
ev_join <- st_join(events_labeled, coastal_admin2_master, join = st_within, left = FALSE)
ev_df   <- st_drop_geometry(ev_join)

stopifnot(all(c("ADM2_NAME","year","Type") %in% names(ev_df)))

counts_raw <- ev_df |>
  mutate(year = as.integer(year)) |>
  count(ADM2_NAME, year, Type, name = "n_conflicts")

# Universe of ADM2 × years (fill zeros)
adm2_vec  <- sort(unique(coastal_admin2_master$ADM2_NAME))
years_vec <- sort(unique(ev_df$year))
# If you want to force 2009–2019, uncomment:
# years_vec <- 2009:2019

panel_univ <- tidyr::expand_grid(ADM2_NAME = adm2_vec, year = years_vec, Type = c("Rapacity","Non-Rapacity"))

adm2_year_type <- panel_univ |>
  left_join(counts_raw, by = c("ADM2_NAME","year","Type")) |>
  mutate(n_conflicts = replace_na(n_conflicts, 0L))

# Wide version (handy for some stats)
adm2_year_wide <- adm2_year_type |>
  tidyr::pivot_wider(
    names_from = Type,
    values_from = n_conflicts,
    values_fill = 0L,
    names_prefix = "n_"
  ) |>
  dplyr::rename(
    n_conflicts_rap    = `n_Rapacity`,
    n_conflicts_nonrap = `n_Non-Rapacity`
  ) |>
  dplyr::mutate(total = n_conflicts_rap + n_conflicts_nonrap)
# ---- 3) DESCRIPTIVE TABLE (overall, by type) ----
desc_by_type <- adm2_year_type |>
  group_by(Type) |>
  summarise(
    Mean          = mean(n_conflicts),
    Median        = median(n_conflicts),
    SD            = sd(n_conflicts),
    Min           = min(n_conflicts),
    Max           = max(n_conflicts),
    Share_zeros   = mean(n_conflicts == 0),
    .groups = "drop"
  )

# Year of peak activity (summing over ADM2)
peak_year <- adm2_year_type |>
  group_by(Type, year) |>
  summarise(Total = sum(n_conflicts), .groups = "drop_last") |>
  slice_max(order_by = Total, n = 1, with_ties = FALSE) |>
  ungroup() |>
  dplyr::select(Type, Peak_year = year, Peak_total = Total)

desc_table <- desc_by_type |>
  left_join(peak_year, by = "Type") |>
  mutate(
    Mean = round(Mean, 3), Median = round(Median, 3), SD = round(SD, 3),
    Share_zeros = round(Share_zeros, 3)
  )

cat("\n--- DESCRIPTIVE TABLE: Rapacity vs Non-Rapacity (ADM2-year) ---\n")
print(desc_table)

# ---- 4) FIGURES ----
dir.create("fig", showWarnings = FALSE)

## (a) National totals per year (line plot)
totals <- adm2_year_type |>
  group_by(Type, year) |>
  summarise(Events = sum(n_conflicts), .groups = "drop")

p_totals <- ggplot(totals, aes(x = year, y = Events, color = Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Rapacity"="#d73027","Non-Rapacity"="blue")) +
  scale_x_continuous(breaks = pretty(totals$year)) +
  labs(title = "Total conflict events per year",
       x = NULL, y = "Number of events", color = NULL) +
  theme_minimal(base_size = 12)
ggsave("fig/fig_totals_rap_vs_nonrap.pdf", p_totals, width = 7, height = 4.5)

## (b) Boxplot: distribution across ADM2 by year and type
p_box <- ggplot(adm2_year_type, aes(x = factor(year), y = n_conflicts, fill = Type)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("Rapacity"="#b2182b","Non-Rapacity"="#2166ac")) +
  labs(title = "Distribution of conflicts per ADM2-year",
       x = "Year", y = "Conflicts per ADM2-year", fill = NULL) +
  theme_minimal(base_size = 12)
ggsave("fig/fig_box_rap_vs_nonrap.pdf", p_box, width = 8, height = 4.5)

## (c) Spatial maps: average yearly intensity per ADM2 (facet by Type)
adm2_means <- adm2_year_type |>
  group_by(ADM2_NAME, Type) |>
  summarise(avg_yearly = mean(n_conflicts), .groups = "drop")

map_df <- coastal_admin2_master |>
  dplyr::select(ADM2_NAME) |>
  left_join(adm2_means, by = "ADM2_NAME")

p_map <- ggplot(map_df) +
  geom_sf(aes(fill = avg_yearly), color = NA) +
  facet_wrap(~Type) +
  scale_fill_viridis(option = "C", trans = "sqrt", labels = label_number(accuracy = 0.1)) +
  labs(title = "Average yearly conflicts per ADM2 (2009–2019)",
       fill = "Avg / year") +
  theme_minimal(base_size = 12)
ggsave("fig/fig_map_avg_by_type.pdf", p_map, width = 9, height = 5.5)

## (d) Ratio map: dominance log(Rapacity / Non-Rapacity)
ratio_df <- adm2_year_wide |>
  group_by(ADM2_NAME) |>
  summarise(
    rap = sum(n_conflicts_rap),
    nonrap = sum(n_conflicts_nonrap),
    ratio = (rap + 1) / (nonrap + 1),
    .groups = "drop"
  )

ratio_map <- coastal_admin2_master |>
  dplyr::select(ADM2_NAME) |>
  left_join(ratio_df, by = "ADM2_NAME") |>
  mutate(log_ratio = log(ratio))

p_ratio <- ggplot(ratio_map) +
  geom_sf(aes(fill = log_ratio), color = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "#d73027",
                       midpoint = 0, name = "log Rapacity/Non-Rapacity") +
  labs(title = "Dominant conflict type by ADM2") +
  theme_minimal(base_size = 12)
ggsave("fig/fig_map_ratio_log.pdf", p_ratio, width = 7.2, height = 5)

## (e) Fatalities per event (optional)
fatal_ts <- events_labeled |>
  st_drop_geometry() |>
  group_by(Type, year) |>
  summarise(mean_fatal = mean(fatalities, na.rm = TRUE),
            median_fatal = median(fatalities, na.rm = TRUE),
            .groups = "drop")

p_fatal <- ggplot(fatal_ts, aes(year, mean_fatal, color = Type)) +
  geom_line(linewidth = 1.1) + geom_point() +
  scale_color_manual(values = c("Rapacity"="#d73027","Non-Rapacity"="#4575b4")) +
  labs(title = "Average fatalities per event",
       x = NULL, y = "Mean fatalities") +
  theme_minimal(base_size = 12)
ggsave("fig/fig_fatalities_mean_by_type.pdf", p_fatal, width = 7, height = 4.2)

# ---- 5) Short narrative snippets for your text (auto-filled numbers) ----
desc_txt <- desc_table |>
  mutate(
    Mean = sprintf("%.2f", Mean),
    SD   = sprintf("%.2f", SD),
    Share_zeros = sprintf("%.2f", Share_zeros)
  )

cat("\n--- TEXT SNIPPETS ---\n")
for (i in seq_len(nrow(desc_txt))) {
  tt <- desc_txt[i, ]
  cat(sprintf(
    "%s events average %s per ADM2-year (SD %s), with zeros in %s of observations. Peak activity occurs in %d (Total=%d).\n",
    tt$Type, tt$Mean, tt$SD, tt$Share_zeros, tt$Peak_year, tt$Peak_total
  ))
}

library(dplyr)
library(ggplot2)

min_events <- 5L  # threshold for masking low-activity ADM2s

ratio_map <- coastal_admin2_master %>%
  dplyr::select(ADM2_NAME) %>%
  dplyr::left_join(
    adm2_year_wide %>%
      dplyr::group_by(ADM2_NAME) %>%
      dplyr::summarise(
        rap   = sum(n_conflicts_rap, na.rm = TRUE),
        non   = sum(n_conflicts_nonrap, na.rm = TRUE),
        total = rap + non,
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        ratio     = (rap + 1) / (non + 1),         # add-one smoothing
        log_ratio = log(ratio),
        keep      = dplyr::if_else(total >= min_events, TRUE, FALSE)
      ),
    by = "ADM2_NAME"
  )

# symmetric color limits around 0, trimmed to 2nd–98th pct to avoid extreme outliers
finite_vals <- ratio_map$log_ratio[is.finite(ratio_map$log_ratio)]
if (length(finite_vals) == 0) finite_vals <- 0
lims <- max(abs(stats::quantile(finite_vals, c(.02, .98), na.rm = TRUE)))

# legend ticks as odds ratios
brks <- log(c(1/8, 1/4, 1/2, 1, 2, 4, 8))
labs <- c("1:8","1:4","1:2","1:1","2:1","4:1","8:1")

p_ratio_nice <- ggplot() +
  geom_sf(data = dplyr::filter(ratio_map, keep),
          aes(fill = log_ratio), color = "white", linewidth = 0.2) +
  geom_sf(data = dplyr::filter(ratio_map, !keep),
          fill = "grey92", color = "white", linewidth = 0.2, alpha = 0.8) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "#b40426",
    midpoint = 0, limits = c(-lims, lims),
    breaks = brks, labels = labs,
    name = "Rapacity : Non-rapacity"
  ) +
  coord_sf(datum = NA) +
  labs(
    title = "Dominant conflict type by ADM2 (coastal regions)",
    subtitle = paste0("Red = Rapacity-dominated | Blue = Non-rapacity-dominated | Grey = <", min_events, " total events"),
    caption = "Note: add-one smoothing avoids zeros; symmetric scale around log(1)=0."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2.2, "cm"),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    plot.title   = element_text(face = "bold")
  ) +
  guides(fill = guide_colorbar(barheight = unit(0.5, "cm"), barwidth = unit(12, "cm")))

ggsave("fig/fig_map_ratio_log_pretty.pdf", p_ratio_nice, width = 8.5, height = 5.3, device = cairo_pdf)
ggsave("fig/fig_map_ratio_log_pretty.png", p_ratio_nice, width = 8.5, height = 5.3, dpi = 300)
