# ============================================================
# IJC445 Coursework — Composite Visualisation (2015–2024)
# Colourful + colourblind-friendly (VIRIDIS: plasma/inferno)
#
# Saves: composite + each chart into /outputs
# Dataset: data/passenger_journeys_sector_quarterly.csv
# ============================================================

# ---- Packages ----
# If needed once:
# install.packages(c("tidyverse","lubridate","scales","patchwork","viridis"))

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(viridis)

# ---- Create folders (safe if they already exist) ----
dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# ---- 1) Load and clean the raw CSV ----
file <- "data/passenger_journeys_sector_quarterly.csv"

raw <- readr::read_csv(file, col_names = FALSE, show_col_types = FALSE)
names(raw) <- c("time_period", "long_distance", "london_se", "regional", "open_access", "total")

quarter_all <- raw %>%
  filter(
    !is.na(time_period),
    str_detect(time_period, "^[A-Za-z]{3} to [A-Za-z]{3} \\d{4}")
  ) %>%
  mutate(
    across(c(long_distance, london_se, regional, open_access, total),
           ~ if_else(as.character(.) == "[z]", "0", as.character(.))),
    across(c(long_distance, london_se, regional, open_access, total),
           ~ readr::parse_number(.)),
    start_month = str_extract(time_period, "^[A-Za-z]{3}"),
    year = str_extract(time_period, "\\d{4}") %>% as.integer(),
    date = ymd(paste(year, start_month, "01"))
  ) %>%
  select(date, time_period, long_distance, london_se, regional, open_access, total) %>%
  arrange(date)

tail(quarter_all$time_period, 10)


# ---- 2) Filter ONCE to 2015–2024 ----
quarter <- quarter_all %>%
  filter(year(date) >= 2015, year(date) <= 2024)

yr_min <- min(year(quarter$date))
yr_max <- max(year(quarter$date))

break_years <- sort(unique(c(seq(yr_min, yr_max, by = 2), yr_max)))
year_breaks <- ymd(paste0(break_years, "-01-01"))
year_breaks <- as.Date(year_breaks)

# ---- 3) Long format sector table ----
sector_long <- quarter %>%
  pivot_longer(
    cols = c(long_distance, london_se, regional, open_access),
    names_to = "sector",
    values_to = "journeys"
  ) %>%
  mutate(
    sector = recode(
      sector,
      long_distance = "Long Distance",
      london_se     = "London & South East",
      regional      = "Regional",
      open_access   = "Open Access"
    ),
    # Fixed order gives consistent colour assignment across plots
    sector = factor(sector, levels = c("London & South East", "Regional", "Long Distance", "Open Access"))
  )

# ============================================================
# CHART 1 — 100% stacked area (sector shares) — PLASMA
# ============================================================
sector_shares <- sector_long %>%
  group_by(date) %>%
  mutate(share = journeys / sum(journeys, na.rm = TRUE)) %>%
  ungroup()

p1_area <- ggplot(sector_shares, aes(x = date, y = share, fill = sector)) +
  geom_area() +
  
  # x-axis labels incl. 2025
  scale_x_date(
    breaks = year_breaks,
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "plasma", end = 0.95) +
  
  # simple COVID annotation (start of major disruption)
  geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dashed") +
  annotate(
    "label",
    x = as.Date("2020-04-01"), y = 0.98,
    label = "COVID disruption",
    fill = "white", colour = "black",
    alpha = 0.9, size = 3,
    vjust = 0.5, hjust = 0,
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.15, "lines")
  ) +
  
  labs(
    title = "Sector share of rail passenger journeys (2015–2024)",
    x = NULL, y = "Share of journeys",
    fill = "Sector",
    caption = paste(
      "Source:  Office of Rail and Road (ORR). 100% stacked area shows sector share; [z] treated as 0.",
      sep = ""
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, colour = "grey30", hjust = 0.5),
    plot.margin = margin(5.5, 5.5, 20, 5.5)
  )


# ============================================================
# CHART 2 — Heatmap (Year × Quarter) of TOTAL journeys — INFERNO
# ============================================================
heat <- quarter %>%
  mutate(
    yr  = year(date),
    qtr = paste0("Q", quarter(date, with_year = FALSE))
  ) %>%
  select(yr, qtr, total)

heat$qtr <- factor(heat$qtr, levels = c("Q1", "Q2", "Q3", "Q4"))

p2_heat <- ggplot(heat, aes(x = qtr, y = yr, fill = total)) +
  geom_tile(color = "grey85", linewidth = 0.25) +
  
  # Colour scale + better legend
  scale_fill_viridis_c(
    option = "inferno",
    end = 1,
    breaks = pretty_breaks(n = 5),
    labels = label_number()
  ) +
  guides(fill = guide_colorbar(barheight = unit(35, "mm"), ticks.colour = "grey30")) +
  
  # Make recent years appear at the top
  scale_y_reverse(breaks = seq(min(heat$yr), max(heat$yr), by = 1)) +
  
  # Highlight 2020 Q2 tile + label
  annotate(
    "label",
    x = "Q2", y = 2020,
    label = "Q2 2020\nLockdown",
    fill = "white", colour = "black",
    alpha = 0.9, size = 3, fontface = "bold",
    label.padding = unit(0.2, "lines"),
    label.r = unit(0.15, "lines")
  ) +
  
  
  labs(
    title = "Seasonality & shocks in total journeys (2015–2024)",
    x = NULL, y = NULL, fill = "Total (m)",
    caption = "Source: Office of Rail and Road (ORR) passenger journeys statistics. 2025 excluded here because it is incomplete/provisional."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    plot.caption = element_text(size = 9, colour = "grey30")
  )


# ============================================================
# CHART 3 — Dot plot (Cleveland) of annual journeys by sector
# Key years: 2015, 2019 (pre-shock), 2020 (shock), 2024 (latest complete year)
# ============================================================

key_years <- c(2015, 2019, 2020, 2024)

annual_total_key <- sector_long %>%
  mutate(yr = year(date)) %>%
  group_by(yr, sector) %>%
  summarise(journeys = sum(journeys, na.rm = TRUE), .groups = "drop") %>%
  filter(yr %in% key_years) %>%
  mutate(
    yr = factor(yr, levels = key_years),
    # keep a consistent y-order in all facets (largest at top)
    sector = fct_reorder(sector, journeys, .fun = max, .desc = TRUE)
  )

p3_dot <- ggplot(annual_total_key, aes(x = journeys, y = sector, colour = sector)) +
  geom_point(size = 3) +
  facet_wrap(~ yr, nrow = 1) +
  scale_color_viridis_d(option = "plasma", end = 1, guide = "none") +
  labs(
    title = "Annual passenger journeys by sector in key years",
    x = "Passenger journeys (millions)",
    y = NULL,
    caption = "Key years compare start of period (2015), pre-shock baseline (2019), disruption (2020), and latest complete year (2024)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(size = 9, colour = "grey30", hjust = 0.5)
  )



# ============================================================
# CHART 4 — HHI concentration (annual) — PLASMA
# HHI_10000 = sum((share*100)^2). Higher = more concentrated.
# ============================================================

# ANNUAL totals (used for chart 4-HHI)
annual <- sector_long %>%
  mutate(yr = year(date)) %>%
  group_by(yr, sector) %>%
  summarise(journeys = sum(journeys, na.rm = TRUE), .groups = "drop")


hhi <- annual %>%
  group_by(yr) %>%
  mutate(share = journeys / sum(journeys, na.rm = TRUE)) %>%
  summarise(
    HHI_10000 = sum((share * 100)^2, na.rm = TRUE),
    .groups = "drop"
  )

baseline_hhi <- hhi %>%
  filter(yr >= 2015, yr <= 2019) %>%
  summarise(b = mean(HHI_10000, na.rm = TRUE)) %>%
  pull(b)

p4_hhi <- ggplot(hhi, aes(x = yr, y = HHI_10000)) +
  geom_col(fill = viridis(1, option = "plasma", end = 0.9)) +
  
  # Baseline reference (pre-shock average)
  geom_hline(yintercept = baseline_hhi, linetype = "dashed", alpha = 0.6) +
  annotate(
    "label",
    x = min(hhi$yr) + 0.6, y = baseline_hhi,
    label = "Mean 2015–2019",
    fill = "white", colour = "black",
    alpha = 0.9, size = 3,
    hjust = 0, vjust = -0.4,
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.15, "lines")
  )+

  
  # 2020 marker
  geom_vline(xintercept = 2020, linetype = "dashed", alpha = 0.4) +
  
  scale_x_continuous(breaks = seq(min(hhi$yr), max(hhi$yr), by = 1)) +
  labs(
    title = "Concentration of journeys across sectors (HHI, annual)",
    x = NULL,
    y = "HHI (0–10,000; higher = more concentrated)",
    caption = "HHI = sum of squared sector shares × 10,000. Computed from ORR passenger journeys by sector (annual totals)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, colour = "grey30", hjust = 0.5)
  )


# ============================================================
# COMPOSITE (2 × 2)
# ============================================================

p1_area_c <- p1_area + labs(caption = NULL) + theme(plot.caption = element_blank())
p2_heat_c <- p2_heat + labs(caption = NULL) + theme(plot.caption = element_blank())
p3_dot_c  <- p3_dot  + labs(caption = NULL) + theme(plot.caption = element_blank())
p4_hhi_c  <- p4_hhi  + labs(caption = NULL) + theme(plot.caption = element_blank())

composite <- (p1_area_c | p2_heat_c) / (p3_dot_c | p4_hhi_c)
composite


# ============================================================
# SAVE FILES (PNG + PDF)
# ============================================================
ggsave(
  filename = "outputs/IJC445_composite_2015_2024_colourful.png",
  plot = composite,
  width = 14, height = 9, dpi = 300
)

ggsave(
  filename = "outputs/IJC445_composite_2015_2024_colourful.pdf",
  plot = composite,
  width = 14, height = 9
)

ggsave("outputs/chart1_sector_shares_plasma.png", p1_area, width = 10, height = 6, dpi = 300)
ggsave("outputs/chart2_heatmap_total_inferno.png", p2_heat, width = 10, height = 6, dpi = 300)
ggsave("outputs/chart3_dot_keyyears_plasma.png", p3_dot, width = 12, height = 4.5, dpi = 300)
ggsave("outputs/chart4_hhi_plasma.png", p4_hhi, width = 10, height = 6, dpi = 300)

print(list.files("outputs"))
