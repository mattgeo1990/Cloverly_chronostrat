# ================================================================
# midK_chemostrat_plots_CLEAN.R
# Purpose: Reproduce mid-Cretaceous chemostratigraphy plots.
# Notes:
#   • This file keeps the plotting code unchanged to preserve figures.
#   • Only readability and organization are improved (comments, spacing).
#   • Assumes 'scripts/setup.R' loads packages and data as in your project.
# Author: <your name>
# Last updated: <today>
# ================================================================


# --- Setup ---
# Check if data is loaded; if not, run setup
if (!exists("CLC_d13C_clean") || !exists("CCC_d13C_clean") ||
    !exists("combined_isotope_data")) {
  source(here::here("scripts", "setup.R"))
}

# Create delta notation for x-axis
Cdelt <- expression("δ"^13 * "Corg (‰ vs. VPDB)")


# --- Plot: Full Composite (Herrle et al., 2015) ---
HerrleComp <- ggplot(data, aes(x = d13Corg_VPBD, y = Age_Ma_Gradstein)) +
  geom_point(alpha = 0.5, size = 1.5, color = "blue") +
  geom_path(aes(x = rollmean(d13Corg_VPBD, 3, na.pad = TRUE))) +
  labs(
    x = Cdelt,
    y = "Age (Ma; Gradstein et al. 2012)",
    title = "Herrle et al. (2015) composite"
  ) +
  scale_y_reverse(
    breaks = seq(
      floor(88),
      ceiling(max(data$Age_Ma_Gradstein, na.rm = TRUE)),
      by = 2
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )

# Print the composite plot
print(HerrleComp)

# Export plot PNG
ggsave(
  filename = here("results", "figures", "HerrleComp_chemostrat.png"),
  plot = HerrleComp,
  width = 2,
  height = 8,
  dpi = 300
)

# Erba et al. (1999) ------------------------------------------------------

# subset
Erba99 <- subset(data, Source_C_data == "Erba_et_al._(1999)")

# sort depth
  # initialize age column

  Erba99 <- Erba99[(order(Erba99$Age_Ma_Gradstein)), ]

# create the ggplot object
  ggErba99 <- ggplot(Erba99, aes(x = d13Corg_VPBD, y = Age_Ma_Gradstein)) +
    geom_point(size = 1.5, color = "blue", alpha = 0.2) +
    geom_path(aes(x = rollmean(d13Corg_VPBD, 3, na.pad = TRUE))) +
    labs(
      x = Cdelt,
      y = "Age (Ma) Gradstein et al. 2012?",
      title = "Erba et al. (1999)"
    ) +
    scale_y_reverse(
      breaks = seq(
        floor(88),
        ceiling(max(data$Age_Ma_Gradstein, na.rm = TRUE)),
        by = 0.5
      )
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line.y = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm")
    )

# print
ggErba99


# Export plot PNG
ggsave(
  filename = here("results", "figures", "ggErba99_chemostrat.png"),
  plot = ggErba99,
  width = 2,
  height = 8,
  dpi = 300
)


# Herrle et al. (2004) ----------------------------------------------------

# subset by source
Herrle04<- subset(data, Source_C_data == "Herrle_et_al._(2004)")

# sort depth
Herrle04 <- Herrle04[(order(Herrle04$Age_Ma_Gradstein)), ]

# create the ggplot object
ggHerrle04 <- ggplot(Herrle04, aes(x = d13Corg_VPBD, y = Age_Ma_Gradstein)) +
  geom_point(size = 1.5, color = "blue", alpha = 0.2) +  # Blue points with black outlines +  # Customize with appropriate geom (e.g., geom_line, geom_smooth)
  geom_path(aes(x=rollmean(d13Corg_VPBD, 3, na.pad=TRUE))) +   # Add a line
  scale_y_reverse(
    breaks = seq(
      floor(88),
      ceiling(max(data$Age_Ma_Gradstein, na.rm = TRUE)),
      by = 0.5
    )
  ) +
  labs(
    x = Cdelt,
    y = "Age (Ma; Gradstein et al. 2012?",
    title = "Herrle et al. (2004)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Hide gridlines
    axis.line.y = element_blank(),  # Hide y-axis line
    axis.title.y = element_blank(),  # Hide y-axis title
    panel.border = element_blank(),  # Hide side borders
    axis.ticks = element_line(color = "black"),  # Show x-axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Set the length of x-axis ticks
  )

# print
ggHerrle04

# Export plot PNG
ggsave(
  filename = here("results", "figures", "ggHerrle04_chemostrat.png"),
  plot = ggHerrle04,
  width = 2,
  height = 8,
  dpi = 300
)


# Gale_et_al._(2011) ------------------------------------------------------

# subset by source
Gale11<- subset(data, Source_C_data == "Gale_et_al._(2011)")

# sort depth
Gale11 <- Gale11[(order(Gale11$Age_Ma_Gradstein)), ]

# create the ggplot object
ggGale11 <- ggplot(Gale11, aes(x = d13Corg_VPBD, y = Age_Ma_Gradstein)) +
  geom_point(size = 1.5, color = "blue", alpha = 0.2) +  # Blue points with black outlines +  # Customize with appropriate geom (e.g., geom_line, geom_smooth)
  geom_path(aes(x=rollmean(d13Corg_VPBD, 3, na.pad=TRUE))) +   # Add a line
  labs(
    x = Cdelt,
    y = "Age (Ma; Gradstein et al. 2012?",
    title = "Gale et al. (2011)"
  ) +
  scale_y_reverse(
    breaks = seq(
      floor(88),
      ceiling(max(data$Age_Ma_Gradstein, na.rm = TRUE)),
      by = 0.2
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Hide gridlines
    axis.line.y = element_blank(),  # Hide y-axis line
    axis.title.y = element_blank(),  # Hide y-axis title
    panel.border = element_blank(),  # Hide side borders
    axis.ticks = element_line(color = "black"),  # Show x-axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Set the length of x-axis ticks
  )

# print
ggGale11

ggsave(
  filename = here("results", "figures", "ggGale11_chemostrat.png"),
  plot = ggGale11,
  width = 2,
  height = 8,
  dpi = 300
)

# Plot subset 108-110 Ma
# subset by source
Gale11_109subset <- subset(Gale11, Age_Ma_Gradstein < 110 & Age_Ma_Gradstein > 108)

# sort depth
Gale11_109subset <- Gale11_109subset[(order(Gale11_109subset$Age_Ma_Gradstein)), ]

# create the ggplot object
ggGale11_109subset <- ggplot(Gale11_109subset, aes(x = d13Corg_VPBD, y = Age_Ma_Gradstein)) +
  geom_point(size = 1.5, color = "blue", alpha = 0.2) +  # Blue points with black outlines +  # Customize with appropriate geom (e.g., geom_line, geom_smooth)
  geom_path(aes(x=rollmean(d13Corg_VPBD, 3, na.pad=TRUE))) +   # Add a line
  labs(
    x = Cdelt,
    y = "Age (Ma; Gradstein et al. 2012?",
    title = "Gale2011_109subset"
  ) +
  scale_y_reverse(
    breaks = seq(
      floor(88),
      ceiling(max(data$Age_Ma_Gradstein, na.rm = TRUE)),
      by = 0.1
    )
  ) +
  scale_x_reverse(
    breaks = seq(
      floor(min(data$d13Corg_VPBD, na.rm = TRUE)),
      ceiling(max(data$d13Corg_VPBD, na.rm = TRUE)),
      by = 0.25
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Hide gridlines
    axis.line.y = element_blank(),  # Hide y-axis line
    axis.title.y = element_blank(),  # Hide y-axis title
    panel.border = element_blank(),  # Hide side borders
    axis.ticks = element_line(color = "black"),  # Show x-axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Set the length of x-axis ticks
  )

# print
ggGale11_109subset

ggsave(
  filename = here("results", "figures", "ggGale11_109subset_chemostrat.png"),
  plot = ggGale11_109subset,
  width = 2,
  height = 8,
  dpi = 300
)


# Jarvis_et_al._(2006) ----------------------------------------------------

# subset by source
Jarvis06 <- subset(data, Source_C_data == "Jarvis_et_al._(2006)")

# sort depth
Jarvis06 <- Jarvis06[(order(Jarvis06$Age_Ma_Gradstein)), ]

# create the ggplot object
ggJarvis06 <- ggplot(Jarvis06, aes(x = d13Corg_VPBD, y = Age_Ma_Gradstein)) +
  geom_point(size = 1.5, color = "blue", alpha = 0.2) +  # Blue points with black outlines +  # Customize with appropriate geom (e.g., geom_line, geom_smooth)
  geom_path(aes(x=rollmean(d13Corg_VPBD, 3, na.pad=TRUE))) +   # Add a line
  labs(
    x = Cdelt,
    y = "Age (Ma; Gradstein et al. 2012?",
    title = "Jarvis et al. (2006)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Hide gridlines
    axis.line.y = element_blank(),  # Hide y-axis line
    axis.title.y = element_blank(),  # Hide y-axis title
    panel.border = element_blank(),  # Hide side borders
    axis.ticks = element_line(color = "black"),  # Show x-axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Set the length of x-axis ticks
  ) + scale_y_reverse()

# print
ggJarvis06

ggsave(
  filename = here("results", "figures", "ggJarvis06_chemostrat.png"),
  plot = ggJarvis06,
  width = 2,
  height = 8,
  dpi = 300
)

### Herrle 2015 Arctic Data --------------------------------

# Sort so geom_path will work properly
Herrle_arctic_sorted <- Herrle_arctic %>%
  arrange(depth_m)

ggplot(data = Herrle_arctic_sorted, aes(x = d13Corg_VPDB, y = depth_m)) +
  geom_point(color = "grey40", alpha = 0.5, size = 2) +
  geom_path(color = "blue", linewidth = 0.5) +
  labs(
    x = expression(delta^{13}*C[org]~"\u2030 (VPDB)"), # nice isotope label
    y = "Depth (m)",
    title = "Herrle et al. (2015) – Arctic δ¹³Corg Record"
  ) +
  theme_minimal(base_size = 14) +                      # clean minimal theme
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# And by interpolated age
Herrle_arctic_sorted <- Herrle_arctic %>%
  arrange(age_Ma_interp)

ggplot(data = Herrle_arctic_sorted, aes(x = d13Corg_VPDB, y = age_Ma_interp)) +
  geom_point(color = "grey40", alpha = 0.5, size = 2) +
  geom_path(color = "blue", linewidth = 0.5) +
  scale_y_reverse() +  # <-- reverse age axis so it decreases upward
  labs(
    x = expression(delta^{13}*C[org]~"\u2030 (VPDB)"),
    y = "Modeled Age (Ma)",
    title = "Herrle et al. (2015) – Arctic δ¹³Corg Record"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

### Gottberg 2022 Cedar Mountain Formation d13C ---------------

# 1) Sort by depth and compute centered 3-pt rolling mean
Gottberg_CMF_org_smooth <- Gottberg_CMF_org %>%
  arrange(depth_m) %>%
  mutate(d13C_roll3 = rollmean(d13C, k = 3, fill = NA, align = "center"))

# 2) Plot: grey points + blue rolling-mean path (sorted), depth downward
ggplot(Gottberg_CMF_org_smooth, aes(y = depth_m)) +
  geom_point(aes(x = d13C), color = "grey40", alpha = 0.6, size = 2) +
  geom_path(
    data = Gottberg_CMF_org_smooth %>% filter(!is.na(d13C_roll3)),
    aes(x = d13C_roll3),
    color = "blue",
    linewidth = 1
  ) +
  labs(
    title = expression("Gottberg – CMF " * delta^13 * "C"[org]),
    x = expression(delta^13*C[org]~"\u2030 (VPDB)"),
    y = "Depth (m)"
  ) +
  theme_minimal()


### Cloverly - Marnes Bleues visual comparison ----------

ggplot(plot_df, aes(x = age)) +
  geom_point(aes(y = raw, color = dataset),
             alpha = 0.45, size = 2, na.rm = TRUE) +
  geom_line(aes(y = roll, color = dataset, linetype = dataset),
            linewidth = 1, na.rm = TRUE) +
  scale_x_reverse() +
  labs(
    title = "δ¹³C Records: CCC vs. Gale (2011)",
    x = "Age (Ma)",
    y = expression(delta^13*C~"(‰ VPDB)"),
    color = "Record",
    linetype = "Record"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~ dataset, scales = "free_x") +
  coord_flip()

# Filter to event window
plot_event_df <- plot_df %>%
  filter(age >= 107, age <= 110)

# Plot
ggplot(plot_df, aes(x = raw)) +
  geom_point(aes(y = age, color = dataset),
             alpha = 0.45, size = 2, na.rm = TRUE) +
  geom_line(aes(x = roll, y = age)) +
  scale_y_reverse() +
  labs(
    title = expression("δ"^13*"C Records: CCC vs. Gale (2011), 107–110 Ma"),
    x = "Age (Ma)",
    y = expression(delta^13*C~"(‰ VPDB)"),
    color = "Record",
    linetype = "Record"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~ dataset, scales = "free_x")

library(dplyr)
library(ggplot2)

# Prep CLC (adjust column names here if yours differ)
clc_long <- CLC_d13C_clean %>%
  select(age, raw = avg_d13C_VPDB, roll = roll_mean_clean) %>%
  mutate(dataset = "CLC")

# Prep Gale (note: d13Corg_VPBD ends with D)
gale_long <- Gale11 %>%
  select(age = Age_Ma_Gradstein,
         raw = d13Corg_VPBD,
         roll = d13C_3pt_avg) %>%
  mutate(dataset = "Gale 2011")

# Combine and zoom to 107–110 Ma
plot_event_df <- bind_rows(clc_long, gale_long) %>%
  filter(age >= 104, age <= 110)

# Faceted, rotated age plot
ggplot(plot_event_df, aes(x = age)) +
  geom_point(aes(y = raw, color = dataset),
             alpha = 0.45, size = 2, na.rm = TRUE) +
  geom_line(aes(y = roll, color = dataset, linetype = dataset),
            linewidth = 1, na.rm = TRUE) +
  scale_x_reverse() +
  labs(
    title = expression("δ"^13*"C Records: CLC vs. Gale (2011), 107–110 Ma"),
    x = "Age (Ma)",
    y = expression(delta^13*C~"(‰ VPDB)"),
    color = "Record",
    linetype = "Record"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~ dataset, scales = "free_x") +
  coord_flip()

### Bralower 1999 ---------

# Plot Santa Rosa
ggplot(SantaRosa_org_smooth, aes(y = depth_m)) +
  geom_point(aes(x = d13Corg_VPDB), color = "grey40", alpha = 0.6, size = 2) +
  geom_path(
    data = SantaRosa_org_smooth %>% filter(!is.na(d13C_roll3)),
    aes(x = d13C_roll3),
    color = "blue",
    linewidth = 1
  ) +
  labs(
    title = "Santa Rosa Canyon — Organic Carbon Isotopes",
    x = expression(delta^13*C[org]~"\u2030 (VPDB)"),
    y = "Depth (m)"
  ) +
  theme_minimal()

# Plot La Boca
ggplot(LaBoca_org_smooth, aes(y = depth_m)) +
  geom_point(aes(x = d13Corg_VPDB), color = "grey40", alpha = 0.6, size = 2) +
  geom_path(
    data = LaBoca_org_smooth %>% filter(!is.na(d13C_roll3)),
    aes(x = d13C_roll3),
    color = "blue",
    linewidth = 1
  ) +
  labs(
    title = "La Boca Canyon — Organic Carbon Isotopes",
    x = expression(delta^13*C[org]~"\u2030 (VPDB)"),
    y = "Depth (m)"
  ) +
  theme_minimal()

