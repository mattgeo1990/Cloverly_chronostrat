#### Setup ####
library(dplyr)
library(readr)
library(here)

#### Load data ####
# Read cleaned Crooked Creek isotope data
ccc <- read_csv(here("data", "processed", "CCC_d13Corg_clean.csv"))

# Read Crooked Creek age-depth model
age_model <- read_csv(here("data", "raw", "CrookedCreek_age_model.csv"))
library(ggplot2)

# Plot Crooked Creek age-depth model
ggplot_CCC_age_model <- ggplot(age_model, aes(y = `Height (m)`)) +
  geom_ribbon(aes(xmin = `0.975`, xmax = `most_likely_0.025`), fill = "lightblue", alpha = 0.4) +
  geom_path(aes(x = `0.5`), color = "blue") +
  scale_x_reverse(
    breaks = seq(
      floor(min(age_model$`Youngest Case`, na.rm = TRUE)),
      ceiling(max(age_model$`Oldest Case`, na.rm = TRUE)),
      by = 1
    )
  ) +
  scale_y_continuous(breaks = seq(
    floor(min(age_model$`Height (m)`, na.rm = TRUE)),
    ceiling(max(age_model$`Height (m)`, na.rm = TRUE)),
    by = 5
  )) +
  labs(
    x = "Age (Ma)",
    y = "Stratigraphic Height (m)",
    title = "Crooked Creek Model"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_blank(), 
    plot.background = element_blank(),
    panel.grid.major = element_line(color = "grey80", size = 0.3),   # major gridlines
    panel.grid.minor = element_line(color = "grey90", size = 0.2),   # minor gridlines
    panel.grid.major.y = element_blank(),  # remove major gridlines on y-axis
    panel.grid.minor.x = element_blank(),  # remove minor gridlines on x-axis
    panel.border = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )

# Print the plot
print(ggplot_CCC_age_model)

# Export as PNG
ggsave(
  filename = here("results", "figures", "CCC_age_model_V1.png"),
  plot = ggplot_CCC_age_model,
  width = 3,
  height = 6,
  dpi = 300
)


#### Interpolate modeled age at each sample depth ####
# Use base::approx to interpolate based on Strat_m_above_Pryor
ccc <- ccc %>%
  mutate(age_Ma = ifelse(Strat_m_above_Pryor >= 0,
                         approx(x = age_model$`Height (m)`,
                                y = age_model$`0.5`,
                                xout = Strat_m_above_Pryor,
                                rule = 2)$y,
                         NA))

#### Save processed data ####
write.csv(ccc, here("data", "processed", "CCC_d13Corg_with_age.csv"), row.names = FALSE)
