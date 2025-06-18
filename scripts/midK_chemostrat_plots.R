
#### Setup ####

# Load required packages
library(ggplot2)
library(zoo)
library(readr)
library(dplyr)
library(here)

# Create delta notation for x-axis
Cdelt <- expression("δ"^13 * "Corg (‰ vs. VPDB)")

#### Read composite data ####

# Read in the composite data using a project-relative path
data <- read_csv(here("data", "raw", "HerrleEtAl2015_CompositeCIdata.csv"))

# Prepare data: sort, convert factor, and compute 3-point average
data <- data %>%
  arrange(Age_Ma_Gradstein) %>%
  mutate(
    Source_C_data = as.factor(Source_C_data),
    d13C_3pt_avg = rollmean(d13Corg_VPBD, 3, fill = NA, align = "center")
  )

#### Plot: Full Composite (Herrle et al., 2015) ####

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




