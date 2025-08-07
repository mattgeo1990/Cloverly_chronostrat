### Setup ------------------------------------------------------------

# Load required packages
library(dplyr)
library(zoo)
library(ggplot2)
library(dtw)  # install.packages("dtw") if needed
library(stringr)

# Read data (replace with appropriate paths)
# CCC_isotope_data <- read.csv("CCC_data.csv")
# CLC_isotope_data <- read.csv("CLC_data.csv")

CLC_zircon_ages <- read.csv(here("data", "raw", "CHATGPT_CLC_Zircon_Age_Estimates.csv"))
CCC_zircon_ages <- read.csv(here("data", "raw", "CHATGPT_CCC_Zircon_Age_Estimates.csv"))

### Preprocessing ----------------------------------------------------

# Merge Zircon Age Estimate Datasets

  # Add section labels
    CLC_zircon_ages <- CLC_zircon_ages %>%
      mutate(section = "CLC")
    
    CCC_zircon_ages <- CCC_zircon_ages %>%
      mutate(section = "CCC")

  # Check for consistent column names
  # View 
      names(CLC_zircon_ages)
  # View names(CCC_zircon_ages)

  # Standardize column names (modify as needed based on your actual CSV headers)
  CLC_zircon_ages <- CLC_zircon_ages %>%
    rename(
      sample = Label,        # Change this if different
      depth = Estimated_Depth_m,       # or whatever your column is
      age = Age_Ma            # assumed column for age
    )
  
  CCC_zircon_ages <- CCC_zircon_ages %>%
    rename(
      sample = Sample,        # Change this if different
      depth = Approx_Depth_m,       # or whatever your column is
      age = Age_Ma            # assumed column for age
    )
  
  # Combine into one dataset
  zircon_combined <- bind_rows(CLC_zircon_ages, CCC_zircon_ages)
  
  # Preview
  print(head(zircon_combined))

  
  # Clean up column names and whitespace
  zircon_combined_clean <- zircon_combined %>%
    mutate(
      sample = str_trim(sample),
      section = str_trim(section),
      Notes = str_trim(Notes),
      Stratigraphic_Unit = str_trim(Stratigraphic_Unit),
      section = factor(section, levels = c("CCC", "CLC"))
    ) %>%
    select(section, sample, depth, age, Uncertainty_Ma, Stratigraphic_Unit, Notes) %>%
    arrange(section, depth)
  
  # View cleaned head
  print(head(zircon_combined_clean))

  # Adjust CCC depths relative to the base of the Little Sheep Member (-10 meters)
  zircon_combined_clean <- zircon_combined_clean %>%
    mutate(
      depth = ifelse(section == "CCC", depth - 10, depth)  # Subtract 10 m from all CCC depths
    )
  
  # Plot zircon age vs depth with error bars
    # Determine shared depth range
    depth_min <- floor(min(zircon_combined_clean$depth, na.rm = TRUE))
    depth_max <- ceiling(max(zircon_combined_clean$depth, na.rm = TRUE))
    
    # Plot zircon age vs. depth with error bars and fixed depth scale across sections
    ggplot(zircon_combined_clean, aes(x = age, y = depth)) +
      geom_point(aes(color = section), size = 2) +
      geom_errorbarh(aes(xmin = age - Uncertainty_Ma, xmax = age + Uncertainty_Ma), height = 0.5) +
      scale_y_continuous(limits = c(depth_min, depth_max)) +  # Keep depth increasing upward
      facet_wrap(~section, scales = "fixed") +             # Tie y-axes between facets
      labs(
        title = "Zircon U-Pb Dates by Depth",
        x = "Zircon Age (Ma)",
        y = "Stratigraphic Depth (m)"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

  
# --- Clean and label CCC data
CCC_processed <- CCC_isotope_data %>%
  select(sample,
         Strat_m_above_Pryor,
         age,
         avg_d13C_roll3) %>%
  rename(
    depth = Strat_m_above_Pryor,
    d13C = avg_d13C_roll3
  ) %>%
  mutate(section = "CCC")

# --- Clean and label CLC data
CLC_processed <- CLC_isotope_data %>%
  select(Identifier.1,
         Depth,
         age,
         d.13C.12C) %>%
  rename(
    sample = Identifier.1,
    depth  = Depth,
    d13C   = d.13C.12C
  ) %>%
  mutate(section = "CLC")

# --- Combine datasets
combined_isotope_data <- bind_rows(CCC_processed, CLC_processed) %>%
  filter(!is.na(age), !is.na(d13C))

# Export combined data
write.csv(combined_isotope_data, here("data", "processed", "Cloverly_d13C_combined_clean.csv"), row.names = FALSE)

### Outlier Detection -----------------------------------------------

# --- Define function to flag outliers using IQR
flag_outliers <- function(df) {
  df %>%
    group_by(section) %>%
    mutate(
      Q1 = quantile(d13C, 0.25, na.rm = TRUE),
      Q3 = quantile(d13C, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      is_outlier = d13C < (Q1 - 1.5 * IQR) | d13C > (Q3 + 1.5 * IQR)
    ) %>%
    ungroup()
}

# --- Apply function and review
combined_with_outliers <- flag_outliers(combined_isotope_data)

# --- Visualize outliers
ggplot(combined_with_outliers, aes(x = d13C, y = depth)) +
  geom_point(aes(color = is_outlier), size = 2, alpha = 0.8) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  facet_wrap(~section, scales = "free_y") +
  labs(
    title = "δ¹³C Outlier Visualization by Section",
    x = "δ¹³C (‰ VPDB)",
    y = "Stratigraphic Depth (m)",
    color = "Outlier"
  ) +
  theme_minimal()

# --- Remove identified outlier(s)
combined_isotope_data <- combined_with_outliers %>%
  filter(!(sample == "A-13" & section == "CLC"))

### Data Exploration ------------------------------------------------

# --- δ13C vs Age
ggplot(combined_isotope_data, aes(x = d13C, y = age, color = section)) +
  geom_path() +
  geom_point(size = 1) +
  scale_y_reverse() +
  theme_minimal() +
  labs(
    x = expression(delta^13*C~"(‰, VPDB)"),
    y = "Age (Ma)",
    title = "δ¹³C vs. Age: CCC and CLC"
  ) +
  scale_color_manual(values = c("CCC" = "blue", "CLC" = "red"))

# --- δ13C vs Depth
ggplot(combined_isotope_data, aes(x = d13C, y = depth, color = section)) +
  geom_path() +
  geom_point(size = 1) +
  theme_minimal() +
  labs(
    x = expression(delta^13*C~"(‰, VPDB)"),
    y = "Stratigraphic Height (m)",
    title = "δ¹³C vs. Depth: CCC and CLC"
  ) +
  scale_color_manual(values = c("CCC" = "blue", "CLC" = "red"))

### Interpolation ----------------------------------------------------

# --- Define common age grid (0.1 Ma spacing)
common_age_grid <- seq(
  from = max(min(CCC_processed$age, na.rm = TRUE), min(CLC_processed$age, na.rm = TRUE)),
  to   = min(max(CCC_processed$age, na.rm = TRUE), max(CLC_processed$age, na.rm = TRUE)),
  by   = 0.1
)

# --- Interpolate CCC
CCC_interp <- approx(CCC_processed$age, CCC_processed$d13C, xout = common_age_grid, rule = 2) %>%
  as.data.frame() %>%
  rename(age = x, d13C_CCC = y)

# --- Interpolate CLC
CLC_interp <- approx(CLC_processed$age, CLC_processed$d13C, xout = common_age_grid, rule = 2) %>%
  as.data.frame() %>%
  rename(age = x, d13C_CLC = y)

# --- Merge
interp_df <- left_join(CCC_interp, CLC_interp, by = "age")

### Correlation Analysis ---------------------------------------------

# --- Trim to Albian (age ≤ 112 Ma)
interp_df_trimmed <- interp_df %>%
  filter(age <= 112)

# --- Pearson correlation
correlation_result <- cor(
  interp_df_trimmed$d13C_CCC,
  interp_df_trimmed$d13C_CLC,
  use = "complete.obs",
  method = "pearson"
)
print(paste("Pearson correlation coefficient:", round(correlation_result, 3)))

# --- Cross-correlation (lag test)
ccf(
  interp_df_trimmed$d13C_CCC,
  interp_df_trimmed$d13C_CLC,
  na.action = na.pass,
  lag.max = 20,
  main = "Cross-Correlation Between CCC and CLC"
)

### Dynamic Time Warping ---------------------------------------------

# --- Prepare vectors without NAs
x <- interp_df_trimmed$d13C_CCC
y <- interp_df_trimmed$d13C_CLC
valid <- complete.cases(x, y)
x <- x[valid]
y <- y[valid]

# --- Run DTW
alignment <- dtw(x, y, keep = TRUE)

# --- Visualize alignment
plot(alignment, type = "twoway", col = c("blue", "red"),
     main = "DTW Alignment: CCC vs. CLC δ¹³C Curves")

# --- DTW distance
alignment$distance

# Visualize the Warped Alignment
dtwPlotTwoWay(alignment, ts1 = x, ts2 = y, main = "DTW Warping Path")


warped_df <- data.frame(
  CCC_age = interp_df_trimmed$age[valid][alignment$index1],
  CLC_age = interp_df_trimmed$age[valid][alignment$index2],
  CCC_d13C = x[alignment$index1],
  CLC_d13C = y[alignment$index2]
)

ggplot(warped_df, aes(x = CCC_age)) +
  geom_line(aes(y = CCC_d13C, color = "CCC (warped)")) +
  geom_line(aes(x = CLC_age, y = CLC_d13C, color = "CLC (warped)")) +
  scale_x_reverse() +
  labs(title = "Warped δ¹³C Curves", x = "Age (Ma)", y = "δ¹³C (‰ VPDB)", color = "Curve") +
  theme_minimal()

# Create a dataframe showing how DTW aligned CCC and CLC values
warped_df <- data.frame(
  CCC_age  = interp_df_trimmed$age[valid][alignment$index1],
  CLC_age  = interp_df_trimmed$age[valid][alignment$index2],
  CCC_d13C = x[alignment$index1],
  CLC_d13C = y[alignment$index2]
)

# Plot the warped (best-aligned) δ13C curves
library(ggplot2)

ggplot() +
  geom_line(data = warped_df, aes(x = CCC_age, y = CCC_d13C, color = "CCC (warped)"), size = 1) +
  geom_line(data = warped_df, aes(x = CCC_age, y = CLC_d13C, color = "CLC (aligned)"), size = 1) +
  scale_x_reverse() +
  labs(
    title = "Best-Match Alignment of δ¹³C Curves (via DTW)",
    x = "Age (Ma)",
    y = expression(delta^13*C~"(‰ VPDB)"),
    color = "Section"
  ) +
  scale_color_manual(values = c("CCC (warped)" = "blue", "CLC (aligned)" = "red")) +
  theme_minimal()


### Interactive plot -----------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Sample dataset (replace with interp_df_trimmed)
df <- interp_df_trimmed

ui <- fluidPage(
  titlePanel("Manual Alignment of δ13C Curves"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("shift", "Time Shift (Ma):", min = -5, max = 5, value = 0, step = 0.1),
      sliderInput("stretch", "Time Stretch (x):", min = 0.5, max = 2, value = 1, step = 0.01),
      sliderInput("offset", "Vertical Offset (‰):", min = -10, max = 10, value = 0, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("alignPlot")
    )
  )
)

server <- function(input, output) {
  output$alignPlot <- renderPlot({
    
    # Apply transformations to CLC curve
    df_trans <- df %>%
      mutate(
        age_trans = (age * input$stretch) + input$shift,
        d13C_CLC_adj = d13C_CLC + input$offset
      )
    
    ggplot() +
      geom_line(data = df_trans, aes(x = age, y = d13C_CCC), color = "blue", size = 1) +
      geom_line(data = df_trans, aes(x = age_trans, y = d13C_CLC_adj), color = "red", size = 1) +
      scale_x_reverse() +
      labs(title = "Manual Alignment of δ¹³C Curves",
           x = "Age (Ma)",
           y = expression(delta^13*C~"(‰, VPDB)")) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
