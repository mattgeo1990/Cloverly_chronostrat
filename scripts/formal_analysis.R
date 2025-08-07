
# Setup -------------

# Check if data is loaded; if not, run setup
if (!exists("CLC_d13C_clean") || !exists("CCC_d13C_clean") || 
    !exists("combined_isotope_data")) {
  source(here::here("scripts", "setup.R"))
}

# Subset CLC Albian interval for comparison with Crooked Creek
CLC_Albian <- subset(CLC_d13C_clean, age < 120)

# Basic plots -------------------------------------------------

# Plot δ13C vs. age
ggplot(combined_isotope_data, aes(x = d13C, y = age, color = section)) +
  geom_path() +
  geom_point(size = 1) +
  scale_y_reverse() +
  labs(
    x = expression(delta^13*C~"(‰, VPDB)"),
    y = "Age (Ma)",
    title = "δ¹³C vs. Age: CCC and CLC"
  ) +
  scale_color_manual(values = c("CCC" = "blue", "CLC" = "red")) +
  theme_minimal()


# Plot δ13C vs. depth
# Set the datum depth
datum_depth <- 0  # 0 meters

ggplot(combined_isotope_data, aes(x = d13C, y = depth, color = section)) +
  geom_path() +
  geom_point(size = 1) +
  geom_hline(yintercept = datum_depth, linetype = "dashed", color = "black") +
  annotate(
    "text",
    x = min(combined_isotope_data$d13C, na.rm = TRUE),  # place text at left edge
    y = datum_depth + 1,  # slightly above line for visibility
    label = "Top of Pryor",
    hjust = 0,
    size = 3.5
  ) +
  labs(
    x = expression(delta^13*C~"(‰, VPDB)"),
    y = "Stratigraphic Depth (m)",
    title = "δ¹³C vs. Depth: CCC and CLC"
  ) +
  scale_color_manual(values = c("CCC" = "blue", "CLC" = "red")) +
  theme_minimal()



# Interative d13C-age plot ------------------------------------

# Define UI
ui <- fluidPage(
  titlePanel("Compare δ13C Curves"),
  plotlyOutput("carbonPlot")
)

# Define server logic
server <- function(input, output) {
  output$carbonPlot <- renderPlotly({
    plot_ly() %>%
      add_trace(
        data = CLC_Albian,
        x = ~age,
        y = ~roll_mean_clean,
        type = 'scatter',
        mode = 'lines',
        name = 'CLC_Albian',
        line = list(color = 'blue')
      ) %>%
      add_trace(
        data = CCC_d13C_clean,
        x = ~age,
        y = ~roll_mean_clean,
        type = 'scatter',
        mode = 'lines',
        name = 'CCC',
        line = list(color = 'red')
      ) %>%
      layout(
        yaxis = list(title = "δ13C", autorange = "reversed"),
        xaxis = list(title = "Age (Ma)"),
        hovermode = "compare"
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)


### CCC-CLC Age Window Correlation Analysis ----------------------------------------

# 1. Filter and arrange by age
ccc_age <- combined_isotope_data %>%
  filter(section == "CCC") %>%
  select(age, d13C) %>%
  arrange(age)

clc_age <- combined_isotope_data %>%
  filter(section == "CLC") %>%
  select(age, d13C) %>%
  arrange(age)

# 2. Create common age grid
common_ages <- seq(
  max(min(ccc_age$age), min(clc_age$age)),
  min(max(ccc_age$age), max(clc_age$age)),
  by = 0.1
)

# 3. Interpolate δ13C values to common grid
# Collapse duplicate ages by taking the mean d13C
ccc_age_unique <- ccc_age %>%
  group_by(age) %>%
  summarise(d13C = mean(d13C, na.rm = TRUE), .groups = "drop")

# Collapse duplicate ages by taking the mean d13C
clc_age_unique <- clc_age %>%
  group_by(age) %>%
  summarise(d13C = mean(d13C, na.rm = TRUE), .groups = "drop")

# Interpolate CCC
ccc_interp <- approx(
  x = ccc_age_unique$age,
  y = ccc_age_unique$d13C,
  xout = common_ages,
  rule = 2
)$y

# Interpolate CLC
clc_interp <- approx(
  x = clc_age_unique$age,
  y = clc_age_unique$d13C,
  xout = common_ages,
  rule = 2
)$y

# 4. Set window size (e.g., 1 Myr window = 10 points if step is 0.1)
window_size <- 10

# 5. Calculate rolling correlation
rolling_cor <- rollapply(
  data = data.frame(ccc = ccc_interp, clc = clc_interp),
  width = window_size,
  by = 1,
  FUN = function(x) cor(x[, 1], x[, 2], use = "complete.obs"),
  align = "center",
  fill = NA,
  by.column = FALSE
)

# 6. Prepare results for plotting
rolling_results <- data.frame(
  center_age = rollmean(common_ages, k = window_size, align = "center", fill = NA),
  correlation = rolling_cor
)

# 7. Plot
ggplot(rolling_results, aes(x = center_age, y = correlation)) +
  geom_line(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Rolling Pearson Correlation of δ13C by Age (1 Myr window)",
    x = "Age (Ma)",
    y = "Pearson r"
  ) +
  theme_minimal()

### CCC-CLC Age Dynamic Time Warp -------------------------------------------------


# 1. Prepare vectors and filter out NAs
valid_indices <- complete.cases(ccc_interp, clc_interp)
x <- ccc_interp[valid_indices]
y <- clc_interp[valid_indices]
ages <- common_ages[valid_indices]

# 2. Run DTW alignment
alignment <- dtw(x, y, keep = TRUE)

# 3. Print DTW distance
cat("DTW distance between CCC and CLC δ13C profiles:", alignment$distance, "\n")

# 4. Create aligned dataframe using warping path
warped_df <- data.frame(
  CCC_age  = ages[alignment$index1],
  CLC_age  = ages[alignment$index2],
  CCC_d13C = x[alignment$index1],
  CLC_d13C = y[alignment$index2]
)

# 5. Plot DTW alignment path (index view)
plot(alignment, type = "twoway", col = c("blue", "red"),
     main = "DTW Alignment: CCC vs. CLC δ¹³C Profiles")

# 6. Plot aligned δ13C curves by CCC age
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

### CCC - Marnes Bleues Age Window Correlation Analysis

# --- 1. Filter and clean the two datasets ---

# Gale11 data
gale_age <- Gale11 %>%
  select(age = Age_Ma_Gradstein, d13C = d13C_3pt_avg) %>%
  arrange(age) %>%
  filter(!is.na(age), !is.na(d13C)) %>%
  group_by(age) %>%
  summarise(d13C = mean(d13C, na.rm = TRUE), .groups = "drop")

# --- 2. Create a common age grid (e.g., 0.1 Ma spacing) ---

common_ages <- seq(
  from = max(min(ccc_age$age), min(gale_age$age)),
  to   = min(max(ccc_age$age), max(gale_age$age)),
  by = 0.1
)

# --- 3. Interpolate to shared age grid ---

ccc_interp <- approx(x = ccc_age$age, y = ccc_age$d13C, xout = common_ages, rule = 2)$y
gale_interp <- approx(x = gale_age$age, y = gale_age$d13C, xout = common_ages, rule = 2)$y

# --- 4. Combine for correlation analysis ---

interp_df <- data.frame(
  age = common_ages,
  d13C_CCC = ccc_interp,
  d13C_Gale11 = gale_interp
)

# Optional: preview
head(interp_df)

# You can now do:
# - Pearson correlation: cor(interp_df$d13C_CCC, interp_df$d13C_Gale11)
# - Rolling window: use rollapply()
# - DTW: use dtw()