### Setup ------------------------------------------------------------

# Load required packages
library(dplyr)
library(zoo)
library(ggplot2)
library(dtw)  # install.packages("dtw") if needed

# Read data (replace with appropriate paths)
# CCC_isotope_data <- read.csv("CCC_data.csv")
# CLC_isotope_data <- read.csv("CLC_data.csv")

### Preprocessing ----------------------------------------------------

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