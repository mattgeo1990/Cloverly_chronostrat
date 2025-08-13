
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

# 3. Collapse duplicate ages and interpolate δ13C values to common grid
ccc_interp <- ccc_age %>%
  group_by(age) %>%
  summarise(d13C = mean(d13C, na.rm = TRUE), .groups = "drop") %>%
  { approx(.$age, .$d13C, xout = common_ages, rule = 2)$y }

clc_interp <- clc_age %>%
  group_by(age) %>%
  summarise(d13C = mean(d13C, na.rm = TRUE), .groups = "drop") %>%
  { approx(.$age, .$d13C, xout = common_ages, rule = 2)$y }

# 4. Set window size (e.g., 1 Myr window = 10 points if step is 0.1)
window_size <- 10

# Make sure inputs are numeric vectors of equal length
stopifnot(is.numeric(ccc_interp), is.numeric(clc_interp), length(ccc_interp) == length(clc_interp))

# 5. Calculate rolling correlation (coerce to matrix + guard inside FUN)
rolling_cor <- rollapply(
  data = cbind(ccc = ccc_interp, clc = clc_interp),  # matrix, not data.frame
  width = window_size,
  by = 1,
  align = "center",
  fill = NA,
  by.column = FALSE,
  FUN = function(m) {
    m <- as.matrix(m)
    cor(m[, 1], m[, 2], use = "complete.obs")
  }
)

# 6. Prepare results for plotting
rolling_results <- data.frame(
  center_age = rollmean(common_ages, k = window_size, align = "center", fill = NA),
  correlation = as.numeric(rolling_cor)
)

# 5. Identify peak correlation
peak_idx <- which.max(rolling_results$correlation)
peak_age <- rolling_results$center_age[peak_idx]
peak_r <- rolling_results$correlation[peak_idx]

# Zero-lag Pearson correlation for full interval
cor_test_event <- cor.test(ccc_interp, clc_interp)

### CCC-CLC Age Dynamic Time Warp -------------------------------------------------

valid_indices <- complete.cases(ccc_interp, clc_interp)
x <- ccc_interp[valid_indices]
y <- clc_interp[valid_indices]
ages <- common_ages[valid_indices]

alignment <- dtw(x, y, keep = TRUE)

### Print Summary for Manuscript --------------------------------------------------

cat("CCC–CLC δ13C correlation analysis:\n")
cat(sprintf("- Overall zero-lag Pearson correlation: r = %.3f (95%% CI %.3f–%.3f, p = %.2e, n = %d)\n",
            cor_test_event$estimate,
            cor_test_event$conf.int[1],
            cor_test_event$conf.int[2],
            cor_test_event$p.value,
            cor_test_event$parameter + 2))
cat(sprintf("- Peak rolling correlation of r = %.3f at ~%.2f Ma (1 Myr sliding window)\n",
            peak_r, peak_age))
cat(sprintf("- DTW distance between profiles: %.3f\n",
            alignment$distance))
### CCC - Marnes Bleues Age Window Correlation Analysis -------

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


### CCC - Herrle Composite Correlation Analyses --------------

## Cross-correlation CCC - Gale et al. 2011 (Marnes Bleues Fm.) ----

# Choose common age range (overlap only)
age_min <- max(min(CCC_d13C_clean$age, na.rm = TRUE),
               min(Gale11$Age_Ma_Gradstein, na.rm = TRUE))
age_max <- min(max(CCC_d13C_clean$age, na.rm = TRUE),
               max(Gale11$Age_Ma_Gradstein, na.rm = TRUE))

# Define a regular grid (e.g., 0.01 Ma steps)
age_grid <- seq(age_min, age_max, by = 0.01)

# Interpolate CCC and Gale onto the grid
ccc_interp <- approx(x = CCC_d13C_clean$age,
                     y = CCC_d13C_clean$roll_mean_clean,
                     xout = age_grid)$y

gale_interp <- approx(x = Gale11$Age_Ma_Gradstein,
                      y = Gale11$d13C_3pt_avg,
                      xout = age_grid)$y

# Remove any NAs created by interpolation
valid_idx <- complete.cases(ccc_interp, gale_interp)
ccc_interp <- ccc_interp[valid_idx]
gale_interp <- gale_interp[valid_idx]

# Cross-correlation analysis
ccf_res <- ccf(ccc_interp, gale_interp, lag.max = 50, plot = TRUE)

# The lag with maximum correlation:
best_lag <- ccf_res$lag[which.max(ccf_res$acf)]
best_corr <- max(ccf_res$acf)

cat("Max correlation =", round(best_corr, 3), "at lag", best_lag, "steps\n")
cat("Lag in Ma =", best_lag * 0.01, "\n")  # multiply by step size


# Shift Gale relative to CCC by best lag found earlier
lag_steps <- best_lag  # from ccf_res
if (lag_steps > 0) {
  gale_shifted <- gale_interp[(1 + lag_steps):length(gale_interp)]
  ccc_shifted  <- ccc_interp[1:(length(ccc_interp) - lag_steps)]
} else if (lag_steps < 0) {
  lag_steps <- abs(lag_steps)
  gale_shifted <- gale_interp[1:(length(gale_interp) - lag_steps)]
  ccc_shifted  <- ccc_interp[(1 + lag_steps):length(ccc_interp)]
} else {
  gale_shifted <- gale_interp
  ccc_shifted  <- ccc_interp
}

# Pearson's r after alignment
pearson_r_aligned <- cor(ccc_shifted, gale_shifted, use = "complete.obs", method = "pearson")
cat("Pearson's r after optimal shift =", round(pearson_r_aligned, 3), "\n")

# Define event window for l'Arboudeyesse (~107–110 Ma)
event_window <- c(107, 110)

# Logical index for ages in the window
sel <- age_grid >= min(event_window) & age_grid <= max(event_window)

# Pearson's r just for that window
pearson_event <- cor(
  ccc_interp[sel],
  gale_interp[sel],
  use = "complete.obs",
  method = "pearson"
)

# Also run cor.test() for p-value & CI
cor_test_event <- cor.test(
  ccc_interp[sel],
  gale_interp[sel],
  method = "pearson"
)

cat("Pearson's r (107–110 Ma) =", round(pearson_event, 3), "\n")
print(cor_test_event)
















# --- 1. Filter and clean the two datasets ---

# Herrle 2015 composite data
Herrle_age <- Herrle_composite_data %>%
  select(age = Age_Ma_Gradstein, d13C = d13C_3pt_avg) %>%
  arrange(age) %>%
  filter(!is.na(age), !is.na(d13C)) %>%
  group_by(age) %>%
  summarise(d13C = mean(d13C, na.rm = TRUE), .groups = "drop")

# --- 2. Create a common age grid (e.g., 0.1 Ma spacing) ---

common_ages <- seq(
  from = max(min(ccc_age$age), min(Herrle_age$age)),
  to   = min(max(ccc_age$age), max(Herrle_age$age)),
  by = 0.1
)

# --- 3. Interpolate to shared age grid ---

ccc_interp <- approx(x = ccc_age$age, y = ccc_age$d13C, xout = common_ages, rule = 2)$y
herrle_interp <- approx(x = Herrle_age$age, y = Herrle_age$d13C, xout = common_ages, rule = 2)$y

# --- 4. Combine for correlation analysis ---

interp_df <- data.frame(
  age = common_ages,
  d13C_CCC = ccc_interp,
  d13C_Herrle = herrle_interp
)

# Optional: preview
head(interp_df)

# You can now do:
# - Pearson correlation: cor(interp_df$d13C_CCC, interp_df$d13C_Gale11)
# - Rolling window: use rollapply()
# - DTW: use dtw()

### CLC - Gale 2011 comparisons ------------------------
## PACKAGES ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(dtw)        # install.packages("dtw")

## INPUTS (edit if your column names differ) ------------------------------
# CLC dataframe: CLC_d13C_clean with columns: age, avg_d13C_VPDB, roll_mean_clean
# Gale dataframe: Gale11 with columns: Age_Ma_Gradstein, d13C_3pt_avg, d13Corg_VPBD

## 0) Choose interval and grid -------------------------------------------
# Use the full overlap or a focused event window (set event_window = NULL to use full)
event_window <- c(107, 110)   # set to NULL for full-overlap analysis
step_Ma <- 0.01               # grid spacing (Ma)

# Overlap limits
age_min <- max(min(CLC_d13C_clean$age, na.rm=TRUE),
               min(Gale11$Age_Ma_Gradstein, na.rm=TRUE))
age_max <- min(max(CLC_d13C_clean$age, na.rm=TRUE),
               max(Gale11$Age_Ma_Gradstein, na.rm=TRUE))

if (!is.null(event_window)) {
  age_min <- max(age_min, min(event_window))
  age_max <- min(age_max, max(event_window))
}

age_grid <- seq(age_min, age_max, by = step_Ma)

## 1) Interpolate to shared grid ------------------------------------------
# Use rolling means for smoother correlation; switch to raw if you prefer
clc_interp  <- approx(x = CLC_d13C_clean$age,
                      y = CLC_d13C_clean$roll_mean_clean,
                      xout = age_grid, rule = 1)$y

gale_interp <- approx(x = Gale11$Age_Ma_Gradstein,
                      y = Gale11$d13C_3pt_avg,
                      xout = age_grid, rule = 1)$y

# Keep only complete pairs
ok <- complete.cases(clc_interp, gale_interp)
age_grid   <- age_grid[ok]
clc_interp <- clc_interp[ok]
gale_interp<- gale_interp[ok]

## (Optional) Z-score within the analysis window (shape-only comparison)
zscore <- function(v) (v - mean(v, na.rm=TRUE)) / sd(v, na.rm=TRUE)
clc_z   <- zscore(clc_interp)
gale_z  <- zscore(gale_interp)

## 2) CROSS-CORRELATION ----------------------------------------------------
# a) Zero-lag Pearson r
r0 <- cor(clc_z, gale_z, use="complete.obs")
ct0 <- cor.test(clc_z, gale_z, method="pearson")

cat(sprintf("Zero-lag Pearson r = %.3f (p = %.2g)\n", r0, ct0$p.value))

# b) CCF to find best lag (in steps and Ma)
ccf_res <- ccf(clc_z, gale_z, lag.max = 100, plot = FALSE)  # increase lag.max if needed
best_idx <- which.max(ccf_res$acf)
best_lag_steps <- ccf_res$lag[best_idx]
best_lag_Ma    <- best_lag_steps * step_Ma
best_r_ccf     <- ccf_res$acf[best_idx]

cat(sprintf("Max CCF r = %.3f at lag %d steps (%.3f Ma)\n",
            best_r_ccf, best_lag_steps, best_lag_Ma))

# c) Pearson r after shifting by best lag
shift_series <- function(a, b, lag_steps) {
  if (lag_steps > 0) {
    list(a = a[1:(length(a)-lag_steps)], b = b[(1+lag_steps):length(b)])
  } else if (lag_steps < 0) {
    lag_steps <- abs(lag_steps)
    list(a = a[(1+lag_steps):length(a)], b = b[1:(length(b)-lag_steps)])
  } else list(a = a, b = b)
}
shifted <- shift_series(clc_z, gale_z, best_lag_steps)
r_shift <- cor(shifted$a, shifted$b, use="complete.obs")
ct_shift<- cor.test(shifted$a, shifted$b, method="pearson")

cat(sprintf("Pearson r after optimal shift = %.3f (p = %.2g)\n",
            r_shift, ct_shift$p.value))

# Optional: Nice CCF plot
ccf_df <- data.frame(lag = ccf_res$lag * step_Ma, acf = ccf_res$acf)
ggplot(ccf_df, aes(lag, acf)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line() +
  geom_point(data = subset(ccf_df, acf == max(acf)), size = 2) +
  labs(title = "CLC vs. Gale (2011): Cross-correlation",
       x = "Lag (Ma)  [positive = Gale older]",
       y = "Correlation (ACF)") +
  theme_minimal()

## 3) DYNAMIC TIME WARPING (DTW) -------------------------------------------
# DTW on z-scored series; consider constraints to avoid over-warping
# e.g., window.type="sakoechiba", window.size = round(0.1 * length(clc_z))
alignment <- dtw(clc_z, gale_z, keep = TRUE)
cat(sprintf("DTW distance (z-scored) = %.3f\n", alignment$distance))

# a) Classic two-way plot (index space)
plot(alignment, type = "twoway", col = c("blue", "red"),
     main = "DTW Alignment: CLC (blue) vs Gale (red)")

# b) Build warping table in AGE space
warped_df <- data.frame(
  CLC_age   = age_grid[alignment$index1],
  Gale_age  = age_grid[alignment$index2],
  CLC_z     = clc_z[alignment$index1],
  Gale_z    = gale_z[alignment$index2]
)

# c) Age–Age map (deviations from 1:1 show diachrony / warping)
ggplot(warped_df, aes(x = CLC_age, y = Gale_age)) +
  geom_path() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_reverse() + scale_y_reverse() +
  coord_equal() +
  labs(title = "DTW Age Mapping: CLC → Gale (2011)",
       x = "CLC age (Ma)", y = "Gale age (Ma)") +
  theme_minimal()

# d) Aligned curves, plotted vs CLC age (shape-only, z-scored)
ggplot(warped_df, aes(x = CLC_age)) +
  geom_line(aes(y = CLC_z, color = "CLC (warped ref)"), linewidth = 0.9) +
  geom_line(aes(y = Gale_z, color = "Gale (aligned)"), linewidth = 0.9) +
  scale_x_reverse() +
  labs(title = "DTW-aligned δ13C (z-scored): CLC vs. Gale (2011)",
       x = "Age (Ma)", y = "z-score", color = "Record") +
  theme_minimal() +
  theme(legend.position = "bottom")

## 4) QUICK NOTES -----------------------------------------------------------
# - Z-scoring makes the comparison "shape-only" (baseline and amplitude removed).
# - If warping looks extreme, try constraints:
#     alignment <- dtw(clc_z, gale_z, keep = TRUE,
#                      window.type = "sakoechiba",
#                      window.size = round(0.1 * length(clc_z)),
#                      step.pattern = symmetric2)  # or rabinerJuangStepPattern(6, "c")
# - You can run the same code with raw (unscaled) series by replacing clc_z/gale_z with clc_interp/gale_interp.

## Packages ----------------------------------------------------------------
library(dplyr)
library(dtw)   # install.packages("dtw")

## Helpers -----------------------------------------------------------------
zscore <- function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE)

shift_series <- function(a, b, lag_steps) {
  # returns a list with a and b trimmed to equal length after shifting by lag_steps
  if (lag_steps > 0) {
    list(a = a[1:(length(a) - lag_steps)], b = b[(1 + lag_steps):length(b)])
  } else if (lag_steps < 0) {
    k <- abs(lag_steps)
    list(a = a[(1 + k):length(a)], b = b[1:(length(b) - k)])
  } else {
    list(a = a, b = b)
  }
}

analyze_pair <- function(label,
                         age_A, val_A,        # series A (e.g., CCC or CLC)
                         age_B, val_B,        # series B (e.g., Gale)
                         window = NULL,       # c(107,110) for event window; NULL for full overlap
                         step_Ma = 0.01,      # interpolation step in Ma
                         use_zscore = TRUE,   # shape-only comparison
                         ccf_lag_max_steps = NULL,  # default: length/4
                         dtw_window_frac = NULL) {  # e.g., 0.1 for Sakoe-Chiba band
  
  # 1) Determine overlap and grid -----------------------------------------
  age_min <- max(min(age_A, na.rm=TRUE), min(age_B, na.rm=TRUE))
  age_max <- min(max(age_A, na.rm=TRUE), max(age_B, na.rm=TRUE))
  if (!is.null(window)) {
    age_min <- max(age_min, min(window))
    age_max <- min(age_max, max(window))
  }
  age_grid <- seq(age_min, age_max, by = step_Ma)
  
  # 2) Interpolate onto shared grid ---------------------------------------
  A_interp <- approx(x = age_A, y = val_A, xout = age_grid, rule = 1)$y
  B_interp <- approx(x = age_B, y = val_B, xout = age_grid, rule = 1)$y
  ok <- complete.cases(A_interp, B_interp)
  age_grid <- age_grid[ok]
  A_interp <- A_interp[ok]
  B_interp <- B_interp[ok]
  if (length(A_interp) < 10) stop("Too few overlapping points after interpolation.")
  
  # 3) Optional z-scoring (shape-only) ------------------------------------
  if (use_zscore) {
    A_use <- zscore(A_interp)
    B_use <- zscore(B_interp)
    unit_label <- "z-score"
  } else {
    A_use <- A_interp
    B_use <- B_interp
    unit_label <- "raw units"
  }
  
  # 4) Zero-lag Pearson ----------------------------------------------------
  ct0 <- suppressWarnings(cor.test(A_use, B_use, method = "pearson"))
  r0  <- unname(ct0$estimate)
  p0  <- ct0$p.value
  
  # 5) Cross-correlation (find best lag) ----------------------------------
  if (is.null(ccf_lag_max_steps)) ccf_lag_max_steps <- floor(length(A_use)/4)
  ccf_res <- ccf(A_use, B_use, lag.max = ccf_lag_max_steps, plot = FALSE)
  best_idx <- which.max(ccf_res$acf)
  best_lag_steps <- ccf_res$lag[best_idx]
  best_lag_Ma    <- best_lag_steps * step_Ma
  best_r_ccf     <- ccf_res$acf[best_idx]
  
  # 6) Pearson after optimal shift ----------------------------------------
  shifted <- shift_series(A_use, B_use, best_lag_steps)
  ct_shift <- suppressWarnings(cor.test(shifted$a, shifted$b, method = "pearson"))
  r_shift  <- unname(ct_shift$estimate)
  p_shift  <- ct_shift$p.value
  
  # 7) Dynamic Time Warping ------------------------------------------------
  if (is.null(dtw_window_frac)) {
    alignment <- dtw(A_use, B_use, keep = TRUE)
  } else {
    wsize <- max(1, round(dtw_window_frac * length(A_use)))
    alignment <- dtw(A_use, B_use, keep = TRUE,
                     window.type = "sakoechiba",
                     window.size = wsize)
  }
  dtw_distance <- alignment$distance
  
  # DTW-aligned correlation (paired along the warping path)
  r_dtw <- suppressWarnings(cor(A_use[alignment$index1],
                                B_use[alignment$index2],
                                use = "complete.obs",
                                method = "pearson"))
  
  # Warping table (if you want to plot later)
  warped_df <- data.frame(
    age_A = age_grid[alignment$index1],
    age_B = age_grid[alignment$index2],
    A_val = A_use[alignment$index1],
    B_val = B_use[alignment$index2]
  )
  
  # 8) Return summary row + extras ----------------------------------------
  summary <- tibble::tibble(
    Comparison = label,
    Window_Ma  = if (is.null(window)) "full overlap" else paste(range(window), collapse = "–"),
    Unit       = unit_label,
    Zero_lag_r = r0,
    Zero_lag_p = p0,
    Max_CCF_r  = as.numeric(best_r_ccf),
    Lag_Ma_at_Max_r = as.numeric(best_lag_Ma),
    r_after_shift = r_shift,
    p_after_shift = p_shift,
    DTW_distance  = as.numeric(dtw_distance),
    r_DTW_aligned = as.numeric(r_dtw),
    N_points_used = length(A_use)
  )
  
  list(summary = summary,
       age_grid = age_grid,
       A_series = A_use,
       B_series = B_use,
       ccf = ccf_res,
       dtw = alignment,
       warped_df = warped_df)
}

## === HOW TO CALL IT ======================================================

# Use rolling means for smoother comparison:
#   - CLC/CCC: roll_mean_clean
#   - Gale:    d13C_3pt_avg
# Event window (set to NULL for full overlap):
evt <- c(107, 110)

# CCC vs Gale
res_ccc_gale <- analyze_pair(
  label = "CCC vs Gale",
  age_A = CCC_d13C_clean$age,
  val_A = CCC_d13C_clean$roll_mean_clean,
  age_B = Gale11$Age_Ma_Gradstein,
  val_B = Gale11$d13C_3pt_avg,
  window = evt,
  step_Ma = 0.01,
  use_zscore = TRUE,
  ccf_lag_max_steps = NULL,   # auto
  dtw_window_frac = NULL      # or try 0.10 for constraints
)

# CLC vs Gale
res_clc_gale <- analyze_pair(
  label = "CLC vs Gale",
  age_A = CLC_d13C_clean$age,
  val_A = CLC_d13C_clean$roll_mean_clean,
  age_B = Gale11$Age_Ma_Gradstein,
  val_B = Gale11$d13C_3pt_avg,
  window = evt,
  step_Ma = 0.01,
  use_zscore = TRUE,
  ccf_lag_max_steps = NULL,
  dtw_window_frac = NULL
)

# Combine summaries into one results table
results_table <- dplyr::bind_rows(res_ccc_gale$summary, res_clc_gale$summary)
print(results_table, n = Inf)

## Access pieces if you want to plot:
# res_clc_gale$warped_df     # DTW warping table (age_A = CLC age; age_B = Gale age)
# res_clc_gale$ccf           # cross-correlation object
# res_clc_gale$dtw           # dtw alignment object

### Ruby Ranch to Cloverly comparisons ------ 

# Compare Gottberg 2022
