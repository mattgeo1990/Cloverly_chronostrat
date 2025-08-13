### Project Setup ------------------------------------------------------------------

# Load or install required packages

required_packages <- c(
  "ggplot2",
  "readr",
  "dplyr",
  "zoo",
  "here",
  "tidyr",
  "lubridate",
  "ggpubr",
  "plotly"
)


check_and_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

invisible(lapply(required_packages, check_and_load))


### Load Data ----------------------------------------------------------------------

# Read in Cody Landfill (CLC) isotope data
  CLC_isotope_data <- read.csv(here("data", "raw", "CLC_results1.csv"))
  
# Read in Crooked Creek (CCC) isotopedata
  raw_data <- read_csv(here("data", "raw", "Kalu_CCC_d13Corg_RAW.csv"))
  
  # old code that loaded celaned results from CCC_data_wrangle.R
  # all of that cleaning now takes place in the current script
  # CCC_d13C_clean <- read.csv(here("data", "processed", "CCC_d13Corg_clean.csv"))
  
# Read in Crooked Creek depth data
  depths_data <- read_csv(here("data", "raw", "Kalu_CCC_d13Corg_DEPTHS_ONLY.csv"))

# Load Jack's age models
  CLC_age_model <- read.csv(here("data", "raw", "CLC_age_model_hiatus.csv"))
  CCC_age_model <- read.csv(here("data", "raw", "Crooked_Creek_age_model.csv"))

# Read in the Herrle et al. 2015 composite data 
  Herrle_composite_data <- read_csv(here("data", "raw", "HerrleEtAl2015_CompositeCIdata.csv"))

# Read in Herrle 2015 arctic data
  Herrle_arctic <- read.csv(here("data", "raw", "Herrle2015_arctic_data.csv"))

# Read in Gottberg 2022 Cedar Mountain Fm d13Corg data
 Gottberg_CMF_org <- read.csv(here("data", "raw", "Gottberg_org_CMF.csv"))
  
# Read in Bralower 1999 data
SantaRosa_org <- read.csv(here("data", "raw", "BralowerEtAl1999_SantaRosa_d13Corg.csv"))
LaBoca_org <- read.csv(here("data", "raw", "BralowerEtAl1999_LaBoca_d13Corg.csv"))


# prelim data wrangling --------------------
  
# Prepare La Boca data: sort and compute 3-point average
LaBoca_org_smooth <- LaBoca_org %>%
  rename(
    d13Corg_VPDB = bralower.La.Boca.Canyon.C_organic,
    depth_m      = height.in.m
  ) %>%
  arrange(depth_m) %>%
  mutate(d13C_roll3 = rollmean(d13Corg_VPDB, k = 3, fill = NA, align = "center"))


# Prepare Santa Rosa data: sort and compute 3-point average
SantaRosa_org_smooth <- SantaRosa_org %>%
  rename(
    d13Corg_VPDB = bralower.Santa.Rosa.Canyon.C_organic,
    depth_m      = height.in.m
  ) %>%
  arrange(depth_m) %>%
  mutate(d13C_roll3 = rollmean(d13Corg_VPDB, k = 3, fill = NA, align = "center"))


  # Prepare data: sort, convert factor, and compute 3-point average
  Herrle_composite_data <- Herrle_composite_data %>%
    arrange(Age_Ma_Gradstein) %>%
    mutate(
      Source_C_data = as.factor(Source_C_data),
      d13C_3pt_avg = rollmean(d13Corg_VPBD, 3, fill = NA, align = "center")
    )
  
  
  # Erba et al. (1999)
  Erba99 <- subset(Herrle_composite_data, Source_C_data == "Erba_et_al._(1999)")
  Erba99 <- Erba99[(order(Erba99$Age_Ma_Gradstein)), ]
  
  # Herrle et al. (2004)
  Herrle04 <- subset(Herrle_composite_data, Source_C_data == "Herrle_et_al._(2004)")
  Herrle04 <- Herrle04[(order(Herrle04$Age_Ma_Gradstein)), ]
  
  # Gale et al. (2011)
  Gale11 <- subset(Herrle_composite_data, Source_C_data == "Gale_et_al._(2011)")
  Gale11 <- Gale11[(order(Gale11$Age_Ma_Gradstein)), ]
  
  # Subset Gale 2011 to 108–110 Ma window
  Gale11_109subset <- subset(Gale11, Age_Ma_Gradstein < 110 & Age_Ma_Gradstein > 108)
  Gale11_109subset <- Gale11_109subset[(order(Gale11_109subset$Age_Ma_Gradstein)), ]
  
  # Jarvis et al. (2006)
  Jarvis06 <- subset(Herrle_composite_data, Source_C_data == "Jarvis_et_al._(2006)")
  Jarvis06 <- Jarvis06[(order(Jarvis06$Age_Ma_Gradstein)), ]
  
  
# Wrangle Crooked Creek data
  
  # Compute stats
  CCC_d13C_stats <- raw_data %>%
    group_by(Sample) %>%
    summarise(
      avg_d13C_VPDB = mean(`d 13C/12C`, na.rm = TRUE),
      SD_1sigma = sd(`d 13C/12C`, na.rm = TRUE),
      SE_1sigma = SD_1sigma / sqrt(n())
    )
  
  # Merge stats into the depths-only table
  CCC_merged <- depths_data %>%
    left_join(CCC_d13C_stats, by = c("sample" = "Sample")) %>%
    arrange(Strat_m_above_Pryor)  # Ensure depth order for moving average
  
  # Wrangle Cody Landfill data
  
  # Compute stats
  CLC_d13C_stats <- CLC_isotope_data %>%
    group_by(Identifier.1) %>%
    summarise(
      avg_d13C_VPDB = mean(`d.13C.12C`, na.rm = TRUE),
      SD_1sigma = sd(`d.13C.12C`, na.rm = TRUE),
      SE_1sigma = SD_1sigma / sqrt(n())
    )
  
  # Merge stats into the depths-only table
  CLC_stats_depths <- CLC_isotope_data %>%
    left_join(CLC_d13C_stats, by = c("Identifier.1" = "Identifier.1")) %>%
    arrange(Depth)  # Ensure depth order for moving average
  
  
  
# Clean data -----------------------------

# Test for and remove d13C outliers
      
    # CCC SECTION
      
      # STEP 1: Detect outliers using global IQR method
      CCC <- CCC_merged %>%
        mutate(
          Q1 = quantile(avg_d13C_VPDB, 0.25, na.rm = TRUE),
          Q3 = quantile(avg_d13C_VPDB, 0.75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower_bound = Q1 - 1.5 * IQR,
          upper_bound = Q3 + 1.5 * IQR,
          outlier = avg_d13C_VPDB < lower_bound | avg_d13C_VPDB > upper_bound
        )
      
      # STEP 2: Plot with outliers flagged
      ggplot(CCC, aes(x = Strat_m_above_Pryor, y = avg_d13C_VPDB)) +
        geom_line(color = "gray") +
        geom_point(aes(color = outlier), size = 2) +
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
        labs(title = "CCC δ13C with Outliers Flagged", y = "δ13C (VPDB)", x = "Strat. Height Above Pryor (m)") +
        theme_minimal()
      
      # STEP 3: Remove outliers and recompute rolling mean
      CCC_d13C_clean <- CCC %>%
        filter(!outlier) %>%
        arrange(Strat_m_above_Pryor) %>%
        mutate(
          roll_mean_clean = rollmean(avg_d13C_VPDB, k = 3, fill = NA)
        )
      
      # STEP 4: Plot cleaned curve
      ggplot(CCC_d13C_clean, aes(x = Strat_m_above_Pryor, y = avg_d13C_VPDB)) +
        geom_point(size = 2, color = "grey") +
        geom_line(aes(y = roll_mean_clean), color = "blue") +
        labs(title = "CCC δ13C Cleaned", y = "δ13C (VPDB)", x = "Strat. Height Above Pryor (m)") +
        theme_minimal()
      
  # CLC SECTION
      
      # STEP 1: Detect outliers using global IQR method
      CLC <- CLC_stats_depths %>%
        mutate(
          Q1 = quantile(avg_d13C_VPDB, 0.25, na.rm = TRUE),
          Q3 = quantile(avg_d13C_VPDB, 0.75, na.rm = TRUE),
          IQR = Q3 - Q1,
          lower_bound = Q1 - 1.5 * IQR,
          upper_bound = Q3 + 1.5 * IQR,
          outlier = avg_d13C_VPDB < lower_bound | avg_d13C_VPDB > upper_bound
        )
      
      
      # STEP 2: Plot with outliers flagged
      ggplot(CLC, aes(x = Depth, y = avg_d13C_VPDB)) +
        geom_line(color = "gray") +
        geom_point(aes(color = outlier), size = 2) +
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
        labs(title = "CLC δ13C with Outliers Flagged", y = "δ13C (VPDB)", x = "Strat. Height Above Pryor (m)") +
        theme_minimal()
      
      # STEP 3: Remove outliers and recompute rolling mean
      CLC_d13C_clean <- CLC %>%
        filter(!outlier) %>%
        arrange(Depth) %>%  # Fix: sort by stratigraphic order
        mutate(
          roll_mean_clean = rollmean(avg_d13C_VPDB, k = 3, fill = NA)
        )
      
      # STEP 4: Plot cleaned curve
      ggplot(CLC_d13C_clean, aes(x = Depth, y = avg_d13C_VPDB)) +
        geom_point(size = 2, color = "grey") +
        geom_line(aes(y = roll_mean_clean), color = "blue") +
        labs(title = "CLC δ13C Cleaned", y = "δ13C (VPDB)", x = "Depth (m)") +
        theme_minimal()
    
## integrate age and isotope data -----------------------------------
    
    # Take a look at Jack's models
    ggplot(CLC_age_model, aes(x = X0.5, y = Height_m)) +
      geom_point(color = "cornflowerblue", size = 1.5) +
      scale_x_reverse() +  # Reverses y-axis to show increasing age downward
      labs(
        x = "Age (Ma)",
        y = "Depth (m)",
        title = "Jack's CLC age model"
      ) +
      theme_minimal()
    
    
    # Cody Landfill 
    # !! Resolve duplicate height values at hiatus !! #
    CLC_age_model$Height_m[31] <- 14.9
    
    # Create an age column and assign NA by default
    CLC_d13C_clean$age <- NA
    
    # Interpolate preferred age from CLC_age_model
    CLC_interp <- approx(
      x = CLC_age_model$Height_m,
      y = CLC_age_model$X0.5,
      xout = CLC_d13C_clean$Depth,  # replace with actual column name
      rule = 2
    )
    
    # Add interpolated age to CLC dataset
    CLC_d13C_clean$age <- CLC_interp$y
    
    # Modify CLC age model to account for unconformity
    # Look at model first
    ggplot(CLC_d13C_clean, aes(x = age, y = Depth)) +
      geom_point(color = "cornflowerblue", size = 1.5) +
      scale_x_reverse() +
      labs(
        x = "Age (Ma)",
        y = "Depth (m)",
        title = "interpolated CLC age model"
      ) +
      theme_minimal()
    
    
    # Crooked Creek
    # Interpolate preferred age from CCC_age_model
    CCC_interp <- approx(
      x = CCC_age_model$Height_m,
      y = CCC_age_model$Preferred_Age,
      xout = CCC_d13C_clean$Strat_m_above_Pryor,  # replace with actual column name
      rule = 2
    )
    
    # Add interpolated age to CCC dataset
    CCC_d13C_clean$age <- CCC_interp$y
    
    
    
 # Create age model for Axel Heiberg Island
    Albian_arctic_age <- data.frame(
      age_Ma = character(),
      height_m = character(),
      stringsAsFactors = FALSE
    )
    
    Albian_arctic_age <- data.frame(
      age_Ma = c(106.58, 111.74),
      height_m = c(750, 600)
    )
    # --- Inputs: two age–depth tie points (edit values as needed) -------------
    age_depth_ties <- data.frame(
      age_Ma   = c(106.58, 111.74),
      depth_m  = c(750, 600)    # use a single, consistent name: depth_m
    )
    
    # --- 0) Make sure Herrle_arctic has a depth_m column ----------------------
    # If your dataframe uses 'height_m', rename it to 'depth_m' for consistency.
    if ("height_m" %in% names(Herrle_arctic) && !"depth_m" %in% names(Herrle_arctic)) {
      Herrle_arctic <- dplyr::rename(Herrle_arctic, depth_m = height_m)
    }
    
    stopifnot("depth_m" %in% names(Herrle_arctic))
    
    # --- 1) Build linear age–depth model (no extrapolation) -------------------
    age_depth_ties <- age_depth_ties[order(age_depth_ties$depth_m), ]
    
    # Interpolation function: age(depth)
    age_fun <- approxfun(
      x    = age_depth_ties$depth_m,
      y    = age_depth_ties$age_Ma,
      rule = 1   # NA outside tie-point range
    )
    
    # --- 2) Interpolate ages into Herrle_arctic by depth ----------------------
    Herrle_arctic <- Herrle_arctic %>%
      dplyr::arrange(depth_m) %>%
      dplyr::mutate(age_Ma_interp = age_fun(depth_m))
    
    # --- 3) Quick coverage check ---------------------------------------------
    range_ties <- range(age_depth_ties$depth_m, na.rm = TRUE)
    range_data <- range(Herrle_arctic$depth_m,    na.rm = TRUE)
    message(sprintf(
      "Tie-point depth range = [%.2f, %.2f] m; data depth range = [%.2f, %.2f] m",
      range_ties[1], range_ties[2], range_data[1], range_data[2]
    ))
    
    # --- 4) Sanity-check plot: age–depth line + tie points --------------------
    if (requireNamespace("ggplot2", quietly = TRUE)) {

      ggplot() +
        geom_line(
          data = Herrle_arctic,
          aes(x = age_Ma_interp, y = depth_m),
          linewidth = 0.7, alpha = 0.6, na.rm = TRUE
        ) +
        geom_point(
          data = age_depth_ties,
          aes(x = age_Ma, y = depth_m),
          color = "red", size = 2
        ) +
        scale_y_reverse() +
        labs(
          x = "Age (Ma)",
          y = "Depth (m)",
          title = "Linear age–depth model and interpolated ages (Herrle Arctic)"
        ) +
        theme_minimal()
    }
    
### Merge data into one dataframe ----------------------------------------------------
    
    # Prepare CCC data
    CCC_processed <- CCC_d13C_clean %>%
      select(sample, Strat_m_above_Pryor, age, avg_d13C_VPDB) %>%
      rename(
        depth = Strat_m_above_Pryor,
        d13C  = avg_d13C_VPDB
      ) %>%
      mutate(section = "CCC", depth = depth - 10)  # Shift depth to base of LSM
    
    # Prepare CLC data
    CLC_processed <- CLC_d13C_clean %>%
      select(Identifier.1, Depth, age, d.13C.12C) %>%
      rename(
        sample = Identifier.1,
        depth  = Depth,
        d13C   = d.13C.12C
      ) %>%
      mutate(section = "CLC")
    
    # Combine both sections
    combined_isotope_data <- bind_rows(CCC_processed, CLC_processed) %>%
      filter(!is.na(age), !is.na(d13C))
    
    

