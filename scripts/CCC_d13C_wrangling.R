#### Setup ####
# Load libraries
library(dplyr)
library(readr)
library(here)
library(zoo)  # for rollapply

# Optional: check project root
message("Project root: ", here::here())

#### Source data from within project ####

# Read in the raw isotope data
raw_data <- read_csv(here("data", "raw", "Kalu_CCC_d13Corg_RAW.csv"))

# Read in the depths-only data
depths_data <- read_csv(here("data", "raw", "Kalu_CCC_d13Corg_DEPTHS_ONLY.csv"))

#### Compute stats and merge into one dataframe ####

# Compute per-sample stats
d13C_stats <- raw_data %>%
  group_by(Sample) %>%
  summarise(
    avg_d13C_VPDB = mean(`d 13C/12C`, na.rm = TRUE),
    SD_1sigma = sd(`d 13C/12C`, na.rm = TRUE),
    SE_1sigma = SD_1sigma / sqrt(n())
  )

# Merge stats into the depths-only table
CCC_merged <- depths_data %>%
  left_join(d13C_stats, by = c("sample" = "Sample")) %>%
  arrange(Strat_m_above_Pryor)  # Ensure depth order for moving average

#### Compute 3-point moving average ####

CCC_merged <- CCC_merged %>%
  mutate(
    d13C_3pt_avg = zoo::rollapply(avg_d13C_VPDB, width = 3, FUN = mean, align = "center", fill = NA),
    depth_3pt_avg = zoo::rollapply(Strat_m_above_Pryor, width = 3, FUN = function(x) x[2], align = "center", fill = NA)
  )

#### Save final dataset ####
write_csv(CCC_merged, here("data", "processed", "CCC_d13Corg_clean.csv"))