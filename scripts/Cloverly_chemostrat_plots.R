# Setup ------------------------

  # Load packages
    library(ggplot2)
    library(readr)
    library(dplyr)
    library(zoo)
    library(here)

  # Read data

    # Read in Cody Landfill data
      CLC_isotope_data <- read.csv(here("data", "raw", "CLC_results1.csv"))

    # Read in cleaned Crooked Creek data
      CCC_isotope_data <- read.csv(here("data", "processed", "CCC_d13Corg_clean.csv"))

    # Load Jack's age models
      CLC_age_model <- read.csv(here::here("data", "raw", "Cody_Landfill_age_model.csv"))
      CCC_age_model <- read.csv(here::here("data", "raw", "Crooked_Creek_age_model.csv"))
      
        # Take a look at Jack's models
        ggplot(CLC_age_model, aes(x = Height_m, y = Preferred_Age)) +
          geom_point(color = "cornflowerblue", size = 1.5) +
          scale_y_reverse() +  # Reverses y-axis to show increasing age downward
          labs(
            x = "Depth (m)",
            y = "Age (Ma)",
            title = "Jack's CLC age model"
          ) +
          theme_minimal()
        
    # Load MLA revised CLC age model with unconformity
      CLC_age_model_MLA <- read.csv(here::here("data", "raw", "CLC_unconformity_mla_age_model.csv"))
      
        # Take a look at the model
        ggplot(CLC_age_model_MLA, aes(x = Height_m, y = Preferred_Age)) +
          geom_point(color = "cornflowerblue", size = 1.5) +
          scale_y_reverse() +  # Reverses y-axis to show increasing age downward
          labs(
            x = "Depth (m)",
            y = "Age (Ma)",
            title = "MLA's CLC age model"
          ) +
          theme_minimal()
        
    # Take a look at Jack's models
      ggplot(CLC_age_model, aes(x = Height_m, y = Preferred_Age)) +
        geom_point(color = "cornflowerblue", size = 1.5) +
        scale_y_reverse() +  # Reverses y-axis to show increasing age downward
        labs(
          x = "Depth (m)",
          y = "Age (Ma)",
          title = "Jack's CLC age model"
        ) +
        theme_minimal()
      
    # !!! omit CLC outlier !!! # probably a bust sample, maybe CO3 included
      CLC_isotope_data <- CLC_isotope_data[which(CLC_isotope_data$d.13C.12C < -15), ]
  
## integrate age models with isotope data ####
      
      # Cody Landfill - some isotope data outside range of age model so we put NA for age
        
        # Define interpolation range based on the age model
        min_height <- min(CLC_age_model_MLA$Height_m)
        max_height <- max(CLC_age_model_MLA$Height_m)
        
        # Mask values outside range to avoid flat extrapolation
        valid_heights <- CLC_isotope_data$Height_m >= min_height & CLC_isotope_data$Height_m <= max_height
        
        # Create an age column and assign NA by default
        CLC_isotope_data$age <- NA
        
        # Interpolate only for values within the valid range
        CLC_isotope_data$age[valid_heights] <- approx(
          x = CLC_age_model_MLA$Height_m,
          y = CLC_age_model_MLA$Preferred_Age,
          xout = CLC_isotope_data$Height_m[valid_heights],
          rule = 1  # No extrapolation
        )$y
          
        # Interpolate preferred age from CLC_age_model
            CLC_interp <- approx(
              x = CLC_age_model_MLA$Height_m,
              y = CLC_age_model_MLA$Preferred_Age,
              xout = CLC_isotope_data$Depth,  # replace with actual column name
              rule = 2
            )
            
          # Add interpolated age to CLC dataset
          CLC_isotope_data$age <- CLC_interp$y
          
        # Modify CLC age model to account for unconformity
          # Look at model first
            ggplot(CLC_isotope_data, aes(x = Depth, y = age)) +
              geom_point(color = "cornflowerblue", size = 1.5) +
              scale_y_reverse() +  # Reverses y-axis to show increasing age downward
              labs(
                x = "Depth (m)",
                y = "Age (Ma)",
                title = "interpolated CLC age model"
              ) +
              theme_minimal()
            
            # Manual edits
            CLC_isotope_data$age[CLC_isotope_data$age >= 110 & CLC_isotope_data$age <= 120] <- 109.1
            CLC_isotope_data$age[CLC_isotope_data$age >= 121 & CLC_isotope_data$age <= 123] <- 109.1
            
            # Look at model again
            ggplot(CLC_isotope_data, aes(x = Depth, y = age)) +
              geom_point(color = "cornflowerblue", size = 1.5) +
              scale_y_reverse() +  # Reverses y-axis to show increasing age downward
              labs(
                x = "Depth (m)",
                y = "Age (Ma)",
                title = "interpolated CLC age model"
              ) +
              theme_minimal()
            
            
      # Crooked Creek
        # Interpolate preferred age from CCC_age_model
        CCC_interp <- approx(
          x = CCC_age_model$Height_m,
          y = CCC_age_model$Preferred_Age,
          xout = CCCclean_isotope_data$Strat_m_above_Pryor,  # replace with actual column name
          rule = 2
        )
        
        # Add interpolated age to CCC dataset
        CCCclean_isotope_data$age <- CCC_interp$y
        
        
# Cody Landfill ------------
        ## By Depth ####
      # CLC d13C vs Depth
        gg_CLC_d13C_Depth <- ggplot(CLC_isotope_data[ order(CLC_isotope_data$Depth), ], aes(x = d.13C.12C, y = Depth)) +
          geom_point(size = 1.5, color = "cornflowerblue") +
          geom_path(aes(x=rollmean(d.13C.12C, 3, na.pad=TRUE)), colour = "black") +   # Add a line 
          labs(
            x = "d13Cdoc",
            y = "Stratigraphy (m above base of LSM)",
            title = "CLC C-isotope Chemostrat"
          ) +
          theme_minimal() +
          ylim(min = 0, max = 70) +
        coord_cartesian(xlim = c(-32, -20)) +
          theme(
            panel.background = element_blank(), 
            plot.background = element_blank(),
            panel.grid = element_blank(),  # Hide gridlines
            axis.line.y = element_blank(),  # Hide y-axis line
            #axis.ticks.y = element_blank(),  # Hide y-axis ticks
            #axis.text.y = element_blank(),  # Hide y-axis labels
            axis.title.y = element_blank(),  # Hide y-axis title
            panel.border = element_blank(),  # Hide side borders
            axis.ticks = element_line(color = "black"),  # Show x-axis ticks
            axis.ticks.length = unit(0.2, "cm")  # Set the length of x-axis ticks
          )
        
        # Display the plot
        print(gg_CLC_d13C_Depth)
        
        # Export the plot
        ggsave(
          filename = "CLC_d13C_Depth.png",     # Output file name
          plot = gg_CLC_d13C_Depth,               # Plot object
          path = here::here("results", "figures"),           # Directory 
          width = 3,                              # Width in inches
          height = 8,                             # Height in inches
          dpi = 300                               # Resolution
        )
        
 ## By Age ####
        
        # CLC d13C vs Age
        gg_CLC_d13C_Age <- ggplot(CLC_isotope_data[ order(CLC_isotope_data$age), ], aes(x = d.13C.12C, y = age)) +
          geom_point(size = 1.5, color = "cornflowerblue") +
          geom_path(aes(x=rollmean(d.13C.12C, 3, na.pad=TRUE)), colour = "black") +   # Add a line 
          labs(
            x = "d13Cdoc",
            y = "Age (Ma)",
            title = "CLC C-isotope Chemostrat"
          ) +
          theme_minimal() +
          scale_y_reverse(limits = c(125, 105)) +
          coord_cartesian(xlim = c(-32, -20)) +
          theme(
            panel.background = element_blank(), 
            plot.background = element_blank(),
            panel.grid = element_blank(),  # Hide gridlines
            axis.line.y = element_blank(),  # Hide y-axis line
            #axis.ticks.y = element_blank(),  # Hide y-axis ticks
            #axis.text.y = element_blank(),  # Hide y-axis labels
            axis.title.y = element_blank(),  # Hide y-axis title
            panel.border = element_blank(),  # Hide side borders
            axis.ticks = element_line(color = "black"),  # Show x-axis ticks
            axis.ticks.length = unit(0.2, "cm")  # Set the length of x-axis ticks
          )
        
        # Display the plot
        print(gg_CLC_d13C_Age)
        
        # Export the plot
        ggsave(
          filename = "CLC_d13C_Age.png",     # Output file name
          plot = gg_CLC_d13C_Age,               # Plot object
          path = here::here("results", "figures"),           # Directory 
          width = 3,                              # Width in inches
          height = 8,                             # Height in inches
          dpi = 300                               # Resolution
        )     
        

        
        
# Crooked Creek ---------
  ## By Depth ####
        # CLC d13C vs Depth
        gg_CCC_d13C_Depth <- ggplot(CLC_isotope_data[ order(CLC_isotope_data$Strat_m_above_Pryor), ], aes(x = d.13C.12C, y = Strat_m_above_Pryor)) +
          geom_point(size = 1.5, color = "cornflowerblue") +
          geom_path(aes(x=rollmean(d.13C.12C, 3, na.pad=TRUE)), colour = "black") +   # Add a line 
          labs(
            x = Cdelt,
            y = "Stratigraphy (m above base of LSM)",
            title = "CCC d13C by Depth"
          ) +
          theme_minimal() +
          ylim(min = 0, max = 70) +
          coord_cartesian(xlim = c(-32, -20)) +
          theme(
            panel.background = element_blank(), 
            plot.background = element_blank(),
            panel.grid = element_blank(),  # Hide gridlines
            axis.line.y = element_blank(),  # Hide y-axis line
            #axis.ticks.y = element_blank(),  # Hide y-axis ticks
            #axis.text.y = element_blank(),  # Hide y-axis labels
            axis.title.y = element_blank(),  # Hide y-axis title
            panel.border = element_blank(),  # Hide side borders
            axis.ticks = element_line(color = "black"),  # Show x-axis ticks
            axis.ticks.length = unit(0.2, "cm")  # Set the length of x-axis ticks
          )
        
        # Display the plot
        print(gg_CCC_d13C_Depth)
        
        # Export the plot
        ggsave(
          filename = "CCC_d13C_Depth.png",     # Output file name
          plot = gg_CCC_d13C_Depth,               # Plot object
          path = here::here("results", "figures"),           # Directory 
          width = 3,                              # Width in inches
          height = 8,                             # Height in inches
          dpi = 300                               # Resolution
        )
## By Age ####
        # CCC d13C vs Age
        gg_CCC_d13C_Age <- ggplot(CCCclean_isotope_data[ order(CCCclean_isotope_data$age), ], aes(x = avg_d13C_VPDB, y = age)) +
          geom_point(size = 1.5, color = "cornflowerblue") +
          geom_path(aes(x=rollmean(avg_d13C_VPDB, 3, na.pad=TRUE)), colour = "black") +   # Add a line 
          labs(
            x = "d13Cdoc",
            y = "Stratigraphy (m above base of LSM)",
            title = "CCC d13C by Age"
          ) +
          theme_minimal() +
          scale_y_reverse(limits = c(125, 105)) +
          coord_cartesian(xlim = c(-32, -20)) +
          theme(
            panel.background = element_blank(), 
            plot.background = element_blank(),
            panel.grid = element_blank(),  # Hide gridlines
            axis.line.y = element_blank(),  # Hide y-axis line
            #axis.ticks.y = element_blank(),  # Hide y-axis ticks
            #axis.text.y = element_blank(),  # Hide y-axis labels
            axis.title.y = element_blank(),  # Hide y-axis title
            panel.border = element_blank(),  # Hide side borders
            axis.ticks = element_line(color = "black"),  # Show x-axis ticks
            axis.ticks.length = unit(0.2, "cm")  # Set the length of x-axis ticks
          )
        
        # Display the plot
        print(gg_CCC_d13C_Age)
        
        # Export the plot
        ggsave(
          filename = "CCC_d13C_Age.png",     # Output file name
          plot = gg_CCC_d13C_Age,               # Plot object
          path = here::here("results", "figures"),           # Directory 
          width = 3,                              # Width in inches
          height = 8,                             # Height in inches
          dpi = 300                               # Resolution
        )
        
        
# combined plots -----------------------------------------------------------------------
       
  ## age ------------------------------
     # Define delta notation for axis label
        Cdelt <- expression(delta^13 * "C" ~ "(‰ vs. VPDB)")
        
        # Determine shared y-axis range
        common_y_limits <- c(125, 105)
        
        # Redefine each plot with standardized formatting
        gg_CLC_d13C_Age <- ggplot(CLC_isotope_data[order(CLC_isotope_data$age), ]) +
          geom_point(aes(x = d.13C.12C, y = age), size = 1.5, color = "cornflowerblue") +
          geom_path(aes(x = rollmean(d.13C.12C, 3, na.pad = TRUE), y = age), color = "black") +
          scale_y_reverse(limits = common_y_limits) +
          coord_cartesian(xlim = c(-32, -18)) +
          labs(
            x = Cdelt,
            y = "Age (Ma)",
            title = expression("Cody Landfill " * delta^13 * "C by Age")) + 
          theme_minimal() +
          theme(
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(0.2, "cm")
          )
        
        gg_CCC_d13C_Age <- ggplot(CCCclean_isotope_data[order(CCCclean_isotope_data$age), ]) +
          geom_point(aes(x = avg_d13C_VPDB, y = age), size = 1.5, color = "cornflowerblue") +
          geom_path(aes(x = rollmean(avg_d13C_VPDB, 3, na.pad = TRUE), y = age), color = "black") +
          scale_y_reverse(limits = common_y_limits) +
          coord_cartesian(xlim = c(-32, -18)) +
          labs(
            x = Cdelt,
            y = "Age (Ma)",
            title = expression("Crooked Creek " * delta^13 * "C by Age")) + 
          theme_minimal() +
          theme(
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(0.2, "cm")
          )
        
        # Combine with patchwork
        library(patchwork)
        
        gg_d13C_combined_by_age <- gg_CLC_d13C_Age + gg_CCC_d13C_Age +
          plot_layout(ncol = 2, guides = "collect") &
          theme(legend.position = "bottom")
        
        # Print combined plot
        print(gg_d13C_combined_by_age)      
        
        # Export the plot
        ggsave(
          filename = "d13C_combined_by_age.png",     # Output file name
          plot = gg_d13C_combined_by_age,               # Plot object
          path = here::here("results", "figures"),
          width = 11,        
          height = 8.5,       
          dpi = 300
        )
        
  ## depth ------------------------------

        # Determine shared y-axis range
         common_y_limits <- c(0, 70)
        
        # Redefine each plot with standardized formatting
        gg_CLC_d13C_Depth <- ggplot(CLC_isotope_data[order(CLC_isotope_data$Depth), ]) +
          geom_point(aes(x = d.13C.12C, y = Depth), size = 1.5, color = "cornflowerblue") +
          geom_path(aes(x = rollmean(d.13C.12C, 3, na.pad = TRUE), y = Depth), color = "black") +
          scale_y_continuous(limits = common_y_limits) +
          coord_cartesian(xlim = c(-32, -18)) +
          labs(
            x = Cdelt,
            y = "Depth (m above Pryor Mbr)",
            title = expression("Cody Landfill " * delta^13 * "C by Depth")) + 
          theme_minimal() +
          theme(
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(0.2, "cm")
          )
        
        CCCclean_isotope_data
        gg_CCC_d13C_Depth <- ggplot(CCCclean_isotope_data[order(CCCclean_isotope_data$Strat_m_above_Pryor), ]) +
          geom_point(aes(x = avg_d13C_VPDB, y = Strat_m_above_Pryor), size = 1.5, color = "cornflowerblue") +
          geom_path(aes(x = rollmean(avg_d13C_VPDB, 3, na.pad = TRUE), y = Strat_m_above_Pryor), color = "black") +
          scale_y_continuous(limits = common_y_limits) +
          coord_cartesian(xlim = c(-32, -18)) +
          labs(
            x = Cdelt,
            y = "Depth (m above Pryor Mbr)",
            title = expression("Crooked Creek " * delta^13 * "C by Depth")) + 
          theme_minimal() +
          theme(
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.grid = element_blank(),
            axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(0.2, "cm")
          )
        
        
        gg_d13C_combined_by_depth <- gg_CLC_d13C_Depth + gg_CCC_d13C_Depth +
          plot_layout(ncol = 2, guides = "collect") &
          theme(legend.position = "bottom")
        
        # Print combined plot
        print(gg_d13C_combined_by_depth)      
        
        # Export the plot
        ggsave(
          filename = "d13C_combined_by_depth.png",     # Output file name
          plot = gg_d13C_combined_by_depth,               # Plot object
          path = here::here("results", "figures"),
          width = 11,        
          height = 8.5,       
          dpi = 300
        ) 
        
        
     
# junkyard ------------------------------------------    
        
     
# OLD DEPTH PLOT   
# Order by depth
CCCclean <- CCCclean %>%
  arrange(age_Ma)
# Add 3-point rolling average column
CCCclean <- CCCclean %>%
  arrange(Strat_m_above_Pryor) %>%
  mutate(d13C_3pt_avg = rollmean(d.13C.12C, 3, fill = NA, align = "center"))
# Create the plot
ggplot_CrookedCreek <- ggplot(CCCclean, aes(x = d.13C.12C, y = Strat_m_above_Pryor)) +
  geom_point(size = 1.5, color = "blue", alpha = 0.2) +
  geom_path(aes(x = d13C_3pt_avg, y = Strat_m_above_Pryor), colour = "black") +
  labs(
    x = expression(delta^13*C ~ "(‰ VPDB)"),
    y = "Stratigraphy (m above base of LSM)",
    title = "CCC C-isotope Chemostrat"
  ) +
  coord_cartesian(xlim = c(-29, -20), ylim = c(-6, 70)) +
  theme_minimal() +
  theme(
    panel.background = element_blank(), 
    plot.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )
# Print plot to viewer
print(ggplot_CrookedCreek)
# Export as PNG
ggsave(
  filename = here("results", "figures", "CCC_C_isotope_chemostrat_labeled.png"),
  plot = ggplot_CrookedCreek,
  width = 2,
  height = 5,
  dpi = 300
)
