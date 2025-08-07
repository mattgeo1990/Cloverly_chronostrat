# Setup ------------------------

# Check if data is loaded; if not, run setup
if (!exists("CLC_d13C_clean") || !exists("CCC_isotope_data") || 
    !exists("CLC_age_model") || !exists("CCC_age_model")) {
  source(here::here("scripts", "setup.R"))
}


# Cody Landfill ------------
        ## By Depth ####
      # CLC d13C vs Depth
        gg_CLC_d13C_Depth <- ggplot(CLC_d13C_clean[ order(CLC_d13C_clean$Depth), ], aes(x = d.13C.12C, y = Depth)) +
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
        gg_CLC_d13C_Age <- ggplot(CLC_d13C_clean[ order(CLC_d13C_clean$age), ], aes(x = d.13C.12C, y = age)) +
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
        # CCC d13C vs Depth
        gg_CCC_d13C_Depth <- ggplot(CCC_d13C_clean[ order(CCC_d13C_clean$Strat_m_above_Pryor), ], aes(x = avg_d13C_VPDB, y = Strat_m_above_Pryor)) +
          geom_point(size = 1.5, color = "cornflowerblue") +
          geom_path(aes(x=rollmean(avg_d13C_VPDB, 3, na.pad=TRUE)), colour = "black") +   # Add a line 
          labs(
            x = "d13Cord",
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
        gg_CCC_d13C_Age <- ggplot(CCC_d13C_clean[ order(CCC_d13C_clean$age), ], aes(x = avg_d13C_VPDB, y = age)) +
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
        common_y_limits <- c(125, 100)
        
        # Redefine each plot with standardized formatting
        gg_CLC_d13C_Age <- ggplot(CLC_d13C_clean[order(CLC_d13C_clean$age), ]) +
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
        
        gg_CCC_d13C_Age <- ggplot(CCC_d13C_clean[order(CCC_d13C_clean$age), ]) +
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
        gg_CLC_d13C_Depth <- ggplot(CLC_d13C_clean[order(CLC_d13C_clean$Depth), ]) +
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
        
        CCC_d13C_clean
        gg_CCC_d13C_Depth <- ggplot(CCC_d13C_clean[order(CCC_d13C_clean$Strat_m_above_Pryor), ]) +
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
        
# combined Albian -----------------------
        

        # Determine shared y-axis range
        common_y_limits <- c(112, 104)
        
        # Redefine each plot with standardized formatting
        gg_CLC_Age_Albian <- ggplot(CLC_d13C_clean[order(CLC_d13C_clean$age), ]) +
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
        
        gg_CCC_d13C_Age <- ggplot(CCC_d13C_clean[order(CCC_d13C_clean$age), ]) +
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

        gg_Albian_by_age <- gg_CLC_Age_Albian + gg_CCC_d13C_Age +
          plot_layout(ncol = 2, guides = "collect") &
          theme(legend.position = "bottom")
        
        # Print combined plot
        print(gg_Albian_by_age)      
        
        # Export the plot
        ggsave(
          filename = "d13C_combined_by_age.png",     # Output file name
          plot = gg_d13C_combined_by_age,               # Plot object
          path = here::here("results", "figures"),
          width = 11,        
          height = 8.5,       
          dpi = 300
        )   
        
        
        