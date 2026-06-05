library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)


#set times new roman font for all plots
library(showtext)
library(sysfonts)
font_add("Times New Roman", "C:/Windows/Fonts/times.ttf",
         regular = "C:/Windows/Fonts/times.ttf",
         bold    = "C:/Windows/Fonts/timesbd.ttf"
)
showtext_auto()
showtext_opts(dpi = 200)



#color palette for plots
my_colors <- c(
  "MOD1" = "#1B9E77",
  "S5"   = "#7570B3",
  "SD"   = "#E7298A")

my_colors_gs <- c(
  "S1_MOD1" = "#1B9E77",
  "S5_modern_beach" = "#7570B3" ,
  "sand_dune_1m" = "#E7298A"
)
#--------------- DATA LOI ----------------
loi_data <- read.csv("loi_data_modern.csv", sep = ";")

loi_long <- loi_data %>%
  pivot_longer(
    cols = c(loi_om, caco3, residue),
    names_to = "fraction",
    values_to = "value"
  )


loi_long$fraction <- recode(
  loi_long$fraction,
  "loi_om" = "OM (%)",
  "caco3"  = "CaCO3 (%)",
  "residue" = "Minerogenic (%)"
)




#---- modern samples
loi_modern <- loi_long |>
  filter(site %in% c("SD", "S5", "MOD1", "BR"))
loi_modern <- loi_modern |>
  mutate(
    fraction = factor(
      fraction,
      levels= c("OM (%)", "CaCO3 (%)", "Minerogenic (%)"))
  )
#---------------PLOTS LOI --------------------
#S1----


df_faceted1 <- loi_long %>%
  filter(site == "S1") %>%
  mutate(
    mid_depth = as.numeric(mid_depth),
    fraction = factor(
      fraction,
      levels = c("OM (%)", "CaCO3 (%)", "Minerogenic (%)")
    )
  ) %>%
  arrange(fraction, mid_depth)

## presentation

core_log_plot1 <- ggplot(df_faceted1,
                         aes(x = value, y = mid_depth)) +
  theme_bw(base_size = 12) +
  
  geom_hline(
    yintercept = c(20, 40, 60, 80, 100, 120, 140, 160, 180, 200),
    color = "grey95", linewidth = 0.4
  ) + 
  
  
  geom_point(size = 1.5) +
  # geom_path(linewidth = 0.6,
  #           color = "grey") +
  
  scale_y_reverse(
    limits = c(220,0),
    breaks = seq(0, 220, by = 40),
    expand = c(0, 0),
    sec.axis = dup_axis(name = NULL)
  ) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    minor_breaks = NULL,
    position = "top",
    expand = c(0, 0)
  ) +
  
  facet_wrap(
    ~ fraction,
    nrow = 1,
    strip.position = "top"
  ) +
  
  labs(
    x = NULL,
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )


#plot the figure------------------
core_log_plot1 +
  ggtitle("SITE 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

#compare to modern samples------------


core_log_plot1_vlines <- core_log_plot1 +
  geom_vline(
    data = loi_modern,
    aes(
      xintercept = value,
      colour = site
    ),
    linewidth = 0.6,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = my_colors,
    labels = c(
      "BR" = "Raised beach ridge",
      "MOD1" = "Modern river",
      "S5" = "Modern beach",
      "SD" = "Relict sand dune"
    )
  ) +
  labs(colour = "Legend") +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

core_log_plot1_vlines +
  ggtitle("SITE 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        strip.text.x = element_text(size = 16, face = "bold")
  )


#----


#SITE2----

df_faceted2 <- loi_long %>%
  filter(site == "S2") %>%
  mutate(
    mid_depth = as.numeric(mid_depth),
    fraction = factor(
      fraction,
      levels = c("OM (%)", "CaCO3 (%)", "Minerogenic (%)")
    )
  ) %>%
  arrange(fraction, mid_depth)

## presentation

core_log_plot2 <- ggplot(df_faceted2,
                         aes(x = value, y = mid_depth)) +
  # geom_hline(
  #   yintercept = ref_depths1,
  #   color = "grey80",
  #   linetype = "dashed", 
  #   linewidth = 0.4
  # ) +
  
  geom_hline(
    yintercept = c(20, 40, 60, 80, 100, 120, 140),
    color = "grey95", linewidth = 0.4
  ) + 
  
  geom_point(size = 1.5) +
  # geom_path(linewidth = 0.6,
  #           color = "black") +
  
  scale_y_reverse(
    limits = c(150,0),
    breaks = seq(0, 150, by = 40),
    expand = c(0, 0),
    sec.axis = dup_axis(name = NULL)
  ) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    minor_breaks = NULL,
    position = "top",
    expand = c(0, 0)
  ) +
  
  facet_wrap(
    ~ fraction,
    nrow = 1,
    strip.position = "top"
  ) +
  
  labs(
    x = NULL,
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )



core_log_plot2 +
  ggtitle("SITE 2") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

##compare to modern samples------------
core_log_plot2_vlines <- core_log_plot2 +
  geom_vline(
    data = loi_modern,
    aes(
      xintercept = value,
      colour = site
    ),
    linewidth = 0.8,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = my_colors,
    labels = c(
      "BR" = "Raised beach ridge",
      "MOD1" = "Modern river",
      "S5" = "Modern beach",
      "SD" = "Relict sand dune"
    )
  ) +
  labs(colour = "Legend") +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

core_log_plot2_vlines +
  ggtitle("SITE 2") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        strip.text.x = element_text(size = 16, face = "bold")
  )

#SITE3-----


df_faceted3 <- loi_long %>%
  filter(site == "S3") %>%
  mutate(
    mid_depth = as.numeric(mid_depth),
    fraction = factor(
      fraction,
      levels = c("OM (%)", "CaCO3 (%)", "Minerogenic (%)")
    )
  ) %>%
  arrange(fraction, mid_depth)

## presentation

core_log_plot3 <- ggplot(df_faceted3,
                         aes(x = value, y = mid_depth)) +
  
  
  geom_hline(
    yintercept = c(20, 40, 60, 80, 100, 120, 140, 160),
    color = "grey95", linewidth = 0.4
  ) + 
  
  geom_point(size = 1.5) +
  # geom_path(linewidth = 0.6,
  #           color = "black") +
  
  scale_y_reverse(
    limits = c(180,0),
    breaks = seq(0, 180, by = 40),
    expand = c(0, 0),
    sec.axis = dup_axis(name = NULL)
  ) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    minor_breaks = NULL,
    position = "top",
    expand = c(0, 0)
  ) +
  
  facet_wrap(
    ~ fraction,
    nrow = 1,
    strip.position = "top"
  ) +
  
  labs(
    x = NULL,
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )



core_log_plot3 +
  ggtitle("SITE 3") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

##compare to modern samples------------
core_log_plot3_vlines <- core_log_plot3 +
  geom_vline(
    data = loi_modern,
    aes(
      xintercept = value,
      colour = site
    ),
    linewidth = 0.8,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = my_colors,
    labels = c(
      "BR" = "Raised beach ridge",
      "MOD1" = "Modern river",
      "S5" = "Modern beach",
      "SD" = "Relict sand dune"
    )
  ) +
  labs(colour = "Legend") +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

core_log_plot3_vlines +
  ggtitle("SITE 3") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        strip.text.x = element_text(size = 16, face = "bold"))
#P1S-----

df_faceted_P1S <- loi_long %>%
  filter(site == "P1S") %>%
  mutate(
    mid_depth = as.numeric(mid_depth),
    fraction = factor(
      fraction,
      levels = c("OM (%)", "CaCO3 (%)", "Minerogenic (%)")
    )
  ) %>%
  arrange(fraction, mid_depth)

## presentation

core_log_plot_P1S <- ggplot(df_faceted_P1S,
                            aes(x = value, y = mid_depth)) +
  
  
  geom_hline(
    yintercept = c(2, 4, 6, 8, 10, 12),
    color = "grey95", linewidth = 0.4
  ) + 
  
  geom_point(size = 1.5) +
  # geom_path(linewidth = 0.6,
  #           color = "black") +
  
  scale_y_reverse(
    limits = c(17,0),
    breaks = seq(0, 17, by = 4),
    expand = c(0, 0),
    sec.axis = dup_axis(name = NULL)
  ) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    minor_breaks = NULL,
    position = "top",
    expand = c(0, 0)
  ) +
  
  facet_wrap(
    ~ fraction,
    nrow = 1,
    strip.position = "top"
  ) +
  
  labs(
    x = NULL,
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )



core_log_plot_P1S +
  ggtitle("SITE P1S") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

##compare to modern samples------------
core_log_plot_P1S_vlines <- core_log_plot_P1S +
  geom_vline(
    data = loi_modern,
    aes(
      xintercept = value,
      colour = site
    ),
    linewidth = 0.8,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = my_colors,
    labels = c(
      "BR" = "Raised beach ridge",
      "MOD1" = "Modern river",
      "S5" = "Modern beach",
      "SD" = "Relict sand dune"
    )
  ) +
  labs(colour = "Legend") +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

core_log_plot_P1S_vlines +
  ggtitle("SITE P1S") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        strip.text.x = element_text(size = 16, face = "bold"))

#P2S----

df_faceted_P2S <- loi_long %>%
  filter(site == "P2S") %>%
  mutate(
    mid_depth = as.numeric(mid_depth),
    fraction = factor(
      fraction,
      levels = c("OM (%)", "CaCO3 (%)", "Minerogenic (%)")
    )
  ) %>%
  arrange(fraction, mid_depth)

## presentation

core_log_plot_P2S <- ggplot(df_faceted_P2S,
                            aes(x = value, y = mid_depth)) +
  # geom_hline(
  #   yintercept = ref_depths1,
  #   color = "grey80",
  #   linetype = "dashed", 
  #   linewidth = 0.4
  # ) +
  
  geom_hline(
    yintercept = c(2, 4, 6, 8, 10, 12),
    color = "grey95", linewidth = 0.4
  ) + 
  
  geom_point(size = 1.5) +
  # geom_path(linewidth = 0.6,
  #           color = "black") +
  
  scale_y_reverse(
    limits = c(15,0),
    breaks = seq(0,15, by = 4),
    expand = c(0,0),
    sec.axis = dup_axis(name = NULL)
  ) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    minor_breaks = NULL,
    position = "top",
    expand = c(0, 0)
  ) +
  
  facet_wrap(
    ~ fraction,
    nrow = 1,
    strip.position = "top"
  ) +
  
  labs(
    x = NULL,
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )



core_log_plot_P2S +
  ggtitle("SITE P2S") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

##compare to modern samples------------
core_log_plot_P2S_vlines <- core_log_plot_P2S +
  geom_vline(
    data = loi_modern,
    aes(
      xintercept = value,
      colour = site
    ),
    linewidth = 0.8,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = my_colors,
    labels = c(
      "BR" = "Raised beach ridge",
      "MOD1" = "Modern river",
      "S5" = "Modern beach",
      "SD" = "Relict sand dune"
    )
  ) +
  labs(colour = "Legend") +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

core_log_plot_P2S_vlines +
  ggtitle("SITE P2S") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        strip.text.x = element_text(size = 16, face = "bold"))

#P3S-----

df_faceted_P3S <- loi_long %>%
  filter(site == "P3S") %>%
  mutate(
    mid_depth = as.numeric(mid_depth),
    fraction = factor(
      fraction,
      levels = c("OM (%)", "CaCO3 (%)", "Minerogenic (%)")
    )
  ) %>%
  arrange(fraction, mid_depth)

## presentation

core_log_plot_P3S <- ggplot(df_faceted_P3S,
                            aes(x = value, y = mid_depth)) +
  # geom_hline(
  #   yintercept = ref_depths1,
  #   color = "grey80",
  #   linetype = "dashed", 
  #   linewidth = 0.4
  # ) +
  
  geom_hline(
    yintercept = c(4, 8, 12, 16, 20, 24),
    color = "grey95", linewidth = 0.4
  ) + 
  
  geom_point(size = 1.5) +
  # geom_path(linewidth = 0.6,
  #           color = "black") +
  
  scale_y_reverse(
    limits = c(22,0),
    breaks = seq(0, 22, by = 4),
    expand = c(0, 0),
    sec.axis = dup_axis(name = NULL)
  ) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    minor_breaks = NULL,
    position = "top",
    expand = c(0, 0)
  ) +
  
  facet_wrap(
    ~ fraction,
    nrow = 1,
    strip.position = "top"
  ) +
  
  labs(
    x = NULL,
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )



core_log_plot_P3S +
  ggtitle("SITE P3S") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

##compare to modern samples------------
core_log_plot_P3S_vlines <- core_log_plot_P3S +
  geom_vline(
    data = loi_modern,
    aes(
      xintercept = value,
      colour = site
    ),
    linewidth = 0.8,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = my_colors,
    labels = c(
      "BR" = "Raised beach ridge",
      "MOD1" = "Modern river",
      "S5" = "Modern beach",
      "SD" = "Relict sand dune"
    )
  ) +
  labs(colour = "Legend") +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

core_log_plot_P3S_vlines +
  ggtitle("SITE P3S") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),         strip.text.x = element_text(size = 16, face = "bold"))

#P4S----

df_faceted_P4S <- loi_long %>%
  filter(site == "P4S") %>%
  mutate(
    mid_depth = as.numeric(mid_depth),
    fraction = factor(
      fraction,
      levels = c("OM (%)", "CaCO3 (%)", "Minerogenic (%)")
    )
  ) %>%
  arrange(fraction, mid_depth)

## presentation

core_log_plot_P4S <- ggplot(df_faceted_P4S,
                            aes(x = value, y = mid_depth)) +
  # geom_hline(
  #   yintercept = ref_depths1,
  #   color = "grey80",
  #   linetype = "dashed", 
  #   linewidth = 0.4
  # ) +
  
  geom_hline(
    yintercept = c( 4, 8, 12, 16, 20, 24, 28),
    color = "grey95", linewidth = 0.4
  ) +
  
  geom_point(size = 1.5) +
  # geom_path(linewidth = 0.6,
  #           color = "black") +
  
  scale_y_reverse(
    limits = c(28, 0),
    breaks = seq(0, 28, by = 6),
    expand =  c(0,0),
    sec.axis = dup_axis(name = NULL)
  ) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    minor_breaks = NULL,
    position = "top",
    expand = c(0, 0)
  ) +
  
  facet_wrap(
    ~ fraction,
    nrow = 1,
    strip.position = "top"
  ) +
  
  labs(
    x = NULL,
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )



core_log_plot_P4S +
  ggtitle("SITE P4S") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

##compare to modern samples------------
core_log_plot_P4S_vlines <- core_log_plot_P4S +
  geom_vline(
    data = loi_modern,
    aes(
      xintercept = value,
      colour = site
    ),
    linewidth = 0.8,
    alpha = 0.8,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(
    values = my_colors,
    labels = c(
      "BR" = "Raised beach ridge",
      "MOD1" = "Modern river",
      "S5" = "Modern beach",
      "SD" = "Relict sand dune"
    )
  ) +
  labs(colour = "Legend") +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

core_log_plot_P4S_vlines +
  ggtitle("SITE P4S") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),         strip.text.x = element_text(size = 16, face = "bold"))

#---------------------------------------------GA DATA ------------------------------------------
gs_data_raw <- read.csv2("gs_data.csv")
gs_data <- na.omit(gs_data_raw)

#--------WEIGHTED MEAN ----------------------
wm <- gs_data |>
  group_by(site, sample_id) |>
  summarise(
    weighted_mean = weighted.mean(sieve_mm, g_retained),
    .groups = "drop"
  ) |>
  mutate(weighted_mean = round(weighted_mean, 3))

wmM <- wm |>
  filter(site %in% c("SD", "S5", "MOD1", "BR"))

desired_order <- c(
  # --- SITE 1 ---
  "S1-U2-58-63cm",
  "S1-U3-92-94cm",
  "S1-U4-103-108cm",
  "S1-U5-128-133cm",
  "S1-U6-142-146cm",
  "S1-U7-182-186cm",
  "S1-U8-208-213cm",
  
  # --- SITE 2 ---
  "S2-U1-10-15cm",
  "S2-U1-25-30cm",
  "S2-U2-56-61cm",
  "S2-U3-72-77cm",
  "S2-U4-79-84cm",
  "S2-U5-88-93cm",
  "S2-U6-97-101cm",
  "S2-U7-105-110cm",
  "S2-U8-121-126cm",
  "S2-U8-135-140cm",
  
  # --- SITE 3 ---
  "S3.1-U1-30-33cm",
  "S3.1-U2-47-51cm",
  "S3.1-U2-60-65cm",
  "S3.2-U3-90-95cm",
  "S3.2-U3-135-140cm",
  "S3.2-U4-154-159cm",
  "S3.2-U5-165-170cm")


wm <- wm |>
  mutate(sample_id = factor(sample_id, levels = desired_order)) |>
  arrange(sample_id)
# extract midpoints ----


wm <- wm %>%
  mutate(
    sample_id = str_trim(sample_id),
    # extract ONLY the final depth segment before "cm", e.g. "10-15"
    depth_segment = str_extract(sample_id, "\\d+-\\d+(?=cm$)"),
    
    depth_start = as.numeric(str_extract(depth_segment, "^\\d+")),
    depth_end   = as.numeric(str_extract(depth_segment, "\\d+$")),
    
    mid_depth = (depth_start + depth_end) / 2
  ) %>%
  filter(!is.na(mid_depth)) %>%
  arrange(site, mid_depth) 





# plots -----------
wm1 <- wm |>
  filter(site %in% "S1")
wm2 <- wm |>
  filter(site %in% "S2")
wm3 <- wm |>
  filter(site %in% "S3")


#S1----------------------------------------

wm_plot1 <- ggplot(wm1, aes(weighted_mean, mid_depth)) +
  
  
  geom_hline(
    yintercept = c(20, 40, 60, 80, 100, 120, 140, 160, 180, 200),
    color = "grey95", linewidth = 0.4
  ) + 
  
  geom_point(size = 1.5) +
  # geom_path(
  #   aes(group = 1),
  #   linewidth = 0.6,
  #   color = "black"
  # ) +
  
  
  scale_y_reverse(
    limits = c(220,0),
    breaks = seq(0, 220, by = 40),
    expand = c(0, 0)
  ) +
  
  
  scale_x_log10(
    limits = c(0.125, 16),
    breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
    minor_breaks = NULL,
    labels = c("0.125", "0.25", "0.5", "1", "2", "4", "8", "16"),
    position = "top",
    expand = c(0, 0),
    name = expression(bold("Mean grain size (mm)"))
  ) +
  
  
  labs(
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )


#plot results-----
wm_plot1 +
  ggtitle("SITE 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

#compare modern samples------
wm_plot1_vlines <- wm_plot1 +
  geom_vline(
    data = wmM,
    aes(
      xintercept = weighted_mean,
      colour = sample_id
    ),
    linewidth = 0.8,
    alpha = 0.8
  ) +
  scale_color_manual(
    values = my_colors_gs,
    breaks = c(
      "beach_ridge",
      "S1_MOD1",
      "S5_modern_beach" ,
      "sand_dune_1m" 
    ),
    labels = c(
      "beach_ridge" = "Raised beach ridge",
      "S1_MOD1" = "Modern river",
      "S5_modern_beach" = "Modern beach",
      "sand_dune_1m" = "Relict sand dune")
  ) +
  labs(colour = "Legend")+
  
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

wm_plot1_vlines +
  ggtitle("SITE 1") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.x.top = element_text(size = 16, face = "bold"))

#s2---------------------------
wm_plot2 <- ggplot(wm2, aes(weighted_mean, mid_depth)) +
  
  
  geom_hline(
    yintercept = c(20, 40, 60, 80, 100, 120, 140),
    color = "grey95", linewidth = 0.4
  ) + 
  
  geom_point(size = 1.5) +
  # geom_path(
  #   aes(group = 1),
  #   linewidth = 0.6,
  #   color = "black"
  # ) +
  # 
  
  scale_y_reverse(
    limits = c(150,0),
    breaks = seq(0, 150, by = 40),
    expand = c(0, 0),
  ) +
  
  
  scale_x_log10(
    limits = c(0.125, 16),
    breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
    labels = c("0.125", "0.25", "0.5", "1", "2", "4", "8", "16"),
    minor_breaks = NULL,
    position = "top",
    expand = c(0, 0),
    name = expression(bold("Mean grain size (mm)"))
  ) +
  
  
  labs(
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )



wm_plot2 +
  ggtitle("SITE 2") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

#compare modern samples------
wm_plot2_vlines <- wm_plot2 +
  geom_vline(
    data = wmM,
    aes(
      xintercept = weighted_mean,
      colour = sample_id
    ),
    linewidth = 0.8,
    alpha = 0.8
  ) +
  scale_color_manual(
    values = my_colors_gs,
    breaks = c(
      "beach_ridge",
      "S1_MOD1",
      "S5_modern_beach" ,
      "sand_dune_1m" 
    ),
    labels = c(
      "beach_ridge" = "Raised beach ridge",
      "S1_MOD1" = "Modern river",
      "S5_modern_beach" = "Modern beach",
      "sand_dune_1m" = "Relict sand dune")
  ) +
  labs(colour = "Legend")+
  
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

wm_plot2_vlines +
  ggtitle("SITE 2") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.x.top = element_text(size = 16, face = "bold"))


#S3-----------------------------
wm_plot3 <- ggplot(wm3, aes(weighted_mean, mid_depth)) +
  
  
  geom_hline(
    yintercept = c(20, 40, 60, 80, 100, 120, 140, 160),
    color = "grey95", linewidth = 0.4
  ) + 
  
  geom_point(size = 1.5) +
  # geom_path(
  #   aes(group = 1),
  #   linewidth = 0.6,
  #   color = "black"
  # ) +
  
  
  scale_y_reverse(
    limits = c(180,0),
    breaks = seq(0, 180, by = 40),
    expand = c(0, 0),
  ) +
  
  
  scale_x_log10(
    limits = c(0.125, 16),
    breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
    minor_breaks = NULL,
    labels = c("0.125", "0.25", "0.5", "1", "2", "4", "8", "16"),
    position = "top",
    expand = c(0, 0),
    name = expression(bold("Mean grain size (mm)"))
  ) +
  
  
  labs(
    y = "Depth (cm)"
  ) +
  
  theme_bw(base_size = 12) +
  theme(
    
    # Turn OFF y-grid, keep x-grid
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Style the x-gridlines
    panel.grid.major.x = element_line(color = "grey95", linewidth = 0.4),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    
    panel.spacing = unit(0.6, "cm"),
    
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_text(size = 11, face = "bold"),
    
    axis.title.y = element_text(margin = margin(r = 8)),
    
    plot.margin = margin(t = 5, r = 45, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off") +
  theme(
    text = element_text(family = "Times New Roman")
  )



wm_plot3 +
  ggtitle("SITE 3") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

#compare samples ----
wm_plot3_vlines <- wm_plot3 +
  geom_vline(
    data = wmM,
    aes(
      xintercept = weighted_mean,
      colour = sample_id
    ),
    linewidth = 0.8,
    alpha = 0.8
  ) +
  scale_colour_manual(
    values = my_colors_gs,
    breaks = c(
      "beach_ridge",
      "S1_MOD1",
      "S5_modern_beach" ,
      "sand_dune_1m" 
    ),
    labels = c(
      "beach_ridge" = "Raised beach ridge",
      "S1_MOD1" = "Modern river",
      "S5_modern_beach" = "Modern beach",
      "sand_dune_1m" = "Relict sand dune")
  ) +
  labs(colour = "Legend")+
  
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

wm_plot3_vlines +
  ggtitle("SITE 3") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.x.top = element_text(size = 16, face = "bold"))


#-------------------------------CUMULATIVE CUVRE--------------------------------------------
#dont need this, as i moved it up so delete later
# gs_data_raw <- read.csv2("gs_data.csv")
# gs_data <- na.omit(gs_data_raw)

gs_data <- gs_data %>% 
  mutate(sieve_mm = as.numeric(sieve_mm)) %>%
  mutate(pct_coarser = 100 - pct_finer)

#add phi values to the dataset
gs_data <- gs_data %>%
  mutate(phi = -log2(sieve_mm))


mm_breaks <- c(16,8, 4, 2, 1, 0.5, 0.250, 0.125, 0.063, 0.045)
phi_breaks <- -log2(mm_breaks)

# CLASS BOUNDARIES
class_bounds_mm <- c(16, 4, 2, 1, 0.5, 0.25, 0.125, 0.063, 0.045)
class_names     <- c("pebble", "gravel", "vc.sand","c.sand", "m.sand", "f.sand", "vf.sand", "silt & clay")

class_mids_mm  <- sqrt(class_bounds_mm[-length(class_bounds_mm)] * class_bounds_mm[-1])
class_mids_phi <- -log2(class_mids_mm)

# boundaries in phi for vlines
class_bounds_phi <- -log2(class_bounds_mm)

class_labels <- class_names

# THE PLOT ------------------------------------------------------

gs_modern <- gs_data |>
  filter(site %in% c("SD", "S5", "MOD1", "BR"),
         sieve_mm > 0) |>
  mutate(phi = -log2(sieve_mm))

#SITE1-----

gs_data %>%
  filter(site == "S1", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # CLASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  #only site  
  geom_line(linewidth = 1) +
  
  
  
  
  scale_color_manual(values = c(
    "#7570b3",
    "#66a61e",
    "#e6ab02",
    "#ee0000",
    "#48494b",
    "#1f78b4",
    "#8b4726"
  ),
  
  #order of legend - remember to copy from dataset, need to be the exact same
  breaks = c(
    "S1-U2-58-63cm",
    "S1-U3-92-94cm",
    "S1-U4-103-108cm",
    "S1-U5-128-133cm",
    "S1-U6-142-146cm",
    "S1-U7-182-186cm",
    "S1-U8-208-213cm"
  )
  )+
  
  
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "SITE 1",
    color = "Legend"
  ) +
  
  
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95),
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  )+
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_text(face = "bold")
  )

#SITE1 compare to modern -----
gs_data %>%
  filter(site == "S1", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # CLASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  #only site  
  geom_line(linewidth = 1) +
  
  #modern
  geom_line(
    data = gs_modern,
    aes(x = phi, y = pct_finer, color = site, group = site),
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  
  
  scale_color_manual(
    values = c(
      # S1 samples 
      "S1-U2-58-63cm" = "#7570b3",
      "S1-U3-92-94cm" = "#66a61e",
      "S1-U4-103-108cm"  = "#e6ab02",
      "S1-U5-128-133cm" = "#ee0000",
      "S1-U6-142-146cm" = "#48494b",
      "S1-U7-182-186cm" = "#1f78b4",
      "S1-U8-208-213cm" = "#8b4726",
      
      # modern reference curves
      "SD"   = "black",
      "MOD1" = "grey40",
      "S5"   = "grey60",
      "BR"   = "grey70"
    ),
    
    breaks = c(
      "S1-U2-58-63cm",
      "S1-U3-92-94cm",
      "S1-U4-103-108cm",
      "S1-U5-128-133cm",
      "S1-U6-142-146cm",
      "S1-U7-182-186cm",
      "S1-U8-208-213cm",
      "SD", "MOD1", "S5", "BR"
    ),
    
    labels = c(
      "S1-U2-58-63cm" = "S1-U2-58-63cm",
      "S1-U3-92-94cm" = "S1-U3-92-94cm",
      "S1-U4-103-108cm" = "S1-U4-103-108cm",
      "S1-U5-128-133cm" = "S1-U5-128-133cm",
      "S1-U6-142-146cm" = "S1-U6-142-146cm",
      "S1-U7-182-186cm" = "S1-U7-182-186cm",
      "S1-U8-208-213cm" = "S1-U8-208-213cm",

      "SD"   = "Relict sand dune",
      "MOD1" = "Modern river",
      "S5"   = "Modern beach",
      "BR"   = "Raised beach ridge"
    ))+
  
  
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "SITE 1",
    color = "Legend"
  ) +
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95),
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_text(face = "bold")
  )


#SITE2------------------------------------------------
gs_data %>%
  filter(site == "S2", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # LASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    "#1b9e77",  
    "#8b8378",  
    "#7570b3", 
    "#ff00ff",  
    "#66a61e",  
    "#e6ab02",  
    "#ee0000",  
    "#48494b",  
    "#1f78b4",  
    "#8b4726"   
  )) +
  
  #order of legend - remember to copy from dataset, need to be the exact same
  # scale_color_discrete(
  #   breaks = c("S2-U1-10-15cm",
  #              "S2-U1-25-30cm",
  #              "S2-U2-56-61cm",
  #              "S2-U3-72-77cm",
  #              "S2-U4-79-84cm",
  #              "S2-U5-88-93cm",
  #              "S2-U6-97-101cm",
  #              "S2-U7-105-110cm",
  #              "S2-U8-121-126cm",
  #              "S2-U8-135-140cm",
  #   )
  # )+
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "SITE 2",
    color = "Legend"
  ) +
  
  
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95)
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  )+
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_text(face = "bold")
  )
#S2 compare to modern -----
#modern


gs_data %>%
  filter(site == "S2", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # LASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  geom_line(linewidth = 1) +
  
  #modern
  geom_line(
    data = gs_modern,
    aes(x = phi, y = pct_finer, color = site, group = site),
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  
  scale_color_manual(values = c(
    "S2-U1-10-15cm"   = "#1b9e77",
    "S2-U1-25-30cm"   = "#8b8378",
    "S2-U2-56-61cm"   = "#7570b3",
    "S2-U3-72-77cm"   = "#ff00ff",
    "S2-U4-79-84cm"   = "#66a61e",
    "S2-U5-88-93cm"   = "#e6ab02",
    "S2-U6-97-101cm"  = "#ee0000",
    "S2-U7-105-110cm" = "#48494b",
    "S2-U8-121-126cm" = "#1f78b4",
    "S2-U8-135-140cm" = "#8b4726",
    "SD"   = "black",
    "MOD1" = "grey40",
    "S5"   = "grey60",
    "BR"   = "grey70"
  ),
  
  breaks = c("S2-U1-10-15cm",
             "S2-U1-25-30cm",
             "S2-U2-56-61cm",
             "S2-U3-72-77cm",
             "S2-U4-79-84cm",
             "S2-U5-88-93cm",
             "S2-U6-97-101cm",
             "S2-U7-105-110cm",
             "S2-U8-121-126cm",
             "S2-U8-135-140cm",
             "SD", "MOD1", "S5", "BR"
  ),
  
  labels = c(
    "S2-U1-10-15cm"   = "S2-U1-10-15cm",
    "S2-U1-25-30cm"   = "S2-U1-25-30cm",
    "S2-U2-56-61cm"   = "S2-U2-56-61cm",
    "S2-U3-72-77cm"   = "S2-U3-72-77cm",
    "S2-U4-79-84cm"   = "S2-U4-79-84cm",
    "S2-U5-88-93cm"   = "S2-U5-88-93cm",
    "S2-U6-97-101cm"  = "S2-U6-97-101cm",
    "S2-U7-105-110cm" = "S2-U7-105-110cm",
    "S2-U8-121-126cm" = "S2-U8-121-126cm",
    "S2-U8-135-140cm" = "S2-U8-135-140cm",
    
    "SD"   = "Relict sand dune",
    "MOD1" = "Modern river",
    "S5"   = "Modern beach",
    "BR"   = "Raised beach ridge")
  
  )+
  
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "SITE 2",
    color = "Legend"
  ) +
  
  
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95)
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_text(face = "bold")
  )



#SITE3-----------------------------------------------------------------
gs_data %>%
  filter(site == "S3", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # LASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    "#7570b3",  
    "#66a61e",  
    "#e6ab02",  
    "#ee0000",  
    "#48494b",  
    "#1f78b4",  
    "#8b4726"   
  ),
  
  
  #order of legend - remember to copy from dataset, need to be the exact same
  breaks = c(
    "S3.1-U1-30-33cm",
    "S3.1-U2-47-51cm",
    "S3.1-U2-60-65cm",
    "S3.2-U3-90-95cm",
    "S3.2-U3-135-140cm",
    "S3.2-U4-154-159cm",
    "S3.2-U5-165-170cm")
  )+
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "SITE 3",
    color = "Legend"
  ) +
  
  
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95)
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  ) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_text(face = "bold")
  )
#S3 comparte to modern-------
gs_data %>%
  filter(site == "S3", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # LASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  geom_line(linewidth = 1) +
  
  #modern
  geom_line(
    data = gs_modern,
    aes(x = phi, y = pct_finer, color = site, group = site),
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  
  scale_color_manual(values = c(
    "S3.1-U1-30-33cm"     = "#7570b3",
    "S3.1-U2-47-51cm"    = "#66a61e",
    "S3.1-U2-60-65cm"    = "#e6ab02",
    "S3.2-U3-90-95cm"    = "#ee0000",
    "S3.2-U3-135-140cm"  = "#48494b",
    "S3.2-U4-154-159cm"  = "#1f78b4",
    "S3.2-U5-165-170cm"  = "#8b4726",
    
    "SD"   = "black",
    "MOD1" = "grey40",
    "S5"   = "grey60",
    "BR"   = "grey70"
  ),
  
  
  breaks = c(
    "S3.1-U1-30-33cm",
    "S3.1-U2-47-51cm",
    "S3.1-U2-60-65cm",
    "S3.2-U3-90-95cm",
    "S3.2-U3-135-140cm",
    "S3.2-U4-154-159cm",
    "S3.2-U5-165-170cm",
    "SD", "MOD1", "S5", "BR"),
  
  labels = c(
    # SITE 3 labels
    "S3.1-U1-30-33cm"     = "S3.1-U1-30-33cm",
    "S3.1-U2-47-51cm"    = "S3.1-U2-47-51cm",
    "S3.1-U2-60-65cm"    = "S3.1-U2-60-65cm",
    "S3.2-U3-90-95cm"    = "S3.2-U3-90-95cm",
    "S3.2-U3-135-140cm"  = "S3.2-U3-135-140cm",
    "S3.2-U4-154-159cm"  = "S3.2-U4-154-159cm",
    "S3.2-U5-165-170cm"  = "S3.2-U5-165-170cm",
    
    "SD"   = "Relict sand dune",
    "MOD1" = "Modern river",
    "S5"   = "Modern beach",
    "BR"   = "Raised beach ridge"
  )
  
  )+
  
  
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "SITE 3",
    color = "Legend"
  ) +
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95)
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  )+
  theme(
    text = element_text(family = "Times New Roman"),
    legend.title = element_text(face = "bold")
  )


#S5 -------------------------------------------------------------
p_S5 <- gs_data %>%
  filter(site == "S5", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # LASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  geom_line(linewidth = 1) +
  
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "Modern Beach",
    color = "Sample name"
  ) +
  
  
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95)
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  )+
  theme(legend.position = "none") +
  theme(
    text = element_text(family = "Times New Roman")
  )

#SD -------------------------------------------------------------
p_SD <- gs_data %>%
  filter(site == "SD", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # LASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  geom_line(linewidth = 1) +
  
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "Relict Sand Dune",
    color = "Sample name"
  ) +
  
  
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95)
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  )+
  theme(legend.position = "none") +
  theme(
    text = element_text(family = "Times New Roman")
  )
#mod1 -------------------------------------------------------------
p_MOD1 <- gs_data %>%
  filter(site == "MOD1", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # LASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  geom_line(linewidth = 1) +
  
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "Modern River",
    color = "Sample name"
  ) +
  
  
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95)
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  )+
  theme(legend.position = "none") +
  theme(
    text = element_text(family = "Times New Roman")
  )
#BR -------------------------------------------------------------
p_BR<- gs_data %>%
  filter(site == "BR", sieve_mm > 0) %>%
  mutate(phi = -log2(sieve_mm)) %>%
  ggplot(aes(x = phi, y = pct_finer, color = sample_id, group = sample_id)) +
  
  # LASS BOUNDARY 
  geom_vline(
    xintercept = class_bounds_phi,
    color = "grey70",
    linewidth = 0.4,
    linetype = "solid"
  ) +
  
  geom_line(linewidth = 1) +
  
  scale_x_continuous(
    breaks = phi_breaks,
    labels = mm_breaks,
    name   = "Grain size (mm)",
    expand = c(0, 0),
    limits = range(phi_breaks),
    
    sec.axis = dup_axis(
      name   = NULL,
      breaks = class_mids_phi,
      labels = class_labels
    )
  ) +
  
  scale_y_continuous(
    name = "Cumulative Percent Finer by Weight (%)",
    sec.axis = sec_axis(~ 100 - ., name = "Cumulative Percent Coarser by Weight (%)")
  ) +
  
  labs(
    title = "Beach Ridge",
    color = "Sample name"
  ) +
  
  
  
  
  theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5),
    axis.title.x.top = element_text(margin = margin(b = 8)),
    axis.text.x.top  = element_text(size = 9, lineheight = 0.95)
  ) +
  theme(
    axis.text.x.bottom = element_text(angle = 30, hjust = 1),
    axis.text.x.top    = element_text(angle = 0)   # keep top labels horizontal
  ) +
  theme(legend.position = "none") +
  theme(
    text = element_text(family = "Times New Roman")
  )
#plot modern against each other --------------------
(p_S5 ) /
  (p_MOD1) /
  (p_SD)

