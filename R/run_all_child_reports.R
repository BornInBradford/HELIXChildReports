
source("R/config.R")

library(readr)
library(ggplot2)
library(dplyr)
library(cowplot)

chart_labels_file <- "R/chart_labels.csv"
chart_settings_file <- "R/chart_settings.csv"
cohort_country_file <-  "R/cohort_country.csv"
cohort_language_file <- "R/cohort_language.csv"

# read chart settings with cohort language filter
chart_settings <- read_csv(chart_settings_file)
cohort_language <- read_csv(cohort_language_file)
chart_labels <- read_csv(chart_labels_file)
cohort_country <- read_csv(cohort_country_file)

# get cohort language and filter labels and country information
output_language <- cohort_language$lan[cohort_language$hs_cohort == cfg_cohort]
chart_labels <- chart_labels %>% filter(lan == output_language)
cohort_country <- cohort_country %>% filter(lan == output_language)

# merge chart settings with language
chart_settings <- chart_settings %>% inner_join(chart_labels)

# read and filter data by cohort
all_data <- read_csv(cfg_data_file)#, col_types = "ccnnnnnnnnnnn")

# add country information
all_data <- all_data %>% left_join(cohort_country)

# make change to sedentary hours
all_data <- all_data %>% mutate(sed_hr = hs_sd_wk/60)
# make change to TV/video in week tv_wd
all_data <- all_data %>% mutate(tv_wd = hs_tv_wdt/60)
# make change to TV/video in weekend hours
all_data <- all_data %>% mutate(tv_we = hs_tv_wet/60)
# make change to inactive computer/video games in week hours
all_data <- all_data %>% mutate(vg_wd = hs_comp_wdt/60)
# make change to inactive computer/video games in weekend hours
all_data <- all_data %>% mutate(vg_we = hs_comp_wet/60)
# sum tv/video + computer/video in week
all_data <- all_data %>% rowwise() %>% mutate(tv_vg_wkd = sum(c(tv_wd, vg_wd, na.rm=T)))
# sum tv/video + computer/video in week
all_data <- all_data %>% rowwise() %>% mutate(tv_vg_wed = sum(c(tv_we, vg_we, na.rm=T)))
# change class
all_data$no2 <- as.numeric(all_data$hs_no2_yr_hs_h)
all_data$ndvi100 <- as.numeric(all_data$hs_ndvi100_h)

# get cohort children
cohort_children <- all_data %>% filter(hs_cohort == cfg_cohort) %>% select(HelixID)
# apply child filter if there is one
if(!is.null(cfg_child_filter)) cohort_children <- cohort_children %>% filter(HelixID %in% cfg_child_filter)

########## losing tv_vg_wkd, tv_vg_wed, no2 below

# make chart data
# one column for each exposure
# one row for each country plus one for all
all_data_summary <- all_data %>% 
  group_by(country) %>% 
  summarise_at(chart_settings$var, mean, na.rm = TRUE) %>%
  ungroup()
# marginal_means <- all_data_summary %>% 
#   summarise_at(chart_settings$var, mean, na.rm = TRUE) %>%
#   mutate(country = "All")
#all_data_summary <- bind_rows(all_data_summary, marginal_means)

output_plot <- list()

# loop through charts and children
for(h in 1:nrow(cohort_children)) {
  
  child_output_path <- file.path(cfg_output_path, paste0(cfg_cohort, "_", cohort_children$HelixID[h]))
  dir.create(child_output_path, showWarnings = FALSE)
  
  message(paste0("Child #", h, ": ", cohort_children$HelixID[h]))
  
  for(c in 1:nrow(chart_settings)) {
 
    hline <- pull(all_data[all_data$HelixID == cohort_children$HelixID[h], chart_settings$var[c]])
    
    g <- ggplot(all_data_summary) + 
      geom_bar(aes_string(x="country", y=chart_settings$var[c], fill="country"), 
               position = "dodge", stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values=c("cyan3", "cyan3", "cyan3", "cyan3", "cyan3", "darkorange")) +
      scale_y_continuous() +
      geom_hline(aes_string(yintercept=hline), color="red", size=1) +
      labs(title = "", x="", y="") + 
      theme(plot.margin = margin(t=chart_settings$top[c], r=chart_settings$right[c], b=chart_settings$bottom[c], chart_settings$left[c]),
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            legend.background = element_rect(fill = "transparent"), 
            legend.box.background = element_rect(fill = "transparent"),
            axis.text=element_text(size=chart_settings$textsize[c]))
    
    
    output_plot[[chart_settings$name[c]]] <- g
  }
  
  # output pngs
  
  for(o in which(!is.na(chart_settings$grid1))) {
    
    # do we need 1 on its own or 2 side by side?  
    if(is.na(chart_settings$grid2[o])) {
      
      png_filename <- file.path(child_output_path, paste0(cohort_children$HelixID[h], "_", chart_settings$grid1[o], "_barplot.png"))
      g = output_plot[[chart_settings$grid1[o]]]
      
    } else {
      
      png_filename <- file.path(child_output_path, paste0(cohort_children$HelixID[h], "_", chart_settings$grid1[o], "_", chart_settings$grid2[o], "_barplot.png"))
      g <- plot_grid(output_plot[[chart_settings$grid1[o]]], output_plot[[chart_settings$grid2[o]]])
      
    }
    
    ggsave(filename = png_filename,
           plot = g,
           device = "png",
           width=chart_settings$width[o],
           height=chart_settings$height[o],
           units=("mm"),
           dpi=300,
           bg = "transparent")
    
    
  }
  
}


