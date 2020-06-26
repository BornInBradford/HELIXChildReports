
source("R/config.R")

library(readr)
library(ggplot2)
library(dplyr)

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
all_data <- read_csv(cfg_data_file, col_types = "ccnnnnnnnnnnn")

# add country information
all_data <- all_data %>% left_join(cohort_country)

# make change to sedentary hours
all_data <- all_data %>% mutate(sed_hr = hs_sd_wk/60)

# get cohort children
cohort_children <- all_data %>% filter(hs_cohort == cfg_cohort) %>% select(HelixID)
# apply child filter if there is one
if(!is.null(cfg_child_filter)) cohort_children <- cohort_children %>% filter(HelixID %in% cfg_child_filter)


# make chart data
# one column for each exposure
# one row for each country plus one for all
all_data_summary <- all_data %>% 
  group_by(country) %>% 
  summarise_at(chart_settings$var, mean, na.rm = TRUE) %>%
  ungroup()
marginal_means <- all_data_summary %>% 
  summarise_at(chart_settings$var, mean, na.rm = TRUE) %>%
  mutate(country = "All")
all_data_summary <- bind_rows(all_data_summary, marginal_means)

# loop through charts and children
for(c in 1:nrow(chart_settings)) {
  for(h in 1:nrow(cohort_children)) {

    # make single plot
    g <- ggplot(all_data_summary) + 
      geom_bar(aes_string(x="country", y=chart_settings$var[c], fill="country"), 
               position = "dodge", stat = "summary", show.legend = FALSE) +
      scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
      scale_y_continuous(limits = c(chart_settings$y_lower[c], chart_settings$y_upper[c])) +
      geom_hline(aes(yintercept=pull(all_data[all_data$HelixID == cohort_children$HelixID[h], chart_settings$var[c]]), linetype = chart_settings$linetype[c]), color="red", size=1) +
      labs(title = chart_settings$title[c], x="", y="") + 
      theme(panel.background=element_blank(),
            legend.title = element_blank(),
            legend.key = element_rect(fill = NA))
    # output png
    ggsave(filename = paste0(cfg_output_path, cohort_children$HelixID[h], "_", chart_settings$name[c], "_barplot.png"),
           plot = g, 
           device = "png",
           width=8, 
           height=5, 
           units=("in"), 
           dpi=300)

  }
}


