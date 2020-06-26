
source("R/config.R")

library(readr)
library(ggplot2)
library(dplyr)

chart_labels_file <- "R/chart_labels.csv"
chart_settings_file <- "R/chart_settings.csv"
cohort_country_file <-  "R/cohort_country.csv"
cohort_language_file <- "R/cohort_language.csv"

# read and filter data by cohort
all_data <- read_csv(cfg_data_file, col_types = "ccnnnnnnnnnnn")
cohort_data <- all_data %>% filter(hs_cohort == cfg_cohort)

# apply child filter if there is one
if(!is.null(cfg_child_filter)) cohort_data <- cohort_data %>% filter(HelixID %in% cfg_child_filter)

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

# loop through charts
c <- 1




# make chart data
# one column for each exposure
# one row for each country plus one for all

# make single plot


# test code

ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=hs_total_fruits, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_fruit), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 25) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "hs_total_fruits"], linetype = "Your intake"), color="red", size=1) + # add line for participant's intake
  labs(title = "Fruit portions/week", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_fruit.pdf", width=8, height=5, units=("in"), dpi=300)

