
source("R/config.R")

library(readr)
library(ggplot2)
library(dplyr)

all_data <- read_csv(cfg_data_file, col_types = "ccnnnnnnnnnnn")

cohort_data <- all_data %>% filter(hs_cohort == cfg_cohort)

if(!is.null(cfg_child_filter)) cohort_data <- cohort_data %>% filter(HelixID %in% cfg_child_filter)

chart_labels <- read_csv(cfg_labels_file) %>% filter(lan == cfg_language)
cohort_country <- read_csv(cfg_cohorts_file) %>% filter(lan == cfg_language)

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

