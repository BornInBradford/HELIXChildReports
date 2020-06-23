options(scipen=100)
options(digits=4)
library(ggplot2)

setwd("U:/Born In Bradford/ATHLETE/WP1_Subcohort_Planning/Data for engagement")
dat <- read.csv("U:/Born In Bradford/ATHLETE/WP1_Subcohort_Planning/Data for engagement/athlete_maribel_serena_20200519.csv")

#somehow classed as factor - make numeric
dat$hs_no2_yr_hcp2_h <- as.numeric(dat$hs_no2_yr_hcp2_h)
dat$hs_ndvi100_h <- as.numeric(dat$hs_ndvi100_h)

# put into hours instead of minutes
dat$sed_hr <- dat$hs_sd_wk/60

# just BiB
bib <- dat[dat$hs_cohort=="BIB",]

# change cohort names to country names
levels(dat$hs_cohort) <- list("United Kingdom"="BIB", "France"="EDEN", "Spain"="INMA",
                              "Lithuania"="KANC", "Norway"="MOBA", "Crete"="RHEA")

# create a 'helix' cohort
dat$helix[!is.na(dat$hs_cohort)] <- "All children"

# create variable which gives average of all HELIX cohorts for each exposure variable
dat$helix_fruit <- mean(dat$hs_total_fruits, na.rm=T)
dat$helix_veg <- mean(dat$hs_total_veg, na.rm=T)
dat$helix_bev <- mean(dat$hs_beverages, na.rm=T)
dat$helix_sweets <- mean(dat$hs_total_sweets, na.rm=T)
dat$helix_bakery <- mean(dat$hs_bakery_prod, na.rm=T)
dat$helix_no2 <- mean(dat$hs_no2_yr_hcp2_h, na.rm=T)
dat$helix_ndvi <- mean(dat$hs_ndvi100_h, na.rm=T)
dat$helix_mvpa <- mean(dat$hs_mvpa_alt, na.rm=T)
dat$helix_sed <- mean(dat$sed_hr, na.rm=T)
dat$helix_sleep_wd <- mean(dat$dif_hours_wd, na.rm=T)
dat$helix_sleep_we <- mean(dat$dif_hours_we, na.rm=T)

###### FRUIT
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


###### VEG
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=hs_total_veg, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_veg), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 25) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "hs_total_veg"], linetype = "Your intake"), color="red", size=1) + # add line for participant's intake
  labs(title = "Vegetable portions/week", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_veg.pdf", width=8, height=5, units=("in"), dpi=300)


###### Fizzy beverages
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=hs_beverages, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_bev), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 25) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "hs_beverages"], linetype = "Your intake"), color="red", size=1) + # add line for participant's intake
  labs(title = "Fizzy drinks/week", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_bev.pdf", width=8, height=5, units=("in"), dpi=300)


###### Sweets
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=hs_total_sweets, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_sweets), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 25) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "hs_total_sweets"], linetype = "Your intake"), color="red", size=1) + # add line for participant's intake
  labs(title = "Sweets/week", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_sweets.pdf", width=8, height=5, units=("in"), dpi=300)


###### Baked goods
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=hs_bakery_prod, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_bakery), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 25) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "hs_bakery_prod"], linetype = "Your intake"), color="red", size=1) + # add line for participant's intake
  labs(title = "Baked goods items/week", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_bakery.pdf", width=8, height=5, units=("in"), dpi=300)


###### NO2
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=hs_no2_yr_hcp2_h, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_no2), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 200) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "hs_no2_yr_hcp2_h"], linetype = "Your exposure"), color="red", size=1) + # add line for participant's intake
  labs(title = "Nitrogen dioxide (NO2) exposure", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_no2.pdf", width=8, height=5, units=("in"), dpi=300)


###### NDVI100
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=hs_ndvi100_h, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_ndvi), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 1500) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "hs_ndvi100_h"], linetype = "Your exposure"), color="red", size=1) + # add line for participant's intake
  labs(title = "Amount of green space within 100m of your home", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_ndvi.pdf", width=8, height=5, units=("in"), dpi=300)


###### Physical activity
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=hs_mvpa_alt, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_mvpa), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 100) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "hs_mvpa_alt"], linetype = "Your level"), color="red", size=1) + # add line for participant's intake
  labs(title = "Minutes/day of moderate-to-vigorous physical activity", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_mvpa.pdf", width=8, height=5, units=("in"), dpi=300)


###### Sedentary activity
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=sed_hr, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_sed), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 10) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "sed_hr"], linetype = "Your level"), color="red", size=1) + # add line for participant's intake
  labs(title = "Hours/day of sedentary activity", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_sed.pdf", width=8, height=5, units=("in"), dpi=300)


###### Sleep duration weekdays
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=dif_hours_wd, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_sleep_wd), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 15) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "dif_hours_wd"], linetype = "Your level"), color="red", size=1) + # add line for participant's intake
  labs(title = "Hours/day of sleep during weekdays", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_sleep_weekdays.pdf", width=8, height=5, units=("in"), dpi=300)


###### Sleep duration weekend
ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=dif_hours_we, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  geom_bar(aes(x=helix, y=helix_sleep_we), position="dodge", stat="summary", show.legend=FALSE)+
  ylim(0, 15) +
  scale_fill_manual(values=c("chartreuse4", "pink1", "pink1", "pink1", "pink1", "pink1", "pink1")) +
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "dif_hours_we"], linetype = "Your level"), color="red", size=1) + # add line for participant's intake
  labs(title = "Hours/day of sleep during weekends", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA)) 
ggsave("TEST_sleep_weekends.pdf", width=8, height=5, units=("in"), dpi=300)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
tmp <- as.data.frame(dat[dat$HelixID=="BIB10041","hs_total_fruits"])


# n, mean (SD) by cohort
aggregate(dat$hs_no2_yr_hcp2_h~dat$hs_cohort, FUN=function(x) c(n = length(na.omit(x)), mean = mean(x), sd = sd(x)))
aggregate(dat$hs_total_fruits~dat$hs_cohort, FUN=function(x) c(n = length(na.omit(x)), mean = mean(x), sd = sd(x)))
aggregate(dat$hs_mvpa_alt~dat$hs_cohort, FUN=function(x) c(n = length(na.omit(x)), mean = mean(x), max = max(x)))


# entire BiB - fruit - uSing ID 10041  
ggplot(bib, aes(x=hs_total_fruits)) + 
  geom_density() +
  geom_vline(aes(xintercept=bib[bib$HelixID=="BIB10041", "hs_total_fruits"]), color="blue", linetype="dashed", size=1) +
  labs(title = "BIB - fruit", x="Fruit portions/week", y="") + 
  theme(legend.position="none",
        panel.background=element_blank(),
        axis.text.y = element_blank()) 
ggsave("TEST_BiB_fruit.pdf", width=3, height=3, units=("in"), dpi=300)



# entire HELIX - fruit
ggplot(dat, aes(x=hs_total_fruits, color=hs_cohort)) + 
  geom_density() +
  #geom_vline(aes(xintercept=mean(dat$hs_total_fruits)), color="blue", linetype="dashed", size=1) +
  labs(title = "HELIX - fruit", x="Fruit portions/week", y="") + 
  theme(panel.background=element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right",
        legend.title = element_blank()) +
  annotate("segment", x=11.3, xend=11.3, y=0, yend=0.1, color="red", lwd=1)
ggsave("TEST_HELIX_fruit.pdf", width=3, height=3, units=("in"), dpi=300)





ggplot(dat) + 
  geom_bar(aes(x=hs_cohort, y=hs_total_fruits, fill=hs_cohort), 
           position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE) + 
  ylim(0, 25) +
  scale_fill_brewer(palette="Dark2") +
  geom_hline(aes(yintercept=dat$helix_fruit, linetype = "HELIX average"), color="red", size=2) + #adds red dashed bar for HELIX mean
  geom_hline(aes(yintercept=bib[bib$HelixID=="BIB10041", "hs_total_fruits"], linetype = "Your intake"), color="blue", size=1) + # add line for participant's intake
  labs(title = "Fruit portions/week", x="", y="") + 
  theme(panel.background=element_blank(),
        legend.title = element_blank())
ggsave("TEST_HELIX_fruit.pdf", width=5, height=4, units=("in"), dpi=300)
