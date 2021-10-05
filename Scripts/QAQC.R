########################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# This script checks to see if papers in the meta data match papers in the
# raw data and vice versa.
########################################################################

library(dplyr); library(ggplot2); library(sjPlot)

########################################################################


raw_data <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/raw_data.csv")

meta_data <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/meta_data.csv")

raw_authors <- unique(as.character(raw_data$Paper..Authors...Year.))

meta_authors <- unique(as.character(meta_data$X))

### data check

#check authors list in raw authors vs meta authors
check <- NULL
temporary <- NULL

for (a in raw_authors) {
  temporary <- c(a, a %in% meta_authors)
  check <- rbind(check, temporary)
  
}
check

#check authors list in meta authors vs raw authors
check <- NULL
temporary <- NULL

for (a in meta_authors) {
  temporary <- c(a, a %in% raw_authors)
  check <- rbind(check, temporary)
  
}
check


########################################################################

# this is to assess if number of times a factor is repeated is related to effect
# size (it does not look like it, even log transformed dependent and independent
# variables have an R^2 of ~ 0.1 and are non-significant)

samp_size_dat <- read.csv("~/CnGV-CoGV-Meta-Analysis/Data/cngv_model_data.csv")


trait_dat <- samp_size_dat %>%
  group_by(factor = alt_trait) %>% 
  summarise(number = length(alt_trait), mean = mean(abs(mean_ES), variance = var(abs(mean_ES))))

grad_dat <- samp_size_dat %>%
  group_by(factor = Gradient) %>% 
  summarise(number = length(Gradient), mean = mean(abs(mean_ES)), variance = var(abs(mean_ES)))

class_dat <- samp_size_dat %>%
  group_by(factor = Class) %>% 
  summarise(number = length(Class), mean = mean(abs(mean_ES)), variance = var(abs(mean_ES)))

all_dat <- bind_rows(trait_dat, grad_dat, class_dat)

untran_mod <- summary(lm(mean ~ number, data = all_dat))

tab_model(untran_mod)

plot1 <- ggplot(all_dat, aes(x = number, y = mean)) +
  geom_point() +
  labs(x = "Number of studies", y = "Average Effect Size") +
  theme_classic(base_size = 14)

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/numstudy_effectcheck.pdf", plot1)




