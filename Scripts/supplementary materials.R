################################################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# Script to make figures, tables, etc for supplementary materials.
################################################################################################

library(brms); library(dplyr); library(knitr); library(kableExtra); library(sjPlot); library(tidybayes)
################################################################################################
###load in models/data

#int only mode for cngv
int_mod <- readRDS("~/CnGV-CoGV Meta-analysis/Data/model output/mod_norm_logtrans_trait_2randeff_student_sp.rds")

#int only mode for cogv
int_mod_co <- readRDS("~/CnGV-CoGV Meta-analysis/Data/model output/mod_norm_logtrans_trait_2randeff_student_co.rds")

#metaregression mod for cngv
metareg_mod

#model data

mod_dat <- read.csv("~/CnGV-CoGV Meta-analysis/Data/raw_data.csv")

################################################################################################
### make tables for model summary
int_mod_posts <- exp(posterior_samples(int_mod))

# int only model cngv
int_mod_summary <- data.frame(posterior_summary(int_mod_posts[1:4]))

int_mod_summary <- round(int_mod_summary, 2)


int_mod_summary %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_summary.pdf")




# int only model cogv

int_mod_co_posts <- exp(posterior_samples(mod_2randeff_co))

# int only model cngv
int_mod_co_summary <- data.frame(posterior_summary(int_mod_co_posts[1:4]))

int_mod_co_summary <- round(int_mod_co_summary, 2)


int_mod_co_summary %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_co_summary.pdf")

# metaregression summary


################################################################################################

### posterior predictive check for models

# intercept only model for countergradient variation

int_mod_ppcheck <- pp_check(int_mod)

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_ppcheck.pdf", plot = int_mod_ppcheck)


#intercept only model for cogradient variation

int_mod_co_ppcheck <- pp_check(int_mod_co)

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_co_ppcheck.pdf", plot = int_mod_co_ppcheck)

# metaregression model for countergradient variation

################################################################################################

###### priors for cngv int mod
int_mod_priors <- data.frame(int_mod$prior)

int_mod_priors[4:9,1] <- "cauchy(0, 2)"

int_mod_priors[,c(1:4,9)]%>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_priors.pdf")



##### priors for cngv int mod
int_mod_priors_co <- data.frame(int_mod_co$prior)

int_mod_priors_co[4:9,1] <- "cauchy(0, 2)"

int_mod_priors_co[,c(1:4,9)]%>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_co_priors.pdf")






################################################################################################

# for pseudo phylogenies see make "phylo tree.R" file

################################################################################################

# for forestplots for intercept models for co and counter with all papers see "random effect models plots.R" file




