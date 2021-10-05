################################################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# Script to make figures, tables, etc for supplementary materials.
################################################################################################

library(brms); library(dplyr); library(knitr); library(kableExtra); library(sjPlot); library(tidybayes); library(bayesplot); library(tidybayes); library(patchwork)
################################################################################################
###load in models/data

#int only mode for cngv
int_mod <- readRDS("~/CnGV-CoGV-Meta-analysis/Data/model_output/mod_norm_logtrans_trait_2randeff_student_sp.rds")
get_variables(int_mod)

#int only mode for cogv
int_mod_co <- readRDS("~/CnGV-CoGV-Meta-analysis/Data/model_output/mod_norm_logtrans_trait_2randeff_student_co.rds")

#metaregression mod for cngv
metareg_mod <- ("~/CnGV-CoGV-Meta-Analysis/Data/model_output/mod_metareg_noyear_sp.RDS")

#model data

mod_dat <- read.csv("~/CnGV-CoGV-Meta-analysis/Data/raw_data.csv")

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

int_mod_co_posts <- exp(posterior_samples(int_mod_co))

# int only model cngv
int_mod_co_summary <- data.frame(posterior_summary(int_mod_co_posts[1:4]))

int_mod_co_summary <- round(int_mod_co_summary, 2)


int_mod_co_summary %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_co_summary.pdf")

# metaregression summary

################################################################################################
# posterior distributions and chain mixing

cngv_chains <- mcmc_trace(int_mod, facet_args = list(ncol = 1, strip.position = "left"))

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_chainmixing.png", 
       cngv_chains, dpi = 300)

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
# table for Stan control variables

mod_ctrlvars <- matrix(nrow = 3, ncol = 7)
colnames(mod_ctrlvars) <- c("Model", "Iterations", "Burn-in", "Cores", "Thin", "adapt_delta", "max_treedepth")
mod_ctrlvars[1,] <- c("Countergradient Random Effects", 20000, 7500, 4, 1, 0.995, 20)
mod_ctrlvars[2,] <- c("Cogradient Random Effects", 10000, 2500, 4, 1, 0.99, 18)
mod_ctrlvars[3,] <- c("Countergradient Metaregression", 22500, 7500, 4, 10, 0.995, 20)
mod_ctrlvars <- data.frame(mod_ctrlvars)

mod_ctrlvars %>% 
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/mod_cntrlvars.pdf")

################################################################################################
# make trace plots for each variable

#for int_mod
get_variables(int_mod)
int_mod_tracevars <- get_variables(int_mod)[1:4]

# for int_mod_co
get_variables(int_mod_co)
int_mod_co_tracevars <- get_variables(int_mod_co)[1:4]

int_mod_traceplot <- mcmc_trace(int_mod, pars = int_mod_tracevars, facet_args = list(ncol = 1, strip.position = "left")) + ggtitle(label = "a)") + theme(legend.position = 'none')
int_mod_co_traceplot <- mcmc_trace(int_mod_co, pars = int_mod_co_tracevars, facet_args = list(ncol = 1, strip.position = "left")) + ggtitle(label = "b)") +labs(y = NULL) 

int_mods_traceplots <- (int_mod_traceplot + int_mod_co_traceplot) 

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mods_traceplots.png", int_mods_traceplots, height = 11, width = 8, units = "in", dpi = 300)

################################################################################################

### posterior predictive check for models

# intercept only model for countergradient variation

int_mod_ppcheck <- pp_check(int_mod) + ggtitle(label = "a)")


#intercept only model for cogradient variation

int_mod_co_ppcheck <- pp_check(int_mod_co) + ggtitle(label = "b)")

#combine two
int_mods_ppchecks <- int_mod_ppcheck + int_mod_co_ppcheck

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mods_ppchecks.png", plot = int_mods_ppchecks)

# metaregression model for countergradient variation



################################################################################################

# for pseudo phylogenies see make "phylo tree.R" file

################################################################################################

# for forestplots for intercept models for co and counter with all papers see "random effect models plots.R" file




