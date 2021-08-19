#library(dplyr)
library(brms)
metaregression_data <- read.csv("~/scratch/snyder/s/sparks35/CNGV_analysis/data/trait_level_data.csv")

vcv_mat <- as.matrix(read.csv("~/scratch/snyder/s/sparks35/CNGV_analysis/data/vcv_randeff_mat.csv", row.names = 1, sep = ","))

#remove spaces in names

rownames(vcv_mat) <- gsub(" ", "_", rownames(vcv_mat))
colnames(vcv_mat) <- rownames(vcv_mat)

metaregression_data$Species <- gsub(" ", "_", metaregression_data$Species)
                     
# counts of factor levels 
# metaregression_data %>%
#   group_by(alt_trait) %>%
#   summarise(counts = length(alt_trait))
#   
# metaregression_data %>%
#   group_by(Gradient) %>%
#   summarise(counts = length(Gradient))
# 
# metaregression_data %>%
#   group_by(Experiment.type..CG.or.RT.) %>%
#   summarise(counts = length(Experiment.type..CG.or.RT.))
# 
# metaregression_data %>%
#   group_by(Species) %>%
#   summarise(counts = length(Species))
# 


#mcmc variables
draws <- 30000
burn <-5000
cores <- 4
cntrl_vars <- list(adapt_delta = 0.92, max_treedepth = 18)


# metaregression model
mod_metareg <- brm(
  log(temp.mn) | se(std_err)  ~ Species + alt_trait + Gradient + # fixed effects
    (1|Paper.Name/Trait) + (1 | gr(Species, cov = vcv_mat)), #random effects
  data = metaregression_data, #data for full model
  data2 = list(vcv_mat = vcv_mat),
  family = gaussian(),
  iter = draws,
  warmup = burn,
  cores = cores,
  control = cntrl_vars)

summary(mod_metareg)

saveRDS(mod_metareg, "mod_metareg.RDS")

# metaregression w/ weak priors

short_priors <- prior_c <- c(set_prior("normal(0, 1)", class = "Intercept"),
                             set_prior("cauchy(0, 5)", class = "sd"))

mod_metareg_weakpriors <- brm(
  log(temp.mn) | se(std_err)  ~ Species + alt_trait + Gradient + # fixed effects
    (1|Paper.Name/Trait) + (1 | gr(Species, cov = vcv_mat)), #random effects
  data = metaregression_data, #data for full model
  data2 = list(vcv_mat = vcv_mat),
  prior = short_priors,
  family = gaussian(),
  iter = draws,
  warmup = burn,
  cores = cores,
  control = cntrl_vars)

summary(mod_metareg_weakpriors)

saveRDS(mod_metareg_weakpriors, "mod_metareg_weakpriors.RDS")





