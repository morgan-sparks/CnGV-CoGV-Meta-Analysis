#library(dplyr)
library(brms); library(ape)
setwd("/scratch/bell/sparks35/CNGV_analysis/output")

### load in data
#model data
#metaregression_data <-  read.csv("/scratch/bell/sparks35/CNGV_analysis/data/cngv_model_data.csv")
metaregression_data <- read.csv("~/Dropbox/PhD Work/Critical Review/Data/cngv_model_data.csv")

#correlation matrix for phylogenetic random effect
#vcv_mat <- as.matrix(read.csv("/scratch/bell/sparks35/CNGV_analysis/data/cngv_vcv_randeff_mat.csv", row.names = 1, sep = ","))
vcv_mat <- as.matrix(read.csv("~/Dropbox/PhD Work/Critical Review/Data/cngv_vcv_randeff_mat.csv", row.names = 1, sep = ","))
#remove spaces in names and replace with "_"
rownames(vcv_mat) <- gsub(" ", "_", rownames(vcv_mat))
colnames(vcv_mat) <- rownames(vcv_mat)

# fix a NCBI naming quirk "Eilema depressum to Eleima depressa
rownames(vcv_mat)[which(rownames(vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"
colnames(vcv_mat)[which(colnames(vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"

metaregression_data$Species <- gsub(" ", "_", metaregression_data$Species)

# change Hylya cinerea to updated classification (which it is in vcv_mat from NCBI)
metaregression_data[which(metaregression_data$Species == "Hyla_cinerea"), "Species"] <- "Dryophytes_cinereus"


#turn year into continuous variable and species into factor
metaregression_data$Year <- as.numeric(as.character(metaregression_data$Year))
metaregression_data$Species <- as.factor(metaregression_data$Species)

#abs val of effect size, only care about magnitude of effect
metaregression_data$mean_ES <- abs(metaregression_data$mean_ES)
droplevels(metaregression_data$Species) # drop old Hyla level

### recode Paper.Name into study number to help some downstream analyses (R is struggling with some 
# names with non-standard English punctuation)

paper_number <- as.integer(as.factor(metaregression_data$Paper.Name))

metaregression_data <- cbind(paper_number, metaregression_data)


#mcmc variables
draws <- 22500
burn <-7500
cores <- 4
cntrl_vars <- list(adapt_delta = 0.995, max_treedepth = 20)

strong_priors <-  c(prior(normal(0, 2),  "b"),
                              prior(cauchy(0, 2),  "sd"))

# metaregression model w/ strong priors
mod_metareg_sp <- brm(
  log(mean_ES) | se(var_ES/sqrt(samp.size_ES))  ~ 0 + alt_trait + Gradient + Year + Class + # fixed effects
    (1|paper_number/Trait) + (1 | gr(Species, cov = vcv_mat)), #random effects
  data = metaregression_data, #data for full model
  data2 = list(vcv_mat = vcv_mat),
  family = student(),
  thin = 10,
  iter = draws,
  warmup = burn,
  cores = cores,
  prior = strong_priors,
  control = cntrl_vars)

summary(mod_metareg_sp)

saveRDS(mod_metareg_sp, "/scratch/bell/sparks35/CNGV_analysis/output/mod_metareg_sp.RDS")

sessionInfo()


# metaregression w/ weak priors

# short_priors <- prior_c <- c(set_prior("normal(0, 1)", class = "Intercept"),
#                              set_prior("cauchy(0, 5)", class = "sd"))
# 
# mod_metareg_weakpriors <- brm(
#   log(temp.mn) | se(std_err)  ~ Species + alt_trait + Gradient + # fixed effects
#     (1|Paper.Name/Trait) + (1 | gr(Species, cov = vcv_mat)), #random effects
#   data = metaregression_data, #data for full model
#   data2 = list(vcv_mat = vcv_mat),
#   prior = short_priors,
#   family = gaussian(),
#   iter = draws,
#   warmup = burn,
#   cores = cores,
#   control = cntrl_vars)
# 
# summary(mod_metareg_weakpriors)
# 
# saveRDS(mod_metareg_weakpriors, "mod_metareg_weakpriors.RDS")



