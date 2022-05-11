library(brms); library(ape)
setwd("/scratch/bell/sparks35/CNGV_analysis/output")

### load in data
#model data
real_model_data_trait <-  read.csv("/scratch/bell/sparks35/CNGV_analysis/data/cngv_model_data_allES.csv")

#correlation matrix for phylogenetic random effect
vcv_mat <- as.matrix(read.csv("/scratch/bell/sparks35/CNGV_analysis/data/cngv_vcv_randeff_mat.csv", row.names = 1, sep = ","))

#remove spaces in names and replace with "_"
rownames(vcv_mat) <- gsub(" ", "_", rownames(vcv_mat))
colnames(vcv_mat) <- rownames(vcv_mat)

# fix a NCBI naming quirk "Eilema depressum to Eleima depressa
rownames(vcv_mat)[which(rownames(vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"
colnames(vcv_mat)[which(colnames(vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"

real_model_data_trait$Species <- gsub(" ", "_", real_model_data_trait$Species)

# change Hylya cinerea to updated classification (which it is in vcv_mat from NCBI)
real_model_data_trait[which(real_model_data_trait$Species == "Hyla_cinerea"), "Species"] <- "Dryophytes_cinereus"

# change Idotea_balthica to updated classification (which it is in vcv_mat from NCBI)
real_model_data_trait[which(real_model_data_trait$Species == "Idotea_balthica"), "Species"] <- "Idotea_baltica"

#turn year into continuous variable and species into factor
real_model_data_trait$Year <- as.numeric(as.character(real_model_data_trait$Year))
real_model_data_trait$Species <- as.factor(real_model_data_trait$Species)
droplevels(real_model_data_trait$Species)

#abs val of effect size, only care about magnitude of effect
real_model_data_trait$mean_ES <- abs(real_model_data_trait$Value)


### recode Paper.Name into study number to help some downstream analyses (R is struggling with some
# names with non-standard English punctuation)

paper_number <- as.integer(as.factor(real_model_data_trait$Paper.Name))

real_model_data_trait <- cbind(paper_number, real_model_data_trait)



#mcmc variables
draws <- 22500
burn <-7500
cores <- 4
cntrl_vars <- list(adapt_delta = 0.995, max_treedepth = 20)

strong_priors <-  c(prior(normal(0, 1),  "b"),
                    prior(cauchy(0, 2),  "sd"))

# metaregression model w/ strong priors
mod_metareg_sp <- brm(
  log(Value) |  se(Standard.Deviation/sqrt(Sample.Size))  ~ alt_trait + Gradient + Class + # fixed effects
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

saveRDS(mod_metareg_sp, "/scratch/bell/sparks35/CNGV_analysis/output/mod_metareg_noyear_sp_wInt_allES.RDS")

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