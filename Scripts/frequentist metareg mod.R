################################################################################################
# Morgan Sparks, sparks35@purdue.edu, July 2021
# 
# Script to run similar analyses using frequentist models in metafor package.
################################################################################################


library(metafor); library(sjPlot)
################################################################################################
#CnGV analysis


### metaregression first
# load data

metaregression_data <- read.csv("~/CnGV-CoGV Meta-analysis/Data/cngv_model_data.csv")
metaregression_data$mean_ES <- abs(metaregression_data$mean_ES)
metaregression_data$Species <- as.character(metaregression_data$Species)

vcv_mat <- as.matrix(read.csv("~/CnGV-CoGV Meta-analysis/Data/cngv_vcv_randeff_mat.csv", row.names = 1, sep = ","))

#remove spaces in names

rownames(vcv_mat) <- gsub(" ", "_", rownames(vcv_mat))
colnames(vcv_mat) <- rownames(vcv_mat)

metaregression_data$Species <- gsub(" ", "_", metaregression_data$Species)

# fix a NCBI naming quirk "Eilema depressum to Eleima depressa
rownames(vcv_mat)[which(rownames(vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"
colnames(vcv_mat)[which(colnames(vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"

metaregression_data$Species[which(metaregression_data$Species == "Hyla_cinerea")] <- "Dryophytes_cinereus"

#turn year into continuous variable and species into factor
metaregression_data$Year <- as.numeric(as.character(metaregression_data$Year))
metaregression_data$Species <- as.factor(metaregression_data$Species)
droplevels(metaregression_data$Species)

#abs val of effect size, only care about magnitude of effect
metaregression_data$mean_ES <- abs(metaregression_data$mean_ES)


### recode Paper.Name into study number to help some downstream analyses (R is struggling with some 
# names with non-standard English punctuation)

paper_number <- as.integer(as.factor(metaregression_data$Paper.Name))

metaregression_data <- cbind(paper_number, metaregression_data)


### model for metaregression
metareg_mod <- rma.mv(log(mean_ES), var_ES,
                   mods = ~ 0 + alt_trait + Gradient + Year + Class, 
                   random = list(~ 1|paper_number/Trait, ~ 1|Species),# first random effect is trait within paper, second is phylogenetic effect
                   R = list(Species = vcv_mat),# specifying var_cov matrix for phylogenetic effect
                   Rscale = "none", # don't scale var_cov matrix
                   data = metaregression_data, 
                   method = "REML")

summary(metareg_mod)

tab_model(metareg_mod, transform = "exp")

exp(metareg_mod$beta)

vals <- cbind(beta = exp(metareg_mod$b), 
              ci_upper = exp(metareg_mod$ci.lb), 
              ci_lower = exp(metareg_mod$ci.ub))
vals

forest(metareg_mod, atransf=exp)

### model for randeff
randeff_mod <- rma.mv(log(mean_ES), var_ES,
                      random = list(~ 1|paper_number/Trait, ~ 1|Species), # first random effect is trait within paper, second is phylogenetic effect
                      R = list(Species = vcv_mat), # specifying var_cov matrix for phylogenetic effect
                      Rscale = "none", # don't scale var_cov matrix
                      data = metaregression_data, 
                      method = "REML")

summary(randeff_mod)

tab_model(randeff_mod, transform = "exp")

randeff_vals <- cbind(beta = exp(randeff_mod$b), 
              ci_upper = exp(randeff_mod$ci.lb), 
              ci_lower = exp(randeff_mod$ci.ub))
randeff_vals

forest(randeff_mod, atransf = exp)

confint.rma.mv(randeff_mod, type = "PL")

#funnel plot
funnel(randeff_mod)

################################################################################################
# model for cogradient

### load data

real_model_data_trait <-  read.csv("~/CnGV-CoGV Meta-analysis/Data/cogv_model_data.csv")

# remove a row of data that didn't have a NCBI entry:
real_model_data_trait <-  real_model_data_trait[-which(real_model_data_trait$Species == "Warramaba whitei"),]

#correlation matrix for phylogenetic random effect
vcv_mat <- as.matrix(read.csv("~/CnGV-CoGV Meta-analysis/Data/cogv_vcv_randeff_mat.csv", row.names = 1, sep = ","))

#remove spaces in names and replace with "_"
rownames(vcv_mat) <- gsub(" ", "_", rownames(vcv_mat))
colnames(vcv_mat) <- rownames(vcv_mat)

real_model_data_trait$Species <- gsub(" ", "_", real_model_data_trait$Species)

# fix a NCBI naming quirk "Eilema depressum to Eleima depressa
rownames(vcv_mat)[which(rownames(vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"
colnames(vcv_mat)[which(colnames(vcv_mat)=="Eilema_depressum")] <- "Eilema_depressa"

#turn year into continuous variable and species into factor
real_model_data_trait$Year <- as.numeric(as.character(real_model_data_trait$Year))
real_model_data_trait$Species <- as.factor(real_model_data_trait$Species)

#abs val of effect size, only care about magnitude of effect
real_model_data_trait$mean_ES <- abs(real_model_data_trait$mean_ES)

### recode Paper.Name into study number to help some downstream analyses (R is struggling with some
# names with non-standard English punctuation)

paper_number <- as.integer(as.factor(real_model_data_trait$Paper.Name))

real_model_data_trait <- cbind(paper_number, real_model_data_trait)

### random effects model

randeff_mod_co <- rma.mv(log(mean_ES), var_ES,
                      random = list(~ 1|paper_number/Trait, ~ 1|Species), # first random effect is trait within paper, second is phylogenetic effect
                      R = list(Species = vcv_mat), # specifying var_cov matrix for phylogenetic effect
                      Rscale = "none", # don't scale var_cov matrix
                      data = real_model_data_trait, 
                      method = "REML")

summary(randeff_mod_co)

tab_model(randeff_mod_co, transform = "exp")
