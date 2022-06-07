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

# change Uca_pugilator to updated classification (which it is in vcv_mat from NCBI)
real_model_data_trait[which(real_model_data_trait$Species == "Uca_pugilator"), "Species"] <- "Leptuca_pugilator"

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

### set priors (these are moderately strong)

priors <- c(prior(normal(0,2), class = Intercept),
            prior(cauchy(0,2), class = sd))

# run model

mod_norm_logtrans_trait_student<-
  brm(log(abs(yi)) | se(Standard.Deviation/sqrt(Sample.Size)) # log of mean ES to normalize, |se() weights the on standard error of measurment (convention for meta-analysis)
      ~ 1 + # intercept only model
        (1|paper_number/Trait) + # trait nested in paper random effect
        (1|gr(Species, cov = vcv_mat)), # phylogenetic random effect
      data = real_model_data_trait,
      data2 = list(vcv_mat = vcv_mat), # var-cov for phylogentic random effect
      family = student(),
      iter = 20000,
      warmup = 7500,
      cores = 4,
      thin = 1,
      prior = priors,
      control = list(adapt_delta = 0.995, max_treedepth = 20)) #upped adapt_delta to lower divergent transitions


summary(mod_norm_logtrans_trait_student)

saveRDS(mod_norm_logtrans_trait_student, "mod_norm_logtrans_trait_2randeff_student_sp_allES.rds")

sessionInfo()