library(brms); library(ape)
setwd("/scratch/bell/sparks35/CNGV_analysis/output")

### load in data
#model data
real_model_data_trait <-  read.csv("/scratch/bell/sparks35/CNGV_analysis/data/cogv_model_data_allES.csv")

real_model_data_trait <-  read.csv("./Data//cogv_model_data_allES.csv")

# remove a row of data that didn't have a NCBI entry:
real_model_data_trait <-  real_model_data_trait[-which(real_model_data_trait$Species == "Warramaba whitei"),]

#correlation matrix for phylogenetic random effect
vcv_mat <- as.matrix(read.csv("/scratch/bell/sparks35/CNGV_analysis/data/cogv_vcv_randeff_mat.csv", row.names = 1, sep = ","))

#remove spaces in names and replace with "_"
rownames(vcv_mat) <- gsub(" ", "_", rownames(vcv_mat))
colnames(vcv_mat) <- rownames(vcv_mat)

real_model_data_trait$Species <- gsub(" ", "_", real_model_data_trait$Species)

#turn year into continuous variable and species into factor
real_model_data_trait$Year <- as.numeric(as.character(real_model_data_trait$Year))
real_model_data_trait$Species <- as.factor(real_model_data_trait$Species)

#abs val of effect size, only care about magnitude of effect
real_model_data_trait$Value <- abs(real_model_data_trait$Value)

### recode Paper.Name into study number to help some downstream analyses (R is struggling with some
# names with non-standard English punctuation)

paper_number <- as.integer(as.factor(real_model_data_trait$Paper.Name))

real_model_data_trait <- cbind(paper_number, real_model_data_trait)

# set prior
priors <- c(prior(normal(0,2), class = Intercept),
            prior(cauchy(0,2), class = sd))

# run model

mod_norm_logtrans_trait_co<-
  brm(log(Value) | se(Standard.Deviation/sqrt(Sample.Size)) # log of mean ES to normalize, |se() weights the on standard error of measurement (convention for meta-analysis)
      ~ 1 + # intercept only model
        (1|paper_number/Trait) + # trait nested in paper random effect
        (1|gr(Species, cov = vcv_mat)), # phylogenetic random effect
      data = real_model_data_trait,
      data2 = list(vcv_mat = vcv_mat), # var-cov for phylogentic random effect
      family = student(),
      iter = 10000,
      warmup = 2500,
      cores = 4,
      thin = 1,
      prior = priors,
      control = list(adapt_delta = 0.99, max_treedepth = 18)) #upped adapt_delta to lower divergent transitions

summary(mod_norm_logtrans_trait_co)

saveRDS(mod_norm_logtrans_trait_co, "mod_norm_logtrans_trait_2randeff_student_co_sp_allES.rds")


sessionInfo()