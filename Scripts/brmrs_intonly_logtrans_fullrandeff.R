library(brms); library(ggplot2); library(dplyr); library(ape); library(taxize)
setwd("/scratch/snyder/s/sparks35/CNGV_analysis/output")
# 
# my_dat <-  read.csv("/scratch/snyder/s/sparks35/CNGV_analysis/data/countgrad_data_20190724 analysis.csv")
# 
# my_meta <- read.csv("/scratch/snyder/s/sparks35/CNGV_analysis/data/Meta Data analysis.csv")
# 
# cngv_data_ES <- escalc(m1i =Value,# mean of a group
#                                 sd1i =Standard.Deviation, #SD of a group
#                                 n1i=Sample.Size, #sample size of a group
#                                 m2i =Value.b, #mean of b group
#                                 sd2i =Standard.Deviation.b, #SD of b group
#                                 n2i=Sample.Size.b, #sample size of b group
#                                 data = my_dat, 
#                                 append = T,#add results to dataframe
#                                 measure = "SMD") # use standardizes mean difference (Hedge's g)
# 
# model_data <- merge(cngv_data_ES, #merge the two dataframes by columns in the first and second
#                     my_meta,
#                     by.x = c("Paper..Authors...Year."),
#                     by.y = c("X"),
#                     all.x = T,
#                     no.dups = T)
# 
# temp <- NULL
# temp2 <- NULL
# 
# OUT <- NULL
# OUT <- as.data.frame(OUT)
# for(i in levels(model_data$Paper.Name)){
#   temp <- model_data[which(model_data$Paper.Name==i),]
#   temp$Trait <- droplevels(temp$Trait)
#   for(j in levels(temp$Trait)){
#     temp2 <- temp[which(temp$Trait==j),]
#     temp.mn <- mean(temp2$yi)
#     temp.var <- sum(temp2$vi)/(length(temp2$vi)^2)
#     temp.row <- cbind(temp2[1,], temp.mn, temp.var)
#     temp.ss <- mean(temp2$Sample.Size)
#     temp.row <- cbind(temp2[1,], temp.mn, temp.var, temp.ss)
#     OUT <- rbind(OUT, temp.row)
#       
#     }
#   }

real_model_data_trait <-  read.csv("/scratch/snyder/s/sparks35/CNGV_analysis/data/trait_level_data.csv")



#turn year into continuous variable and species into factor
real_model_data_trait$Year <- as.numeric(as.character(real_model_data_trait$Year))
real_model_data_trait$Species <- as.factor(real_model_data_trait$Species)

#abs val of effect size
# real_model_data_trait$temp.mn <- abs(real_model_data_trait$temp.mn)
# 
# # add in column of standard error
# real_model_data_trait <- real_model_data_trait %>%
#   mutate(std_err = temp.var/sqrt(temp.ss))

##### make covariance matrix

# trait_level_data <- read.csv("~/Desktop/brmstest results/trait_level_data.csv")
# trait_level_data$Species <- as.character(trait_level_data$Species)
# 
# trait_level_data <- trait_level_data[!duplicated(trait_level_data$Species), ]
# 
# taxize_class <- classification(trait_level_data$Species, db = "ncbi")
# taxize_tree<- class2tree(taxize_class, varstep = TRUE, check = TRUE)
# vcv_mat <- vcv.phylo(taxize_tree$phylo, corr = TRUE)
# 
# write.csv(vcv_mat, "~/Desktop/brmstest results/vcv_randeff_mat.csv")

vcv_mat <- as.matrix(read.csv("/scratch/snyder/s/sparks35/CNGV_analysis/data/vcv_randeff_mat.csv", row.names = 1, sep = ","))

#remove spaces in names

rownames(vcv_mat) <- gsub(" ", "_", rownames(vcv_mat))
colnames(vcv_mat) <- rownames(vcv_mat)

real_model_data_trait$Species <- gsub(" ", "_", real_model_data_trait$Species)

# run model 

mod_norm_logtrans_trait<- brm(
  log(temp.mn) | se(std_err)  ~ 1 + (1|Paper.Name/Trait) + (1|gr(Species, cov = vcv_mat)), # model fit 
  data = real_model_data_trait,
  data2 = list(vcv_mat = vcv_mat),
  family = gaussian(),
  iter = 10000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.88, max_treedepth = 18)) #upped adapt_delta to lower divergent transitions

summary(mod_norm_logtrans_trait)

saveRDS(mod_norm_logtrans_trait, "mod_norm_logtrans_trait_2randeff.rds")

#######


prior_c <- c(set_prior("normal(0, 1)", class = "Intercept"),
             set_prior("cauchy(0, 0.3)", class = "sd"))

mod_norm_logtrans_trait_weakprior <- brm(
  log(temp.mn) | se(std_err)  ~ 1 + (1|Paper.Name/Trait), # model fit 
  data = real_model_data_trait,
  family = gaussian(),
  prior = prior_c,
  iter = 10000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.88, max_treedepth = 18)) #upped adapt_delta to lower divergent transitions

summary(mod_norm_logtrans_trait_weakprior)

saveRDS(mod_norm_logtrans_trait_weakprior, "mod_norm_logtrans_trait_weakprior_2randeff.rds")