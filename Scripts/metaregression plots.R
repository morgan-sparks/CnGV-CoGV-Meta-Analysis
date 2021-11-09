library(tidyverse); library(tidybayes); library(ggridges); library(metafor); library(brms); library(glue); library(ggbeeswarm); library(patchwork); library(knitr); library(kableExtra)

###
# blog post laying out methodology here:
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/forest-plots-for-bayesian-meta-analysis.html
###

### load model
# 
# metareg_mod <- readRDS("~/Dropbox/PhD Work/Critical Review/Data/model output/mod_metareg_sp.RDS")
# 
# metareg_mod
# 
# ### look at posteriors
# 
# #exponentiate because log transformed in model
# posteriors <- exp(posterior_samples(metareg_mod))
# 
# posterior_summary(posteriors[1:35])
# 
# ### get variables
# 
# head(get_variables(metareg_mod), n = 30) # get fixed effect variable names
# 
# fixed_eff_list <- get_variables(metareg_mod)[1:20] # make a list of all variables to use
# fixed_eff_list <- as.factor(fixed_eff_list)
# fixed_eff_list # look at list
# 
# traits_result <- NULL
# temp_result <- NULL
# for (i in 2:8){
#  temp_result <- spread_draws(metareg_mod, b_Intercept,fixed_eff_list[[i]]) %>%
#    mutate(posterior_val = exp(b_Intercept) + exp(fixed_eff_list[[i]]),
#            variable = gsub(pattern = "b_alt_trait", replacement = "", x =fixed_eff_list[i]))
# 
#  traits_result <- bind_rows(traits_result, temp_result)
# }
# 
# for (i in fixed_eff_list[2:8]){
#   print(i)
#   print(gsub(pattern = "b_alt_trait", replacement = "", x =i))
# }
# 
# test <- metareg_mod %>%
#   gather_draws(exp(c(b_alt_traitciliaryactivity,
#                 b_alt_traitdevelopmentalrate,
#                 b_alt_traitgonadalsize,
#                 b_alt_traitgrowthrate,
#                 b_alt_traitmass,
#                 b_alt_traitmetabolicrate,
#                 b_alt_traitreproductiverate))) %>%
#   mean_qi()
# 
# ###
# 
# ciliary_activity <- spread_draws(metareg_mod, b_Intercept, b_alt_traitciliaryactivity) %>%
#   mutate(posterior_val = exp(b_Intercept) + exp(b_alt_traitciliaryactivity),
#          variable = "Ciliary activity")
# 
# development_rate <- spread_draws(metareg_mod, b_Intercept, b_alt_traitdevelopmentalrate) %>%
#   mutate(posterior_val = exp(b_Intercept) + exp(b_alt_traitdevelopmentalrate),
#          variable = "Developmental rate")
# 
# gonadal_size <- spread_draws(metareg_mod, b_Intercept, b_alt_traitgonadalsize) %>%
#   mutate(posterior_val = exp(b_Intercept) + exp(b_alt_traitgonadalsize),
#          variable = "Gonadal size")
# 
# beta_intercept <- spread_draws(metareg_mod, b_Intercept) %>%
#   mutate(posterior_val = exp(b_Intercept),
#          variable = "Intercept")
#                                                                                                                                                                                                                
#   
# all_draws <- bind_rows(ciliary_activity, development_rate, gonadal_size, beta_intercept) %>% 
#   ungroup() %>% # Ensure that Average effect is on the bottom of the forest plot
#   mutate(variable = fct_relevel(variable, "Intercept"))
#              
# all_draws_sum <- group_by(all_draws, variable) %>% 
#   mean_qi(posterior_val)
# 
# ggplot(data = all_draws, aes(posterior_val, variable))+
#   geom_density_ridges(rel_min_height = 0.01, col = NA, scale = 1, fill = "dodgerblue", alpha = 0.75) +
#   geom_pointinterval(data = all_draws_sum, size = 1)  +
#   #xlim(-1, 10) +
#   theme_classic()
#                                                                                                                                                                                                  

########################################################################


#### same for medium strength priors

mod_metareg_strongprior <- readRDS("~/CnGV-CoGV-Meta-analysis/Data/model_output/mod_metareg_noyear_sp.RDS")
get_variables(mod_metareg_strongprior)
mod_metareg_strongprior

posteriors_sp <- round(exp(posterior_samples(mod_metareg_strongprior)), digits = 3)

posterior_summary(posteriors_sp[1:34])
posts <- data.frame(posterior_summary(posteriors_sp[1:34]))

post_sum <-  data.frame(posterior_summary(posteriors_sp[1:34]))

post_sum <- round(post_sum[,1:4], 3)

post_sum # make sure rounding worked

### make sure these match names in posterior summary command
row.names(post_sum) <- c("Body shape", "Body size", "Carotenoid concentration", "Ciliary activity", "Developmental rate", "Gamete size", "Growth rate", 
                         "Metabolic rate", "Phenology", "Reproductive rate", "Thermal response","Elevation", 
                         "Latitude", "Migration distance", "Photoperiod", "Predator", "Salinity", "Shade cover", "Soil phosphate",
                         "Temperature", "Urbanisation", "Wave action",
                         "Amphibia", "Anthaozoa", "Asteraceae", "Bivalvia", "Gastropoda", "Insecta","Liliopsida", "Malacostraca", "Reptilia",
                         "SD Paper Number", "SD Paper Number:Trait", "SD Species"
                         )


#make summary table and send to folder for paper files
post_sum %>% 
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable("~/Dropbox/PhD Work/Critical Review/Work for Publication/Tables:Figures/Table 1.pdf")
  
### gather the draws for each element

# traits

body_shape <- spread_draws(mod_metareg_strongprior, b_alt_traitbody_shape) %>%
  mutate(posterior_val = exp(b_alt_traitbody_shape),
         variable = "Body shape", covariate = "Trait")

body_size <- spread_draws(mod_metareg_strongprior, b_alt_traitbody_size) %>%
  mutate(posterior_val = exp(b_alt_traitbody_size),
         variable = "Body size", covariate = "Trait")

carotenoid_concentration <- spread_draws(mod_metareg_strongprior, b_alt_traitcarotenoid_concentration) %>%
  mutate(posterior_val = exp(b_alt_traitcarotenoid_concentration),
         variable = "Carotenoid concentration", covariate = "Trait")


ciliary_activity <- spread_draws(mod_metareg_strongprior, b_alt_traitciliary_activity) %>%
  mutate(posterior_val = exp(b_alt_traitciliary_activity),
         variable = "Ciliary activity", covariate = "Trait")


development_rate <- spread_draws(mod_metareg_strongprior, b_alt_traitdevelopmental_rate ) %>%
  mutate(posterior_val = exp(b_alt_traitdevelopmental_rate),
         variable = "Developmental rate", covariate = "Trait")


gamete_size <- spread_draws(mod_metareg_strongprior, b_alt_traitgamete_size) %>%
  mutate(posterior_val = exp(b_alt_traitgamete_size),
         variable = "Gamete size", covariate = "Trait")

growth_rate <- spread_draws(mod_metareg_strongprior, b_alt_traitgrowth_rate) %>%
  mutate(posterior_val = exp( b_alt_traitgrowth_rate),
         variable = "Growth rate", covariate = "Trait")

# mass <- spread_draws(mod_metareg_strongprior, b_Intercept, b_alt_traitmass) %>%
#   mutate(posterior_val = exp(b_alt_traitmass),
#          variable = "Mass", covariate = "Trait")

metabolic_rate <- spread_draws(mod_metareg_strongprior,b_alt_traitmetabolic_rate) %>%
  mutate(posterior_val = exp(b_alt_traitmetabolic_rate),
         variable = "Metabolic rate", covariate = "Trait")

reproductive_rate <- spread_draws(mod_metareg_strongprior, b_alt_traitreproductive_rate) %>%
  mutate(posterior_val = exp(b_alt_traitreproductive_rate),
         variable = "Reproductive rate", covariate = "Trait")

phenology <- spread_draws(mod_metareg_strongprior,  b_alt_traitphenology) %>%
  mutate(posterior_val = exp(b_alt_traitphenology),
         variable = "Phenology", covariate = "Trait")

thermal_response <- spread_draws(mod_metareg_strongprior,  b_alt_traitthermal_response) %>%
  mutate(posterior_val = exp(b_alt_traitthermal_response),
         variable = "Thermal response", covariate = "Trait")

# Gradients
elevation <- spread_draws(mod_metareg_strongprior, b_Gradientelevation) %>%
  mutate(posterior_val = exp(b_Gradientelevation),
         variable = "Elevation", covariate = "Gradient")

latitude <- spread_draws(mod_metareg_strongprior, b_Gradientlatitude) %>%
  mutate(posterior_val = exp(b_Gradientlatitude),
         variable = "Latitude", covariate = "Gradient")

migration_distance <- spread_draws(mod_metareg_strongprior,  b_Gradientmigrationdistance) %>%
  mutate(posterior_val = exp(b_Gradientmigrationdistance),
         variable = "Migration distance", covariate = "Gradient")

photoperiod <- spread_draws(mod_metareg_strongprior,  b_Gradientphotoperiod) %>%
  mutate(posterior_val = exp(b_Gradientphotoperiod),
         variable = "Photoperiod", covariate = "Gradient")

predator <- spread_draws(mod_metareg_strongprior,  b_Gradientpredator) %>%
  mutate(posterior_val = exp(b_Gradientpredator),
         variable = "Predator", covariate = "Gradient")


salinity <- spread_draws(mod_metareg_strongprior, b_Gradientsalinity) %>%
  mutate(posterior_val = exp(b_Gradientsalinity),
         variable = "Salinity", covariate = "Gradient")

shade_cover <- spread_draws(mod_metareg_strongprior, b_Gradientshadecover ) %>%
  mutate(posterior_val = exp(b_Gradientshadecover ),
         variable = "Shade cover", covariate = "Gradient")

soil_phosphate <- spread_draws(mod_metareg_strongprior,  b_Gradientsoilphosphatelevel) %>%
  mutate(posterior_val = exp(b_Gradientsoilphosphatelevel),
         variable = "Soil phosphate", covariate = "Gradient")

temperature <- spread_draws(mod_metareg_strongprior, b_Gradienttemperature) %>%
  mutate(posterior_val = exp(b_Gradienttemperature),
         variable = "Temperature", covariate = "Gradient")

urbanisation <- spread_draws(mod_metareg_strongprior,  b_Gradienturbanisation) %>%
  mutate(posterior_val = exp(b_Gradienturbanisation),
         variable = "Urbanisation", covariate = "Gradient")

wave_action <- spread_draws(mod_metareg_strongprior, b_Gradientwaveaction) %>%
  mutate(posterior_val = exp(b_Gradientwaveaction),
         variable = "Wave action", covariate = "Gradient")

#year (now removed)
# year <- spread_draws(mod_metareg_strongprior, b_Year ) %>%
#   mutate(posterior_val = exp(b_Year ),
#          variable = "Year", covariate = "Year")

#Class
amphibia <- spread_draws(mod_metareg_strongprior, b_ClassAmphibia ) %>%
  mutate(posterior_val = exp(b_ClassAmphibia ),
         variable = "Amphibia", covariate = "Class")

anthozoa <- spread_draws(mod_metareg_strongprior,b_ClassAnthozoa ) %>%
  mutate(posterior_val = exp(b_ClassAnthozoa ),
         variable = "Anthozoa", covariate = "Class")

asteraceae <- spread_draws(mod_metareg_strongprior,b_ClassAsteraceae  ) %>%
  mutate(posterior_val = exp(b_ClassAsteraceae  ),
         variable = "Asteraceae", covariate = "Class")

bivalvia <- spread_draws(mod_metareg_strongprior,b_ClassBivalvia) %>%
  mutate(posterior_val = exp(b_ClassBivalvia  ),
         variable = "Bivalvia", covariate = "Class")


# gasterosteidae <- spread_draws(mod_metareg_strongprior,b_ClassGasterosteidae) %>%
#   mutate(posterior_val = exp(b_ClassGasterosteidae),
#          variable = "Actinopterygii", covariate = "Class")

gastropoda <- spread_draws(mod_metareg_strongprior,b_ClassGastropoda   ) %>%
  mutate(posterior_val = exp(b_ClassGastropoda),
         variable = "Gastropoda", covariate = "Class")

insecta <- spread_draws(mod_metareg_strongprior,b_ClassInsecta   ) %>%
  mutate(posterior_val = exp(b_ClassInsecta),
         variable = "Insecta", covariate = "Class")

liliopsida <-spread_draws(mod_metareg_strongprior,b_ClassLiliopsida) %>%
  mutate(posterior_val = exp(b_ClassLiliopsida),
         variable = "Liliopsida", covariate = "Class")

malacostraca  <-spread_draws(mod_metareg_strongprior,b_ClassMalacostraca) %>%
  mutate(posterior_val = exp(b_ClassMalacostraca),
         variable = "Malacostraca", covariate = "Class")

reptilia  <-spread_draws(mod_metareg_strongprior,b_ClassReptilia) %>%
  mutate(posterior_val = exp(b_ClassReptilia),
         variable = "Reptilia", covariate = "Class")



#intercept

# beta_intercept <- spread_draws(mod_metareg_strongprior, b_Intercept) %>%
#   mutate(posterior_val = exp(b_Intercept),
#          variable = "Intercept", covariate = "Intercept")


all_draws <- bind_rows(body_shape, body_size, carotenoid_concentration, ciliary_activity, development_rate, gamete_size, growth_rate,  
                        metabolic_rate, phenology, reproductive_rate, thermal_response,
                       elevation, latitude, migration_distance, photoperiod, predator, salinity, shade_cover, soil_phosphate,
                       temperature, urbanisation, wave_action,
                       amphibia, anthozoa, asteraceae, bivalvia, gastropoda, insecta,liliopsida, malacostraca, reptilia)  %>%
  ungroup() %>% # Ensure that Average effect is on the bottom of the forest plot
  mutate(variable = fct_relevel(variable,c("Body shape", "Body size", "Carotenoid concentration", "Ciliary activity", "Developmental rate", "Gamete size", "Growth rate", 
                                           "Metabolic rate", "Phenology", "Reproductive rate", "Thermal response", "Elevation",
                                           "Latitude", "Migration distance","Photoperiod", "Predator", "Salinity", "Shade cover", "Soil phosphate",
                                           "Temperature", "Urbanisation", "Wave action",
                                            "Amphibia", "Anthozoa", "Asteraceae", "Bivalvia", "Gastropoda", "Insecta","Liliopsida", "Malacostraca", "Reptilia"
                                           )))

all_draws$covariate <- as.factor(all_draws$covariate)

all_draws_sum <- group_by(all_draws, variable) %>% 
  mean_qi(posterior_val)

all_draws_sum_med <- group_by(all_draws, variable) %>% 
  median_qi(posterior_val)


### plot

cust_pal <- c("#E6AB02", "#1B9E77", "#7570B3","#66A61E" )

#using geom_density_ridges
d_ridges <- ggplot(data = all_draws, aes(posterior_val, variable))+
  geom_density_ridges(aes(fill= covariate), rel_min_height = 0.1, col = NA, scale = 1, alpha = 0.75) +
  geom_pointinterval(data = all_draws_sum, aes(posterior_val, variable, xmin = .lower, xmax = .upper ), color = "black", size = 1)  +
  geom_point(data = all_draws_sum_med, aes(posterior_val, variable), shape = 18, color = "darkgrey", size = 2) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 1.01, linetype = "dashed") +
  geom_text(data = mutate_if(all_draws_sum, is.numeric, round, 2),
            aes(label = glue("{posterior_val} [{.lower}, {.upper}]"), x = Inf), hjust = "inward", vjust = -.5) +
  scale_color_manual(values = cust_pal) +
  labs(y = "", x = "Effect size") +
  xlim(0,10)+
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

# using geom quasirandom
quasi_random <- ggplot(data = all_draws, aes(posterior_val, variable))+
  geom_quasirandom(aes(color= covariate), groupOnX = FALSE, size = 0.1) +
  geom_pointinterval(data = all_draws_sum, aes(posterior_val, variable, xmin = .lower, xmax = .upper ), color = "black", size = 1)  +
  geom_point(data = all_draws_sum_med, aes(posterior_val, variable), shape = 18, color = "darkgrey", size = 2) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 1.01, linetype = "dashed") +
  geom_text(data = mutate_if(all_draws_sum, is.numeric, round, 2),
            aes(label = glue("{posterior_val} [{.lower}, {.upper}]"), x = Inf), hjust = "inward", vjust = -.5) +
  scale_color_manual(values = cust_pal) +
  labs(y = "", x = "Effect size") +
  xlim(0,15)+
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
       



d_ridges + quasi_random

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Tables:Figures/Fig. 3.pdf", quasi_random,
       width = 6, height = 8, units = "in", dpi = 300)


##
metareg_emmeans <- emmeans::emmeans(mod_metareg_strongprior, specs = ~ Class + Gradient + alt_trait)
metareg_conts <-  emmeans::contrast(metareg_emmeans)

bayestestR::describe_posterior(mod_metareg_strongprior,
                   estimate = "median", dispersion = TRUE,
                   ci = .9, ci_method = "hdi",
                   test = c("bayesfactor"))

