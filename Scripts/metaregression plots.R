################################################################################################
# Morgan Sparks, sparks35@purdue.edu, December 2021
# 
# Script to make metaregression plot for manuscript. It uses the emmeans package and a variety
# of bayesian plotting tools to make emmean posterior distributions for metaregression model.
################################################################################################

library(tidyverse); library(tidybayes); library(ggridges); library(metafor); library(brms); library(glue); library(ggbeeswarm); library(patchwork); library(knitr); library(kableExtra); library(emmeans)

###
# blog post laying out methodology here:
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/forest-plots-for-bayesian-meta-analysis.html
###


mod_metareg_strongprior <- readRDS("~/CnGV-CoGV-Meta-analysis/Data/model_output/mod_metareg_noyear_sp_wInt_allES_correct_parallel.RDS")
get_variables(mod_metareg_strongprior)
mod_metareg_strongprior

# check for divergent transitions
mod_metareg_strongprior["divergent__"]

posteriors_sp <- round(exp(posterior_samples(mod_metareg_strongprior)), digits = 3)


posterior_summary(posteriors_sp[1:34])

post_int_sum<-  data.frame(posterior_summary(posteriors_sp[1:34]))

post_int_sum <- round(post_int_sum[,1:4], 2)

post_int_sum # make sure rounding worked

### make sure these match names in posterior summary command
row.names(post_int_sum) <- c("Intercept", "Body size", "Carotenoid concentration", "Ciliary activity", "Developmental rate", "Gamete size", "Growth rate", 
                             "Metabolic rate", "Phenology", "Reproductive rate", "Thermal response","Elevation", 
                             "Latitude", "Migration distance", "Photoperiod", "Predator presence", "Salinity", "Shade cover", "Soil phosphate",
                             "Temperature", "Urbanisation", "Wave action",
                             "Amphibia", "Anthaozoa", "Magnoliopsida", "Bivalvia", "Gastropoda", "Insecta","Liliopsida", "Malacostraca", "Reptilia",
                             "SD Paper Number", "SD Paper Number:Trait", "SD Species"
)


#make summary table and send to folder for paper files
post_int_sum %>% 
  kbl() %>%
  kable_classic(full_width = F, html_font = "Times New Roman") %>%
  save_kable("~/Dropbox/PhD Work/Critical Review/Work for Publication/Tables:Figures/Table 1 wInt.pdf")

write.table(file = "~/Downloads/Table1.csv", post_int_sum, sep = ",")
####

#gather main effect emmean draws for each covariate
gradient_emmeans <- emmeans(mod_metareg_strongprior, ~ Gradient) %>% 
  gather_emmeans_draws() %>% # grab and gather expected marginal means for each draw for all Gradient estimates
  mutate(covariate = "Gradient") %>%  # add a column full of "Gradients"
  rename(Variable = Gradient) # rename Gradient column to Variable to combine all later

class_emmeans <- emmeans(mod_metareg_strongprior, ~ Class) %>% # do same for Class
  gather_emmeans_draws() %>% 
  mutate(covariate = "Class") %>% 
  rename(Variable = Class)

trait_emmeans <- emmeans(mod_metareg_strongprior, ~ alt_trait) %>% # do same for alt_trait
  gather_emmeans_draws() %>% 
  mutate(covariate = "Trait") %>% 
  rename(Variable = alt_trait)

## bind and exponentiate emmean draws
all_emmeans <- bind_rows(trait_emmeans, gradient_emmeans, class_emmeans,) %>% 
  mutate(.value = exp(.value))

## rename levels
# note the change of asteraceae to Magnoliopsida (Asteraceae was incorrectly included as a Class in model)

all_emmeans[,"Variable"] <-  dplyr::recode(all_emmeans$Variable,
                                           "body_shape"="Body shape", "body_size"="Body size", "carotenoid_concentration" = "Carotenoid concentration", "ciliary_activity"="Ciliary activity", "developmental_rate" = "Developmental rate",
                                           "gamete_size"= "Gamete size", "growth_rate" = "Growth rate", "metabolic_rate" = "Metabolic rate", "phenology" = "Phenology", "reproductive_rate" = "Reproductive rate",
                                           "thermal_response" = "Thermal response", "carotenoid availability" = "Carotenoid availability", "elevation" = "Elevation", "latitude" = "Latitude", "migration distance" = "Migration distance",
                                           "photoperiod" = "Photoperiod", "predator" = "Predator presence", "salinity" = "Salinity", "shade cover" = "Shade cover", "soil phosphate level" = "Soil phosphate level",
                                           "temperature" = "Temperature", "urbanisation" = "Urbanisation", "wave action" = "Wave action", "Liliopsida " = "Liliopsida", "Asteraceae" = "Magnoliopsida",
                                           )

# reorder factor levels for plot 
all_emmeans <- all_emmeans %>%  
  ungroup() %>%
  mutate(Variable = fct_relevel(Variable,c( "Thermal response","Reproductive rate","Phenology","Metabolic rate","Growth rate","Gamete size","Developmental rate","Ciliary activity","Carotenoid concentration","Body size","Body shape",                 
                                            "Wave action","Urbanisation","Temperature","Soil phosphate level","Shade cover", "Salinity","Predator presence", "Photoperiod", "Migration distance","Latitude", "Elevation", "Carotenoid availability",   
                                            "Reptilia", "Malacostraca","Magnoliopsida","Liliopsida", "Insecta","Gastropoda","Bivalvia","Anthozoa", "Amphibia","Actinopterygii"
                                            )))         
  


## calculte summary info for mean and median
all_emmeans_sum <- group_by(all_emmeans, Variable) %>% 
  mean_qi(.value)
all_emmeans_sum_5 <- group_by(all_emmeans, Variable) %>% 
  mean_qi(.value, .width = .5 )

all_emmeans_sum_med <- group_by(all_emmeans, Variable) %>% 
  median_qi(.value)

cust_pal <- c("#E6AB02", "#1B9E77", "#7570B3","#66A61E" )

# save full distribution for supplementry materials
supp_fig_big <- ggplot(data = all_emmeans, aes(.value, Variable))+
  geom_quasirandom(aes(color= covariate), groupOnX = FALSE, size = 0.01) +
  geom_pointinterval(data = all_emmeans_sum, aes(.value, Variable, xmin = .lower, xmax = .upper ), color = "grey50", size = 2)  +
  geom_pointinterval(data = all_emmeans_sum_5, aes(.value, Variable, xmin = .lower, xmax = .upper ), color = "black", size =4)  +
  #geom_point(data = all_emmeans_sum_med, aes(.value, Variable), shape = 18, color = "darkgrey", size = 2) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 1.03, linetype = "dashed") +
  geom_text(data = mutate_if(all_emmeans_sum, is.numeric, round, 2),
            aes(label = glue("{.value} [{.lower}, {.upper}]"), x = Inf), hjust = "inward", vjust = -0.25, size = 2.5) +
  scale_color_manual(values = cust_pal) +
  labs(y = "", x = "Estimated Marginal Mean", subtitle = "b)") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", axis.text.y=element_blank(), axis.ticks.y = element_blank())

supp_fig_small<- ggplot(data = all_emmeans, aes(.value, Variable))+
  geom_density_ridges(aes(fill= covariate), rel_min_height = 0.1, col = NA, scale = 1, height = 0.9) +
  geom_pointinterval(data = all_emmeans_sum, aes(.value, Variable, xmin = .lower, xmax = .upper, size = 1), color = "grey50", interval_size_range = c(0.25, .5))  +
  geom_pointinterval(data = all_emmeans_sum_5, aes(.value, Variable, xmin = .lower, xmax = .upper, size = 1), color = "black", interval_size_range = c(0.5, 1))  +
  geom_vline(xintercept = 1.05, linetype = "dashed") +
  geom_point(data = all_emmeans_sum_med, aes(.value, Variable), shape = 18, color = "darkgrey", size = 2.5) +
  geom_vline(xintercept = 0) +
  # geom_text(data = mutate_if(all_emmeans_sum, is.numeric, round, 2),
  #           aes(label = glue("{.value} [{.lower}, {.upper}]"), x = Inf), hjust = "inward", vjust = -0.5, size = 2.5) +
  scale_fill_manual(values = cust_pal) +
  labs(y = "", x = "Estimated Marginal Mean", subtitle = "a)") +
  xlim(-0.5,15)+
  theme_classic(base_size = 14) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"))

supp_fig <- supp_fig_small + supp_fig_big


ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/metareg_mod_all.png", supp_fig,
         width = 10, height = 7, units = "in", dpi = 600)

## pdf for main manuscript
pub_fig <- ggplot(data = all_emmeans, aes(.value, Variable))+
  geom_density_ridges(aes(fill= covariate), rel_min_height = 0.1, col = NA, scale = 1, height = 0.9) +
  geom_pointinterval(data = all_emmeans_sum, aes(.value, Variable, xmin = .lower, xmax = .upper, size = 1), color = "grey40", interval_size_range = c(.35, .65))  +
  geom_pointinterval(data = all_emmeans_sum_5, aes(.value, Variable, xmin = .lower, xmax = .upper, size = 1), color = "black", interval_size_range = c(0.5, 1))  +
  geom_vline(xintercept = 1.05, linetype = "dashed") +
  geom_point(data = all_emmeans_sum_med, aes(.value, Variable), shape = 18, color = "darkgrey", size = 2.5) +
  geom_vline(xintercept = 0) +
  geom_text(data = mutate_if(all_emmeans_sum, is.numeric, round, 2),
            aes(label = glue("{.value} [{.lower}, {.upper}]"), x = Inf), hjust = "inward", vjust = -0.5, size = 2.75) +
  scale_fill_manual(values = cust_pal) +
  labs(y = "", x = "Estimated Marginal Mean") +
  coord_cartesian(x = c(-0.5,5))+
  theme_classic(base_size = 14) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.background = element_rect(fill = "white"))

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Tables:Figures/Fig. 3.pdf", pub_fig,
         width = 6, height = 8, units = "in", dpi = 600)


########################################################################################################################################################################################################################
########################################################################
##################################################
#########################
#########



###############
# below is doing the same but for model coefficients
# it is not presented in manuscript

###############


#### same for medium strength priors

mod_metareg_strongprior <- readRDS("~/CnGV-CoGV-Meta-analysis/Data/model_output/mod_metareg_noyear_sp_wInt_allES_correct_parallel.RDS")
get_variables(mod_metareg_strongprior)
mod_metareg_strongprior

posteriors_sp <- round(exp(posterior_samples(mod_metareg_strongprior)), digits = 3)

posts <- data.frame(posterior_summary(posteriors_sp[1:35]))

post_sum <-  data.frame(posterior_summary(posteriors_sp[1:34]))

post_sum <- round(post_sum[,1:4], 2)

post_sum # make sure rounding worked


### make sure these match names in posterior summary command
row.names(post_sum) <- c("Intercept", "Body size", "Carotenoid concentration", "Ciliary activity", "Developmental rate", "Gamete size", "Growth rate", 
                         "Metabolic rate", "Phenology", "Reproductive rate", "Thermal response","Elevation", 
                         "Latitude", "Migration distance", "Photoperiod", "Predator presence", "Salinity", "Shade cover", "Soil phosphate",
                         "Temperature", "Urbanisation", "Wave action",
                         "Amphibia", "Anthozoa", "Magnoliopsida", "Bivalvia", "Gastropoda", "Insecta","Liliopsida", "Malacostraca", "Reptilia",
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

Magnoliopsida <- spread_draws(mod_metareg_strongprior,b_ClassMagnoliopsida  ) %>%
  mutate(posterior_val = exp(b_ClassMagnoliopsida  ),
         variable = "Magnoliopsida", covariate = "Class")

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
                       amphibia, anthozoa, Magnoliopsida, bivalvia, gastropoda, insecta,liliopsida, malacostraca, reptilia)  %>%
  ungroup() %>% # Ensure that Average effect is on the bottom of the forest plot
  mutate(variable = fct_relevel(variable,c("Body shape", "Body size", "Carotenoid concentration", "Ciliary activity", "Developmental rate", "Gamete size", "Growth rate", 
                                           "Metabolic rate", "Phenology", "Reproductive rate", "Thermal response", "Elevation",
                                           "Latitude", "Migration distance","Photoperiod", "Predator", "Salinity", "Shade cover", "Soil phosphate",
                                           "Temperature", "Urbanisation", "Wave action",
                                            "Amphibia", "Anthozoa", "Magnoliopsida", "Bivalvia", "Gastropoda", "Insecta","Liliopsida", "Malacostraca", "Reptilia"
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
  geom_vline(xintercept = 1.03, linetype = "dashed") +
  geom_text(data = mutate_if(all_draws_sum, is.numeric, round, 2),
            aes(label = glue("{posterior_val} [{.lower}, {.upper}]"), x = Inf), hjust = "inward", vjust = -.5) +
  scale_color_manual(values = cust_pal) +
  labs(y = "", x = "Effect size") +
  xlim(0,15)+
  theme_classic(base_size = 14) +
  theme(legend.position = "none")
       



d_ridges + quasi_random




quasi_random_all <- ggplot(data = all_draws, aes(posterior_val, variable))+
  geom_quasirandom(aes(color= covariate), groupOnX = FALSE, size = 0.1) +
  geom_pointinterval(data = all_draws_sum, aes(posterior_val, variable, xmin = .lower, xmax = .upper ), color = "black", size = 1)  +
  geom_point(data = all_draws_sum_med, aes(posterior_val, variable), shape = 18, color = "darkgrey", size = 2) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 1.03, linetype = "dashed") +
  geom_text(data = mutate_if(all_draws_sum, is.numeric, round, 2),
            aes(label = glue("{posterior_val} [{.lower}, {.upper}]"), x = Inf), hjust = "inward", vjust = -.5) +
  scale_color_manual(values = cust_pal) +
  labs(y = "", x = "Effect size") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")







