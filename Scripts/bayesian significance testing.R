library(bayestestR)

#intercept only model CnGV
int_mod <- readRDS("~/CnGV-CoGV-Meta-analysis/Data/model output/mod_norm_logtrans_trait_2randeff_student_sp.rds")
int_mod

bayeint_param_BF <- bayesfactor_parameters(int_mod, null = c(log(1e-16),log(0.2))) #Effect sizes between 0 and 0.2 

int_BF <- bayesfactor(int_mod)

describe_posterior(int_mod)


# metareg mod
metareg_mod <- readRDS("~/CnGV-CoGV-Meta-analysis/Data/model output/mod_metareg_sp.RDS")

metareg_mod_BF <- bayesfactor_parameters(metareg_mod, null = c(log(1e-16),log(0.2))) #Effect sizes between 0 and 0.2 





bayesfactor_parameters(mod, null = c(log(1e-16),log(0.2)))

## load in mod from cluster
metareg_modBF <-readRDS("~/CnGV-CoGV-Meta-analysis/Data/model output/metareg_modBF.RDS")
metareg_modBF

effectsize::interpret_bf(exp(metareg_modBF$log_BF), include_value = T)





