library(bayesplot);library(tidybayes); library(brms)
mod_2randeff <- readRDS("~/Desktop/brmstest results/mod_norm_logtrans_trait_2randeff.rds")

summary(mod_2randeff)


#print exponentiated results
posteriors <- exp(posterior_samples(mod_2randeff))

posterior_summary(posteriors[1:5])

#plot check
plot(mod_2randeff)

plot(posteriors [1:5])

prior_summary(mod_2randeff)

pairs(mod_2randeff)

pp_check(mod_2randeff)

#check some divergence issues: 
lp_cp <- log_posterior(mod_2randeff)
np_cp <- nuts_params(mod_2randeff)

posterior_cp <- as.array(mod_2randeff)

mcmc_parcoord(posterior_cp, np = np_cp)

mcmc_pairs(posterior_cp, np = np_cp)

mcmc_scatter(posterior_cp, np = np_cp, pars = c("sd_Paper.Name:Trait__Intercept", "sd_Species__Intercept"))

mcmc_nuts_divergence(np_cp, lp_cp)
### same with weak priors

mod_wp_2randeff <- readRDS("~/Desktop/brmstest results/mod_norm_logtrans_trait_weakprior_2randeff.rds")

mod_wp_2randeff

#### meta reg model
mod_metareg <- readRDS("~/Desktop/brmstest results/mod_metareg.RDS")

mod_metareg

### meta reg with weak priors
mod_metareg_weakprior <- readRDS("~/Desktop/brmstest results/mod_metareg_weakpriors.RDS")

mod_metareg_weakprior

#### meta reg with strong priors

mod_metareg_stongprior <- readRDS("~/Desktop/brmstest results/mod_metareg_sp.RDS")
mod_metareg_stongprior

posteriors_sp <- exp(posterior_samples(mod_metareg_stongprior))

posterior_summary(posteriors_sp[1:20])
