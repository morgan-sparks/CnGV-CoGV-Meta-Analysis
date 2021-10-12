############################################################################################################################
# Morgan Sparks, msparks1309@gmail.com, 6/29/2021
# 
# This script plots reaction norms for a visual check of the QA/QC plots for CnGV and CoGV in "effect size calculation.R"
# Figures should not have positive and negative values, they should only be one or the other, positive and negative is indicative
# of GxE response. As such, those with GxE will be removed from data final data in "effect size calculation.R"

# Need to load in raw_dat_cngv, raw_dat_cogv objects from "effect size calculation.R" to run script.
############################################################################################################################
### CnGV visual checks

data_levels <- levels(raw_dat_cngv$Paper..Authors...Year.)

cngv_check <- raw_dat_cngv[which(raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

ggplot(cngv_check) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Paper..Authors...Year. + Trait + Experiment.., scales = "free") +
  theme_classic()

# GxE Papers to remove
# Brown et al. 1998--growth rate weight
# Grether et al. 2005--carotenoid concentration, exp 3
# Lindgren and Laurila 2009--growth rate and size at metamorphosis
# Robinson 2013--growth rate
# Secor et al. 2000--growth rate


############################################################################################################################
### CoGV visual checks
cogv_check <- raw_dat_cogv[which(raw_dat_cogv$Comparison.a.or.b. %in% c("a","b")),]

ggplot(cogv_check) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Paper..Authors...Year. + Trait + Experiment.., scales = "free") +
  theme_classic()
