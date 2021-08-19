############################################################################################################################
# Morgan Sparks, msparks1309@gmail.com, 6/29/2021
# 
# This script plots reaction norms for a visual check of the QA/QC plots for CnGV and CoGV in "effect size calculation.R"
# Figures should not have positive and negative values, they should only be one or the other, positive and negative is indicative
# of GxE response. As such, those with GxE will be removed from data final data in "effect size calculation.R"

# Need to load in raw_dat_cngv, raw_dat_cogv objects from "effect size calculation.R" to run script.
############################################################################################################################
### CnGV visual checks

data_levels <- levels(raw_dat_cngv$Paper.Name)

# 1 --> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[7] & raw_dat_cngv$Trait == "developmental rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

# 2 --> GxE
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[8] & raw_dat_cngv$Trait == "growth rate dry mass" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#3 ---> GxE
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[13] & raw_dat_cngv$Trait == "growth rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#4 ---> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[17] & raw_dat_cngv$Trait == "growth rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#5 ---> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[23] & raw_dat_cngv$Trait == "digestion rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#6 ---> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[23] & raw_dat_cngv$Trait == "growth rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#7 ---> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[25] & raw_dat_cngv$Trait == "growth rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#8 ---> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[25] & raw_dat_cngv$Trait == "developmental rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#9 ---> exp 1 good, exp 2 GxE
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[25] & raw_dat_cngv$Trait == "mass" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()


#10 ---> GxE
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[34] & raw_dat_cngv$Trait == "Incubation time" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#11 ---> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[35] & raw_dat_cngv$Trait == "growth rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#12 ---> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[38] & raw_dat_cngv$Trait == "ciliary activity" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#13 ---> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[64] & raw_dat_cngv$Trait == "digestion rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

#14 ---> good
test_dat <- raw_dat_cngv[which(raw_dat_cngv$Paper.Name == data_levels[65] & raw_dat_cngv$Trait == "growth rate" & raw_dat_cngv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()

############################################################################################################################
### CoGV visual checks

data_levels <- levels(droplevels(raw_dat_cogv$Paper.Name))

#14 ---> good (just repeat data, removed)
test_dat <- raw_dat_cogv[which(raw_dat_cogv$Paper.Name == data_levels[8] & raw_dat_cogv$Trait == "shell thickness" & raw_dat_cogv$Comparison.a.or.b. %in% c("a","b")),]

test_dat 

ggplot(test_dat[1:4,]) +
  geom_point(aes(x = Treatment, y = Value, color = Comparison.a.or.b.)) +
  geom_line(aes(x = Treatment, y = Value, color = Comparison.a.or.b., group = Comparison.a.or.b.)) +
  facet_wrap(~Experiment..) +
  theme_classic()
