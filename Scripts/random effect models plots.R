library(tidybayes); library(ggridges); library(tidyverse); library(metafor); library(brms); library(forcats)

################################################################################################
#` plots for CnGV

mod_2randeff <- readRDS("~/CnGV-CoGV-Meta-analysis/Data/model_output/mod_norm_logtrans_trait_2randeff_student_sp.rds")
get_variables(mod_2randeff)
### see for example https://github.com/mvuorre/brmstools

########################
# workflow for paper number plot

# posterior summary
posteriors <- exp(posterior_samples(mod_2randeff))

posterior_summary(posteriors[1:5])

# pull out posterior distributions for paper variable + b_Intercept
out_r <- spread_draws(mod_2randeff, r_paper_number[paper_number, term], b_Intercept) %>%
  mutate(b_Intercept = exp(r_paper_number))
out_r$paper_number <- as.character(out_r$paper_number)

# pull out b_Intercept and save it as average
out_f <- spread_draws(mod_2randeff, b_Intercept) %>% 
  mutate(paper_number = "Average")
out_f$b_Intercept <- exp(out_f$b_Intercept)



# bind the former two together
out_all <- bind_rows(out_r, out_f) %>% 
  ungroup() %>% # Ensure that Average effect is on the bottom of the forest plot
  mutate(paper_number = fct_relevel(paper_number, "Average"))

# calculate the mean quantile interval 
# http://mjskay.github.io/tidybayes/articles/tidybayes.html#point-summaries-and-intervals-with-the-point_interval-functions-medianmeanmode_qihdi
out_all_sum <- group_by(out_all,paper_number) %>% 
  mean_qi(b_Intercept)

reorder_object <- c("Average", 1:58)

ggplot(data = out_all_sum, aes(b_Intercept, factor(paper_number, levels = reorder_object)))+
  geom_density_ridges(data = out_all, rel_min_height = 0.01, col = NA, scale = 1, fill = "dodgerblue", alpha = 0.75) +
  geom_pointintervalh( size = 1) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    # Use glue package to combine strings
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward") +
  labs(x = "Intercept", y = "Paper Number") +
  xlim(0,8) +
  theme_classic() +
  ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_paper_randeff_forestplot.pdf", 
         width = 4, height = 6, units = "in", dpi = 300)

########################

# workflow for trait nested in paper number plot
out_r <- spread_draws(mod_2randeff, `r_paper_number:Trait`[paper.number.trait, term], b_Intercept) %>%
  mutate(b_Intercept = exp(`r_paper_number:Trait`))
out_r$paper.number.trait<- as.character(out_r$paper.number.trait)

# pull out b_Intercept and save it as average
out_f <- spread_draws(mod_2randeff, b_Intercept) %>% 
  mutate(paper.number.trait = "Average")
out_f$b_Intercept <- exp(out_f$b_Intercept)


# bind the former two together
out_all <- bind_rows(out_r, out_f) %>%
  ungroup()

out_all$paper.number.trait <- as.factor(out_all$paper.number.trait)

out_all_sum <- group_by(out_all,paper.number.trait) %>% 
  mean_qi(b_Intercept)

ggplot(data = out_all_sum, aes(b_Intercept, paper.number.trait))+
  geom_density_ridges(data = out_all, rel_min_height = 0.01, col = NA, scale = 1, fill = "dodgerblue", alpha = 0.75) +
  geom_pointintervalh(size = 0.5) +
  geom_text(
    data = mutate_if(out_all_sum, is.numeric, round, 2),
    # Use glue package to combine strings
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward", size =2) +
  labs(x = "Intercept", y = "Trait Nested in Paper Number") +
  xlim(0,15) +
  theme_classic(base_size = 8) +
  ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_paperXtrait_randeff_forestplot.pdf", 
         width = 4, height = 8, units = "in", dpi = 300)




################################################################################################

### make with cogradient

mod_2randeff_co <- readRDS("~/CnGV-CoGV-Meta-Analysis/Data/model_output/mod_norm_logtrans_trait_2randeff_student_co.rds")
get_variables(mod_2randeff_co)


########################
# workflow for paper number plot

#### posterior summary
posteriors_co <- exp(posterior_samples(mod_2randeff_co))

posterior_summary(posteriors_co[1:5])

####

out_r_co <- spread_draws(mod_2randeff_co, r_paper_number[paper_number, term], b_Intercept) %>%
  mutate(b_Intercept = exp(r_paper_number))
out_r_co$paper_number <- as.character(out_r_co$paper_number)

out_f_co <- spread_draws(mod_2randeff_co, b_Intercept) %>% 
  mutate(paper_number = "Average")
out_f_co$b_Intercept <- exp(out_f_co$b_Intercept)

out_all_co <- bind_rows(out_r_co, out_f_co) %>% 
  ungroup() %>% # Ensure that Average effect is on the bottom of the forest plot
  mutate(paper_number = fct_relevel(paper_number, "Average"))

out_all_sum_co <- group_by(out_all_co, paper_number) %>% 
  mean_qi(b_Intercept)

reorder_object_co <- c("Average", 1:15)

ggplot(data = out_all_sum_co, aes(b_Intercept, factor(paper_number, levels = reorder_object_co)))+
  geom_density_ridges(data = out_all_co, rel_min_height = 0.01, col = NA, scale = 1, fill = "dodgerblue", alpha = 0.75) +
  geom_pointintervalh( size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(
    data = mutate_if(out_all_sum_co, is.numeric, round, 2),
    # Use glue package to combine strings
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward") +
  labs(x = "Intercept", y = "Paper Number") +
  xlim(0,15) +
  theme_classic() +
  ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_co_paper_randeff_forestplot.pdf", 
         width = 4, height = 6, units = "in", dpi = 300)
`
########################
# workflow for trait nested in paper number plot

out_r_co <- spread_draws(mod_2randeff_co, `r_paper_number:Trait`[paper.number.trait, term], b_Intercept) %>%
  mutate(b_Intercept = exp(`r_paper_number:Trait`))
out_r_co$paper.number.trait <- as.character(out_r_co$paper.number.trait)

out_f_co <- spread_draws(mod_2randeff_co, b_Intercept) %>% 
  mutate(paper.number.trait = "Average")
out_f_co$b_Intercept <- exp(out_f_co$b_Intercept)

out_all_co <- bind_rows(out_r_co, out_f_co) %>% 
  ungroup() 

out_all_sum_co <- group_by(out_all_co, paper.number.trait) %>% 
  mean_qi(b_Intercept)

ggplot(data = out_all_sum_co, aes(b_Intercept, paper.number.trait))+
  geom_density_ridges(data = out_all_co, rel_min_height = 0.01, col = NA, scale = 1, fill = "dodgerblue", alpha = 0.75) +
  geom_pointintervalh( size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(
    data = mutate_if(out_all_sum_co, is.numeric, round, 2),
    # Use glue package to combine strings
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf),
    hjust = "inward") +
  labs(x = "Intercept", y = "Trait nested in Paper Number") +
  xlim(0,15) +
  theme_classic() +
  ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Supplementary Materials/int_mod_co_paperXtrait_randeff_forestplot.pdf", 
         width = 4, height = 6, units = "in", dpi = 300)

################################################################################################
### Distribution plot for cngv and cogv together

out_cn <- spread_draws(mod_2randeff, b_Intercept) %>% 
  mutate(adaptation= "Countergradient")
out_cn$b_Intercept <- exp(out_cn$b_Intercept)

out_co <- spread_draws(mod_2randeff_co, b_Intercept) %>% 
  mutate(adaptation = "Cogradient")
out_co$b_Intercept <- exp(out_co$b_Intercept)

out_both <- bind_rows(out_cn, out_co)


out_both_sum <- bind_rows(out_cn, out_co) %>%
  group_by(adaptation) %>%
  mean_qi(b_Intercept)

out_both_sum_med <- bind_rows(out_cn, out_co) %>%
  group_by(adaptation) %>%
  median_qi(b_Intercept)


fig2 <- ggplot() +
  # geom_density_ridges(rel_min_height = 0.01, col = NA, 
  #                     scale = 1, fill = "dodgerblue", alpha = 0.75) +
  # geom_pointintervalh(data = out_both_sum, size = 4) +
  geom_dots(data = out_both, aes(x = b_Intercept, y = adaptation, color = adaptation)) +
  scale_color_manual(values=c("darkgreen", "darkorchid4")) +
  geom_pointinterval(data = out_both_sum, aes(x= b_Intercept, y = adaptation, xmin = .lower, xmax = .upper), size = 4)+
  geom_point(data = out_both_sum_med, aes(x =b_Intercept, y =adaptation), shape = 18, size = 4, color = "darkgrey") +
geom_text(
    data = mutate_if(out_both_sum, is.numeric, round, 2),
    # Use glue package to combine strings
    aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf, y = adaptation),
    hjust = "inward", nudge_y = 0.33) +
  xlim(0,5) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Effect size", y = NULL) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none",
        axis.text.y = element_text(angle=90, hjust = 0.5))

ggsave("~/Dropbox/PhD Work/Critical Review/Work for Publication/Tables:Figures/Fig. 2.pdf", fig2,
       width = 4, height = 4, units = "in", dpi = 300)


