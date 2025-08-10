library(rstanarm)
options(mc.cores = parallel::detectCores())
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)
library(loo)

conflict <- read.csv("conflict_DATA.csv", header = TRUE)
conflict_1992 <- conflict %>% filter(year >= 1992)

##data transformations ----------------------------------
conflict_1992$tot_pop_log <- log(conflict_1992$tot_pop)
conflict_1992$tot_gdpcap_log <- log(conflict_1992$gdp_cap)
conflict_1992$imr_z <- (conflict_1992$imr - mean(conflict_1992$imr)) / sd(conflict_1992$imr)
conflict_1992$per15_24_z <- (conflict_1992$per15_24 - mean(conflict_1992$per15_24)) / sd(conflict_1992$per15_24)
conflict_1992$ed_attain_z <- (conflict_1992$ed_attain_lwr - mean(conflict_1992$ed_attain_lwr)) / sd(conflict_1992$ed_attain_lwr)
conflict_1992$polity2_abs <- abs(conflict_1992$polity2)
conflict_1992$conflict_occur_yn <- NA
N <- nrow(conflict_1992)
for (i in 1:N) {
  if (conflict_1992$conflict_occur[i] == 1){
    conflict_1992$conflict_occur_yn[i] <- "Yes"
  } else {
    conflict_1992$conflict_occur_yn[i] <- "No"
  }
}

##EDA for variables of interest ----------------------------------
#youth cohort 
youthplot_1 <- ggplot(conflict_1992, aes(x = per15_24, y = conflict_occur_yn)) + 
  geom_jitter(size = 0.2) + xlab('15-24 % of Adult Pop.') + ylab('Conflict Occur')

youthplot_2 <- conflict_1992 %>% 
  mutate(percent_bracket = 
           cut(per15_24, breaks = seq(0, 30, by = 3))) %>% 
  group_by(percent_bracket) %>% 
  summarize(conflict_rate = mean(conflict_occur_yn == "Yes")) %>% 
  ggplot(aes(x = percent_bracket, y = conflict_rate)) + 
  geom_point() + xlab('15-24 % of Adult Pop.') + ylab('Conflict Occur') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

grid.arrange(youthplot, youthplot_2, ncol=2)

#total population (log) 
popplot_1 <- ggplot(conflict_1992, aes(x = tot_pop_log, y = conflict_occur_yn)) + 
  geom_jitter(size = 0.2) + xlab('Total Population (thousands, log)') + ylab('Conflict Occur')

popplot_2 <- conflict_1992 %>% 
  mutate(percent_bracket = 
           cut(tot_pop_log, breaks = seq(0, 20, by = 1))) %>% 
  group_by(percent_bracket) %>% 
  summarize(conflict_rate = mean(conflict_occur_yn == "Yes")) %>% 
  ggplot(aes(x = percent_bracket, y = conflict_rate)) + 
  geom_point() + xlab('Total Population (thousands, log)') + ylab('Conflict Occur') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

grid.arrange(popplot_1, popplot_2, ncol=2)

#educational attainment 
edplot_1 <- ggplot(conflict_1992, aes(x = ed_attain_lwr, y = conflict_occur_yn)) + 
  geom_jitter(size = 0.2) + xlab('Lower Secondary School Comp. Rate, Males') + ylab('Conflict Occur')

edplot_2 <- conflict_1992 %>% 
  mutate(percent_bracket = 
           cut(ed_attain_lwr, breaks = seq(0, 110, by = 10))) %>% 
  group_by(percent_bracket) %>% 
  summarize(conflict_rate = mean(conflict_occur_yn == "Yes")) %>% 
  ggplot(aes(x = percent_bracket, y = conflict_rate)) + 
  geom_point() + xlab('Lower Secondary School Comp. Rate, Males') + ylab('Conflict Occur') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

grid.arrange(edplot_1, edplot_2, ncol=2)

#gdp per capita (log) 
gdpplot_1 <- ggplot(conflict_1992, aes(x = tot_gdpcap_log, y = conflict_occur_yn)) + 
  geom_jitter(size = 0.2) + xlab('GDP per Capita, infl. adj, PPP (log)') + ylab('Conflict Occur')

gdpplot_2 <- conflict_1992 %>% 
  mutate(percent_bracket = 
           cut(tot_gdpcap_log, breaks = seq(0, 20, by = 1))) %>% 
  group_by(percent_bracket) %>% 
  summarize(conflict_rate = mean(conflict_occur_yn == "Yes")) %>% 
  ggplot(aes(x = percent_bracket, y = conflict_rate)) + 
  geom_point() + xlab('GDP per Capita, infl. adj, PPP (log)') + ylab('Conflict Occur') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

grid.arrange(gdpplot_1, gdpplot_2, ncol=2)

#IMR
imrplot_1 <- ggplot(conflict_1992, aes(x = imr, y = conflict_occur_yn)) + 
  geom_jitter(size = 0.2) + xlab('Infant Mortality Rate') + ylab('Conflict Occur')

imrplot_2 <- conflict_1992 %>% 
  mutate(percent_bracket = 
           cut(imr, breaks = seq(0, 250, by = 10))) %>% 
  group_by(percent_bracket) %>% 
  summarize(conflict_rate = mean(conflict_occur_yn == "Yes")) %>% 
  ggplot(aes(x = percent_bracket, y = conflict_rate)) + 
  geom_point() + xlab('Infant Mortality Rate') + ylab('Conflict Occur') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

grid.arrange(imrplot_1, imrplot_2, ncol=2)

#polity score
polyplot_1 <- ggplot(conflict_1992, aes(x = polity2, y = conflict_occur_yn)) + 
  geom_jitter(size = 0.2) + xlab('Polity Score') + ylab('Conflict Occur')

polyplot_2 <- conflict_1992 %>% 
  group_by(polity2) %>% 
  summarize(conflict_rate = mean(conflict_occur_yn == "Yes")) %>% 
  ggplot(aes(x = polity2, y = conflict_rate)) + 
  geom_point() + xlab('Polity Score') + ylab('Conflict Occur') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

grid.arrange(polyplot_1, polyplot_2, ncol=2)

#conflict prior year
conflplot_1 <- ggplot(conflict_1992, aes(x = conflict_prior_year, y = conflict_occur_yn)) + 
  geom_jitter(size = 0.2) + xlab('Conflict, 1-yr Lagged') + ylab('Conflict Occur')

conflplot_2 <- conflict_1992 %>% 
  group_by(conflict_prior_year) %>% 
  summarize(conflict_rate = mean(conflict_occur_yn == "Yes")) %>% 
  ggplot(aes(x = conflict_prior_year, y = conflict_rate)) + 
  geom_point() + xlab('Conflict, 1-yr Lagged') + ylab('Conflict Occur') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

grid.arrange(conflplot_1, conflplot_2, ncol=2)

conflict_1992 %>% group_by(hegre_region) %>% summarise(total_conflicts = sum(num_conflicts)) %>% ggplot() +
  labs(title = "Total Conflicts by Region 1992-2018", x = "Region", y = "Total # Conflicts") +
  scale_fill_brewer(palette = "Set2") +
  geom_bar(aes(x = reorder(hegre_region, -total_conflicts), y = total_conflicts, fill = "orange"), stat = "identity", show.legend = TRUE)


##MODEL 1: Global Model ----------------------------------
model_1 <- stan_glm(conflict_occur ~ tot_pop_log + per15_24_z + conflict_prior_year + ed_attain_z + imr_z + polity2_abs + tot_gdpcap_log, 
                    data = conflict_1992,
                    family=binomial(link="logit"),
                    prior_intercept = normal(-1.7, 1.5),
                    prior = normal(0, 2.5, autoscale = TRUE),
                    chains = 4, iter = 5000*2, seed = 0417)

tidy(model_1, effects = "fixed", conf.int = TRUE, conf.level = 0.80)
summary(model_1, digits = 3)

#diagnostic plots for simulation stability
mcmc_trace(model_1)
mcmc_dens_overlay(model_1)
mcmc_acf(model_1)

#model evaluation
proportion_conflict <- function(x){mean(x == 1)}
pp_check(model_1, nreps = 100,
         plotfun = "stat", stat = "proportion_conflict") + 
  xlab("probability of conflict")

#loo cross-validation
#intercept model (to compare model 1)
prob <- 0.5
loo_intc <- round(log(prob)*sum(conflict_1992$conflict_occur) + log(1-prob)*sum(1-conflict_1992$conflict_occur),1)

#global model
loo_1 <- loo(model_1)


##MODEL 2: MLM Intercept Model ----------------------------------
model_2 <- stan_glmer(
  conflict_occur ~ tot_pop_log + per15_24_z + conflict_prior_year + ed_attain_z + imr_z + polity2_abs + tot_gdpcap_log + (1 | hegre_region), 
  data = conflict_1992, family = binomial(link="logit"),
  prior_intercept = normal(-1.7, 1.5),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 0417
)

tidy(model_2, effects = "fixed", conf.int = TRUE, conf.level = 0.80)
summary(model_2, digits = 3)

#diagnostic plots for simulation stability
mcmc_trace(model_2)
mcmc_dens_overlay(model_2)
mcmc_acf(model_2)

#model evaluation
proportion_conflict <- function(x){mean(x == 1)}
pp_check(model_2, nreps = 100,
         plotfun = "stat", stat = "proportion_conflict") + 
  xlab("probability of conflict")


#mlm-intercept model
loo_2 <- loo(model_2)


##MODEL 3: MLM Intercept-Slope Model ----------------------------------
model_3 <- stan_glmer(
  conflict_occur ~ tot_pop_log + per15_24_z + conflict_prior_year + ed_attain_z + imr_z + polity2_abs + tot_gdpcap_log + (1 + per15_24_z | hegre_region), 
  data = conflict_1992, family = binomial(link="logit"),
  prior_intercept = normal(-1.7, 1.5),
  prior = normal(0, 2.5, autoscale = TRUE), 
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 0417
)

summary(model_3, digits = 3)

#model evaluation
proportion_conflict <- function(x){mean(x == 1)}
pp_check(model_3, nreps = 100,
         plotfun = "stat", stat = "proportion_conflict") + 
  xlab("probability of conflict")


#mlm-intercept model
loo_3 <- loo(model_3)


##Posterior Prediction w/ MODEL 2(MLM Intercept Model) ------------------------
set.seed(0417)
conflict_2019 <- read.csv("conflict_Prediction.csv", header = TRUE)
conflict_2019$tot_pop_log <- log(conflict_2019$tot_pop)
conflict_2019$tot_gdpcap_log <- log(conflict_2019$gdp_cap)
conflict_2019$imr_z <- (conflict_2019$imr - mean(conflict_2019$imr)) / sd(conflict_2019$imr)
conflict_2019$per15_24_z <- (conflict_2019$per15_24 - mean(conflict_2019$per15_24)) / sd(conflict_2019$per15_24)
conflict_2019$ed_attain_z <- (conflict_2019$ed_attain_lwr - mean(conflict_2019$ed_attain_lwr)) / sd(conflict_2019$ed_attain_lwr)
conflict_2019$polity2_abs <- abs(conflict_2019$polity2)

newconflict_prediction <- posterior_predict(model_2, newdata = conflict_2019)

# Summarize the posterior predictions of conflict
colMeans(newconflict_prediction)



