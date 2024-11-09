# load libraries
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(HDInterval)
library(cowplot)
library(patchwork)
library(rio)
library(mcmcse)
library(loo) # for WAIC and LOOIC calculations
library(ggdist)

# fitting the model -------------------------------------------------
# load data
data <- rio::import("data/happiness.csv")
# run data_manipulation.R
source("code/data_manipulation.R")

df_x <- data %>% dplyr::select(-score, -country, -continent)


# TODO: Standardise the data?
# compile stan model
model <- cmdstan_model("model/model_mterms.stan")
model_cauchy <- cmdstan_model("model/model_mterms_cauchy.stan")

# prepare data from stan --------------------------------------------

# Economy only ######################################################
df_x1 <- data %>% dplyr::select(economy)
# add a column of 1s for the intercept
df_x1$intercept <- 1
# order colmns so intercept is first
df_x1 <- df_x1[, c(2, 1)]
stan_data1 <- list(
  n = dim(df_x1)[1],
  m = dim(df_x1)[2],
  X = df_x1,
  y = data$score
)

# Perceived corruption only ##########################################
df_x2 <- data %>% dplyr::select(perceived_corruption)
# add a column of 1s for the intercept
df_x2$intercept <- 1
# order colmns so intercept is first
df_x2 <- df_x2[, c(2, 1)]
stan_data2 <- list(
  n = dim(df_x2)[1],
  m = dim(df_x2)[2],
  X = df_x2,
  y = data$score
)

# Economy and perceived corruption ###################################
df_x3 <- data %>% dplyr::select(economy, perceived_corruption)
# add a column of 1s for the intercept
df_x3$intercept <- 1
# order colmns so intercept is first
df_x3 <- df_x3[, c(3, 1, 2)]
stan_data3 <- list(
  n = dim(df_x3)[1],
  m = dim(df_x3)[2],
  y = data$score,
  X = df_x3
)

# Continent models ###################################################
df_x_cont <- data %>% dplyr::select(
  -score, -country, -continent, -year, -latitude, -longitude
)

# Continent Intercept ################################################
df_x4 <- df_x_cont
stan_data4 <- list(
  n = dim(df_x4)[1],
  m = dim(df_x4)[2],
  X = df_x4,
  y = data$score
)

# Continent Econ Slope ###############################################
df_x5 <- df_x_cont
df_x5$europe_econ <- df_x5$economy * df_x5$continent_Europe
df_x5$asia_econ <- df_x5$economy * df_x5$continent_Asia
df_x5$africa_econ <- df_x5$economy * df_x5$continent_Africa
df_x5$americas_econ <- df_x5$economy * df_x5$continent_Americas
df_x5$oceania_econ <- df_x5$economy * df_x5$continent_Oceania
# remove ecoomy column as we are using all the continents
df_x5 <- df_x5 %>% dplyr::select(-economy)
stan_data5 <- list(
  n = dim(df_x5)[1],
  m = dim(df_x5)[2],
  X = df_x5,
  y = data$score
)

# Continent Econ and Perceived Corruption Slope ######################
df_x6 <- df_x5
df_x6$europe_corup <- df_x6$perceived_corruption * df_x6$continent_Europe
df_x6$asia_corup <- df_x6$perceived_corruption * df_x6$continent_Asia
df_x6$africa_corup <- df_x6$perceived_corruption * df_x6$continent_Africa
df_x6$americas_corup <- df_x6$perceived_corruption * df_x6$continent_Americas
df_x6$oceania_corup <- df_x6$perceived_corruption * df_x6$continent_Oceania
# remove perceived_corruption column as we are using all the continents
df_x6 <- df_x6 %>% dplyr::select(-perceived_corruption)
stan_data6 <- list(
  n = dim(df_x6)[1],
  m = dim(df_x6)[2],
  X = df_x6,
  y = data$score
)

# fit model
fit1 <- model$sample(
  data = stan_data1,
  parallel_chains = 4,
)

fit1cauchy <- model_cauchy$sample(
  data = stan_data1,
  parallel_chains = 4,
)

fit2 <- model$sample(
  data = stan_data2,
  parallel_chains = 4,
)

fit2cauchy <- model_cauchy$sample(
  data = stan_data2,
  parallel_chains = 4,
)

fit3 <- model$sample(
  data = stan_data3,
  parallel_chains = 4,
)

fit3cauchy <- model_cauchy$sample(
  data = stan_data3,
  parallel_chains = 4,
)

fit4 <- model$sample(
  data = stan_data4,
  parallel_chains = 4,
)

fit4cauchy <- model_cauchy$sample(
  data = stan_data4,
  parallel_chains = 4,
)

fit5 <- model$sample(
  data = stan_data5,
  parallel_chains = 4,
  max_treedepth = 15
)

fit5cauchy <- model_cauchy$sample(
  data = stan_data5,
  parallel_chains = 4,
  max_treedepth = 15
)

fit6 <- model$sample(
  data = stan_data6,
  parallel_chains = 4,
  max_treedepth = 15
)

fit6cauchy <- model_cauchy$sample(
  data = stan_data6,
  parallel_chains = 4,
  max_treedepth = 15
)

# diagnostics -------------------------------------------------------
# create diagnostics function
diagnostics <- function(fit) {
  # trace plots
  p <- mcmc_trace(fit$draws(c("b", "sigma"))) +
    theme_bw() +
    theme(legend.position = "none")
  # summary statistics
  stats <- fit$summary(c("b", "sigma"))
  fit$cmdstan_diagnose()

  list(p, stats)
}
diagnostics(fit1)
diagnostics(fit2)
diagnostics(fit3)
diagnostics(fit4)
diagnostics(fit5)
diagnostics(fit6)

# plotting setup
plot_theme <- theme(
  plot.title = element_text(size = 20, face = "bold"),
  strip.text = element_text(size = 15),
  axis.text = element_text(size = 15),
  axis.title = element_text(size = 15),
  legend.title = element_text(size = 15),
  legend.text = element_text(size = 12)
)

# Analysis --------------------------------------------------
# samples
# ...

# Compare models --------------------------------------------------

# LOOIC ------------------------------------------------------------------------
df_looic <- data.frame(
  looic = numeric(),
  SE = numeric(),
  model = factor(),
  distribution = factor()
)
log_lik <- fit1$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "economy_only",
  distribution = "Normal"
))

log_lik <- fit1cauchy$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "economy_only",
  distribution = "Cauchy"
))

log_lik <- fit2$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "perceived_corruption_only",
  distribution = "Normal"
))

log_lik <- fit2cauchy$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "perceived_corruption_only",
  distribution = "Cauchy"
))

log_lik <- fit3$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "economy_and_perceived_corruption",
  distribution = "Normal"
))

log_lik <- fit3cauchy$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "economy_and_perceived_corruption",
  distribution = "Cauchy"
))

log_lik <- fit4$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "continent_economy_perceived_corruption",
  distribution = "Normal"
))

log_lik <- fit4cauchy$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "continent_economy_perceived_corruption",
  distribution = "Cauchy"
))

log_lik <- fit5$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "interaction_continent_economy",
  distribution = "Normal"
))

log_lik <- fit5cauchy$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "interaction_continent_economy",
  distribution = "Cauchy"
))

log_lik <- fit6$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "interaction_continent_economy_perceived_corruption",
  distribution = "Normal"
))

log_lik <- fit6cauchy$draws(c("log_lik"))
r_eff <- relative_eff(log_lik)
loo <- loo(log_lik, r_eff = r_eff)
df_looic <- rbind(df_looic, data.frame(
  looic = loo$estimates[3, 1],
  SE = loo$estimates[3, 2],
  model = "interaction_continent_economy_perceived_corruption",
  distribution = "Cauchy"
))

# calculate_looic <- function(model, distribution) {
#   log_lik <- model$draws(c("log_lik"))
#   r_eff <- relative_eff(log_lik)
#   loo <- loo(log_lik, r_eff = r_eff)
#   df_looic <- rbind(df_looic, data.frame(
#     looic = loo$estimates[3, 1],
#     SE = loo$estimates[3, 2],
#     model = model$name,
#     distribution = distribution
#   ))
# }

# # Usage example:
# calculate_looic(fit6, "Normal")
# calculate_looic(fit6cauchy, "Cauchy")

ggplot(data = df_looic, aes(x = looic, y = model, fill = distribution)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75)) +
  geom_errorbar(
    aes(xmin = looic - SE, xmax = looic + SE),
    width = 0.2,
    position = position_dodge(width = 0.75)
  ) +
  theme_bw() +
  labs(
    title = "Model Fit Comparison",
    x = "LOOIC",
    y = "Model"
  ) +
  scale_fill_manual(values = c("steelblue", "coral2")) +
  plot_theme

# LOOIC weights ------------------------------------------------------------
# Pick only the normal models
# df_looic <- df_looic %>%
# filter(distribution == "Normal")

# pick only the models with a looic within 2 SE of the best model
df_looic <- df_looic %>%
  filter(looic < min(looic) + 2 * SE)

# TODO: filter out really shit models
# calculate looic weights
# calculate delta_looic
df_looic$delta_looic <- abs(df_looic$looic - min(df_looic$looic))

# calculate weights
df_looic$weight <-
  exp(-0.5 * df_looic$delta_looic) / sum(exp(-0.5 * df_looic$delta_looic))
df_looic$weight <- round(df_looic$weight, 2)

# calculate worst and best case scenario for each model in terms of +/- SE
# worst case: looic + SE for the choosen model, looic - SE for the rest
# best case: looic - SE for the choosen model, looic + SE for the rest
df_looic$weight_plus_se <- 0
df_looic$weight_minus_se <- 0
for (i in seq_len(nrow(df_looic))) {
  # worst case
  # best variant of other models
  looic_plus_se <- df_looic$looic - df_looic$SE
  # worst of current model
  looic_plus_se[i] <- df_looic$looic[i] + df_looic$SE[i]
  # weights
  delta_looic <- abs(looic_plus_se - min(looic_plus_se))
  weights <- exp(-0.5 * delta_looic) / sum(exp(-0.5 * delta_looic))
  df_looic$weight_plus_se[i] <- round(weights[i], 2)

  # best case
  # worst variant of other models
  looic_minuse_se <- df_looic$looic + df_looic$SE
  # best of current model
  looic_minuse_se[i] <- df_looic$looic[i] - df_looic$SE[i]
  # weights
  delta_looic <- abs(looic_minuse_se - min(looic_minuse_se))
  weights <- exp(-0.5 * delta_looic) / sum(exp(-0.5 * delta_looic))
  df_looic$weight_minus_se[i] <- round(weights[i], 2)
}
# plot looic weights
ggplot(data = df_looic, aes(x = weight, y = model)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(
    aes(xmin = weight_minus_se, xmax = weight_plus_se),
    width = 0.2
  ) +
  theme_bw() +
  labs(
    title = "LOOIC Weights",
    x = "Weight",
    y = "Model"
  ) +
  plot_theme

# Beta plot ---------------------------------------------------------------
# for best model - lowest looic
# get the draws
# get the mean of the draws
df_beta <- as_draws_df(fit6$draws("b"))
df_beta <- df_beta %>% dplyr::select(-.chain, -.iteration, -.draw)
# re-name the columns
colnames(df_beta) <- colnames(df_x6)
df_beta <- df_beta %>% gather(Beta, Value)
# plot coefficients in stat-eye plot
order_list <- c("Beta1", "Beta2", "Beta3", "Beta4") # Replace with your desired order

ggplot(data = df_beta, aes(x = Value, y = factor(Beta, levels = order_list))) +
  stat_eye(alpha = 0.75, aes(fill = Beta), show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(c(-5, 10)) +
  ggtitle("Betas") +
  theme_bw() +
  plot_theme

# plot just the slope coefficients
df_beta_slope <- df_beta %>%
  # cannot begin with continent
  filter(!grepl("continent", Beta))

# plot coefficients for slope parameters
ggplot(data = df_beta_slope, aes(x = Value, y = Beta)) +
  stat_eye(alpha = 0.75, aes(fill = Beta), show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(c(-5, 5)) +
  ggtitle("Betas") +
  theme_bw() +
  plot_theme

# plot only america parameters (case-insensitive)
df_beta_americas <- df_beta %>%
  filter(grepl("americas", Beta, ignore.case = TRUE))

# plot america coefficients
ggplot(data = df_beta_americas, aes(x = Value, y = Beta)) +
  stat_eye(alpha = 0.75, aes(fill = Beta), show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(c(-4, 8)) +
  scale_x_continuous(breaks = seq(-5, 9, by = 1)) + # Specify x-axis ticks
  ggtitle("America Betas") +
  theme_bw() +
  plot_theme

# means of america coefficients
df_beta_americas %>%
  group_by(Beta) %>%
  summarise(mean(Value))

# Probability calculations
df_beta <- as_draws_df(fit6$draws("b"))
df_beta <- df_beta %>% dplyr::select(-.chain, -.iteration, -.draw)
# re-name the columns
colnames(df_beta) <- colnames(df_x6)
# get the draws for america economy
df_beta_americas <- df_beta %>%
  dplyr::select("americas_econ") %>%
  pull()

mcse(df_beta_americas > 0)

# get the draws for america perceived_corruption
df_beta_americas_corrup <- df_beta %>%
  dplyr::select("americas_corup") %>%
  pull()

mcse(df_beta_americas_corrup < 0)

mcse(abs(df_beta_americas) > abs(df_beta_americas_corrup))
