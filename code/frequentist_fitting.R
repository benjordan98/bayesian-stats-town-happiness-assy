# data_test <- rio::import("C:\\Users\\benjo\\Documents\\Git\\bayesian_statistics_assignments\\bs_assignment_4\\data\\world.csv")
data_test <- rio::import("data/world.csv")

lm_econ_only <- lm(score ~ economy, data = data_test)
lm_corrup_only <- lm(score ~ perceived_corruption, data = data_test)
lm_corrup_econ <- lm(score ~ perceived_corruption + economy, data = data_test)
lm_corrup_econ_cont <- lm(score ~ perceived_corruption + economy + continent, data = data_test)
lm_corrup_econ_cont2 <- lm(score ~ perceived_corruption + continent:economy + continent, data = data_test)
lm_corrup_econ_cont2_alt <- lm(score ~ perceived_corruption + continent / economy, data = data_test)
lm_corrup_econ_cont3 <- lm(score ~ perceived_corruption:continent + economy:continent, data = data_test)
lm_corrup_econ_cont3b <- lm(score ~ economy + perceived_corruption + perceived_corruption:continent + economy:continent, data = data_test)
lm_corrup_econ_cont4 <- lm(score ~ -1 + perceived_corruption:continent + economy:continent + continent, data = data_test)
lm_corrup_econ_cont4_year <- lm(score ~ perceived_corruption:continent + economy:continent + continent + year, data = data_test)
lm_lm_corrup_econ_cont_year <- lm(score ~ perceived_corruption:continent + economy:continent + year, data = data_test)

# AIC of each model
AIC(lm_corrup_econ_cont2)
AIC(lm_corrup_econ_cont3)
AIC(lm_corrup_econ_cont4)
AIC(lm_lm_corrup_econ_cont_year)
AIC(lm_corrup_econ_cont4_year)

BIC(lm_corrup_econ_cont2)
BIC(lm_corrup_econ_cont3)
BIC(lm_corrup_econ_cont4)
BIC(lm_lm_corrup_econ_cont_year)
BIC(lm_corrup_econ_cont4_year)

# reproducing following with one hot encoded data
lm_corrup_econ_cont2_alt <- lm(score ~ perceived_corruption + continent / economy, data = data_test)
#
lm_2_encoeded <- lm(
    score ~ perceived_corruption + economy * continent_Europe + economy * continent_Asia
        + economy * continent_Americas + economy * continent_Oceania + economy * continent_Africa
        - economy,
    data = data_test
)
summary(lm_2_encoeded)
summary(lm_corrup_econ_cont2_alt)
