# Paqueterías
library(ggplot2)
library(drda)


# Cargar archivo ----
df_fret_200 <- read.csv(file = "C:/Users/HP/Desktop/FRET R SCRIPTS/all_biosensors.csv", 
                        header = TRUE, sep = ",")

dose <- df_fret_200$Treatment
relative_viability <- df_fret_200$Normalized




dose <- rep(c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100), each = 3)
relative_viability <- c(
  0.877362, 0.812841, 0.883113, 0.873494, 0.845769, 0.999422, 0.888961,
  0.735539, 0.842040, 0.518041, 0.519261, 0.501252, 0.253209, 0.083937,
  0.000719, 0.049249, 0.070804, 0.091425, 0.041096, 0.000012, 0.092564
  )

fit_ll4 <- drda(relative_viability ~ dose, mean_function = "loglogistic4")

log_dose <- log(dose)
fit_l4 <- drda(relative_viability ~ log_dose)


test_data <- data.frame(d = dose, x = log_dose, y = relative_viability)

summary(fit_l4)
coef(fit_l4)
sigma(fit_l4)
coef(fit_ll4)
deviance(fit_l4)
vcov(fit_l4)
residuals(fit_l4)
predict(fit_l4)

fit_l2 <- drda(y ~ x, data = test_data, mean_function = "ll2")
anova(fit_l2)

drda(mean)
fit_gz <- drda(y ~ x, data = test_data, mean_function = "lgz")
fit_l4 <- drda(y ~ x, data = test_data, mean_function = "ll4")
anova(fit_l2, fit_gz, fit_l4)

plot(fit_l4)
plot(fit_gz)
plot(fit_ll4)
