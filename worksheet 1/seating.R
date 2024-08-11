install.packages("survival")
library(survival)
# Example data
time <- c(5, 6, 6, 2, 4, 4, 8, 8, 10, 12)  # Survival times
status <- c(1, 1, 1, 0, 1, 0, 1, 0, 1, 1)  # Event occurred (1) or censored (0)
surv_object <- Surv(time, status)
km_fit <- survfit(surv_object ~ 1)
summary(km_fit)
plot(km_fit, xlab = "Time", ylab = "Survival Probability", main = "Kaplan-Meier Survival Curve")