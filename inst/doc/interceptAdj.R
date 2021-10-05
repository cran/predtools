## ---- include = FALSE---------------------------------------------------------

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------

library(predtools)
library(magrittr)
library(dplyr)
library(ggplot2)


## -----------------------------------------------------------------------------

data(dev_data)
data(val_data)


## ----echo=FALSE---------------------------------------------------------------
set.seed(1)
val_data$y_alt <- ifelse(val_data$y == 0, 0,
                         ifelse(runif(n = nrow(val_data)) <= 0.5, 0, 1))

val_data %>%
  select(y, y_alt) %>%
  summary() %>%
  knitr::kable()


## ----echo=FALSE---------------------------------------------------------------

knitr::kable(dev_data[1:7,])


## -----------------------------------------------------------------------------

reg<-glm(y~sex+age+severity+comorbidity,data=dev_data,family=binomial(link="logit"))
summary(reg)


## ----echo=FALSE---------------------------------------------------------------

cfs <- coefficients(reg)
str<-paste0(round(cfs[1],4),"+",paste0(round(cfs[-1],4),"*",names(cfs[-1]),collapse="+"))
str_risk_model <- gsub("+-", "-", str, fixed = T)


## -----------------------------------------------------------------------------

dev_data$pred <- predict.glm(reg, type = 'response')
val_data$pred <- predict.glm(reg, newdata = val_data, type = 'response')

calibration_plot(data = dev_data, obs = "y", pred = "pred", title = "Calibration plot for development data")
calibration_plot(data = val_data, obs = "y_alt", pred = "pred", y_lim = c(0, 0.6),
                 title = "Calibration plot for validation data")


## -----------------------------------------------------------------------------

odds_correction_factor <- odds_adjust(p0 = mean(dev_data$y), p1 = mean(val_data$y_alt), v = var(dev_data$pred))
odds_correction_factor


## -----------------------------------------------------------------------------

dev_data$pred <- predict.glm(reg, type = 'response')
val_data$pred <- predict.glm(reg, newdata = val_data, type = 'response')

val_data$odds_adj <- (val_data$pred / (1 - val_data$pred)) * odds_correction_factor
val_data$pred_adj <- val_data$odds_adj / (1 + val_data$odds_adj)

val_data$id <- c(1 : nrow(val_data))
val_data_long <- reshape(data = val_data, direction = "long", varying = c("pred", "pred_adj"), v.name = "preds",
                         idvar = "id", timevar = "Method", times = c("Premitive", "Adjusted"))

calibration_plot(data = val_data, obs = "y_alt", pred = "pred_adj",
                 title = "Calibration plot for development data - after recalibration")
calibration_plot(data = val_data_long, obs = "y_alt", pred = "preds", group = "Method",
                 title = "Calibration plot for development data - before and after recalibration")

