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

knitr::kable(dev_data[1:7,])


## -----------------------------------------------------------------------------

reg <- glm(y~sex+age+severity+comorbidity,data=dev_data,family=binomial(link="logit"))
summary(reg)


## ----echo=FALSE---------------------------------------------------------------

cfs <- coefficients(reg)
str<-paste0(round(cfs[1],4),"+",paste0(round(cfs[-1],4),"*",names(cfs[-1]),collapse="+"))
str_risk_model <- gsub("+-", "-", str, fixed = T)


## -----------------------------------------------------------------------------

dev_data$pred <- predict.glm(reg, type = 'response')
val_data$pred <- predict.glm(reg, newdata = val_data, type = 'response')

calibration_plot(data = dev_data, obs = "y", pred = "pred", title = "Calibration plot for development data")
calibration_plot(data = val_data, obs = "y", pred = "pred", y_lim = c(0, 0.6),
                 title = "Calibration plot for validation data", group = "sex")


