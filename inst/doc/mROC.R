## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(predtools)

## -----------------------------------------------------------------------------
data(dev_data)
data(val_data)


## ----echo=FALSE---------------------------------------------------------------
knitr::kable(dev_data[1:7,])

## -----------------------------------------------------------------------------
reg<-glm(y~sex+age+severity+comorbidity,data=dev_data,family=binomial(link="logit"))
summary(reg)

## ----echo=FALSE---------------------------------------------------------------
cfs<-coefficients(reg)
str<-paste0(round(cfs[1],4),"+",paste0(round(cfs[-1],4),"*",names(cfs[-1]),collapse="+"))
str_risk_model<-gsub("+-","-",str,fixed=T)

## -----------------------------------------------------------------------------
pred<-predict.glm(reg, type='response')

library(pROC)

dev_roc<-roc(response=dev_data[,'y'], predictor=pred)

plot(dev_roc)
title("ROC in the development dataset")

## -----------------------------------------------------------------------------
dev_mroc<-mROC(p=pred)

## -----------------------------------------------------------------------------
plot(dev_roc)
lines(dev_mroc, col="red")

## -----------------------------------------------------------------------------
pred<-predict.glm(reg,newdata = val_data, type="response")

summary(pred)

## ----Comments=FALSE-----------------------------------------------------------

val_roc<-roc(response=val_data[,'y'], predictor=pred)

plot(val_roc)


## -----------------------------------------------------------------------------
val_mroc<-mROC(p=pred)

## -----------------------------------------------------------------------------
plot(val_roc)
lines(val_mroc, col="red")

## -----------------------------------------------------------------------------
res<-mROC_inference(val_data[,'y'],pred)

res


