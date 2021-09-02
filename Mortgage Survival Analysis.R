library(survival)
library(survminer)
library(readxl)
library(data.table)
library(caret)

MortgageData <- read_excel("MortgageData.xlsx")


MortgageData$start_date_year<- format(MortgageData$start_date, format = "%Y")
MortgageData$start_date <- as.Date(MortgageData$start_date)
MortgageData$Initial_date <- fifelse(MortgageData$vintage == MortgageData$start_date_year,MortgageData$start_date, as.Date(paste0(as.character(MortgageData$start_date_year),'-01-01')))



MortgageData$Length <- difftime(MortgageData$end_date, MortgageData$Initial_date, units = "weeks")/4
# View(MortgageData)

df <- MortgageData

ggplot(df, aes(x=cred_score)) + 
  geom_histogram()

ggplot(df, aes(x=DBT_RATIO)) + 
  geom_histogram()

df$cred_level <- cut(df$cred_score, breaks = c(0,599,699,769,800,1000), labels = c("poor","fair","good","very good","exceptional"))
table(df$cred_level)
#df$cred_score <- as.integer(bin_data(df$cred_score, bins=c(0,599,679,739,789,1000)))
df$debt_level <- cut(df$DBT_RATIO, breaks = c(0,0.25,0.6,2), labels = c("Low Risk","Medium Risk","High Risk"))
table(df$debt_level)

# Survival plot based on credit level
fit.credit <- survfit(Surv(Length,event==1)~ cred_level,data=df)
plot(fit.credit,col=1:5, lty=1:5, main="cred_level",xlab="Time in Months",ylab = "Default Probality")
llabel<-gsub("x=","",names(fit.credit$strata))
legend("bottom",legend=llabel,col=1:5,lty=1:5,bty='n')

prop.fail<- fit.credit$n.event / fit.credit$n.risk
time <- fit.credit$time
time0 <- c(0, time[-length(time)])

haz <- prop.fail/(time - time0)
plot(time, haz, ylim = c(0,0.025), type = "n", xlab = "Months to default", ylab = "h(t)")
lines(lowess(time[-1], haz[-1], f= 0.10))

ggsurvplot(fit.credit, fun = function(y) 1-y, xlab="Time in Years", ylab="Default Probability", 
           color = "strata", palette = c("blue", "red", "black", "green", "cyan"),
           linetype = c("dashed"), risk.table = TRUE,
           tables.theme = theme_cleantable())

survdiff(Surv(Length,(event==1))~cred_level,data=df, na.action = na.omit, rho =0)
# Survival plot based on Debt level
fit.debt <- survfit(Surv(Length,event==1)~ debt_level,data=df)
plot(fit.debt,col=1:5, lty=1:5, main="Debt Level",xlab="Time in Years",ylab = "Default Probality")
llabel<-gsub("x=","",names(fit.debt$strata))
legend("bottom",legend=llabel,col=1:5,lty=1:5,bty='n')

ggsurvplot(fit.debt, fun = function(y) -log(y), xlab="Time in Years",ylab = "Default Probality")

survdiff(Surv(Length,(event==1))~debt_level,data=df, na.action = na.omit, rho =0)

fit.cox<-coxph(Surv(Length,event==1)~ cred_level+debt_level,data=df)
fit.cox
ggforest(fit.cox, data= df)
test2.ph <- cox.zph(fit.cox)
test2.ph


df_r <- na.omit(df)
fit_hf <- fitSmoothHazard(default ~     Length,
                          data = df_r,
                          ratio = 100,
                          time = "Length")
plot(fit_hf)



df_r <- na.omit(df)


#Default Model Forecasting
inTrain <- createDataPartition(y = df_r$default,   # outcome variable
                               p = .70,   # % of training data you want
                               list = F)
# create your partitions
train <- df_r[inTrain,]  # training data set
test <- df_r[-inTrain,]  # test data set

ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = T,  # if you want probabilities
                     summaryFunction = twoClassSummary, # for classification
                     #summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)

train$default <- as.factor(train$default)
levels(train$default) <- make.names(levels(factor(train$default)))

logit <- train(default ~ cred_level + debt_level,    # model specification
               data = train,        # train set used to build model
               method = "glm",      # type of model you want to build
               trControl = ctrl,    # how you want to learn
               family = "binomial", # specify the type of glm
               metric = "ROC"       # performance measure
)
logit

# logit using default
logit_trp <- predict(logit, newdata=train, type='prob')[,1]
logit_trc <- predict(logit, newdata=train)
logit_tep <- predict(logit, newdata=test, type='prob')[,1]
logit_tec <- predict(logit, newdata=test)

(cm <- confusionMatrix(data=logit_trc, train$default))

# Pre-Payment Model Forecasting
inTrain2 <- createDataPartition(y = df_r$Prepayment,   # outcome variable
                               p = .70,   # % of training data you want
                               list = F)
# create your partitions
train2 <- df_r[inTrain2,]  # training data set
test2 <- df_r[-inTrain2,]  # test data set

train2$Prepayment <- as.factor(train2$Prepayment)
levels(train2$Prepayment) <- make.names(levels(factor(train2$Prepayment)))

logit2 <- train(Prepayment ~ cred_level + debt_level,    # model specification
               data = train2,        # train set used to build model
               method = "glm",      # type of model you want to build
               trControl = ctrl,    # how you want to learn
               family = "binomial", # specify the type of glm
               metric = "ROC"       # performance measure
)
logit2

# logit using Pre-payment
logit_trp2 <- predict(logit2, newdata=train2, type='prob')[,1]
logit_trc2 <- predict(logit2, newdata=train2)
logit_tep2 <- predict(logit2, newdata=test2, type='prob')[,1]
logit_tec2 <- predict(logit2, newdata=test2)

(cm <- confusionMatrix(data=logit_trc2, train2$Prepayment))
