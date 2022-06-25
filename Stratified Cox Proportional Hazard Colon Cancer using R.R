data=read.csv("D:/ANSUR EAS/dataansur.csv",sep=";")
head(data)
library(survival)

#respon
Y=Surv(data$time,data$status)

#Deskripsi
kmfit <- survfit(Y~1)
kmfit
plot(kmfit)
title("Karakteristik Data")

#Kaplan Meier
kmfit1 <- survfit(Y~data$rx)
kmfit2 <- survfit(Y~data$sex)
kmfit3 <- survfit(Y~data$age)
kmfit4 <- survfit(Y~data$extent)
kmfit5 <- survfit(Y~data$surg)
plot(kmfit1, lty='solid', col=c("black", "purple", "green"), xlab="survival time in days", 
     ylab="survival probabilities")
legend("bottomleft", c("Lev", "Lev+5FU", "Obs"), lty='solid',
       col=c("black", "purple", "green"),cex=0.5)
title("Kaplan-Meier Curve: Rx")
plot(kmfit2, lty='solid', col=c("red", "blue"), xlab="survival time in days", 
     ylab="survival probabilities")
legend("bottomleft", c("Female", "Male"), lty='solid',
       col=c("red", "blue"),cex=0.5)
title("Kaplan-Meier Curve: Sex")
plot(kmfit3, lty='solid', col=c("navy", "orange"), xlab="survival time in days", 
     ylab="survival probabilities")
legend("bottomleft", c("<50 tahun", ">50 tahun"), lty='solid',
       col=c("navy", "orange"),cex=0.5)
title("Kaplan-Meier Curve: Age")
plot(kmfit4, lty='solid', col=c("magenta", "gold", "black", "navy"), 
     xlab="survival time in days", ylab="survival probabilities")
legend("bottomleft", c("Submucosa", "Muscle", "Serosa", "Contiguous Structure"),
       lty='solid', col=c("magenta", "gold", "black", "navy"),cex=0.3)
title("Kaplan-Meier Curve: Extent")
plot(kmfit5, lty='solid', col=c("purple", "brown"), xlab="survival time in days", 
     ylab="survival probabilities")
legend("bottomleft", c("Short", "Long"), lty='solid',
       col=c("purple", "brown"),cex=0.5)
title("Kaplan-Meier Curve: Surg")

#log rank
survdiff(Surv(time,status)~rx, data=data)
survdiff(Surv(time,status)~sex, data=data)
survdiff(Surv(time,status)~age, data=data)
survdiff(Surv(time,status)~extent, data=data)
survdiff(Surv(time,status)~surg, data=data)

#ph assump grafik
par(mfrow=c(2,3))
kmfit2=survfit(Y~data$sex)
plot(kmfit2, fun="cloglog", xlab="time in day using log scale",
     ylab="log-log survival")
title("variabel sex", adj=0, line=0.25)
kmfit=survfit(Y~data$rx)
plot(kmfit, fun="cloglog", xlab="time in day using log scale",
     ylab="log-log survival")
title("variabel rx", adj=0, line=0.25)
kmfit2=survfit(Y~data$age)
plot(kmfit2, fun="cloglog", xlab="time in day using log scale",
     ylab="log-log survival")
title("variabel age", adj=0, line=0.25)
kmfit2=survfit(Y~data$extent)
plot(kmfit2, fun="cloglog", xlab="time in day using log scale",
     ylab="log-log survival")
title("variabel extent", adj=0, line=0.25)
kmfit2=survfit(Y~data$surg)
plot(kmfit2, fun="cloglog", xlab="time in day using log scale",
     ylab="log-log survival")
title("variabel surg", adj=0, line=0.25)
par(mfrow=c(1,1))

#ph asumsi uji gof
model=coxph(Y~sex+rx+age+extent+surg, data=data)
cox.zph(model,transform=rank)
model=coxph(Y~age++rx+extent+surg, data=data)
cox.zph(model,transform=rank)

model1=coxph(Y~age+rx+extent+surg+strata(sex), data=data)
summary(model1)
model2=coxph(Y~rx+extent+surg+strata(sex), data=data)
summary(model2)

#stratified +nointeraction
modelsn=coxph(Y~rx+extent+surg+strata(sex), data=data)
summary(modelsn)

#strata+interaction
modelsi=coxph(Y~(rx+extent+surg)*sex-sex+strata(sex), data=data)
summary(modelsi)

#uji model
LRT=(-2)*(modelsn$loglik[2]-modelsi$loglik[2])
Pvalue = 1 - pchisq(LRT, 3)
Pvalue
LRT

LRT=((-2)*(modelsn$loglik[2]))-(-2*modelsi$loglik[2])
Pvalue = 1 - pchisq(LRT, 3)
Pvalue
LRT
