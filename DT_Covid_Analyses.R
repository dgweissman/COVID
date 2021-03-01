library(stats)
library(ggplot2)
library(patchwork)
library(lm.beta)
load("~/OneDrive - Harvard University/Research/Postdoc/DT/COVID/Data/DT_Covid.RData")

#Change in internalizing from W1 to W2
t.test(dt.covid$SDQC_INTERNALIZING_t2,dt.covid$SDQ_C_INT,paired=T)

#Number of Pandemic-related Stressors
splot1<-ggplot(dt.covid,aes(x=COVID_GLOBAL_STRESS_TOT))+geom_histogram(binwidth = 1,colour='black', fill='gray')+xlab("Pandemic-related Stressors")+ylab("Count")+theme_bw()+ggtitle("Wave 1")+scale_x_continuous(breaks=0:9)+ylim(0,50)
splot2<-ggplot(dt.covid,aes(x=COVID_GLOBAL_STRESS_TOT2))+geom_histogram(binwidth = 1,colour='black', fill='gray')+xlab("Pandemic-related Stressors")+ylab("Count")+theme_bw()+ggtitle("Wave 2")+scale_x_continuous(breaks=0:9)+ylim(0,50)
splot1+splot2
#Pandemic-related Stress
cor(dt.covid$SDQ_C_INT, dt.covid$COVID_GLOBAL_STRESS_TOT,use="complete.obs")

summary(lm(SDQ_C_INT ~ COVID_GLOBAL_STRESS_TOT + YINTERNAL + SEX + Covid_Age, data = dt.covid))
lm.beta(lm(SDQ_C_INT ~ COVID_GLOBAL_STRESS_TOT + YINTERNAL + SEX + Covid_Age, data = dt.covid))

isplot1<-ggplot(dt.covid,aes(x=COVID_GLOBAL_STRESS_TOT,y=SDQ_C_INT))
isplot1<-isplot1+geom_jitter(width=.25,height=.25)+theme_classic()+xlab("Pandemic-related Stressors")+ylab("Internalizing Problems")+geom_smooth(method='lm')+scale_x_continuous(breaks=c(0,2,4,6,8))

#Pandemic-related Stress (Wave 2)
cor(dt.covid$SDQC_INTERNALIZING_t2, dt.covid$COVID_GLOBAL_STRESS_TOT2,use="complete.obs")

summary(lm(SDQC_INTERNALIZING_t2 ~ COVID_GLOBAL_STRESS_TOT2 + SDQ_C_INT + SEX + Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ COVID_GLOBAL_STRESS_TOT2 + SDQ_C_INT + SEX + Covid_Age2, data = dt.covid))

isplot2<-ggplot(dt.covid,aes(x=COVID_GLOBAL_STRESS_TOT2,y=SDQC_INTERNALIZING_t2))
isplot2<-isplot2+geom_jitter(width=.25,height=.25)+theme_classic()+xlab("Pandemic-related Stressors")+ylab("Internalizing Problems")+geom_smooth(method='lm')+scale_x_continuous(breaks=c(0,2,4,6,8))
isplot1+isplot2


#Amygdala Reactivity
fit1<-summary(lm(SDQ_C_INT ~ LAmyg.Fear_GT_Calm + SEX + YINTERNAL+Covid_Age, data = dt.covid))
lm.beta(lm(SDQ_C_INT ~ LAmyg.Fear_GT_Calm + SEX + YINTERNAL+Covid_Age, data = dt.covid))
fit2<-summary(lm(SDQ_C_INT ~ RAmyg.Fear_GT_Calm + SEX + YINTERNAL+Covid_Age, data = dt.covid))
lm.beta(lm(SDQ_C_INT ~ RAmyg.Fear_GT_Calm + SEX + YINTERNAL+Covid_Age, data = dt.covid))
fit1
fit2
pvals<-c(fit1$coefficients[2,4],fit2$coefficients[2,4])
p.adjust(pvals,method="fdr",n=2)

summary(lm(SDQ_C_INT ~ LAmyg.Fear_GT_Scramble + SEX + YINTERNAL+Covid_Age, data = dt.covid))
summary(lm(SDQ_C_INT ~ LAmyg.Calm_GT_Scramble + SEX + YINTERNAL+Covid_Age, data = dt.covid))
laaplot<-ggplot(dt.covid,aes(x=LAmyg.Fear_GT_Calm,y=SDQ_C_INT))
laaplot1<-laaplot+geom_jitter(height=.3,width=0,col=1)+theme_classic()+xlab("Left Amygdala Activation")+ylab("Internalizing Problems")+geom_smooth(method='lm',col=1)+theme(axis.title = element_text(size=25),axis.text=element_text(size=20))
laaplot2<-laaplot+theme_classic()+xlab("Left Amygdala Activation")+ylab("Internalizing Problems")+geom_jitter(height=.3,width=0,aes(x=LAmyg.Fear_GT_Scramble,y=SDQ_C_INT),col=5)+geom_smooth(method='lm',aes(x=LAmyg.Fear_GT_Scramble,y=SDQ_C_INT),col=5)+geom_jitter(height=.3,width=0,aes(x=LAmyg.Calm_GT_Scramble,y=SDQ_C_INT),col=6)+geom_smooth(method='lm',aes(x=LAmyg.Calm_GT_Scramble,y=SDQ_C_INT),col=6)+theme(axis.title = element_text(size=25),axis.text=element_text(size=20))
laaplot1+laaplot2

#Amygdala Reactivity and wave 2 Internalizing
fit1<-summary(lm(SDQC_INTERNALIZING_t2 ~ LAmyg.Fear_GT_Calm + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
fit2<-summary(lm(SDQC_INTERNALIZING_t2 ~ RAmyg.Fear_GT_Calm + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ LAmyg.Fear_GT_Calm + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ RAmyg.Fear_GT_Calm + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
fit1
fit2
pvals<-c(fit1$coefficients[2,4],fit2$coefficients[2,4])
p.adjust(pvals,method="fdr",n=2)

#Amygdala and Hippocampal Volume
fit1<-summary(lm(SDQ_C_INT ~ Amygdala + SEX + YINTERNAL+IntraCranialVol+Covid_Age, data = dt.covid))
fit2<-summary(lm(SDQ_C_INT ~ Hippocampus + SEX + YINTERNAL+IntraCranialVol+Covid_Age, data = dt.covid))
fit1
lm.beta(lm(SDQ_C_INT ~ Amygdala + SEX + YINTERNAL+IntraCranialVol+Covid_Age, data = dt.covid))
fit2
lm.beta(lm(SDQ_C_INT ~ Hippocampus + SEX + YINTERNAL+IntraCranialVol+Covid_Age, data = dt.covid))
pvals<-c(fit1$coefficients[2,4],fit2$coefficients[2,4])
p.adjust(pvals,method="fdr",n=2)

#Amygdala and Hippocampal Volume and wave 2 internalizing
fit1<-summary(lm(SDQC_INTERNALIZING_t2 ~ Amygdala + SEX + YINTERNAL+IntraCranialVol+Covid_Age2, data = dt.covid))
fit2<-summary(lm(SDQC_INTERNALIZING_t2 ~ Hippocampus + SEX + YINTERNAL+IntraCranialVol+Covid_Age2, data = dt.covid))
fit1
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ Amygdala + SEX + YINTERNAL+IntraCranialVol+Covid_Age2, data = dt.covid))
fit2
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ Hippocampus + SEX + YINTERNAL+IntraCranialVol+Covid_Age2, data = dt.covid))
pvals<-c(fit1$coefficients[2,4],fit2$coefficients[2,4])
p.adjust(pvals,method="fdr",n=2)

#Rumination
summary(lm(SDQ_C_INT ~ CRSQ_RUM + SEX + YINTERNAL+Covid_Age, data = dt.covid))
lm.beta(lm(SDQ_C_INT ~ CRSQ_RUM + SEX + YINTERNAL+Covid_Age, data = dt.covid))
summary(lm(SDQC_INTERNALIZING_t2 ~ CRSQ_RUM + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ CRSQ_RUM + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
#Cognitive Reappraisal
summary(lm(SDQC_INTERNALIZING_t2 ~ ERQ_CR + SEX + SDQ_C_INT+Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ ERQ_CR + SEX + SDQ_C_INT+Covid_Age2, data = dt.covid))
#Expressive Suppression
summary(lm(SDQC_INTERNALIZING_t2 ~ ERQ_ES + SEX + SDQ_C_INT+Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ ERQ_ES + SEX + SDQ_C_INT+Covid_Age2, data = dt.covid))

#Amygdala Reactivity x Covid Stress
fit1<-summary(lm(SDQ_C_INT ~ LAmyg.Fear_GT_Calm_c * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
fit2<-summary(lm(SDQ_C_INT ~ RAmyg.Fear_GT_Calm_c * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
fit1
lm.beta(lm(SDQ_C_INT ~ LAmyg.Fear_GT_Calm_c * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
fit2
lm.beta(lm(SDQ_C_INT ~ RAmyg.Fear_GT_Calm_c * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
pvals<-c(fit1$coefficients[7,4],fit2$coefficients[7,4])
p.adjust(pvals,method="fdr",n=2)

#Amygdala Reactivity x wave 2 Covid Stress
fit1<-summary(lm(SDQC_INTERNALIZING_t2 ~ LAmyg.Fear_GT_Calm_c * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
fit2<-summary(lm(SDQC_INTERNALIZING_t2 ~ RAmyg.Fear_GT_Calm_c * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
fit1
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ LAmyg.Fear_GT_Calm_c * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
fit2
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ RAmyg.Fear_GT_Calm_c * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
pvals<-c(fit1$coefficients[7,4],fit2$coefficients[7,4])
p.adjust(pvals,method="fdr",n=2)

#Amygdala and Hippocampal Volume x Covid Stress
fit1<-summary(lm(SDQ_C_INT ~ Amygdala_c * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+IntraCranialVol+Covid_Age, data = dt.covid))
fit2<-summary(lm(SDQ_C_INT ~ Hippocampus_c * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+IntraCranialVol+Covid_Age, data = dt.covid))
fit1
fit2
lm.beta(lm(SDQ_C_INT ~ Amygdala_c * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+IntraCranialVol+Covid_Age, data = dt.covid))
lm.beta(lm(SDQ_C_INT ~ Hippocampus_c * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+IntraCranialVol+Covid_Age, data = dt.covid))
pvals<-c(fit1$coefficients[8,4],fit2$coefficients[8,4])
p.adjust(pvals,method="fdr",n=2)

#Amygdala and Hippocampal Volume x Wave 2 Covid Stress
fit1<-summary(lm(SDQC_INTERNALIZING_t2 ~ Amygdala_c * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+IntraCranialVol+Covid_Age2, data = dt.covid))
fit2<-summary(lm(SDQC_INTERNALIZING_t2 ~ Hippocampus_c * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+IntraCranialVol+Covid_Age2, data = dt.covid))
fit1
fit2
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ Amygdala_c * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+IntraCranialVol+Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ Hippocampus_c * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+IntraCranialVol+Covid_Age2, data = dt.covid))
pvals<-c(fit1$coefficients[8,4],fit2$coefficients[8,4])
p.adjust(pvals,method="fdr",n=2)

#Rumination x Covid Stress
summary(lm(SDQ_C_INT ~ CRSQ_RUMc * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
lm.beta(lm(SDQ_C_INT ~ CRSQ_RUMc * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
summary(lm(SDQC_INTERNALIZING_t2 ~ CRSQ_RUMc * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ CRSQ_RUMc * COVID_GLOBAL_STRESS_TOT2c + SEX + YINTERNAL+Covid_Age2, data = dt.covid))
#Cognitive Reappraisal x Covid Stress
summary(lm(SDQ_C_INT ~ ERQ_CRc * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
lm.beta(lm(SDQ_C_INT ~ ERQ_CRc * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
summary(lm(SDQC_INTERNALIZING_t2 ~ ERQ_CRc * COVID_GLOBAL_STRESS_TOT2c + SEX + SDQ_C_INT+Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ ERQ_CRc * COVID_GLOBAL_STRESS_TOT2c + SEX + SDQ_C_INT+Covid_Age2, data = dt.covid))
#Expressive Suppression x Covid Stress
summary(lm(SDQ_C_INT ~ ERQ_ESc * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
lm.beta(lm(SDQ_C_INT ~ ERQ_ESc * COVID_GLOBAL_STRESS_TOTc + SEX + YINTERNAL+Covid_Age, data = dt.covid))
summary(lm(SDQC_INTERNALIZING_t2 ~ ERQ_ESc * COVID_GLOBAL_STRESS_TOT2c + SEX + SDQ_C_INT+Covid_Age2, data = dt.covid))
lm.beta(lm(SDQC_INTERNALIZING_t2 ~ ERQ_ESc * COVID_GLOBAL_STRESS_TOT2c + SEX + SDQ_C_INT+Covid_Age2, data = dt.covid))

