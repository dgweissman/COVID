library(lavaan)
load("~/OneDrive - Harvard University/Research/Postdoc/DT/COVID/Data/DT_Covid.RData")
dt.covid$YINTERNAL<-(dt.covid$YINTERNAL-min(dt.covid$YINTERNAL,na.rm=T))/(max(dt.covid$YINTERNAL,na.rm=T)-min(dt.covid$YINTERNAL,na.rm=T))
dt.covid$SDQ_C_INT1<-(dt.covid$SDQ_C_INT-min(dt.covid$SDQ_C_INT,na.rm=T))/(max(dt.covid$SDQ_C_INT,na.rm=T)-min(dt.covid$SDQ_C_INT,na.rm=T))
dt.covid$SDQ_C_INT2<-(dt.covid$SDQC_INTERNALIZING_t2-min(dt.covid$SDQC_INTERNALIZING_t2,na.rm=T))/(max(dt.covid$SDQC_INTERNALIZING_t2,na.rm=T)-min(dt.covid$SDQC_INTERNALIZING_t2,na.rm=T))
colnames(dt.covid)[which(colnames(dt.covid)=="COVID_GLOBAL_STRESS_TOTc")]<-"COVID_Stress1"
colnames(dt.covid)[which(colnames(dt.covid)=="COVID_GLOBAL_STRESS_TOT2c")]<-"COVID_Stress2"
dt.covid$SEX<-2-as.numeric(dt.covid$SEX)

#Amygdala Reactivity to fear vs. neutral faces
#Left amygdala, main effects
LAmygMod1 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+LAmyg.Fear_GT_Calm_c+SEX
SDQ_C_INT1 ~ YINTERNAL + LAmyg.Fear_GT_Calm_c + SEX + Covid_Age
Covid_Age2 ~ Covid_Age

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
LAmyg.Fear_GT_Calm_c~~LAmyg.Fear_GT_Calm_c
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
               
# covariances 
YINTERNAL~~LAmyg.Fear_GT_Calm_c
YINTERNAL~~SEX
SEX~~LAmyg.Fear_GT_Calm_c
Covid_Age~~LAmyg.Fear_GT_Calm_c
Covid_Age2~~LAmyg.Fear_GT_Calm_c
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
LAmyg.Fear_GT_Calm_c~1
SEX ~1
Covid_Age~1
Covid_Age2~1
'

LAmygMod1.fit <- sem(LAmygMod1, estimator = "ML", missing ="ML", data = dt.covid)  
summary(LAmygMod1.fit, standardized=TRUE,fit.measures = TRUE)

#Left amygdala, interaction with pandemic-related stress
dt.covid$LAmygxStress1<-dt.covid$LAmyg.Fear_GT_Calm_c*dt.covid$COVID_Stress1
dt.covid$LAmygxStress2<-dt.covid$LAmyg.Fear_GT_Calm_c*dt.covid$COVID_Stress2
LAmygMod2 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+LAmyg.Fear_GT_Calm_c+COVID_Stress2+LAmygxStress2+SEX
SDQ_C_INT1 ~ YINTERNAL + LAmyg.Fear_GT_Calm_c + SEX + Covid_Age+COVID_Stress1+LAmygxStress1
Covid_Age2 ~ Covid_Age
COVID_Stress2~COVID_Stress1
LAmygxStress2~LAmygxStress1

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
LAmyg.Fear_GT_Calm_c~~LAmyg.Fear_GT_Calm_c
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
COVID_Stress1~~COVID_Stress1
COVID_Stress2~~COVID_Stress2
LAmygxStress1~~LAmygxStress1
LAmygxStress2~~LAmygxStress2

# covariances 
YINTERNAL~~LAmyg.Fear_GT_Calm_c
YINTERNAL~~SEX
SEX~~LAmyg.Fear_GT_Calm_c
Covid_Age~~LAmyg.Fear_GT_Calm_c
Covid_Age2~~LAmyg.Fear_GT_Calm_c
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1
COVID_Stress1~~YINTERNAL
COVID_Stress1~~SEX
COVID_Stress1~~LAmyg.Fear_GT_Calm_c
COVID_Stress1~~Covid_Age
COVID_Stress2~~SDQ_C_INT1
COVID_Stress2~~Covid_Age2
COVID_Stress2~~LAmyg.Fear_GT_Calm_c
COVID_Stress2~~SEX
LAmygxStress1~~COVID_Stress1
LAmygxStress1~~YINTERNAL
LAmygxStress1~~SEX
LAmygxStress1~~LAmyg.Fear_GT_Calm_c
LAmygxStress1~~Covid_Age
LAmygxStress2~~COVID_Stress2
LAmygxStress2~~SDQ_C_INT1
LAmygxStress2~~Covid_Age2
LAmygxStress2~~LAmyg.Fear_GT_Calm_c
LAmygxStress2~~SEX
# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
LAmyg.Fear_GT_Calm_c~1
SEX ~1
Covid_Age~1
Covid_Age2~1
COVID_Stress1~1
COVID_Stress2~1
LAmygxStress1~1
LAmygxStress2~1
'
LAmygMod2.fit <- sem(LAmygMod2, estimator = "ML", missing ="ML", data = dt.covid)  
summary(LAmygMod2.fit, , standardized=TRUE, fit.measures = TRUE)

#Right amygdala, main effects
RAmygMod1 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+RAmyg.Fear_GT_Calm_c+SEX
SDQ_C_INT1 ~ YINTERNAL + RAmyg.Fear_GT_Calm_c + SEX + Covid_Age
Covid_Age2 ~ Covid_Age

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
RAmyg.Fear_GT_Calm_c~~RAmyg.Fear_GT_Calm_c
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2

# covariances 
YINTERNAL~~RAmyg.Fear_GT_Calm_c
YINTERNAL~~SEX
SEX~~RAmyg.Fear_GT_Calm_c
Covid_Age~~RAmyg.Fear_GT_Calm_c
Covid_Age2~~RAmyg.Fear_GT_Calm_c
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
RAmyg.Fear_GT_Calm_c~1
SEX ~1
Covid_Age~1
Covid_Age2~1
'

RAmygMod1.fit <- sem(RAmygMod1, estimator = "ML", missing ="ML", data = dt.covid)  
summary(RAmygMod1.fit, standardized=TRUE, fit.measures = TRUE)

#Right amygdala, interaction with pandemic-related stress
dt.covid$RAmygxStress1<-dt.covid$RAmyg.Fear_GT_Calm_c*dt.covid$COVID_Stress1
dt.covid$RAmygxStress2<-dt.covid$RAmyg.Fear_GT_Calm_c*dt.covid$COVID_Stress2
RAmygMod2 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+RAmyg.Fear_GT_Calm_c+COVID_Stress2+RAmygxStress2+SEX
SDQ_C_INT1 ~ YINTERNAL + RAmyg.Fear_GT_Calm_c + SEX + Covid_Age+COVID_Stress1+RAmygxStress1
Covid_Age2 ~ Covid_Age
COVID_Stress2~COVID_Stress1
RAmygxStress2~RAmygxStress1

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
RAmyg.Fear_GT_Calm_c~~RAmyg.Fear_GT_Calm_c
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
COVID_Stress1~~COVID_Stress1
COVID_Stress2~~COVID_Stress2
RAmygxStress1~~RAmygxStress1
RAmygxStress2~~RAmygxStress2

# covariances 
YINTERNAL~~RAmyg.Fear_GT_Calm_c
YINTERNAL~~SEX
SEX~~RAmyg.Fear_GT_Calm_c
Covid_Age~~RAmyg.Fear_GT_Calm_c
Covid_Age2~~RAmyg.Fear_GT_Calm_c
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1
COVID_Stress1~~YINTERNAL
COVID_Stress1~~SEX
COVID_Stress1~~RAmyg.Fear_GT_Calm_c
COVID_Stress1~~Covid_Age
COVID_Stress2~~SDQ_C_INT1
COVID_Stress2~~Covid_Age2
COVID_Stress2~~RAmyg.Fear_GT_Calm_c
COVID_Stress2~~SEX
RAmygxStress1~~COVID_Stress1
RAmygxStress1~~YINTERNAL
RAmygxStress1~~SEX
RAmygxStress1~~RAmyg.Fear_GT_Calm_c
RAmygxStress1~~Covid_Age
RAmygxStress2~~COVID_Stress2
RAmygxStress2~~SDQ_C_INT1
RAmygxStress2~~Covid_Age2
RAmygxStress2~~RAmyg.Fear_GT_Calm_c
RAmygxStress2~~SEX
# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
RAmyg.Fear_GT_Calm_c~1
SEX ~1
Covid_Age~1
Covid_Age2~1
COVID_Stress1~1
COVID_Stress2~1
RAmygxStress1~1
RAmygxStress2~1
'
RAmygMod2.fit <- sem(RAmygMod2, estimator = "ML", missing ="ML", data = dt.covid)  
summary(RAmygMod2.fit, standardized=TRUE, fit.measures = TRUE)

#FDR correction on analyses of amygdala reactivity and internalizing at T1
p.adjust(c(.005,.694,.006,.802),method="fdr")
#FDR correction on analyses of amygdala reactivity and internalizing at T2
p.adjust(c(.830,.811,.939,.280),method="fdr")

#Hippocampal and amygdala volume
dt.covid$IntraCranialVol<-dt.covid$IntraCranialVol/1000000
dt.covid$Hippocampus_c<-dt.covid$Hippocampus_c/1000
dt.covid$Amygdala_c<-dt.covid$Amygdala_c/1000
#Hippocampus, main effects
HippocampusMod1 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+Hippocampus_c+IntraCranialVol+SEX
SDQ_C_INT1 ~ YINTERNAL + Hippocampus_c + SEX + Covid_Age+IntraCranialVol
Covid_Age2 ~ Covid_Age

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
Hippocampus_c~~Hippocampus_c
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
IntraCranialVol~~IntraCranialVol

# covariances 
YINTERNAL~~Hippocampus_c
YINTERNAL~~SEX
SEX~~Hippocampus_c
Covid_Age~~Hippocampus_c
Covid_Age2~~Hippocampus_c
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1
IntraCranialVol~~Hippocampus_c
IntraCranialVol~~Covid_Age
IntraCranialVol~~Covid_Age2
IntraCranialVol~~SEX
IntraCranialVol~~YINTERNAL

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
Hippocampus_c~1
SEX ~1
Covid_Age~1
Covid_Age2~1
IntraCranialVol~1
'

HippocampusMod1.fit <- sem(HippocampusMod1, estimator = "ML", missing ="ML", data = dt.covid)  
summary(HippocampusMod1.fit, standardized=TRUE, fit.measures = TRUE)

#Hippocampal volume, interaction with pandemic-related stress
dt.covid$HipxStress1<-dt.covid$Hippocampus_c*dt.covid$COVID_Stress1
dt.covid$HipxStress2<-dt.covid$Hippocampus_c*dt.covid$COVID_Stress2
HippocampusMod2 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+Hippocampus_c+IntraCranialVol+COVID_Stress2+HipxStress2+SEX
SDQ_C_INT1 ~ YINTERNAL + Hippocampus_c + SEX + Covid_Age+COVID_Stress1+IntraCranialVol+HipxStress1
Covid_Age2 ~ Covid_Age
COVID_Stress2~COVID_Stress1
HipxStress2~HipxStress1

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
Hippocampus_c~~Hippocampus_c
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
COVID_Stress1~~COVID_Stress1
COVID_Stress2~~COVID_Stress2
IntraCranialVol~~IntraCranialVol
HipxStress1~~HipxStress1
HipxStress2~~HipxStress2
# covariances 
YINTERNAL~~Hippocampus_c
YINTERNAL~~SEX
SEX~~Hippocampus_c
Covid_Age~~Hippocampus_c
Covid_Age2~~Hippocampus_c
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1
COVID_Stress1~~YINTERNAL
COVID_Stress1~~SEX
COVID_Stress1~~Hippocampus_c
COVID_Stress1~~Covid_Age
COVID_Stress2~~SDQ_C_INT1
COVID_Stress2~~Covid_Age2
COVID_Stress2~~Hippocampus_c
COVID_Stress2~~SEX
COVID_Stress1~~IntraCranialVol
COVID_Stress2~~IntraCranialVol
Covid_Age~~IntraCranialVol
Covid_Age2~~IntraCranialVol
YINTERNAL ~~ IntraCranialVol
Hippocampus_c~~IntraCranialVol
SEX ~~ IntraCranialVol
COVID_Stress1~~HipxStress1
HipxStress1~~YINTERNAL
HipxStress1~~SEX
HipxStress1~~Hippocampus_c
HipxStress1~~Covid_Age
HipxStress2~~SDQ_C_INT1
HipxStress2~~Covid_Age2
HipxStress2~~Hippocampus_c
COVID_Stress2~~HipxStress2
HipxStress2~~SEX
HipxStress1~~IntraCranialVol
HipxStress2~~IntraCranialVol

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
Hippocampus_c~1
SEX ~1
Covid_Age~1
Covid_Age2~1
COVID_Stress1~1
COVID_Stress2~1
IntraCranialVol~1
HipxStress1~1
HipxStress2~1
'
HippocampusMod2.fit <- sem(HippocampusMod2, estimator = "ML", missing ="ML", data = dt.covid)  
summary(HippocampusMod2.fit, standardized=TRUE, fit.measures = TRUE)

#Amygdala, main effects
AmygdalaMod1 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+Amygdala_c+IntraCranialVol+SEX
SDQ_C_INT1 ~ YINTERNAL + Amygdala_c + SEX + Covid_Age+IntraCranialVol
Covid_Age2 ~ Covid_Age

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
Amygdala_c~~Amygdala_c
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
IntraCranialVol~~IntraCranialVol

# covariances 
YINTERNAL~~Amygdala_c
YINTERNAL~~SEX
SEX~~Amygdala_c
Covid_Age~~Amygdala_c
Covid_Age2~~Amygdala_c
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1
IntraCranialVol~~Amygdala_c
IntraCranialVol~~Covid_Age
IntraCranialVol~~Covid_Age2
IntraCranialVol~~SEX
IntraCranialVol~~YINTERNAL

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
Amygdala_c~1
SEX ~1
Covid_Age~1
Covid_Age2~1
IntraCranialVol~1
'

AmygdalaMod1.fit <- sem(AmygdalaMod1, estimator = "ML", missing ="ML", data = dt.covid)  
summary(AmygdalaMod1.fit, standardized=TRUE, fit.measures = TRUE)

#Amygdala volume, interaction with pandemic-related stress
dt.covid$AmyxStress1<-dt.covid$Amygdala_c*dt.covid$COVID_Stress1
dt.covid$AmyxStress2<-dt.covid$Amygdala_c*dt.covid$COVID_Stress2
AmygdalaMod2 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+Amygdala_c+IntraCranialVol+COVID_Stress2+AmyxStress2+SEX
SDQ_C_INT1 ~ YINTERNAL + Amygdala_c + SEX + Covid_Age+COVID_Stress1+IntraCranialVol+AmyxStress1
Covid_Age2 ~ Covid_Age
COVID_Stress2~COVID_Stress1
AmyxStress2~AmyxStress1

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
Amygdala_c~~Amygdala_c
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
COVID_Stress1~~COVID_Stress1
COVID_Stress2~~COVID_Stress2
IntraCranialVol~~IntraCranialVol
AmyxStress1~~AmyxStress1
AmyxStress2~~AmyxStress2
# covariances 
YINTERNAL~~Amygdala_c
YINTERNAL~~SEX
SEX~~Amygdala_c
Covid_Age~~Amygdala_c
Covid_Age2~~Amygdala_c
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1
COVID_Stress1~~YINTERNAL
COVID_Stress1~~SEX
COVID_Stress1~~Amygdala_c
COVID_Stress1~~Covid_Age
COVID_Stress2~~SDQ_C_INT1
COVID_Stress2~~Covid_Age2
COVID_Stress2~~Amygdala_c
COVID_Stress2~~SEX
COVID_Stress1~~IntraCranialVol
COVID_Stress2~~IntraCranialVol
Covid_Age~~IntraCranialVol
Covid_Age2~~IntraCranialVol
YINTERNAL ~~ IntraCranialVol
Amygdala_c~~IntraCranialVol
SEX ~~ IntraCranialVol
COVID_Stress1~~AmyxStress1
AmyxStress1~~YINTERNAL
AmyxStress1~~SEX
AmyxStress1~~Amygdala_c
AmyxStress1~~Covid_Age
AmyxStress2~~SDQ_C_INT1
AmyxStress2~~Covid_Age2
AmyxStress2~~Amygdala_c
COVID_Stress2~~AmyxStress2
AmyxStress2~~SEX
AmyxStress1~~IntraCranialVol
AmyxStress2~~IntraCranialVol

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
Amygdala_c~1
SEX ~1
Covid_Age~1
Covid_Age2~1
COVID_Stress1~1
COVID_Stress2~1
IntraCranialVol~1
AmyxStress1~1
AmyxStress2~1
'
AmygdalaMod2.fit <- sem(AmygdalaMod2, estimator = "ML", missing ="ML", data = dt.covid)  
summary(AmygdalaMod2.fit, standardized=TRUE, fit.measures = TRUE)

#FDR correction on analyses of hippocampal and amygdala volume and internalizing at T1
p.adjust(c(.364,.784,.478,.083),method="fdr")
#FDR correction on analyses of hippocampal and amygdala volume and internalizing at T2
p.adjust(c(.585,.589,.439,.888),method="fdr")

#Emotion Regulation
#Rumination main effects
RumMod1 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+CRSQ_RUMc+SEX
SDQ_C_INT1 ~ YINTERNAL + CRSQ_RUMc + SEX + Covid_Age
Covid_Age2 ~ Covid_Age

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
CRSQ_RUMc~~CRSQ_RUMc
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2

# covariances 
YINTERNAL~~CRSQ_RUMc
YINTERNAL~~SEX
SEX~~CRSQ_RUMc
Covid_Age~~CRSQ_RUMc
Covid_Age2~~CRSQ_RUMc
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
CRSQ_RUMc~1
SEX ~1
Covid_Age~1
Covid_Age2~1
'

RumMod1.fit <- sem(RumMod1, estimator = "ML", missing ="ML", data = dt.covid)  
summary(RumMod1.fit, standardized=TRUE, fit.measures = TRUE)

#Rumination, interaction with pandemic-related stress
dt.covid$RumxStress1<-dt.covid$CRSQ_RUMc*dt.covid$COVID_Stress1
dt.covid$RumxStress2<-dt.covid$CRSQ_RUMc*dt.covid$COVID_Stress2
RumMod2 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+CRSQ_RUMc+COVID_Stress2+RumxStress2+SEX
SDQ_C_INT1 ~ YINTERNAL + CRSQ_RUMc + SEX + Covid_Age+COVID_Stress1+RumxStress1
Covid_Age2 ~ Covid_Age
COVID_Stress2~COVID_Stress1
RumxStress2~RumxStress1

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
CRSQ_RUMc~~CRSQ_RUMc
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
COVID_Stress1~~COVID_Stress1
COVID_Stress2~~COVID_Stress2
RumxStress1~~RumxStress1
RumxStress2~~RumxStress2

# covariances 
YINTERNAL~~CRSQ_RUMc
YINTERNAL~~SEX
SEX~~CRSQ_RUMc
Covid_Age~~CRSQ_RUMc
Covid_Age2~~CRSQ_RUMc
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1
COVID_Stress1~~YINTERNAL
COVID_Stress1~~SEX
COVID_Stress1~~CRSQ_RUMc
COVID_Stress1~~Covid_Age
COVID_Stress2~~SDQ_C_INT1
COVID_Stress2~~Covid_Age2
COVID_Stress2~~CRSQ_RUMc
COVID_Stress2~~SEX
COVID_Stress1~~RumxStress1
RumxStress1~~YINTERNAL
RumxStress1~~SEX
RumxStress1~~CRSQ_RUMc
RumxStress1~~Covid_Age
COVID_Stress2~~RumxStress2
RumxStress2~~SDQ_C_INT1
RumxStress2~~Covid_Age2
RumxStress2~~CRSQ_RUMc
RumxStress2~~SEX

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
CRSQ_RUMc~1
SEX ~1
Covid_Age~1
Covid_Age2~1
COVID_Stress1~1
COVID_Stress2~1
RumxStress1~1
RumxStress2~1
'
RumMod2.fit <- sem(RumMod2, estimator = "ML", missing ="ML", data = dt.covid)  
summary(RumMod2.fit, standardized=TRUE,fit.measures = TRUE)

#Expressive Suppression main effects
ESMod1 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+ERQ_ESc+SEX
SDQ_C_INT1 ~ YINTERNAL + ERQ_ESc + SEX + Covid_Age
Covid_Age2 ~ Covid_Age

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
ERQ_ESc~~ERQ_ESc
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2

# covariances 
YINTERNAL~~ERQ_ESc
YINTERNAL~~SEX
SEX~~ERQ_ESc
Covid_Age~~ERQ_ESc
Covid_Age2~~ERQ_ESc
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
ERQ_ESc~1
SEX ~1
Covid_Age~1
Covid_Age2~1
'

ESMod1.fit <- sem(ESMod1, estimator = "ML", missing ="ML", data = dt.covid)  
summary(ESMod1.fit, standardized=TRUE, fit.measures = TRUE)

#Expressive Suppression, interaction with pandemic-related stress
dt.covid$ESxStress1<-dt.covid$ERQ_ESc*dt.covid$COVID_Stress1
dt.covid$ESxStress2<-dt.covid$ERQ_ESc*dt.covid$COVID_Stress2
ESMod2 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+ERQ_ESc+COVID_Stress2+ESxStress2+SEX
SDQ_C_INT1 ~ YINTERNAL + ERQ_ESc + SEX + Covid_Age+COVID_Stress1+ESxStress1
Covid_Age2 ~ Covid_Age
COVID_Stress2~COVID_Stress1
ESxStress2~ESxStress1

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
ERQ_ESc~~ERQ_ESc
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
COVID_Stress1~~COVID_Stress1
COVID_Stress2~~COVID_Stress2
ESxStress1~~ESxStress1
ESxStress2~~ESxStress2

# covariances 
YINTERNAL~~ERQ_ESc
YINTERNAL~~SEX
SEX~~ERQ_ESc
Covid_Age~~ERQ_ESc
Covid_Age2~~ERQ_ESc
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1
COVID_Stress1~~YINTERNAL
COVID_Stress1~~SEX
COVID_Stress1~~ERQ_ESc
COVID_Stress1~~Covid_Age
COVID_Stress2~~SDQ_C_INT1
COVID_Stress2~~Covid_Age2
COVID_Stress2~~ERQ_ESc
COVID_Stress2~~SEX
COVID_Stress1~~ESxStress1
ESxStress1~~YINTERNAL
ESxStress1~~SEX
ESxStress1~~ERQ_ESc
ESxStress1~~Covid_Age
COVID_Stress2~~ESxStress2
ESxStress2~~SDQ_C_INT1
ESxStress2~~Covid_Age2
ESxStress2~~ERQ_ESc
ESxStress2~~SEX

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
ERQ_ESc~1
SEX ~1
Covid_Age~1
Covid_Age2~1
COVID_Stress1~1
COVID_Stress2~1
ESxStress1~1
ESxStress2~1
'
ESMod2.fit <- sem(ESMod2, estimator = "ML", missing ="ML", data = dt.covid)  
summary(ESMod2.fit, standardized=TRUE, fit.measures = TRUE)


#Cognitive Reappraisal main effects
CRMod1 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+ERQ_CRc+SEX
SDQ_C_INT1 ~ YINTERNAL + ERQ_CRc + SEX + Covid_Age
Covid_Age2 ~ Covid_Age

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
ERQ_CRc~~ERQ_CRc
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2

# covariances 
YINTERNAL~~ERQ_CRc
YINTERNAL~~SEX
SEX~~ERQ_CRc
Covid_Age~~ERQ_CRc
Covid_Age2~~ERQ_CRc
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
ERQ_CRc~1
SEX ~1
Covid_Age~1
Covid_Age2~1
'

CRMod1.fit <- sem(CRMod1, estimator = "ML", missing ="ML", data = dt.covid)  
summary(CRMod1.fit, standardized=TRUE, fit.measures = TRUE)

#Cognitive Reappraisal, interaction with pandemic-related stress
dt.covid$CRxStress1<-dt.covid$ERQ_CRc*dt.covid$COVID_Stress1
dt.covid$CRxStress2<-dt.covid$ERQ_CRc*dt.covid$COVID_Stress2
CRMod2 <- ' 
# regressions
SDQ_C_INT2 ~ SDQ_C_INT1+Covid_Age2+ERQ_CRc+COVID_Stress2+CRxStress2+SEX
SDQ_C_INT1 ~ YINTERNAL + ERQ_CRc + SEX + Covid_Age+COVID_Stress1+CRxStress1
Covid_Age2 ~ Covid_Age
COVID_Stress2~COVID_Stress1
CRxStress2~CRxStress1

# variances
SDQ_C_INT2 ~~ SDQ_C_INT2
SDQ_C_INT1 ~~ SDQ_C_INT1
YINTERNAL ~~ YINTERNAL
ERQ_CRc~~ERQ_CRc
SEX ~~ SEX
Covid_Age~~Covid_Age
Covid_Age2~~Covid_Age2
COVID_Stress1~~COVID_Stress1
COVID_Stress2~~COVID_Stress2
CRxStress1~~CRxStress1
CRxStress2~~CRxStress2

# covariances 
YINTERNAL~~ERQ_CRc
YINTERNAL~~SEX
SEX~~ERQ_CRc
Covid_Age~~ERQ_CRc
Covid_Age2~~ERQ_CRc
Covid_Age~~SEX
Covid_Age2~~SEX
Covid_Age~~YINTERNAL
Covid_Age2~~SDQ_C_INT1
COVID_Stress1~~YINTERNAL
COVID_Stress1~~SEX
COVID_Stress1~~ERQ_CRc
COVID_Stress1~~Covid_Age
COVID_Stress2~~SDQ_C_INT1
COVID_Stress2~~Covid_Age2
COVID_Stress2~~ERQ_CRc
COVID_Stress2~~SEX
COVID_Stress1~~CRxStress1
CRxStress1~~YINTERNAL
CRxStress1~~SEX
CRxStress1~~ERQ_CRc
CRxStress1~~Covid_Age
COVID_Stress2~~CRxStress2
CRxStress2~~SDQ_C_INT1
CRxStress2~~Covid_Age2
CRxStress2~~ERQ_CRc
CRxStress2~~SEX

# intercepts 
SDQ_C_INT2 ~1
SDQ_C_INT1 ~1
YINTERNAL ~1
ERQ_CRc~1
SEX ~1
Covid_Age~1
Covid_Age2~1
COVID_Stress1~1
COVID_Stress2~1
CRxStress1~1
CRxStress2~1
'
CRMod2.fit <- sem(CRMod2, estimator = "ML", missing ="ML", data = dt.covid)  
summary(CRMod2.fit, standardized=TRUE, fit.measures = TRUE)

p.adjust(c(.018,.842,.000,.044,.000,.335),method="fdr")
p.adjust(c(.791,.537,.018,.946,.647,.822),method="fdr")

#Make Table 2
#Wave 1
pathtab1<-data.frame(matrix(ncol = 4, nrow = 16))
a<-parameterestimates(RumMod1.fit,standardized = T)
pathtab1[1,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="CRSQ_RUMc"),c(4,5,11,7)]
a<-parameterestimates(ESMod1.fit,standardized = T)
pathtab1[2,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="ERQ_ESc"),c(4,5,11,7)]
a<-parameterestimates(CRMod1.fit,standardized = T)
pathtab1[3,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="ERQ_CRc"),c(4,5,11,7)]
a<-parameterestimates(RumMod2.fit,standardized = T)
pathtab1[4,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="RumxStress1"),c(4,5,11,7)]
a<-parameterestimates(ESMod2.fit,standardized = T)
pathtab1[5,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="ESxStress1"),c(4,5,11,7)]
a<-parameterestimates(CRMod2.fit,standardized = T)
pathtab1[6,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="CRxStress1"),c(4,5,11,7)]
a<-parameterestimates(LAmygMod1.fit,standardized = T)
pathtab1[8,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="LAmyg.Fear_GT_Calm_c"),c(4,5,11,7)]
a<-parameterestimates(RAmygMod1.fit,standardized = T)
pathtab1[9,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="RAmyg.Fear_GT_Calm_c"),c(4,5,11,7)]
a<-parameterestimates(LAmygMod2.fit,standardized = T)
pathtab1[10,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="LAmygxStress1"),c(4,5,11,7)]
a<-parameterestimates(RAmygMod2.fit,standardized = T)
pathtab1[11,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="RAmygxStress1"),c(4,5,11,7)]
a<-parameterestimates(AmygdalaMod1.fit,standardized = T)
pathtab1[13,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="Amygdala_c"),c(4,5,11,7)]
a<-parameterestimates(HippocampusMod1.fit,standardized = T)
pathtab1[14,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="Hippocampus_c"),c(4,5,11,7)]
a<-parameterestimates(AmygdalaMod2.fit,standardized = T)
pathtab1[15,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="AmyxStress1"),c(4,5,11,7)]
a<-parameterestimates(HippocampusMod2.fit,standardized = T)
pathtab1[16,]<-a[which(a$lhs=="SDQ_C_INT1" & a$op=="~" & a$rhs=="HipxStress1"),c(4,5,11,7)]
write.csv(pathtab1,"pathtab1.csv",row.names=F,quote=F)
#Wave 2
pathtab2<-data.frame(matrix(ncol = 4, nrow = 16))
a<-parameterestimates(RumMod1.fit,standardized = T)
pathtab2[1,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="CRSQ_RUMc"),c(4,5,11,7)]
a<-parameterestimates(ESMod1.fit,standardized = T)
pathtab2[2,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="ERQ_ESc"),c(4,5,11,7)]
a<-parameterestimates(CRMod1.fit,standardized = T)
pathtab2[3,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="ERQ_CRc"),c(4,5,11,7)]
a<-parameterestimates(RumMod2.fit,standardized = T)
pathtab2[4,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="RumxStress2"),c(4,5,11,7)]
a<-parameterestimates(ESMod2.fit,standardized = T)
pathtab2[5,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="ESxStress2"),c(4,5,11,7)]
a<-parameterestimates(CRMod2.fit,standardized = T)
pathtab2[6,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="CRxStress2"),c(4,5,11,7)]
a<-parameterestimates(LAmygMod1.fit,standardized = T)
pathtab2[8,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="LAmyg.Fear_GT_Calm_c"),c(4,5,11,7)]
a<-parameterestimates(RAmygMod1.fit,standardized = T)
pathtab2[9,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="RAmyg.Fear_GT_Calm_c"),c(4,5,11,7)]
a<-parameterestimates(LAmygMod2.fit,standardized = T)
pathtab2[10,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="LAmygxStress2"),c(4,5,11,7)]
a<-parameterestimates(RAmygMod2.fit,standardized = T)
pathtab2[11,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="RAmygxStress2"),c(4,5,11,7)]
a<-parameterestimates(AmygdalaMod1.fit,standardized = T)
pathtab2[13,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="Amygdala_c"),c(4,5,11,7)]
a<-parameterestimates(HippocampusMod1.fit,standardized = T)
pathtab2[14,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="Hippocampus_c"),c(4,5,11,7)]
a<-parameterestimates(AmygdalaMod2.fit,standardized = T)
pathtab2[15,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="AmyxStress2"),c(4,5,11,7)]
a<-parameterestimates(HippocampusMod2.fit,standardized = T)
pathtab2[16,]<-a[which(a$lhs=="SDQ_C_INT2" & a$op=="~" & a$rhs=="HipxStress2"),c(4,5,11,7)]
write.csv(pathtab2,"pathtab2.csv",row.names=F,quote=F)

#Fit indices
range(c(fitmeasures(RumMod2.fit)["pvalue"],fitmeasures(ESMod2.fit)["pvalue"],fitmeasures(CRMod2.fit)["pvalue"],fitmeasures(LAmygMod2.fit)["pvalue"],fitmeasures(RAmygMod2.fit)["pvalue"],fitmeasures(AmygdalaMod2.fit)["pvalue"],fitmeasures(HippocampusMod2.fit)["pvalue"],fitmeasures(RumMod1.fit)["pvalue"],fitmeasures(ESMod1.fit)["pvalue"],fitmeasures(CRMod1.fit)["pvalue"],fitmeasures(LAmygMod1.fit)["pvalue"],fitmeasures(RAmygMod1.fit)["pvalue"],fitmeasures(AmygdalaMod1.fit)["pvalue"],fitmeasures(HippocampusMod1.fit)["pvalue"])) 
range(c(fitmeasures(RumMod2.fit)["cfi"],fitmeasures(ESMod2.fit)["cfi"],fitmeasures(CRMod2.fit)["cfi"],fitmeasures(LAmygMod2.fit)["cfi"],fitmeasures(RAmygMod2.fit)["cfi"],fitmeasures(AmygdalaMod2.fit)["cfi"],fitmeasures(HippocampusMod2.fit)["cfi"],fitmeasures(RumMod1.fit)["cfi"],fitmeasures(ESMod1.fit)["cfi"],fitmeasures(CRMod1.fit)["cfi"],fitmeasures(LAmygMod1.fit)["cfi"],fitmeasures(RAmygMod1.fit)["cfi"],fitmeasures(AmygdalaMod1.fit)["cfi"],fitmeasures(HippocampusMod1.fit)["cfi"])) 
range(c(fitmeasures(RumMod2.fit)["rmsea"],fitmeasures(ESMod2.fit)["rmsea"],fitmeasures(CRMod2.fit)["rmsea"],fitmeasures(LAmygMod2.fit)["rmsea"],fitmeasures(RAmygMod2.fit)["rmsea"],fitmeasures(AmygdalaMod2.fit)["rmsea"],fitmeasures(HippocampusMod2.fit)["rmsea"],fitmeasures(RumMod1.fit)["rmsea"],fitmeasures(ESMod1.fit)["rmsea"],fitmeasures(CRMod1.fit)["rmsea"],fitmeasures(LAmygMod1.fit)["rmsea"],fitmeasures(RAmygMod1.fit)["rmsea"],fitmeasures(AmygdalaMod1.fit)["rmsea"],fitmeasures(HippocampusMod1.fit)["rmsea"]))
range(c(fitmeasures(RumMod2.fit)["srmr"],fitmeasures(ESMod2.fit)["srmr"],fitmeasures(CRMod2.fit)["srmr"],fitmeasures(LAmygMod2.fit)["srmr"],fitmeasures(RAmygMod2.fit)["srmr"],fitmeasures(AmygdalaMod2.fit)["srmr"],fitmeasures(HippocampusMod2.fit)["srmr"],fitmeasures(RumMod1.fit)["srmr"],fitmeasures(ESMod1.fit)["srmr"],fitmeasures(CRMod1.fit)["srmr"],fitmeasures(LAmygMod1.fit)["srmr"],fitmeasures(RAmygMod1.fit)["srmr"],fitmeasures(AmygdalaMod1.fit)["srmr"],fitmeasures(HippocampusMod1.fit)["srmr"]))

