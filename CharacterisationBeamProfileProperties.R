# Script coded to analyze the results from multiple measurement sessions performed either in WELCOME (LLN) or 
# with the calibration prototype devised in the lab by J.Lambert.The results were summerized in an Excel file.
# The aim of the scipt is to analyze variations of US exposure indices according to different experimental conditions.
# The results of the present analysis  will be presented in the thesis of J.Lambert in the chapter : Characterisation of an ultrasound beam.
library(readxl)
library(ICC)
# define a function to remove outliers
FindOutliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}
# Example of the function use to identify outliers
#tempC <- FindOutliers(Cylinder)
# remove the outliers
#CylinderWithout<- Cylinder[-tempC]
# show the data with the outliers removed
#CylinderWithout
#Define a standard error of the mean function
SEM <-function(data) {
    result <- sd(data)/sqrt(length(data))
}

# ------------------------------------------------Kitchen side------------------------------------------------------

Results <- read_excel("~/Dropbox (NOCIONS)/jlambert/UCL_PhD/Writing/Thesis/Analysis/ResultsAnalysisMeasures.xlsx",
                      col_types = c("text", "text", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric"),
                      na = "NA")
View(Results)
Results$BurstFrequency<-factor(Results$BurstFrequency)
Results$VppFunGen<-factor(Results$VppFunGen)
Results$MechAlignment<-factor(Results$MechAlignment)
Results$Rotation<-factor(Results$Rotation)
Results$NbrCycles<-factor(Results$NbrCycles)
Results$CalibPrototype<-factor(Results$CalibPrototype)
Results$StimPrototype<-factor(Results$StimPrototype)
Results$SkullBone<-factor(Results$SkullBone)
Results$PXI<-factor(Results$PXI)
summary(Results)

## Reliability of the setup
#About the reliability mechanical alignment : brass Cylinder versus aluminum block (used for angular position)
PressureCylinder <- Results$Pressure[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&(Results$Time=='Novembre 2016'|Results$Time=='Aout 2016')]
MeanPressureCylinder <- mean(PressureCylinder)
SemPressureCylinder<-SEM(PressureCylinder)
FocalPointXCylinder <- Results$FocalPointX[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&(Results$Time=='Novembre 2016'|Results$Time=='Aout 2016')]
MeanFocalPointXCylinder <- mean(FocalPointXCylinder)
SemFocalPointXCylinder<-SEM(FocalPointXCylinder)
FocalPointYCylinder <- Results$FocalPointY[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&(Results$Time=='Novembre 2016'|Results$Time=='Aout 2016')]
MeanFocalPointYCylinder <- mean(FocalPointYCylinder)
SemFocalPointYCylinder<-SEM(FocalPointYCylinder)
#
PressureRectangle <- Results$Pressure[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==2&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Angle==0]
MeanPressureRectangle <- mean(PressureRectangle)
SemPressureRectangle<-SEM(PressureRectangle)
FocalPointXRectangle <- Results$FocalPointX[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==2&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Angle==0]
MeanFocalPointXRectangle <- mean(FocalPointXRectangle)
SemFocalPointXRectangle<-SEM(FocalPointXRectangle)
FocalPointYRectangle <- Results$FocalPointY[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==2&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Angle==0]
MeanFocalPointYRectangle <- mean(FocalPointYRectangle)
SemFocalPointYRectangle<-SEM(FocalPointYRectangle)
#
MeanPressureAlignment<-mean(c(PressureCylinder,PressureRectangle))
SemPressureAlignment<- SEM(c(PressureCylinder,PressureRectangle))
MeanFocalPointXAlignment<-mean(c(FocalPointXCylinder,FocalPointXRectangle))
SemFocalPointXAlignment<- SEM(c(FocalPointXCylinder,FocalPointXRectangle))
MeanFocalPointYAlignment<-mean(c(FocalPointYCylinder,FocalPointYRectangle))
SemFocalPointYAlignment<- SEM(c(FocalPointYCylinder,FocalPointYRectangle))

# About the directivity of the hydrophone
PressureAngle<-Results$Pressure[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==2&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Decembre 2016']
MeanPressureAngle<-mean(PressureAngle)
SemPressureAngle<-SEM(PressureAngle)
FocalPointXAngle<-Results$FocalPointX[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==2&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Decembre 2016']
MeanFocalPointXAngle<-mean(FocalPointXAngle)
SemFocalPointXAngle<-SEM(FocalPointXAngle)
FocalPointYAngle<-Results$FocalPointY[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==2&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Decembre 2016']
MeanFocalPointYAngle<-mean(FocalPointYAngle)
SemFocalPointYAngle<-SEM(FocalPointYAngle)

# About the rotation of the emitter
PressureRotation<-Results$Pressure[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='July 2017']
MeanPressureRotation<-mean(PressureRotation)
SemPressureRotation<-SEM(PressureRotation)
FocalPointXRotation<-Results$FocalPointX[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='July 2017']
MeanFocalPointXRotation<-mean(FocalPointXRotation)
SemFocalPointXRotation<-SEM(FocalPointXRotation)
FocalPointYRotation<-Results$FocalPointY[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='July 2017']
MeanFocalPointYRotation<-mean(FocalPointYRotation)
SemFocalPointYRotation<-SEM(FocalPointYRotation)

# About the Reliability of the setup subsection - "Global results"
PressureReliabilitySetup <-c(PressureCylinder,PressureRectangle,PressureAngle,PressureRotation)
MeanPressureReliabilitySetup<-mean(PressureReliabilitySetup)
SemPressureReliabilitySetup<-SEM(PressureReliabilitySetup)
FocalPointXReliabilitySetup <-c(FocalPointXCylinder,FocalPointXRectangle,FocalPointXAngle,FocalPointXRotation)
MeanFocalPointXReliabilitySetup<-mean(FocalPointXReliabilitySetup)
SemFocalPointXReliabilitySetup<-SEM(FocalPointXReliabilitySetup)
FocalPointYReliabilitySetup <-c(FocalPointYCylinder,FocalPointYRectangle,FocalPointYAngle,FocalPointYRotation)
MeanFocalPointYReliabilitySetup<-mean(FocalPointYReliabilitySetup)
SemFocalPointYReliabilitySetup<-SEM(FocalPointYReliabilitySetup)
summary(PressureReliabilitySetup)
summary(FocalPointXReliabilitySetup)
summary(FocalPointYReliabilitySetup)

# Plots
boxplot(PressureCylinder,PressureRectangle, main="Alignment's comparison between the two mechanical parts",ylab="Pressure [Pa]",names=c("Brass cylinder","Aluminium rectangle"))
boxplot(c(PressureCylinder,PressureRectangle),PressureAngle,PressureRotation,main="Comparison between measures perfomed to evaluate the quality of the alignement and the directivity of the hydrophone",ylab="Pressure [Pa]",names=c("Mechanical alignment","Angle","Rotation"))
boxplot(PressureReliabilitySetup,main="Reliability of the experimental setup",ylab="Pressure [Pa]",xlab="Mechanical alignment,Angle and Rotations conditions merged")
#Control performed to check for Outliers in PressureReliabilitySetup or FocalPointYReliabilitySetup --> No outlier identified

## Parametrical analysis of the exposure indices and beam properties (focal distance and focal area dimensions)
# About the effect of the fundamental frequency 
PressureFF1_250<-Results$Pressure[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
IsppaEffFF1_250<-Results$Isppa_Effective[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
MiFF1_250<-Results$MI[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FocalPointXFF1_250<-Results$FocalPointX[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FocalPointYFF1_250<-Results$FocalPointY[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FwhmLengthFF1_250<-Results$FWHM_length[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FwhmWidthFF1_250<-Results$FWHM_width[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
PressureFF1_350<-Results$Pressure[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==350000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
IsppaEffFF1_350<-Results$Isppa_Effective[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==350000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
MiFF1_350<-Results$MI[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==350000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FocalPointXFF1_350<-Results$FocalPointX[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==350000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FocalPointYFF1_350<-Results$FocalPointY[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==350000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FwhmLengthFF1_350<-Results$FWHM_length[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==350000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FwhmWidthFF1_350<-Results$FWHM_width[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==350000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
PressureFF1_500<-Results$Pressure[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
IsppaEffFF1_500<-Results$Isppa_Effective[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
MiFF1_500<-Results$MI[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FocalPointXFF1_500<-Results$FocalPointX[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FocalPointYFF1_500<-Results$FocalPointY[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FwhmLengthFF1_500<-Results$FWHM_length[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FwhmWidthFF1_500<-Results$FWHM_width[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
#Results$ID[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]

PressureFF1<-Results$Pressure[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
IsppaEffFF1<-Results$Isppa_Effective[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
MiFF1<-Results$MI[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FocalPointXFF1<-Results$FocalPointX[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FocalPointYFF1<-Results$FocalPointY[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FwhmLengthFF1<-Results$FWHM_length[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]
FwhmWidthFF1<-Results$FWHM_width[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$VppFunGen==1&Results$MechAlignment==1&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5]

# About the effect of multiple driving voltage
PressureVpp1_250 = Results$Pressure[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==250000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
IsppaEffVpp1_250 = Results$Isppa_Effective[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==250000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
MiVpp1_250 = Results$MI[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==250000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FocalPointXVpp1_250 = Results$FocalPointX[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==250000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FocalPointYVpp1_250 = Results$FocalPointY[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==250000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FwhmLengthVpp1_250 = Results$FWHM_length[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==250000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FwhmWidthVpp1_250 = Results$FWHM_width[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==250000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
PressureVpp1_350 = Results$Pressure[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
IsppaEffVpp1_350 = Results$Isppa_Effective[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
MiVpp1_350 = Results$MI[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FocalPointXVpp1_350 = Results$FocalPointX[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FocalPointYVpp1_350 = Results$FocalPointY[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FwhmLengthVpp1_350 = Results$FWHM_length[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FwhmWidthVpp1_350 = Results$FWHM_width[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==350000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
PressureVpp1_500 = Results$Pressure[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==500000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
IsppaEffVpp1_500 = Results$Isppa_Effective[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==500000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
MiVpp1_500 = Results$MI[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==500000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FocalPointXVpp1_500 = Results$FocalPointX[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==500000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FocalPointYVpp1_500 = Results$FocalPointY[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==500000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FwhmLengthVpp1_500 = Results$FWHM_length[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==500000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FwhmWidthVpp1_500 = Results$FWHM_width[!is.na(Results$DistanceHydroTr)&Results$BurstFrequency==500000&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']

PressureVpp1 = Results$Pressure[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
IsppaEffVpp1 = Results$Isppa_Effective[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
MiVpp1 = Results$MI[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FocalPointXVpp1 = Results$FocalPointX[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FocalPointYVpp1 = Results$FocalPointY[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FwhmLengthVpp1 = Results$FWHM_length[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']
FwhmWidthXVpp1 = Results$FWHM_width[!is.na(Results$DistanceHydroTr)&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==0&Results$NbrCycles==5&Results$Time=='Aout 2016']

# About the effect of different number of cycles constituting an US burst
PressureNumC_250 = Results$Pressure[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
IsppaEffNumC_250 = Results$Isppa_Effective[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
MiNumC_250 = Results$MI[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FocalPointXNumC_250 = Results$FocalPointX[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FocalPointYNumC_250 = Results$FocalPointY[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FwhmLengthNumC_250 = Results$FWHM_length[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FwhmWidthNumC_250 = Results$FWHM_width[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
PressureNumC_350 = Results$Pressure[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==350000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
IsppaEffNumC_350 = Results$Isppa_Effective[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==350000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
MiNumC_350 = Results$MI[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==350000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FocalPointXNumC_350 = Results$FocalPointX[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==350000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FocalPointYNumC_350 = Results$FocalPointY[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==350000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FwhmLengthNumC_350 = Results$FWHM_length[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==350000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FwhmWidthNumC_350 = Results$FWHM_width[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==350000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
PressureNumC_500 = Results$Pressure[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
IsppaEffNumC_500 = Results$Isppa_Effective[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
MiNumC_500 = Results$MI[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FocalPointXNumC_500 = Results$FocalPointX[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FocalPointYNumC_500 = Results$FocalPointY[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FwhmLengthNumC_500 = Results$FWHM_length[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FwhmWidthNumC_500 = Results$FWHM_width[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']

PressureNumC = Results$Pressure[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
IsppaEffNumC = Results$Isppa_Effective[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
MiNumC = Results$MI[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FocalPointXNumC = Results$FocalPointX[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FocalPointYNumC = Results$FocalPointY[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FwhmLengthNumC = Results$FWHM_length[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']
FwhmWidthNumC = Results$FWHM_width[(Results$DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(Results$BurstFrequency==250000|Results$BurstFrequency==350000|Results$BurstFrequency==500000)&Results$SkullBone==0&Results$StimPrototype==1&Results$Time=='Avril 2017']

NumCyclesResults = Results[with(Results,(DistanceHydroTr==2.8&!is.na(Results$DistanceHydroTr))&(BurstFrequency==250000|BurstFrequency==350000|BurstFrequency==500000)&SkullBone==0&StimPrototype==1&Time=='Avril 2017'), ]
tapply(PressureNumCtest$Pressure, list(PressureNumCtest$BurstFrequency), mean)
# Plot data
XcoordPressureFF1<-rep(1,length(PressureFF1))
plot(XcoordFF1,PressureFF1,col=freq,main="Effect of the fundamental frequency on the pressure",ylab="Pressure [Pa]", xlab="")
legend(1.25, 460000, legend=c("250kHz", "350kHz","500kHz"),col=freq, lty=c(1,1,1),cex=0.6,box.lty=0)

XcoordFF1<-rep(c(0.95,1,1.05),length(PressureFF1_250))
plot(XcoordFF1,PressureFF1,col=freq,main="Effect of the fundamental frequency on the pressure",ylab="Pressure [Pa]", xlab="")
legend(1.03, 460000, legend=c("250kHz", "350kHz","500kHz"),col=freq, lty=c(1,1,1),cex=0.6,box.lty=0)

XcoordVpp1<-rep(c(250,350,500),length(PressureVpp1_250))
plot(XcoordVpp1,PressureVpp1,col=Results$BurstFrequency,main="Effect of the peak-to-peak voltage on the pressure",ylab="Pressure [Pa]", xlab="Fundamental frequency ([kHz]")
legend(450, 450000, legend=c("250kHz", "350kHz","500kHz"),col=freq, lty=c(1,1,1),cex=0.6,box.lty=0)
matplot(c(0.6,0.8,1,1.2), cbind(PressureVpp1_250,PressureVpp1_350,PressureVpp1_500),col=c("black","green","magenta"),type="b",pch=1,lty=c(1,1,1))

XcoordNumC<-c(rep(5,3),rep(10,3),rep(20,3),rep(50,3),rep(100,3),rep(200,3))
plot(XcoordNumC,PressureNumCtest$Pressure,col=PressureNumCtest$BurstFrequency)

plot(FocalPointXVpp1,FocalPointYVpp1)



