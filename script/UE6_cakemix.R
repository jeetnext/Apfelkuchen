###########################################################################
#initialisation
###########################################################################

rm(list=ls())    #empty workspace if wanted ...
ls()
library(MASS)
library(AlgDesign)

##########################################################################
#Preparation in MacOS Environment <Different directory system as in Windows>
##########################################################################

#preparation
#dimdoe_path<-"/Users/alfretnara/Dropbox/FRA-UAS/Project - Dimensionless Design of Experiments/HIS_Project"
#script_path<-paste(dimdoe_path,"/R-skripte", sep = "")
#ex_path<-paste(dimdoe_path,"/UE6_cakemix/", sep = "")
#transferring functions
#source(paste(script_path,"/dim_doe_functions_TurnQual19.R", sep = ""))
source("pathscript.R")
ex_path <-setPath(6)


##########################################################################
#start example
##########################################################################
#fetch dataList
dataList<-readDimDoeData(ex_path)
names(dataList)

#fetch u_infos
infosFromU<-fetchInfosFromU(dataList$U)
u_roles<-infosFromU$u_roles
u_roles
u_transform<-infosFromU$u_transform
u_transform
u_gradient<-infosFromU$u_gradient
u_gradient
u_offset<-infosFromU$u_offset
u_offset
u_low<-infosFromU$u_low
u_low
u_high<-infosFromU$u_high
u_high
D<-infosFromU$D
D
V<-infosFromU$V
V

Vok<-suggestVmatrix(u_roles,D,V, debug = 0)
Vok$flag
Vok$V
V<-adjustVmatrix(Vok$V,1,c(1,0),"flshratio")
V<-adjustVmatrix(V,2,c(0,1),"fleggratio")
V

#establish connection between u and x
VsAndWs<-supplyVsAndWs(V,u_roles)
print(VsAndWs$flag)
VSL<-VsAndWs$VSL
VSL
VRES<-VsAndWs$VRES
VRES
WRES<-VsAndWs$WRES
WRES
x_roles<-VsAndWs$x_roles
x_roles
u_roles<-VsAndWs$u_roles
u_roles

#variant 1: generate a RT-design
desParam<-list(numberCP=3)
desType="designFullFactorial"
design<-fetchDesign(1,VsAndWs$VRES,VsAndWs$WRES,NULL,infosFromU$u_low,infosFromU$u_high,x_D_levels$x_D_low,x_D_levels$x_D_high,VsAndWs$u_roles,VsAndWs$x_roles,test_eqpr,desType,desParam)
design$flag
u2uu(design$u_design,u_transform,u_offset,u_gradient)
design$u_design%*%VRES
design$dimDoeMod

transDimDoeMod(design$dimDoeMod,x_roles,forward=TRUE) #should be the same as design$Dfrm
transDimDoeMod(design$Dfrm,x_roles,forward=FALSE)	#should be the same as design$$dimDoeMod

#Importing Response-names and values 
V_resp<-fetchV_resp(dataList$Z,V,import=1)
V_resp
z<-dataList$z_values
z
infosFromZ<-fetchInfosFromZ(dataList$Z)
infosFromZ

##########################################################################
# fit, diagnose and use the model - using lm() and Dfrm
##########################################################################

#define the linear (default for imported designs) or interaction model
Dfrml<-as.list(design$Dfrml)
Dfrml

#Fitting and Diagnosing the fitted model - this uses linear modelling lm( )
#fittedModel<-analyseDesign(Dfrml,design$u_design,z,infosFromZ$z_transform,infosFromZ$z_offset,infosFromZ$z_gradient,V,V_resp,design$MD,debug=1)
fittedModel<-prepareAnalysis(design$u_design,z,infosFromZ$z_transform,infosFromZ$z_offset,infosFromZ$z_gradient,V,V_resp, debug = 1)
fittedModel<-doAnalysis(Dfrml,design$MD,fittedModel$y)
fittedModel

#model coefficients
lapply(fittedModel,function(x) summary(x)$coeff)


#dianostics using residual analysis y-response
lapply(fittedModel,plot)

#RSD
#R2Y<-lapply(fittedModel,function(x) summary(x)$r.square)
#R2Y
RSDY<-lapply(fittedModel,function(x) summary(x)$sigma)
RSDY
RSDinPercent<-lapply(fittedModel,function(x) (10^(summary(x)$sigma)-1)*100)
RSDinPercent


#observed vs predicted for DL-response
y_pred<-lapply(fittedModel, fitted)		#as a list of vectors
y_predM<-z		#this is a 2D-matrix with the correct size - it will be completely overwritten
for(colInd in (1:length(y_pred))) {
  y_predM[,colInd]<-y_pred[[colInd]]
  colnames(y_predM)<-names(y_pred)
}

y_res<-lapply(fittedModel, residuals)		#as a list of vectors
y_resM<-z		#this is a 2D-matrix with the correct size - it will be completely overwritten
for(colInd in (1:length(y_res))) y_resM[,colInd]<-y_res[[colInd]]
y_obsM=y_predM+y_resM
for(i in 1:length(z[1,])){
  dev.new()
  plot(y_predM[,i], y_obsM[,i],main=paste("Obs vs Pred for DL-resp: ",colnames(y_predM)[i],sep=""))
  abline(0,1)
  text(y_predM[,i], y_obsM[,i], row.names(y_obsM), cex=0.8, pos=4)
}

#observed vs predicted for RT-response
l_z_pred<-y_predM%*%solve(V_resp[(nrow(VsAndWs$VRES)+1):nrow(V_resp),])-design$u_design%*%V_resp[1:nrow(VsAndWs$VRES),]
z_pred<-10^l_z_pred
z_pred

R2Z<-diag(cor(z,z_pred))^2
R2Z
for(i in 1:length(z[1,])){
  dev.new()
  plot(z_pred[,i],z[,i],main=paste("Obs vs Pred for RT-resp: ",colnames(z)[i],sep=""))
  abline(0,1)
  text(z_pred[,i],z[,i], row.names(z), cex=0.8, pos=4)
}


