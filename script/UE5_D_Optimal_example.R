# Initialisation and Import
# Empty workspace if wanted

rm(list=ls())
ls()
library(MASS)  #ginv function

debug = 1

source("pathscript.R")
ex_path <-setPath(5)

{
  # Problem specification
  dataList = readDimDoeData(ex_path)
  infosFromU = fetchInfosFromU(dataList$U,debug=0)
  
  # Extracting results:
  u_names <- infosFromU$u_names
  u_roles = infosFromU$u_roles
  u_transform = infosFromU$u_transform
  u_gradient = infosFromU$u_gradient
  u_offset = infosFromU$u_offset
  u_low = infosFromU$u_low
  u_high = infosFromU$u_high
  D = infosFromU$D
  V0 = infosFromU$V
  colnames(V0)
  # W = solve(t(V0)%*%V0)%*%t(V0)
  # W%*%V
  
  t(D)%*%V0 # = 0 => correct
  
  Vok = suggestVmatrix(u_roles,D,V0)
  Vok$flag
  V = Vok$V
  VsAndWs = supplyVsAndWs(V,u_roles)
  VRES = VsAndWs$VRES
  WRES = VsAndWs$WRES
  
  relayCoeff<-refineRelayCoeffs(dataList$rawRelayCoeffs,u_roles, u_transform, u_offset, u_gradient,u_low,u_high,debug=0)
  R0 <- makeRelayMatrix(u_roles,relayCoeff$relayCoeffs)
  R0
  
  relay0 = relayVmatrix(u_names,u_roles,u_transform,u_offset,u_gradient,u_low,u_high,V,R=R0$relayMatrix,rDcolCtrs=0,debug=0)
  relay0
  
  VR0 <- relay0$V
  VR0
  R0 <- relay0$R
  R0
  u0_roles = relay0$new_roles
  u0_roles
  u0_transform = relay0$new_transform
  u0_roles
  u0_gradient = relay0$new_gradient
  u0_gradient
  u0_offset = relay0$new_offset
  u0_low = relay0$new_low
  u0_high = relay0$new_high
  u0_mean = (u0_low+u0_high)/2
  
  VsAndWs0 = supplyVsAndWs(VR0,u0_roles)
  flag0 = VsAndWs0$flag
  flag0
  VSL0 = VsAndWs0$VSL
  VRES0 = VsAndWs0$VRES
  WRES0 = VsAndWs0$WRES
  
  x0_roles = VsAndWs0$x_roles
  u0_roles = VsAndWs0$u_roles
  
  numberCP = 3
  desParam = list(numberCP=numberCP,import=FALSE)
  test_eqpr = TRUE
  desType = ""
  DLFsettings = fetchDLFSettings(VRES0,u0_low,u0_high)
  x0_low = DLFsettings$x_low      #careful: these are logs
  x0_high = DLFsettings$x_high    #careful: these are logs
  
  designOpt0 = fetchDesign(5,VRES0,WRES0,dataList,u0_low,u0_high,x0_low,x0_high,u0_roles,x0_roles,test_eqpr,desType,desParam, debug=FALSE)
  flag0 = designOpt0$flag
  flag0
  
  print("")
  print(designOpt0$des)
  print("Determinant")
  print(getNormDet(designOpt0$des))
}

