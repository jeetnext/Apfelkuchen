#Fitting and Diagnosing the fitted model - this uses linear modelling lm( )
fittedModel<-analyseDesign(Dfrml,design$u_design,z,infosFromZ$z_transform,infosFromZ$z_gradient,infosFromZ$z_offset,V,V_resp,design$MD,debug=0)
############----------------MY CODE GOES HERE NOW 
# for finding the value of coefficient following command is used: fittedModel$Q_1[1]$coefficients[1]
#Following is the code for storing the coefficients of fittedModel in a vector called valueOfFittedModelCoef
valueOfFittedModelCoef<-c()
for (x in 1:4)
{
  
  valueOfFittedModelCoef[x]<-fittedModel$Q_1[1]$coefficients[x]
  
}
valueOfFittedModelCoef #Values of coefficients for Intercept, Fr, cT and I(Fr * cT)
#design$MD contains the data for the defomer contour plotting
design$MD
#For generating the sequence for contours
x_Fr <- seq(min(design$MD[,1]),max(design$MD[,1]),length.out = 100)
x_Fr
y_cT <- seq(min(design$MD[,2]),max(design$MD[,2]),length.out = 100)
y_cT
contourCalculationFun <- function(xFrSeries,ycTSeries,cof1,cof2,cof3,cof4)
{
  
  cof1 + cof2*xFrSeries + cof3*ycTSeries + cof4*xFrSeries*ycTSeries
  
}
contourCalculationFunResultValues <- outer(x_Fr, y_cT, valueOfFittedModelCoef[1], valueOfFittedModelCoef[2], valueOfFittedModelCoef[3], valueOfFittedModelCoef[4], FUN = contourCalculationFun)
contourCalculationFunResultValues
contour(x=x_Fr,y=y_cT,z=contourCalculationFunResultValues)
filled.contour(x=x_Fr,y=y_cT,z=contourCalculationFunResultValues,plot.title = {title(main = "Contour Plot", xlab = "x_Fr", ylab = "y_cT")})

