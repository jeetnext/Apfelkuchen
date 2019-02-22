Contour2<- function(model,var1,var2,const1,const2){
  
  terms<-(transDimDoeMod(as.character(Dfrml),x_roles,forward = FALSE))
  terms
  label<- row.names(DLdesign$x_D_low)
  Xseq<-seq( -1,1, length.out = 100)
  Yseq<-seq( -1,1, length.out = 100)
  # list of Constant values (Const1_low,Const1_high,Const2_low,Const2_high)
  List<-c(-1,1,-1,1)
  par(mfrow=c(2,2))
  ## part1 is for linear model without interaction term
  if(length(terms)==5){ 
    for(i in 1:2){
      for(j in 3:4){
        Constant1 <- List[i]
        Constant2 <- List[j]
      #  k<-model
        fm<-model
        zfunc <- function(x1, x2 , x3, x4) {
          print("Fitted model without interaction term")
          fm$coef[1] + fm$coef[var1+1]*x1 + fm$coef[var2+1]*x2 + fm$coef[const1+1]*x3+fm$coef[const2+1]*x4
        }
        z <- outer(Xseq, Yseq, Constant1,Constant2, FUN = zfunc)
        #
        if(i==1){
          if(j==3)
            mainString <- paste0("low",",","low")
          else 
            mainString <- paste0("low",",","High")
        }
        else{ if(j==3)
          mainString <- paste0("high", ",","low")
        else 
          mainString <- paste0("high", ",","High")
        }
        
        
        
        contour(Xseq, Yseq, z, xlab = label[var1], ylab= label[var2], main = paste(label[const1],",",label[const2],"=",mainString))
        
      }
    }
  }
  ## part2 is for linear model with interaction term
  else{    
    
    for(i in 1:2){
      for(j in 3:4){
        Constant1 <- List[i]
        Constant2 <- List[j]
        #k<-model
        fm<-model
        zfunc <- function(x1, x2 , x3, x4) {
          
          interactionTerms<-terms[[6]]
          index_interaction<-strsplit(as.character(interactionTerms),"") # index ofinteraction term
          # 6 cases to cover interaction between 4 variables
          if((index_interaction[1]==1 && index_interaction[2]==2)|(index_interaction[2]==1 && index_interaction[1]==2) ){
            print("interaction between 1 & 2")
            fm$coef[1] + fm$coef[var1+1]*x1 + fm$coef[var2+1]*x2 + fm$coef[const1+1]*x3+fm$coef[const2+1]*x4+fm$coef[6]*x1*x2
          }
          #
          else if((index_interaction[1]==1 && index_interaction[2]==3)|(index_interaction[2]==1 && index_interaction[1]==3)){
            print("interaction between 1 & 3")
            fm$coef[1] + fm$coef[var1+1]*x1 + fm$coef[var2+1]*x2 + fm$coef[const1+1]*x3+fm$coef[const2+1]*x4+fm$coef[6]*x1*x3
          }
          #
          else if((index_interaction[1]==1 && index_interaction[2]==4)|(index_interaction[2]==1 && index_interaction[1]==4)){
            print("interaction between 1 & 4")
            fm$coef[1] + fm$coef[var1+1]*x1 + fm$coef[var2+1]*x2 + fm$coef[const1+1]*x3+fm$coef[const2+1]*x4+fm$coef[6]*x1*x4
          }
          #
          else if((index_interaction[1]==2 && index_interaction[2]==3)|(index_interaction[2]==2 && index_interaction[1]==3)){
            print("interaction between 2 & 3")
            fm$coef[1] + fm$coef[var1+1]*x1 + fm$coef[var2+1]*x2 + fm$coef[const1+1]*x3+fm$coef[const2+1]*x4+fm$coef[6]*x2*x3
          }
          #
          else if((index_interaction[1]==2 && index_interaction[2]==4)|(index_interaction[2]==2 && index_interaction[1]==4)){
            print("interaction between 2 & 4")
            fm$coef[1] + fm$coef[var1+1]*x1 + fm$coef[var2+1]*x2 + fm$coef[const1+1]*x3+fm$coef[const2+1]*x4+fm$coef[6]*x2*x4
          }
          #
          else if((index_interaction[1]==3 && index_interaction[2]==4)|(index_interaction[2]==3 && index_interaction[1]==4)){
            print("interaction between 3 & 4")
            fm$coef[1] + fm$coef[var1+1]*x1 + fm$coef[var2+1]*x2 + fm$coef[const1+1]*x3+fm$coef[const2+1]*x4+fm$coef[6]*x3*x4
          }
        }
        
        z <- outer(Xseq, Yseq, Constant1,Constant2, FUN = zfunc)
        #
        if(i==1){
          if(j==3)
            mainString <- paste0("low",",","low")
          else 
            mainString <- paste0("low",",","High")
        }
        else{ if(j==3)
          mainString <- paste0("high", ",","low")
        else 
          mainString <- paste0("high", ",","High")
        }
        
        
        contour(Xseq, Yseq, z, xlab = label[var1], ylab= label[var2], main = paste(label[const1],",",label[const2],"=",mainString))
        
      }
    }
  }
}


####### Function Call#################
# Linear model ~1+QVh+PI2+PI3+PI5 and interaction "I(QVh*PI2)"
Contour2(fittedModel[[3]],1,2,3,4)   # QVh , PI2 are variables and PI3,PI4 are constant
# Contour2(model,var1,var2,const1,const2)
Contour2(fittedModel[[3]],2,3,1,4)   # PI3, PI2 are variables and QVh,PI4 are constant

