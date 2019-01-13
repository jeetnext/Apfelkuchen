###########################################################################
#initialisation
###########################################################################

setPath<-function(code){
  #preparation
  dimdoe_path<-"../"
  #dimdoe_path<-"/home/kamal/Desktop/prepare/"
  script_path<-paste(dimdoe_path,"lib/",sep="")
  switch(
    code,
    ex_path<-paste(dimdoe_path,"data/UE1_pendulum17/",sep=""),
    ex_path<-paste(dimdoe_path,"data/Ue2_defoamer_19/",sep=""),
    ex_path<-paste(dimdoe_path,"data/UE3_spruehtrock17/",sep=""),
    ex_path<-paste(dimdoe_path,"data/UE4_RelayExample/",sep="")
  )
  #transferring functions
  source(paste(script_path,"dim_doe_functions_TurnQual.R",sep=""))
  source(paste(script_path,"moodle_lib.R",sep=""))
  return(ex_path)
}
