fwiBAT<-function(dat,init=c(ffmc_yda=85,dmc_yda=6,dc_yda=15,lat=55),out="all",lat.adjust=TRUE){
  names(dat)<-tolower(names(dat))
  if(!is.na(charmatch("dat",search()))) {detach(dat)} 
  out.fwi<-NULL
  for (j in 1:nrow(dat)){
        if (j==1){out.fwi0<-fwi(dat[j,],init=init,out=out,lat.adjust=lat.adjust)} else {
           out.fwi0<-fwi(dat=dat[j,],yda.fwi=out.fwi0,init=init,out=out,lat.adjust=lat.adjust)}
           out.fwi<-rbind(out.fwi,out.fwi0)
        }
  out.fwi
}
