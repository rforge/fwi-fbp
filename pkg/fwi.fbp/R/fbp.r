fbp  <- function(dat=NULL,output="Primary"){                                                                                           # The choice of output include: "Primarry" (default), "Secondary", and "All".
### detach the dataset when it is attached from last fail run
  if(!is.na(charmatch("dat",search()))) {detach(dat)} 

### Here is the main function:
  if(is.null(dat)){                                                                                                                     # default input data is NULL
    FUELTYPE     <- "C2"; ACCEL <- 0; DJ <- 180;D0 <- 0; ELV <- 100; BUIEFF <- 1; HR <- 0; FFMC <- 90
    ISI          <- 0;BUI <- 60; WS <- 10; WD <- 0; GS  <- 0; ASPECT <- 0;PC <- 50; PDF <- 35; CC <- 80
    GFL          <- 3; CBH <- 7; CFL <- -1; LAT <- 55; LONG <- -120; FMC <- 0; THETA <- 0
    dat          <- as.data.frame(cbind(ACCEL,DJ,D0,ELV,BUIEFF,HR,FFMC,ISI,BUI,WS,WD,GS,ASPECT,PC,PDF,CC,GFL,CBH,CFL,LAT,LONG,FMC,THETA))
    dat          <- cbind(FUELTYPE,dat)
    dat[,1]      <- as.character(dat[,1])
    } else {
    names(dat)<-toupper(names(dat))
    ID<-dat$ID;FUELTYPE<-dat$FUELTYPE;FFMC<-dat$FFMC;BUI<-dat$BUI;WS<-dat$WS;WD<-dat$WD;FMC<-dat$FMC
    GS<-dat$GS;LAT<-dat$LAT;LONG<-dat$LONG;ELV<-dat$ELV;DJ<-dat$DJ;D0<-dat$D0;SD<-dat$SD;SH<-dat$SH
    HR<-dat$HR;PC<-dat$PC;PDF<-dat$PDF;GFL<-dat$GFL;CC<-dat$CC;THETA<-dat$THETA;ACCEL<-dat$ACCEL
    ASPECT<-dat$ASPECT;BUIEFF<-dat$BUIEFF;CBH<-dat$CBH;CFL<-dat$CFL;ISI<-dat$ISI
    
    n0 <- nrow(dat)
    if(!exists("FUELTYPE")|is.null(FUELTYPE)) FUELTYPE<-rep("C2",n0);if(!exists("FFMC")|is.null(FFMC)) FFMC<-rep(90,n0);if(!exists("BUI")|is.null(BUI)) BUI<-rep(60,n0)
    if(!exists("WS")|is.null(WS)) WS<-rep(10,n0);if(!exists("WD")|is.null(WD)) WD<-rep(0,n0);if(!exists("FMC")|is.null(FMC)) FMC<-rep(0,n0);if(!exists("GS")|is.null(GS)) GS<-rep(0,n0)
    if(!exists("LAT")|is.null(LAT)) LAT<-rep(55,n0);if(!exists("LONG")|is.null(LONG)) LONG<-rep(-120,n0);if(!exists("ELV")|is.null(ELV)) ELV<-rep(100,n0)
    if(!exists("SD")|is.null(SD)) SD<-rep(0,n0);if(!exists("SH")|is.null(SH)) SH<-rep(0,n0);if(!exists("DJ")|is.null(DJ)) DJ<-rep(180,n0);if(!exists("D0")|is.null(D0)) D0<-rep(0,n0)
    if(!exists("HR")|is.null(HR)) HR<-rep(0,n0);if(!exists("PC")|is.null(PC)) PC<-rep(50,n0);if(!exists("PDF")|is.null(PDF)) PDF<-rep(35,n0);if(!exists("GFL")|is.null(GFL)) GFL<-rep(3,n0)
    if(!exists("CC")|is.null(CC)) CC<-rep(80,n0);if(!exists("THETA")|is.null(THETA)) THETA<-rep(0,n0);if(!exists("ACCEL")|is.null(ACCEL)) ACCEL<-rep(0,n0)
    if(!exists("ASPECT")|is.null(ASPECT)) ASPECT<-rep(0,n0);if(!exists("BUIEFF")|is.null(BUIEFF)) BUIEFF<-rep(1,n0);if(!exists("CBH")|is.null(CBH)) CBH<-rep(7,n0)
    if(!exists("CFL")|is.null(CFL)) CFL<-rep(-1,n0);if(!exists("ISI")|is.null(ISI)) ISI<-rep(0,n0)
    
    ### Data cleaning up
    WD     <- WD * pi/180
    THETA  <- THETA * pi/180
    ASPECT <- ifelse(ASPECT < 0,ASPECT+360,ASPECT)
    ASPECT <- ASPECT * pi/180

    ACCEL  <- ifelse(is.na(ACCEL)|ACCEL < 0,0,1)	                                                                                                   # line (no accelleration effect) */
    DJ     <- ifelse(is.na(DJ)|DJ < 0 | DJ > 366,0,DJ)
    D0     <- ifelse(is.na(D0)|D0 < 0 | D0 > 366,0,D0)
    ELV    <- ifelse(is.na(ELV)|ELV < 0 | ELV > 10000,0,ELV)
    BUIEFF <- ifelse(BUIEFF < 0,0,1)
    BUIEFF <- ifelse(is.na(BUIEFF),1,BUIEFF)
    HR     <- ifelse(HR < 0,-HR,HR)                                                                                                    # Originally "T"
    HR     <- ifelse(HR > 366*24,24,HR)
    HR     <- ifelse(is.na(HR),0,HR)
    FFMC   <- ifelse(is.na(FFMC)|FFMC < 0 | FFMC > 101.,0,FFMC)
    ISI    <- ifelse(is.na(ISI)|ISI < 0 | ISI > 300,0,ISI)
    BUI    <- ifelse(is.na(BUI)|BUI < 0 | BUI > 1000,0,BUI)
    WS     <- ifelse(is.na(WS)|WS < 0 | WS > 300,0,WS)
    WD     <- ifelse(is.na(WD)|WD < -2*pi | WD > 2*pi,0,WD)
    GS     <- ifelse(is.na(GS)|GS < 0 | GS > 200,0,GS)
    GS     <- ifelse(ASPECT < -2*pi | ASPECT > 2*pi,0,GS)
    PC     <- ifelse(is.na(PC)|PC < 0 | PC > 100,50,PC)
    PDF    <- ifelse(is.na(PDF)|PDF < 0 | PDF > 100,35.0,PDF)
    CC     <- ifelse(is.na(CC)|CC <= 0 | CC > 100,95,CC)                                                                                         # originally "c"
    GFL    <- ifelse(is.na(GFL)|GFL <= 0 | GFL > 100,0.35,GFL)                                                                                    # changed from 0.3 to 0.35, pg 6 - 2009 */
    LAT    <- ifelse(is.na(LAT)|LAT < -90 | LAT > 90,55,LAT)
    LONG    <- ifelse(is.na(LONG)|LONG < -180 | LONG > 360,0,LONG)
    THETA  <- ifelse(is.na(THETA)|THETA  < -2*pi | THETA > 2*pi,0,THETA)
    SD     <- ifelse(is.na(SD)|SD < 0 | SD > 100000, -999,SD)
    SH     <- ifelse(is.na(SH)|SH < 0 | SH > 100, -999,SH)
  }

  FUELTYPE<-as.character(FUELTYPE)
  # Convert time from hours to minutes */
  HR     <- HR*60.
  # Corrections to reorient WAZ, SAZ */
  WAZ    <- WD + pi
  WAZ    <- ifelse(WAZ > 2*pi,WAZ-2*pi,WAZ)
  # nb: BMW's data set appears to have ASPECT not SAZ */
  
  SAZ    <- ASPECT + pi
  SAZ    <- ifelse(SAZ > 2*pi,SAZ-2*pi,SAZ)
  
  # Make LONG positive for the Western Hemisphere */
  LONG<-ifelse(LONG < 0, -LONG,LONG)
  
  ## /* Enter FBP Calculations */
  ## Initialize the output variables.
  SFC    <- TFC<-HFI<-CFB<-ROS<-rep(0,length(LONG))                                                                                       # value 0 means non-fuel or ffmc == 0.
  RAZ    <- rep(-999,length(LONG))                                                                                                        # value -999 means non-fuel or ffmc == 0. Nobody cares about the wind direction in such area.
  if (output=="Secondary"|output=="All"){
    FROS <- BROS<-TROS<-HROSt<-FROSt<-BROSt<-TROSt<-FCFB<-
    BCFB <- TCFB<-FFI<-BFI<-TFI<-FTFC<-BTFC<-TTFC<-rep(0,length(LONG))
    TI   <- FTI<-BTI<-TTI<-LB<-WSV<- rep(-999,length(LONG))
    }
  
  #/* presently, we do not accept a zero CBH; use near zero if necessary */
  CBHs   <- c(2,3,8,4,18,7,10,0,6,6,6,6,0,0,0,0,0)
  names(CBHs)<-c("C1","C2","C3","C4","C5","C6","C7","D1","M1","M2","M3","M4","S1","S2","S3","O1a","O1b")
  
  CBH    <- ifelse(CBH <= 0 | CBH > 50, ifelse(FUELTYPE %in% c("C6")&SD>0&SH>0,
          -11.2 + 1.06*SH + 0.00170*SD,                                                                                                  #/* 91 */
          CBHs[FUELTYPE]),CBH)
  CBH    <- ifelse(CBH <0,0.0000001,CBH)
  
  #/* presently, we do not accept a zero CFL,; use near zero if necessary */
  CFLs   <- c(0.75,0.80,1.15,1.20,1.20,1.80,0.5,0,0.80,0.80,0.80,0.80,0,0,0,0,0)
  names(CFLs)<-c("C1","C2","C3","C4","C5","C6","C7","D1","M1","M2","M3","M4","S1","S2","S3","O1a","O1b")
  CFL    <- ifelse(CFL <= 0|CFL>2.0,CFLs[FUELTYPE],CFL)
  
  FMC    <- ifelse(FMC <= 0 | FMC > 120,.FMCcalc(LAT, LONG, ELV, DJ, D0),FMC)
  SFC    <- .SFCcalc(FUELTYPE, FFMC, BUI, PC, GFL)
  BUI    <- ifelse(BUIEFF !=1,0,BUI)
  WSV0   <- .Slopecalc(FUELTYPE, FFMC, BUI, WS, WAZ, GS,SAZ, FMC, SFC, PC, PDF, CC, CBH,ISI,output="WSV")                                     #/* This turns off BUI effect */
  WSV    <- ifelse(GS > 0 & FFMC > 0,WSV0,WS)
  RAZ0   <- .Slopecalc(FUELTYPE, FFMC, BUI, WS, WAZ, GS,SAZ, FMC, SFC, PC, PDF, CC, CBH,ISI,output="RAZ")
  RAZ    <- ifelse(GS > 0 & FFMC > 0,RAZ0,WAZ)
  ISI    <- ifelse(FFMC > 0,.ISIcalc(FFMC, WSV),ISI)
  ROS    <- ifelse(FUELTYPE %in% c("C6"),.C6calc(FUELTYPE,ISI,BUI,FMC,SFC,CBH,option="ROS"),.ROScalc(FUELTYPE,ISI,BUI,FMC,SFC,PC,PDF,CC,CBH))
  CFB    <- ifelse(FUELTYPE %in% c("C6"),.C6calc(FUELTYPE,ISI,BUI,FMC,SFC,CBH,option="CFB"),ifelse(CFL>0,.CFBcalc(FUELTYPE,FMC,SFC,ROS,CBH),0))
  
    #don't think we need this line - done in the line above
  #CFB    <- ifelse(CFL==0,0,CFBcalc(FUELTYPE, FMC, SFC, ROS, CBH))
  
  TFC    <- .TFCcalc(FUELTYPE, CFL, CFB, SFC, PC, PDF)
  HFI    <- .FIcalc(TFC,ROS)
  CFB    <- ifelse(HR < 0,-CFB,CFB)
  RAZ    <- RAZ * 180/pi
  RAZ    <- ifelse(RAZ==360,0,RAZ)                                                                                                       # 360 degree is the same as 0, FBP use 0 instead (Wotton etal 2009)
  FD     <- rep("I",length(CFB))
  FD     <- ifelse(CFB<0.10,"S",FD)
  FD     <- ifelse(CFB>0.90,"C",FD)
  CFC    <- .TFCcalc(FUELTYPE, CFL, CFB, SFC, PC, PDF,option="CFC")
  if (output=="Secondary"|output=="All"){
      SF     <- ifelse (GS >= 70,10,exp(3.533 * (GS/100)^1.2))                                                                         # /* 39 */
      CSI    <- .CFBcalc(FUELTYPE,FMC,SFC,ROS,CBH,option="CSI")
      RSO    <- .CFBcalc(FUELTYPE,FMC,SFC,ROS,CBH,option="RSO")
      BE     <- .BEcalc(FUELTYPE,BUI)
      LB     <- .LBcalc(FUELTYPE, WSV)
      LBt    <- ifelse(ACCEL == 0,LB,.LBtcalc(FUELTYPE,LB,HR,CFB))
      BROS   <- .BROScalc(FUELTYPE,FFMC,BUI,WSV,FMC,SFC,PC,PDF,CC,CBH)
      FROS   <- .FROScalc(ROS, BROS, LB)
     #/* TROS is the rate of spread towards angle THETA */
      E      <- sqrt(1-1/LB/LB)                                                                                                          #/* eccentricity */
      TROS   <- ROS * (1-E)/(1-E*cos(THETA - RAZ))                                                                                       #/* note: this is the old method using the focus as the ignition point */
    #//   TROS <- ROSthetacalc(ROS, FROS, BROS, THETA)                                                                                   #MARC: what is this?
      ROSt   <- ifelse(ACCEL==0,ROS,.ROStcalc(FUELTYPE, ROS, HR, CFB))
      FROSt  <- ifelse(ACCEL==0,FROS,.FROScalc(ROSt, BROSt, LBt))
      BROSt  <- ifelse(ACCEL==0,BROS,.ROStcalc(FUELTYPE, BROS, HR, CFB))
      TROSt  <- ifelse(ACCEL==0,TROS,ROSt * (1.-sqrt(1.-1./LBt/LBt))/(1.-sqrt(1.-1./LBt/LBt)*cos(THETA - RAZ)))                          #/* note: this is the old method using the focus as the ignition point */
      FCFB   <- ifelse(CFL==0,0,ifelse(FUELTYPE %in% c("C6"),0,.CFBcalc(FUELTYPE, FMC, SFC, FROS, CBH)))
      BCFB   <- ifelse(CFL==0,0,ifelse(FUELTYPE %in% c("C6"),0,.CFBcalc(FUELTYPE, FMC, SFC, BROS, CBH)))
      TCFB   <- ifelse(CFL==0,0,ifelse(FUELTYPE %in% c("C6"),0,.CFBcalc(FUELTYPE, FMC, SFC, TROS, CBH)))
      FTFC   <- .TFCcalc(FUELTYPE, CFL, FCFB, SFC, PC, PDF)
      BTFC   <- .TFCcalc(FUELTYPE, CFL, BCFB, SFC, PC, PDF)
      TTFC   <- .TFCcalc(FUELTYPE, CFL, TCFB, SFC, PC, PDF)
    #/* equilibrium values */
      FFI    <- .FIcalc(FTFC, FROS)
      BFI    <- .FIcalc(BTFC, BROS)
      TFI    <- .FIcalc(TTFC, TROS)
  
    #/* For now... */
      TI     <- 0
      FTI    <- 0
      BTI    <- 0
      TTI    <- 0
      HROSt  <- ifelse(HR < 0,-ROSt,ROSt)
      FROSt  <- ifelse(HR < 0,-FROSt,FROSt)
      BROSt  <- ifelse(HR < 0,-BROSt,BROSt)
      TROSt  <- ifelse(HR < 0,-TROSt,TROSt)
    }
  
  if (exists("ID")){
    ID<-ID
    #if(!is.null(dat)) detach(dat)
    if (output == "Primary"){
        FBP    <- data.frame(ID,CFB,CFC,HFI,RAZ,ROS,SFC,TFC)
        FBP[,2:ncol(FBP)]<-apply(FBP[,2:ncol(FBP)],2,function(.x) ifelse(FUELTYPE %in% c("WA","NF"),0,.x))
        FBP} else
    if (output == "Secondary"){
        FBP    <- data.frame(ID,BE,SF,ISI,FMC,D0,RSO,CSI,FROS,BROS,TROS,HROSt,FROSt,BROSt,TROSt,FCFB,BCFB,TCFB,FFI,BFI,TFI,FTFC,BTFC,TTFC,TI,FTI,BTI,TTI,LB,LBt,WSV)
        FBP[,2:ncol(FBP)]<-apply(FBP[,2:ncol(FBP)],2,function(.x) ifelse(FUELTYPE %in% c("WA","NF"),0,.x))
        FBP} else
    if (output == "All") {
        FBP    <- data.frame(ID,CFB,CFC,HFI,RAZ,ROS,SFC,TFC,BE,SF,ISI,FMC,D0,RSO,CSI,FROS,BROS,TROS,HROSt,FROSt,BROSt,TROSt,FCFB,BCFB,TCFB,FFI,BFI,TFI,FTFC,BTFC,TTFC,TI,FTI,BTI,TTI,LB,LBt,WSV)
        FBP[,2:ncol(FBP)]<-apply(FBP[,2:ncol(FBP)],2,function(.x) ifelse(FUELTYPE %in% c("WA","NF"),0,.x))
        FBP}} else {
        ID     <- row.names(dat)
        #if(!is.null(dat)) detach(dat)
        if (output == "Primary"){
            FBP    <- data.frame(ID,CFB,CFC,HFI,RAZ,ROS,SFC,TFC)
            FBP[,2:ncol(FBP)]<-apply(FBP[,2:ncol(FBP)],2,function(.x) ifelse(FUELTYPE %in% c("WA","NF"),0,.x))
            FBP} else
        if (output == "Secondary"){
            FBP    <- data.frame(ID,BE,SF,ISI,FMC,D0,RSO,CSI,FROS,BROS,TROS,HROSt,FROSt,BROSt,TROSt,FCFB,BCFB,TCFB,FFI,BFI,TFI,FTFC,BTFC,TTFC,TI,FTI,BTI,TTI,LB,LBt,WSV)
            FBP[,2:ncol(FBP)]<-apply(FBP[,2:ncol(FBP)],2,function(.x) ifelse(FUELTYPE %in% c("WA","NF"),0,.x))
            FBP} else
        if (output == "All") {
            FBP    <- data.frame(ID,CFB,CFC,HFI,RAZ,ROS,SFC,TFC,BE,SF,ISI,FMC,D0,RSO,CSI,FROS,BROS,TROS,HROSt,FROSt,BROSt,TROSt,FCFB,BCFB,TCFB,FFI,BFI,TFI,FTFC,BTFC,TTFC,TI,FTI,BTI,TTI,LB,LBt,WSV)
            FBP[,2:ncol(FBP)]<-apply(FBP[,2:ncol(FBP)],2,function(.x) ifelse(FUELTYPE %in% c("WA","NF"),0,.x))
            FBP}}
}
