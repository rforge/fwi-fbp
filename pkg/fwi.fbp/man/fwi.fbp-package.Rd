\name{fwi.fbp-package}
\alias{fwi.fbp-package}
\alias{fwi.fbp}
\docType{package}
\title{
Fire Weather Index and Fire Behavior Prediction Systems
}
\description{
The fwi.fbp package is to allow R users to calculate the outputs of the two main components of the Canadian Forest Fire Danger Rating System (CFFDRS): the Fire Weather Index (FWI) System and the Fire Behaviour Prediction (FBP) System.  These systems are widely used internationally to assess fire danger (FWI System) and quantify fire behavior (FBP System). 
}
\details{
\tabular{ll}{
Package: \tab fwi-fbp\cr
Type: \tab Package\cr
Version: \tab 0.33\cr
Date: \tab 2012-08-12\cr
License: \tab GPL-2\cr
}
This package includes three functions that provide maximum functionality in calculating the inputs of the two systems.  Furthermore, the functions have been thoroughly tested against the original standard test databases.
Two functions, fwi and fwiBAT, are used for FWI System calculation, whereas one function, fbp, is used for FBP System calculation.  These functions are not fully independent: their inputs overlap greatly and the user will have to provide FWI System outputs to calculate FBP System outputs.  The fwi function is a low level function that is used to calculate the outputs of the FWI System for one day based on noon local standard time (LST) weather observations of temperature, relative humidity, wind speed, and 24-hour rainfall, as well as the previous day's weather conditions.  The fwiBAT function is similar, but it is higher lever in that it allows the user to directly calculate FWI System outputs from a list of consecutive daily weather observations.  The fbp function calculates the outputs from the Fire Behavior Prediction System based on given fire weather conditions (from the Fire Weather Index (FWI) System (Van Wagner 1987)), fuel type, and slope (optional).
The functions fully operate within the R environment.
}
\author{
Xianli Wang, Alan Cantin, Marc-Andre Parisien, Mike Wotton, Kerry Anderson, and Mike Flannigan

Maintainer: Xianli Wang \email{xianli@ualberta.ca} and  \email{Alan Cantin Alan.Cantin@NRCan-RNCan.gc.ca}
}
\references{
1.  Van Wagner, CE. and T.L. Pickett. 1985. Equations and FORTRAN program for the Canadian Forest Fire Weather Index System. Can. For. Serv., Ottawa, Ont. For. Tech. Rep. 33. 18 p.

2.  Van Wagner, CE. 1987. Development and structure of the Canadian forest fire weather index system. Forest Technology Report 35. (Canadian Forestry Service: Ottawa).

3.  Hirsch K.G. 1996. Canadian forest fire behavior prediction (FBP) system: user's guide. Nat. Resour. Can., Can. For. Serv., Northwest Reg., North. For. Cent., Edmonton, Alberta. Spec. Rep. 7. 122p.

4.  Forestry Canada Fire Danger Group. 1992. Development and structure of the Canadian Forest Fire Behavior Prediction System. Forestry Canada, Ottawa, Ontario Information Report ST-X-3. 63 p.

5.  Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to the 1992 Canadian forest fire behavior prediction system. Nat. Resour. Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario, Canada. Information Report GLC-X-10, 45p.
}

\keyword{ package }
\seealso{
   \code{\link{fwi}},
   \code{\link{fwiBAT}},
   \code{\link{fbp}}
}
\examples{

######## fwi function examples ########
data(test_fwi,package="fwi.fbp")
head(test_fwi)

# Using the default initial values
fwi.out1<-fwi(test_fwi)         

# Using a different set of initial values
fwi.out2<-fwi(test_fwi,init=c(80,10,16,2012,50)) 

# fwi system components calculated based on previous day's 
# fwi outputs. 
fwi.out3<-fwi(test_fwi,fwi.out1)                 

# Using a suite of initials, assuming variables from fwi.out1
# are the initial values for different records
init_suite<-fwi.out1[,c("ffmc","dmc","dc","yr","lat")]
fwi.out4<-fwi(test_fwi,init=init_suite)



######## fwiBAT function examples ########
# The test data is a standard test
# dataset for FWI system(Van Wagner and Pickett 1985). 

data(test_fwi,package="fwi.fbp")
head(test_fwi)

# using the default initial values
fwi.out<-fwiBAT(test_fwi)         

# using a different set of initials 
fwi.out<-fwiBAT(test_fwi,init=c(80,10,16,50)) 

# using only the required input variables:
fwi.out<-fwiBAT(test_fwi[,7:10])



######## fbp function examples ########
# The dataset is the standard test data
# provided by Wotton et al (2009).
data(test_fbp,package="fwi.fbp")
head(test_fbp)

#Primary output (default)
fbp(test_fbp)
#or
fbp(test_fbp,output="Primary") 
#or 
fbp(test_fbp,"P")

#Secondary output          
fbp(test_fbp,"Secondary")
#or
fbp(test_fbp,"S")

#All output          
fbp(test_fbp,"All")
#or
fbp(test_fbp,"A")

#For a single record:
fbp(test_fbp[7,])    
#For a section of the records:
fbp(test_fbp[8:13,])	

#fbp function produces the default values if no data is fed to
#the function:
fbp()

}
