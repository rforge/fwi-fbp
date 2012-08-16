\name{fwi}
\alias{fwi}
\title{
Fire Weather Index function
}
\description{
\code{\link{fwi}} is used to calculate the outputs of the Fire Weather Index (FWI) System for one day based on noon local standard time (LST) weather observations of temperature, relative humidity, wind speed, and 24-hour rainfall, as well as the previous day's weather conditions.  This function could be used for either one weather station or for multiple weather stations or a gridded surface.  This is a lower-level function that allows for maximum flexibility in FWI System component calculations.  This package also contains a similar high-level function, \code{\link{fwiBAT}} (for FWI System batch calculation). 
}
\usage{
fwi(dat, yda.fwi = NULL, init = c(ffmc_yda = 85, dmc_yda = 6, dc_yda = 15, lat = 55),out="all",lat.adjust=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
    A dataframe containing input variables of daily weather observations taken at noon LST.  Variable names have to be the same as in the following list, but are case insensitive.  The order in which the input variables are entered is not important. 

    \tabular{ll}{
        \var{id}   \tab Unique identifier of a weather station or spatial point (no restriction on data type)\cr
        \var{lat}  \tab Latitude (decimal degree)\cr
        \var{long} \tab Longitude (decimal degree)\cr
        \var{yr}   \tab Year of observation\cr
        \var{mon}  \tab Month of the year (integer 1-12)\cr
        \var{day}  \tab Day of the month (integer)\cr
        \var{temp} \tab Temperature (centigrade)\cr    
        \var{rh}   \tab Relative humidity (\%)\cr
        \var{ws}   \tab 10-m height wind speed (km/h)\cr
        \var{prec} \tab 24-hour rainfall (mm)\cr
    }
  }
  \item{yda.fwi}{
    The FWI values calculated for the previous day that will be used for the current day's calculation.  This input should be a dataframe that contains fwi outputs from the previous day. When yda.fwi is fed to the function, the initial (i.e., "startup") values in the function would be ignored.
  }
  \item{init}{
    Initial ("startup") values are provided for the calculation of the first day of a multi-day dataframe.  If the initial FWI System inputs from the previous day are not specified, the function will use default values (see below).  The init argument can also accept a dataframe with the same number of rows as that of the input data if the initial values are to be applied to more than one point (e.g. a grid), station, or year. 

    \tabular{ll}{
        \var{ffmc_yda} \tab Fine Fuel Moisture Code (FFMC; unitless) of the previous day. Default value is 85.\cr
        \var{dmc_yda} \tab Duff Moisture Code (DMC; unitless) of the previous day. Default value is 6.\cr
        \var{dc_yda} \tab Drought Code (DC; unitless) of the previous day. Default value is 15.\cr
        \var{yr} \tab Year of observation (optional, default=2011)\cr
        \var{lat} \tab Latitude of the weather station (optional when yr is not given, default=55).  Latitude values are used to make day length adjustments in the function.\cr
    }
  }
  \item{out}{
     The function offers two output options, out="all" will produce an output include both the input and the FWI System outputs; out="fwi" will generate only the FWI system components.
  }
  \item{lat.adjust}{
    The function offers options for whether day length adjustments should be applied to the calculations.  The default value is "TRUE".
  }
}
\details{
The Daily Severity Rating (dsr, see Values) is a transformation of the Fire Weather Index. Day length adjustments in the function affect the computation of Duff Moisture Code (dmc) and Drought Code (dc) (details see Lawson and Armitage, 2008 (\url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/29152.pdf})). These adjustments are made base on the latitude (lat) and month (mon) of the weather observations.

The default initial (i.e., "start-up") fuel moisture code values (FFMC=85, DMC=6, DC=15) provide a reasonable set of conditions for most springtime conditions in Canada, the Northern U.S., and Alaska. They are not suitable for particularly dry winters and are presumably not appropriate for different parts of the world. However, the FWI System components reach quasi-equilibrium fairly rapidly (10 days or so), in which case unrealistic initial values may only affect the first few days of the fire season.
}
\value{
fwi returns a dataframe which includes both the input and the FWI System variables as describe below: 
  \item{Input Variables }{Including id, long, lat, yr, mon, day, temp, rh, ws, and prec.}
  \item{ffmc }{Fine Fuel Moisture Code}
  \item{dmc }{Duff Moisture Code}
  \item{dc }{Drought Code}
  \item{isi }{Initial Spread Index}
  \item{bui }{Buildup Index}
  \item{fwi }{Fire Weather Index}
  \item{dsr }{Daily Severity Rating}

}
\references{
1.  Van Wagner, CE. and T.L. Pickett. 1985. Equations and FORTRAN program for the Canadian Forest Fire Weather Index System. Can. For. Serv., Ottawa, Ont. For. Tech. Rep. 33. 18 p.

2.  Van Wagner, CE. 1987. Development and structure of the Canadian forest fire weather index system. Forest Technology Report 35. (Canadian Forestry Service: Ottawa).
}
\author{
Xianli Wang, Alan Cantin, Marc-Andre Parisien, Mike Wotton, Kerry Anderson, and Mike Flannigan
}
\seealso{
   \code{\link{fwiBAT}},
   \code{\link{fbp}}
}
\examples{
# The test data is a standard test
# dataset (Van Wagner and Pickett 1985). 

data(test_fwi, package="fwi.fbp")
head(test_fwi)

#calculate single set of calculations on each row of the dataset
fwi.out0<-fwi(test_fwi)

# Using the default initial values for a single row
fwi.out1<-fwi(test_fwi[1,])

# Using a different set of initial values
fwi.out2<-fwi(test_fwi[1,],init=c(80,10,16,2012,50)) 

# fwi system components calculated based on previous day's 
# fwi outputs. 
fwi.out3<-fwi(test_fwi[2,],fwi.out1)                 

# Using a suite of initials, assuming variables from fwi.out1
# are the initial values for different records
init_suite<-fwi.out0[,c("ffmc","dmc","dc","yr","lat")]
fwi.out4<-fwi(test_fwi,init=init_suite)
}
\keyword{ methods }
