\name{fwiBAT}
\alias{fwiBAT}
\title{
Fire Weather Index batch function
}
\description{
fwiBAT generates the Fire Weather Index (FWI) System for one weather station during a continuous period of time.  This is a high-level function in which FWI System components are calculated from a list of consecutive (e.g., all days of a fire season).  For maximum flexibility, the low-level function fwi should be used. 
}
\usage{
fwiBAT(dat, init=c(ffmc_yda=85,dmc_yda=6,dc_yda=15,lat=55),out="all",lat.adjust=TRUE)
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
   fwiBAT returns a data.frame which includes both the input and the FWI variables as describe below: 
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
   \code{\link{fwi}},
   \code{\link{fbp}}
}
\examples{
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

# fwiBAT is not very efficient in dealing with large 
# dataset (e.g. > 50,000 records). Splitting the data into
# smaller pieces speeds up the computation greatly.

# For example, when calculating fwi for a weather station for
# multiple years (e.g. >30) without any breaks (365 days a 
# year),the following script will be many times faster than
# using fwiBAT directly. Because mostly the data feed into
# fwiBAT will be fire season fire weather data within a year,
# the lengthy computation is not a common event. 
out.fwi0<-out.fwi<-NULL
n0<-round(nrow(test_fwi)/365)
n<-ifelse(365*n0>=nrow(test_fwi),n0,n0+1)
for (i in 1:n){
    dat0<-test_fwi[(365*(i-1)+1):(365*i),]
    dat0<-dat0[!is.na(dat0[,1]),]
    if (i==1){
        out.fwi0<-fwiBAT(dat0)
    } else {
        out.fwi0<-   fwiBAT(dat0,init=out.fwi[nrow(out.fwi),c("ffmc","dmc","dc","lat")])
    }
    out.fwi<-rbind(out.fwi,out.fwi0)
}
}
\keyword{ methods }
