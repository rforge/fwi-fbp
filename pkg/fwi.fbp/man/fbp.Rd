\name{fbp}
\alias{fbp}
\title{
Fire Behavior Prediction System function
}
\description{
fbp calculates the outputs from the Fire Behavior Prediction (FBP) System (Forestry Canada Fire Danger Group 1992) based on given fire weather conditions (from the Fire Weather Index (FWI) System (Van Wagner 1987)), fuel type, and slope (optional).  Fire weather, for the purpose of FBP System calculation, comprises daily weather observations of wind speed and wind direction, and associated outputs from the Fire Weather Index System including fine fuel moisture content (FFMC) and buildup index (BUI).  FWI System components can be calculated with the sister functions fwi and fwiBAT. 
}
\usage{
fbp(input = NULL, output = "Primary")
}
\arguments{
  \item{input}{
    The input data, a dataframe containing fuel types, fire weather component, and slope (see below).  These inputs consist of daily record(s) for a single point (e.g. one weather station) or multiple points (multiple weather stations or a gridded surface).  All input variables have to be named exactly as listed below; however, the variables do not have to be in any particular order. Fuel type is of type character; other arguments are numeric.  Missing values in numeric variables could either be assigned as NA or -1.
    
    \tabular{lll}{
        \bold{Inputs} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr
        \var{id} \tab Unique identifier of a weather station or spatial point (no restriction on data type) \tab N/A\cr
        \var{FuelType} \tab FBP System Fuel Types including "C1", "C2", "C3", "C4", "C5", "C6", "C7", "D1", "M1", "M2", "M3", "M4", "S1", "S2", "S3", "O1a", and "O1b" \tab "C2"\cr
        \var{LAT} \tab Latitude [decimal degrees] \tab 55\cr
        \var{LON} \tab Longitude [decimal degrees] \tab -120\cr
        \var{ELV} \tab Elevation [meters above sea level] \tab 100\cr
        \var{FFMC} \tab Fine fuel moisture code [FWI System component] \tab 90\cr
        \var{BUI} \tab Buildup index [FWI System component] \tab 60\cr
        \var{WS} \tab Wind speed [km/h] \tab 10\cr
        \var{GS} \tab Slope [percent] \tab 0\cr
        \var{Dj} \tab Julian day \tab 180\cr
        \var{Aspect} \tab Aspect [decimal degrees] \tab 0\cr
        \var{PC*} \tab Percent Conifer for M1/M2 [percent] \tab 50\cr
        \var{PDF*} \tab Percent Dead Fir for M3/M4 [percent] \tab 35\cr
        \var{cc*} \tab Percent Cured for O1a/O1b [percent] \tab 80\cr
        \var{GFL*} \tab Grass Fuel Load [kg/m^2] \tab 3\cr
        \var{CBH**} \tab Crown to Base Height [m] \tab 7\cr
        \var{WD**} \tab Wind direction [decimal degrees] \tab 0\cr
        \var{Accel**} \tab 1 = point, 0 = line \tab 0\cr
        \var{BUIEff**} \tab Buildup Index effect:1=yes, 0=no \tab 1\cr
        \var{D0**} \tab Julian day of minimum Foliar Moisture Content \tab 0\cr
        \var{hr**} \tab Hours since ignition \tab 0\cr
        \var{ISI**} \tab Initial spread index \tab 0\cr
        \var{CFL**} \tab Crown Fuel Load [kg/m^2] \tab -1\cr
        \var{FMC**} \tab Foliar Moisture Content if known \tab 0\cr
        \var{SH**} \tab C6 Fuel Type Stand Height [m] \tab 0\cr
        \var{SD**} \tab C6 Fuel Type Stand Density [stems/ha] \tab 0\cr
        \var{theta**} \tab Elliptical direction of calculation [degree] \tab 0\cr
    }
  }
  \item{output}{
    FBP output offers 3 options (see details in \bold{Values} section):
    
    \tabular{ll}{
        \bold{Outputs} \tab \bold{Number of outputs}\cr
        \var{Primary (\bold{default})} \tab 7\cr
        \var{Secondary} \tab 30\cr
        \var{All} \tab 37\cr
    }
  }
}
\details{
The FBP System was updated in 2009 (Wotton et al 2009) with several additional equations that were initially not included in the system.  The fbp function took consideration of both versions of the system and tried to provide a complete suite of fire behavior prediction variables.

Default values of optional input variables provide a reasonable mid-range setting. 

Latitude, longitude, elevation, and the date are used to calculate foliar moisture content, as prescribed by the FBP System.  Note that FBP System are not highly sensitive to changes in foliar moisture content.  For the Foliar Moisture Content (FMC), if the foliar moisture content is specified, the fbp function will use this value directly rather than calculate it.  This is also true to other input variables.

}
\value{
  fbp returns a dataframe with primary, secondary, or all of the primary and secondary outputs (output=ALL). 
  
  Primary FBP output includes the following 7 variables:
  \item{CFB }{Crown Fraction Burned [percent]}
  \item{CFC }{Crown Fuel Consumption [kg/m^2]}
  \item{HFI }{Head Fire Intensity [kW/m]}
  \item{RAZ }{Spread direction azimuth}
  \item{ROS }{Rate of Spread [m/min]}
  \item{SFC }{Surface Fuel Consumption [kg/m^2]}
  \item{TFC }{Total Fuel Consumption [kg/m^2]}
  
  Secondary FBP System outputs include the following 30 variables.  Some optional inputs must be provided in order to calculate secondary outputs (see Arguments). 
  \item{BE }{BUI effect on spread rate}
  \item{SF }{Slope Factor (multiplier for ROS increase upslope)}
  \item{ISI }{Initial Spread Index}
  \item{FMC }{Foliar Moisture Content}
  \item{Do }{Julian Date of minimum FMC}
  \item{RSO }{Critical spread rate for crowning [m/min]}
  \item{CSI }{Critical surface intensity for crowning [kW/m]}
  \item{FROS }{Flank Rate of Spread [m/min]}
  \item{BROS }{Back Rate of Spread [m/min]}
  \item{TROS }{Rate of Spread at angle theta [m/min]}
  \item{HROSt }{Head Rate of Spread at time t [m/min]}
  \item{FROSt }{Flank Rate of Spread at time t [m/min]}
  \item{BROSt }{Back Rate of Spread at time t [m/min]}
  \item{TROSt }{Rate of Spread at angle theta at time t [m/min]}
  \item{FCFB }{Flank Crown Fraction Burned [percent]}
  \item{BCFB }{Back Crown Fraction Burned [percent]}
  \item{TCFB }{Crown Fraction Burned at angle theta [percent]}
  \item{FFI }{Front Head Fire Intensity [kW/m]}
  \item{BFI }{Back Head Fire Intensity [kW/m]}
  \item{TFI }{Total Head Fire Intensity [kW/m]}
  \item{FTFC }{Flank Total Fuel Consumption [kg/m^2]  }
  \item{BTFC }{Back Total Fuel Consumption [kg/m^2]  }
  \item{TTFC }{Total Fuel Consumption at angle theta [kg/m^2]  }
  \item{TI }{Time of Crown Fire Initiation [hrs since ignition]}
  \item{FTI }{Time of Flank Crown Fire initiation [hrs since ignition]}
  \item{BTI }{Time of Back Crown Fire initiation [hrs since ignition]}
  \item{TTI }{Time of Crown Fire initiation at angle theta [hrs since ignition]}
  \item{LB }{Length to Breadth ratio}
  \item{LBt }{Length to Breadth ratio after elapsed time t [m]}
  \item{WSV }{Net vectored wind speed [m/hr]}
}
\references{
1.  Hirsch K.G. 1996. Canadian forest fire behavior prediction (FBP) system: user's guide. Nat. Resour. Can., Can. For. Serv., Northwest Reg., North. For. Cent., Edmonton, Alberta. Spec. Rep. 7. 122p.

2.  Forestry Canada Fire Danger Group. 1992. Development and structure of the Canadian Forest Fire Behavior Prediction System. Forestry Canada, Ottawa, Ontario Information Report ST-X-3. 63 p.

3.  Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to the 1992 Canadian forest fire behavior prediction system. Nat. Resour. Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario, Canada. Information Report GLC-X-10, 45p.
}
\author{
Xianli Wang, Alan Cantin, Marc-Andre Parisien, Mike Wotton, Kerry Anderson, and Mike Flannigan
}

\seealso{
   \code{\link{fwi}},
   \code{\link{fwiBAT}}
}
\examples{
# The dataset is the standard test data
# provided by Wotton et al (2009).
data(test_fbp,package="fwi.fbp")
head(test_fbp)

#Primary output (default)
fbp(test_fbp)  

#Secondary output          
fbp(test_fbp,"Secondary")

#All output          
fbp(test_fbp,"All")

#little tests
fbp(test_fbp[7,])    #Single record
fbp(test_fbp[8:13,])  #Section of the records

#fbp function produces the default values if no data is fed to
#the function:
fbp()
}

\keyword{ methods }
