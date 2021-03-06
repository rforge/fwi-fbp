\name{fbp}
\alias{fbp}
\title{
Fire Behavior Prediction System function, Deprecated
}
\description{
fbp calculates the outputs from the Canadian Forest Fire Behavior Prediction (FBP) System (Forestry Canada Fire Danger Group 1992) based on given fire weather and fuel moisture conditions (from the Canadian Forest Fire Weather Index (FWI) System (Van Wagner 1987)), fuel type, date, and slope. Fire weather, for the purpose of FBP System calculation, comprises observations of 10 m wind speed and direction at the time of the fire, and two associated outputs from the Fire Weather Index System, the Fine Fuel Moisture Content (ffmc) and Buildup Index (bui). FWI System components can be calculated with the sister functions \code{\link{fwi}} and \code{\link{fwiBAT}}.
}
\usage{
fbp(input, output="Primary")
}
\arguments{
  \item{input}{
    The input data, a dataframe containing fuel types, fire weather component, and slope (see below). Each vector of inputs defines a single FBP System prediction for a single fuel type and set of weather conditions. The dataframe can be used to evaluate the FBP System for a single fuel type and instant in time, or multiple records for a single point (e.g., one weather station, either hourly or daily for instance) or multiple points (multiple weather stations or a gridded surface). All input variables have to be named as listed below, but they are case insensitive, and do not have to be in any particular order. Fuel type is of type character; other arguments are numeric. Missing values in numeric variables could either be assigned as NA or leave as blank.
    
    \tabular{lll}{
        \bold{Inputs} \tab \bold{Full names of inputs} \tab \bold{Defaults}\cr
        \var{id} \tab Unique identifier of a weather station or spatial point (no restriction on\cr\tab
                      data type). Will use row.names of the Inputs dataset if not specified \tab N/A\cr
        \var{FuelType} \tab FBP System Fuel Types including "C-1", "C-2", "C-3", "C-4", "C-5",\cr\tab
                            "C-6", "C-7", "D-1", "M-1", "M-2", "M-3", "M-4", "S-1", "S-2",\cr\tab
                            "S-3", "O-1a", and "O-1b". "WA" and "NF" stand for "water" and\cr\tab 
                            "non-fuel", respectively. The "-" in the Fuel Type names could\cr\tab
                             be omitted, and the Fuel Type names are also case-insensitive.\tab "C2"\cr
        \var{LAT} \tab Latitude [decimal degrees] \tab 55\cr
        \var{LON} \tab Longitude [decimal degrees] \tab -120\cr
        \var{ELV***} \tab Elevation [meters above sea level] \tab NA\cr
        \var{FFMC} \tab Fine fuel moisture code [FWI System component] \tab 90\cr
        \var{BUI} \tab Buildup index [FWI System component] \tab 60\cr
        \var{WS} \tab Wind speed [km/h] \tab 10\cr
        \var{GS} \tab Ground Slope [percent] \tab 0\cr
        \var{Dj} \tab Julian day \tab 180\cr
        \var{Aspect} \tab Aspect of the slope [decimal degrees] \tab 0\cr
        \var{PC*} \tab Percent Conifer for M1/M2 [percent] \tab 50\cr
        \var{PDF*} \tab Percent Dead Fir for M3/M4 [percent] \tab 35\cr
        \var{cc*} \tab Percent Cured for O1a/O1b [percent] \tab 80\cr
        \var{GFL*} \tab Grass Fuel Load [kg/m^2] \tab 0.35\cr
        \var{CBH**} \tab Crown to Base Height [m] \tab 3\cr
        \var{WD**} \tab Wind direction [decimal degrees] \tab 0\cr
        \var{Accel**} \tab Acceleration: 1 = point, 0 = line \tab 0\cr
        \var{BUIEff**} \tab Buildup Index effect: 1=yes, 0=no \tab 1\cr
        \var{D0**} \tab Julian day of minimum Foliar Moisture Content \tab 0\cr
        \var{hr**} \tab Hours since ignition \tab 1\cr
        \var{ISI**} \tab Initial spread index \tab 0\cr
        \var{CFL**} \tab Crown Fuel Load [kg/m^2] \tab -1\cr
        \var{FMC**} \tab Foliar Moisture Content if known [percent] \tab 0\cr
        \var{SH**} \tab C-6 Fuel Type Stand Height [m] \tab 0\cr
        \var{SD**} \tab C-6 Fuel Type Stand Density [stems/ha] \tab 0\cr
        \var{theta**} \tab Elliptical direction of calculation [degrees] \tab 0\cr
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
  * Variables associated with certain fuel types. These could be skipped if relevant fuel types do not appear in the input data. 
 ** Variables that could be ignored without causing major impacts to the primary outputs.
*** Elevation is only used in the calculation of Foliar Moisture Content (FMC). However, FMC can also be calculated without elevation input. The default is to not use elevation in the calculation of FMC.

}
\details{
The Canadian Forest Fire Behavior Prediction (FBP) System (Forestry Canada Fire Danger Group. 1992) is a subsystem of the Canadian Forest Fire Danger Rating System, which also includes the Canadian Forest Fire Weather Index (FWI) System. The FBP System provides quantitative estimates of head fire spread rate, fuel consumption, fire intensity, and a basic fire description (e.g., surface, crown) for 16 different important forest and rangeland types across Canada. Using a simple conceptual model of the growth of a point ignition as an ellipse through uniform fuels and under uniform weather conditions, the system gives, as a set of secondary outputs, estimates of flank and back fire behavior and consequently fire area perimeter length and growth rate. 

The FBP System evolved since the mid-1970s from a series of regionally developed burning indexes to an interim edition of the nationally develop FBP system issued in 1984. Fire behavior models for spread rate and fuel consumption were derived from a database of over 400 experimental, wild and prescribed fire observations. The FBP System, while providing quantitative predictions of expected fire behavior is intended to supplement the experience and judgment of operational fire managers (Hirsch, 1996). 

The FBP System was updated with some minor corrections and revisions in 2009 (Wotton et al. 2009) with several additional equations that were initially not included in the system. This fbp function included these updates and corrections to the original equations and provides a complete suite of fire behavior prediction variables.

Default values of optional input variables provide a reasonable mid-range setting. 

Latitude, longitude, elevation, and the date are used to calculate foliar moisture content, using a set of models defined in the FBP System; note that this latitude/longitude-based function is only valid for Canada. If the Foliar Moisture Content (FMC) is specified directly as an input, the fbp function will use this value directly rather than calculate it. This is also true of other input variables.

Note that Wind Direction (WD) is the compass direction from which wind is coming. Wind azimuth (not an input) is the direction the wind is blowing to and is 180 degrees from wind direction; in the absence of slope, the wind azimuth is coincident with the direction the head fire will travel (the spread direction azimuth, RAZ). Slope aspect is the main compass direction the slope is facing. Slope azimuth (not an input) is the direction a head fire will spread up slope (in the absence of wind effects) and is 180 from slope aspect (Aspect).  Wind direction and slope aspect are the commonly used directional identifiers when specifying wind and slope orientation respectively.  The input theta specifies an angle (given as a compass bearing) at which a user is interested in fire behavior predictions; it is typically some angle off of the final spread rate direction since if for instance theta=RAZ (the final spread azimuth of the fire) then the rate of spread at angle theta (TROS) will be equivalent to ROS.

}
\value{
  fbp returns a dataframe with primary, secondary, or all output variables, a combination of the primary and secondary outputs. 

  Primary FBP output includes the following 7 variables:
  \item{CFB }{Crown Fraction Burned by the head fire}
  \item{CFC }{Crown Fuel Consumption [kg/m^2]}
  \item{FD  }{Fire description (S=Surface, I=Intermittent, C=Crown)}
  \item{HFI }{Head Fire Intensity [kW/m]}
  \item{RAZ }{Spread direction azimuth [degrees]}
  \item{ROS }{Equilibrium Head Fire Rate of Spread [m/min]}
  \item{SFC }{Surface Fuel Consumption [kg/m^2]}
  \item{TFC }{Total Fuel Consumption [kg/m^2]}
  
  Secondary FBP System outputs include the following 34 variables. In order to calculate the reliable secondary outputs, depending on the outputs, optional inputs may have to be provided.  
  \item{BE }{BUI effect on spread rate}
  \item{SF }{Slope Factor (multiplier for ROS increase upslope)}
  \item{ISI }{Initial Spread Index}
  \item{FFMC }{Fine fuel moisture code [FWI System component]}
  \item{FMC }{Foliar Moisture Content [percent]}
  \item{Do }{Julian Date of minimum FMC}
  \item{RSO }{Critical spread rate for crowning [m/min]}
  \item{CSI }{Critical Surface Intensity for crowning [kW/m]}
  \item{FROS }{Equilibrium Flank Fire Rate of Spread [m/min]}
  \item{BROS }{Equilibrium Back Fire Rate of Spread [m/min]}
  \item{HROSt }{Head Fire Rate of Spread at time hr [m/min]}
  \item{FROSt }{Flank Fire Rate of Spread at time hr [m/min]}
  \item{BROSt }{Back Fire Rate of Spread at time hr [m/min]}
  \item{FCFB }{Flank Fire Crown Fraction Burned}
  \item{BCFB }{Back Fire Crown Fraction Burned}
  \item{FFI }{Equilibrium Spread Flank Fire Intensity [kW/m]}
  \item{BFI }{Equilibrium Spread Back Fire Intensity [kW/m]}
  \item{FTFC }{Flank Fire Total Fuel Consumption [kg/m^2]  }
  \item{BTFC }{Back Fire Total Fuel Consumption [kg/m^2]  }
  \item{DH }{Head Fire Spread Distance after time hr [m] }
  \item{DB }{Back Fire Spread Distance after time hr [m] }
  \item{DF }{Flank Fire Spread Distance after time hr [m] }
  \item{TI }{Time to Crown Fire Initiation [hrs since ignition] }
  \item{FTI }{Time to Flank Fire Crown initiation [hrs since ignition]}
  \item{BTI }{Time to Back Fire Crown initiation [hrs since ignition]}
  \item{LB }{Length to Breadth ratio}
  \item{LBt }{Length to Breadth ratio after elapsed time hr }
  \item{WSV }{Net vectored wind speed [km/hr]}
  \item{TROS* }{Equilibrium Rate of Spread at bearing theta [m/min] }
  \item{TROSt* }{Rate of Spread at bearing theta at time t [m/min] }
  \item{TCFB* }{Crown Fraction Burned at bearing theta }
  \item{TFI* }{Fire Intensity at bearing theta [kW/m] }
  \item{TTFC* }{Total Fuel Consumption at bearing theta [kg/m^2] }
  \item{TTI* }{Time to Crown Fire initiation at bearing theta [hrs since ignition] }

* These outputs represent fire behaviour at a point on the perimeter of an elliptical fire defined by a user input angle theta. theta represents the bearing of a line running between the fire ignition point and a point on the perimeter of the fire. It is important to note that in this formulation the theta is a bearing and does not represent the angle from the semi-major axis (spread direction) of the ellipse. This formulation is similar but not identical to methods presented in Wotton et al (2009) and Tymstra et al (2009).
}

\author{
Xianli Wang, Alan Cantin, Marc-Andre Parisien, Mike Wotton, Kerry Anderson, and Mike Flannigan
}

\references{
1.  Hirsch K.G. 1996. Canadian Forest Fire Behavior Prediction (FBP) System: user's guide. Nat. Resour. Can., Can. For. Serv., Northwest Reg., North. For. Cent., Edmonton, Alberta. Spec. Rep. 7. 122p.

2.  Forestry Canada Fire Danger Group. 1992. Development and structure of the Canadian Forest Fire Behavior Prediction System. Forestry Canada, Ottawa, Ontario Information Report ST-X-3. 63 p. \url{http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/10068.pdf}

3.  Wotton, B.M., Alexander, M.E., Taylor, S.W. 2009. Updates and revisions to the 1992 Canadian forest fire behavior prediction system. Nat. Resour. Can., Can. For. Serv., Great Lakes For. Cent., Sault Ste. Marie, Ontario, Canada. Information Report GLC-X-10, 45p. \url{http://publications.gc.ca/collections/collection_2010/nrcan/Fo123-2-10-2009-eng.pdf}

4.  Tymstra, C., Bryce, R.W., Wotton, B.M., Armitage, O.B. 2009. Development and structure of Prometheus: the Canadian wildland fire growth simulation Model. Nat. Resour. Can., Can. For. Serv., North. For. Cent., Edmonton, AB. Inf. Rep. NOR-X-417.  
}
\examples{
library(fwi.fbp)
# The dataset is the standard test data
# provided by Wotton et al (2009).
data("test_fbp")
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
