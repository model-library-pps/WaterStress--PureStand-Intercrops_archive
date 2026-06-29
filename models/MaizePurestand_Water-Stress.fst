TITLE WATER STRESS?? -- Pure stand

***********************************************************************************************************************************
***********************************************************************************************************************************
********************************* WATER STRESS?? -- Pure stand ********************************************************************
*
***** Authors: Lammert Bastiaans - Esther Mugi
***** Centre for Crop Systems Analysis - Plant Production Systems, Wageningen University & Research
***** February 10, 2023
*
* This model simulates whether water stress was encounterd by a crop grown in pure stand.
* Field observations on LAI-development or fraction absorbed radiation serve as input to the model
* The model simulates the daily (and cumulative) values of absorbed radiation, transpiration and dry matter production.
* In the model a soil moisture balance is tracked, with precipitation as input and transpiration and percolation as output.
* The soil profile is composed of two layers: a rooted zone of fixed size and the zone below. The water module follows the tipping
* bucket principle: if water holding capacity in the rooting zone is reached, additional water input percolates to the next zone.
* If soil moisture content drops below a critical level, the transpiration is reduced and potential transpiration is not fulfilled.
* Dry matter production is then reduced with a ratio of actual over potential transpiration.
*
* Next to LAI or fraction absorbed radiation the model makes use of other crop and site specific information.
* Crop/canopy characteristics: light extinction coefficient, RUE, transpiration coefficient, rooting depth, soil depletion factor
* Soil characteristics: rooting depth, field capacity, wilting point.
* Weather characterstics: daily values of global radiation and precipitation
*
* List of required input parameters:
* Name           Description                                                                               Units
* -----------    -------------------------------------------------------------------------------------     ---------------------
* RHO            Canopy reflection coefficient                                                             --
* KA             Light extinction coefficient                                                              --
* LAI            Leaf Area Index                                                                           --
* RUE            Radiation use efficiency                                                                  g biomass/ MJ PAR
* Fabs           Fraction absorbed radiation                                                               --
* TRANSPC        Transpiration coefficient                                                                 L/g biomass
* Rdepth         Rooting depth                                                                             dm
* FieldCap       Soil moisture content (volume fraction) at field capacity (FC)                            --
* WiltingP       Soil moisture content (volume fraction) at wilting point (WP)                             --
* P              Crop specific soil moisture depletion factor; relative factor between FC(1) and WP(0)     --
* OBSWT          Observed shoot biomass                                                                    kg DM/ha
*
* OUTPUT-variables
* Name           Description                                                                               Units
* -----------    -------------------------------------------------------------------------------------     ---------------------
* DAILY values
* Iabs           daily absorbed amount of photosynthetic active radiation (PAR)                            (MJ PAR)/m2/d
* RTRANSPact     daily amount of transpiration                                                             kg H2O/m2/d or mm/d
* Rperc          daily amount of percoloation                                                              kg H2O/m2/d or mm/d
* GROWTH         daily shoot growth rate                                                                   kg DM/ha/d
* ACCUMULATED values
* IABSTOT        accumulated amount of absorbed PAR                                                        (MJ PAR)/m2
* Wtransp        accumulated amount of transpiration                                                       kg H2O/m2 or mm
* Wperc          accumulated amounbt of percolation                                                        kg H2O/m2 or mm
* WTOT           accumulated amount of shoot biomass                                                       kg DM/ha
*********************************************************************************************************************************

INITIAL
* Intitial
  INCON NILL = 0.
  INCON WTOTI= 0.1; WSOILI = 126.
* Wsoili = initial amount of water in the soil [L/m2 = mm]

**Crop Maize-2018
* PARAM RUE = 1.435
* FUNCTION KATB = 0.,0.031, 30.,0.139, 64.,0.247, 85.,0.467, 177.,0.467
* FUNCTION LAITB = 1.,0.1, 30.,1.217, 64.,2.871, 85.,3.548, 114.,3.175, 142.,3.175, 165.,0., 177.,0.
* FUNCTION FabsTB = 1.,0.001, 30.,0.192, 64.,0.554, 85.,0.758, 114.,0.672, 142.,0.468, 177.,0.272
* FUNCTION OBSWTB =1.,0., 30.,530., 64.,3075., 85.,3795., 114.,7625., 142.,9914., 177.,10200.

**Crop Maize-2019
 PARAM RUE = 1.435
 FUNCTION KATB = 0., 0.031, 27.,0.139, 55.,0.247, 83.,0.467, 153.,0.467
 FUNCTION LAITB = 1.,0.1, 27.,1.198, 55.,2.349, 83.,1.993, 120.,1.183, 135.,0., 153.,0.
 FUNCTION FabsTB = 1.,0.001, 27.,0.106, 55.,0.349, 83.,0.623, 120.,0.490, 153.,0.388
 FUNCTION OBSWTB =1.,0., 27., 368., 55.,906., 83., 2896., 120., 5378., 153.,5671.


* Light absorption
  PARAM RHO=0.07

*  Transpiration
 PARAM TRANSPC = 0.2; P = 0.8
* transpc = transpirationcoefficient in L water/g biomass; P = soil moisture depletion factor

* Soil water availability
 PARAM Rdepth = 6.; FieldCap = 0.21; WiltingP = 0.13
* Rdepth is rooting depth in dm

* Timer and print settings (177)_Maize 2018
 TIMER STTIME=1.;FINTIM=177.;DELT=1.;PRDEL=1.
* Timer and print settings (153)_Maize 2019
* TIMER STTIME=1.;FINTIM=153.;DELT=1.;PRDEL=1.

 TRANSLATION_GENERAL DRIVER='EUDRIV'
  PRINT LAI,Fabs,AVRAD,IABS,Wsoil,RTRANSPact,RATIOActPot,GROWTH, IABStot,Wrain,Wtransp,Wperc,WTOT,OBSWT, Balance

DYNAMIC
* 1. Crop
  WTOT      = INTGRL(WTOTI,GROWTH)
  GROWTHpot = RUE * Iabs * 10.
  GROWTH    = RATIOActPot * GROWTHpot
* factor 10. relates to conversion from g/m2 to kg/ha

  Fabs     = AFGEN(FabsTB, TIME)
  LAI      = AFGEN(LAITB, TIME)
  KA       = AFGEN(KATB, TIME)
* Light absorption
  IABStot  = INTGRL(NILL, Iabs)
*  IABS     = (1.-RHO)*0.5*AVRAD*(1.-EXP(-KA*LAI))
* using LAI and KA to calculate the amount of absorbed radiation
   IABS    = (1.-Rho)*0.5 * AVRAD * Fabs
* using the measured fraction absorbed radiation to calculate the amount of absorbed radiation
* IABS expressed as MJ(PAR) per m2 per day

* 2. Transpiration
  Wcritical   = Wsoilmin + (1.-P)*(Wsoilmax-Wsoilmin)
  RATIOActPot = INSW(Wsoil-Wcritical,(Wsoil-Wsoilmin)/(Wcritical-Wsoilmin),1.)

  RTRANSPpot  = (GROWTHpot/10.)*TRANSPC
  RTRANSPact  = RATIOActPot * RTRANSPpot

* 3. Soil water availability
  SOILvolroot = 10. * 10. * Rdepth
* soil volume rootable zone in dm^3
  WSOILmin    = SOILvolroot * WiltingP
  WSOILmax    = SOILvolroot * FieldCap

  WSOIL = INTGRL(WSOILI, RWSOIL)
    RWSOIL    = RAIN - RTRANSPact - Rperc
    Rperc     = Max(0.,WSOILexp - WSOILmax)
    WSOILexp  = WSOIL + (RAIN - RTRANSPact)*DELT

* 4. Water-balance
  Wrain       = INTGRL(NILL, RAIN)
  Wtransp     = INTGRL(NILL, RTRANSPact)
  Wperc       = INTGRL(NILL, Rperc)

  BALANCE     = (WSOIL - WSOILI)-(Wrain-Wtransp-Wperc)

   WEATHER WTRDIR='E:\PhD stuff\Academic stuff\DATA\Data analysis\Chapter 4\'
*  WEATHER WTRDIR='C:\Users\basti001\Weather\TZ\'
*  WEATHER CNTR='TZ';ISTN=2; IYEAR=2018
   WEATHER CNTR='TZ';ISTN=2; IYEAR=2019


*        Reading weather data from weather file:
*        RDD    Daily global radiation in     J/m2/d
*        TMMN   Daily minimum temperature in  degree C
*        TMMX   Daily maximum temperature in  degree C
*        VP     Vapour pressure in            kPa
*        WN     Wind speed in                 m/s
*        RAIN   Precipitation in              mm
*        LAT    Latitude of the side          degree
*        DOY    Day of year (=TIME)           d

         AVRAD  = RDD*1E-6
*        AVRAD specified in MJ/m2/d
        
* Observed biomass (kg/ha)
  OBSWT     = AFGEN(OBSWTB,TIME)
END
