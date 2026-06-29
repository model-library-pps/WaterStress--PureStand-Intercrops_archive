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
* Field observations on fraction absorbed radiation serve as input to the model
* The model simulates the daily (and cumulative) values of absorbed radiation, transpiration and dry matter production.
* In the model, a soil moisture balance is tracked, with precipitation as input and transpiration and percolation as output.
* The soil profile is composed of two layers: a rooted zone of fixed size and the zone below. The water module follows the tipping
* bucket principle: if water holding capacity in the rooting zone is reached, additional water input percolates to the next zone.
* If soil moisture content drops below a critical level, the transpiration is reduced and potential transpiration is not fulfilled.
* Dry matter production is then reduced with a ratio of actual over potential transpiration.
*
* Next to fraction absorbed radiation, the model makes use of other crop and site specific information.
* Crop/canopy characteristics: RUE, transpiration coefficient, rooting depth, soil depletion factor
* Soil characteristics: rooting depth, field capacity, wilting point.
* Weather characterstics: daily values of global radiation and precipitation
*
* List of required input parameters:
* Name           Description                                                                               Units
* -----------    -------------------------------------------------------------------------------------     ---------------------
* RHO            Canopy reflection coefficient                                                             --
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
  INCON WTOTI= 0.1; WSOILI = 420.
* wsoili = initial amount of water in the soil [L/m2 = mm]

**Crop Long Pigeonpea-2018
* RUE expressed as g biomass/MJ(PAR)
 PARAM RUE = 0.543
 FUNCTION FabsTB = 1.,0.001, 30.,0.073, 64.,0.242, 85.,0.480, 114.,0.768, 142.,0.783, 177.,0.680, 205.,0.518, 240.,0.331, 275.,0.092
 FUNCTION OBSWTB =1.,0., 30.,295., 64.,656., 85.,1599., 114.,1903., 142.,2294., 177.,4203., 205.,4364., 240.,3606., 275.,3135.

**Crop Medium Pigeonpea-2018
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUE = 0.560
* FUNCTION FabsTB = 1.,0.001, 30.,0.074, 64.,0.266, 85.,0.462, 114.,0.670, 142.,0.695, 177.,0.571, 205.,0.500, 240.,0.137
* FUNCTION OBSWTB =1.,0., 30.,291., 64.,530., 85.,1184., 114.,1763., 142.,1966., 177.,4241., 205.,3226., 240.,2619.

**Crop Lablab-2018
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUE = 0.505
* FUNCTION FabsTB = 1.,0., 38.,0., 39.,0.001, 64.,0.226, 85.,0.728, 114.,0.916, 142.,0.960, 177.,0.906, 205.,0.830, 240.,0.337
* FUNCTION OBSWTB =1.,0., 38.,0., 39.,0., 64.,469., 85.,1003., 114.,2308., 142.,2552., 177.,3084., 205.,4005., 240.,2639.

**Crop Long Pigeonpea-2019
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUE = 0.487
* FUNCTION FabsTB = 1.,0.001, 27.,0.069, 55.,0.232, 83.,0.580, 120.,0.834, 153.,0.897, 181.,0.760, 208.,0.483, 237.,0.401, 265.,0.248
* FUNCTION OBSWTB =1.,0., 27.,76., 55.,511., 83.,1450., 120.,2345., 153.,3265., 181.,4539., 208.,5208., 237.,5589., 265.,5825.

**Crop Medium Pigeonpea-2019
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUE = 0.590
* FUNCTION FabsTB = 1.,0.001, 27.,0.111, 55.,0.216, 83.,0.305, 120.,0.597, 153.,0.698, 181.,0.681, 208.,0.333, 237.,0.227
* FUNCTION OBSWTB =1.,0., 27.,60., 55.,432., 83.,1120., 120.,1759., 153.,3205., 181.,3855., 208.,4315., 237.,4789.

**Crop Lablab-2019
* PARAM RUE = 0.737
* FUNCTION FabsTB = 1.,0., 35.,0., 36.,0.001, 55.,0.075, 83.,0.310, 120.,0.449, 153.,0.737, 181.,0.723, 208.,0.203
* FUNCTION OBSWTB =1.,0., 35.,0., 36.,0., 55.,259., 83.,279., 120.,1415., 153.,2999., 181.,4271., 208.,4638.

* Light absorption
  PARAM RHO=0.07

*  Transpiration
PARAM TRANSPC = 0.3; P = 0.65
* transpc = transpirationcoefficient in L water/g biomass; P = soil moisture depletion factor

* Soil water availability
PARAM Rdepth = 20.; FieldCap = 0.21; WiltingP = 0.13
* Rdepth is rooting depth in dm

* Timer and print settings (275)- Long Pigeonpea_2018
 TIMER STTIME=1.;FINTIM=275.;DELT=1.;PRDEL=1.
* Timer and print settings (240)- Medium Pigeonpea & Lablab_2018
* TIMER STTIME=1.;FINTIM=240.;DELT=1.;PRDEL=1.
* Timer and print settings (265)- Long Pigeonpea_2019
* TIMER STTIME=1.;FINTIM=265.;DELT=1.;PRDEL=1.
* Timer and print settings (237)- Medium Pigeonpea_2019
* TIMER STTIME=1.;FINTIM=237.;DELT=1.;PRDEL=1.
* Timer and print settings (208)- Lablab_2019
* TIMER STTIME=1.;FINTIM=208.;DELT=1.;PRDEL=1.

PRINT Fabs,AVRAD,Iabs0,IABS,WTOT,OBSWT,RATIOActPot,Wsoilmin,Wsoil,Wsoilmax,RTRANSPact,Wrain,Wtransp,Wperc,GROWTH, Balance
  TRANSLATION_GENERAL DRIVER='EUDRIV'

DYNAMIC
* 1. Crop
  WTOT=INTGRL(WTOTI,GROWTH)
  GROWTHpot = RUE * Iabs * 10.
  GROWTH = RATIOActPot * GROWTHpot
* factor 10. relates to conversion from g/m2 to kg/ha

  Fabs = AFGEN(FabsTB, TIME)
* Light absorption
   Iabs = (1.-Rho)*0.5 * AVRAD * Fabs
Iabs0 = 0.5 * AVRAD * Fabs
* IABS expressed as MJ(PAR) per m2 per day

* 2. Transpiration
  Wcritical = Wsoilmin + (1.-P)*(Wsoilmax-Wsoilmin)
  RATIOActPot = INSW(Wsoil-Wcritical,(Wsoil-Wsoilmin)/(Wcritical-Wsoilmin),1.)

  RTRANSPpot = (GROWTHpot/10.)*TRANSPC
  RTRANSPact = RATIOActPot * RTRANSPpot

* 3. Soil water availability
  SOILvolroot = 10. * 10. * Rdepth
* soil volume rootable zone in dm^3
  WSOILmin = SOILvolroot * WiltingP
  WSOILmax = SOILvolroot * FieldCap

  WSOIL = INTGRL(WSOILI, RWSOIL)
    RWSOIL = RAIN - RTRANSPact - Rperc
    Rperc = Max(0.,WSOILexp - WSOILmax)
     WSOILexp = WSOIL + (RAIN - RTRANSPact)*DELT

* 4. Water-balance
  Wrain = INTGRL(NILL, RAIN)
  Wtransp = INTGRL(NILL, RTRANSPact)
  Wperc = INTGRL(NILL, Rperc)

  BALANCE = (WSOIL - WSOILI)-(Wrain-Wtransp-Wperc)

   WEATHER WTRDIR='E:\PhD stuff\Academic stuff\DATA\Data analysis\Chapter 4\'
*  WEATHER WTRDIR='C:\Users\basti001\Weather\TZ\'
  WEATHER CNTR='TZ';ISTN=2; IYEAR=2018
*  WEATHER CNTR='TZ';ISTN=2; IYEAR=2019

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
         TMX    = TMMX
         TMN    = TMMN
         TMPA   = 0.5 * (TMX  + TMN)
         TMTMX  = 0.5 * (TMPA + TMX)
         VAPOUR = VP  * 10.
         WIND   = WN

* Observed biomass (kg/ha)
  OBSWT = AFGEN(OBSWTB,TIME)
END

