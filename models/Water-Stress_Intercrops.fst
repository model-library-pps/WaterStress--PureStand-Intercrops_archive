TITLE WATER STRESS?? -- Intercrops

***********************************************************************************************************************************
***********************************************************************************************************************************
********************************* WATER STRESS?? -- Intercrops ********************************************************************
*
***** Authors: Lammert Bastiaans - Esther Mugi
***** Centre for Crop Systems Analysis - Plant Production Systems, Wageningen University & Research
***** February 10, 2023
*
* This model simulates whether water stress was encountered by the component crops of a mixture.
* Field observations on LAI-development or fraction absorbed radiation serve as input to the model
* The model simulates the daily (and cumulative) values of absorbed radiation, transpiration and dry matter production for the two
* component species.
* Total incoming radiation is distributed over the two species, based on plant height, LAI and light extinction coefficient of the
* two species. The canopy is dissected in two layers. The top layer ranges from the maximum height of the tallest species, till the
* maximum height of the shortest species. The bottom layer ranges from the top of the shortest species till ground level. The
* vertical distribution of the LAI of both species is assumed to be homogeneous. Light interception is calculated using Beer's law.
* First, for the top layer which consists of just one species. The light that is transmitted through the first layer serves as input
* to the second layer, which consists of two species. In this layer, first, the light interception by both species combined is
* calculated and then the intercepted light is distributed over the two species based on the product of k*LAI.
* Dry matter production is calculated using a species-specific RUE. The amount of newly produced dry matter is used to calculate
* the potential transpiration, using a species-specific transpiration coefficient.
*
* In the model, a soil moisture balance is tracked, with precipitation as input and transpiration and percolation as output.
* The soil profile is composed of three layers: a first zone till maximum rooting depth of the species with shortest rooting depth,
* the second layer till maximum rooting depth of the other species and the zone below. The water module follows the tipping
* bucket principle: if water holding capacity in the first rooting zone is reached, additional water inflow percolates to the
* second zone. If this zone has also reached its maximum water holding capacity, water will flow to the next zone, out of reach for
* the roots of either of the two species.
* If soil moisture content drops below a critical level for a species, the transpiration is reduced and potential transpiration is
* not fulfilled. This soil moisture depletion factor is crop specific. Transpiration requirement of the species with the deepest
* rooting system is distributed over the two rooting zones, in proportion to the depth of the two layers. If transpiration of the
* first layer cannot be completely met, this amount will be added to the requirement of the second layer.
* If actual transpiration is insufficient to meet the potential transpiration demand of a species, the dry matter production is
* reduced with a ratio of actual over potential transpiration.
*
* Next to LAI or fraction absorbed radiation, the model makes use of other crop and site-specific information.
* Crop/canopy characteristics: light extinction coefficient, plant height, RUE, transpiration coefficient, rooting depth,
* soil depletion factor
* Soil characteristics: rooting depth, field capacity, wilting point.
* Weather characteristics: daily values of global radiation and precipitation
*
* List of required input parameters:
* For crop-specific parameters: Parameters end with M for Maize and L for Legume. 
* Name           Description                                                                               Units
* -----------    -------------------------------------------------------------------------------------     ---------------------
* RHO            Canopy reflection coefficient                                                             --
* KA             Light extinction coefficient                                                              --
* LAI            Leaf Area Index                                                                           --
* HEIGHT         Plant height                                                                              cm
* RUE            Radiation use efficiency                                                                  g biomass/ MJ PAR
* Fabs           Fraction absorbed radiation                                                               --
* TRANSPC        Transpiration coefficient                                                                 L/g biomass
* Rdepth         Rooting depth                                                                             dm
* FieldCap       Soil moisture content (volume fraction) at field capacity (FC)                            --
* WiltingP       Soil moisture content (volume fraction) at wilting point (WP)                             --
* P              Crop specific soil moisture depletion factor; relative factor between FC(1) and WP(0)     --
* OBSW           Observed shoot biomass                                                                    kg DM/ha
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
* IABSAcc        accumulated amount of absorbed PAR                                                        (MJ PAR)/m2
* Wtransp        accumulated amount of transpiration                                                       kg H2O/m2 or mm
* Wperc          accumulated amounbt of percolation                                                        kg H2O/m2 or mm
* WTOT           accumulated amount of shoot biomass                                                       kg DM/ha
*********************************************************************************************************************************

INITIAL
* Intitial
  INCON NILL = 0.
  INCON WTOTMI= 0.1; WTOTLI = 0.1
  INCON WSOILL1I = 126.; WSOILL2I = 294.
* wsoili = initial amount of water in the soil [L/m2 = mm] in Layer 1 and Layer 2

*****2018
**Crop Maize-ldP intercrop 2018_(Maize)
* RUE expressed as g biomass/MJ(PAR)
 PARAM RUEM       = 1.435
 FUNCTION KAMTB   = 0.,0.234, 85.,0.467, 177.,0.467, 275.,0.467
 FUNCTION LAIMTB  = 1.,0.01, 30.,0.981, 64.,2.197, 85.,3.139, 114.,2.074, 142.,2.074, 165.,0., 177.,0., 275.,0.
 FUNCTION HGHMTB  = 1.,0.1, 30.,44., 64.,100., 85.,147., 114.,178., 142.,186., 177.,189., 178.,0., 275.,0.
 FUNCTION OBSWMTB = 1.,0., 30.,484., 64.,3148., 85.,4176., 114.,6986., 142.,8700., 177.,9000., 275.,9000.

**Crop Maize-ldP intercrop 2018_(ldP)
* RUE expressed as g biomass/MJ(PAR)
 PARAM RUEL      = 0.543
 FUNCTION KALTB  = 1.,0.275, 30.,0.275, 64.,0.649, 85.,0.460, 114.,0.499, 142.,0.367, 177.,0.409, 205.,0.396, 240.,0.336,...
                   275.,0.336
 FUNCTION LAILTB = 1.,0.001, 30.,0.069, 64.,0.332, 85.,1.180, 114.,3.232, 142.,3.141, 177.,2.354, 205.,2.051, 240.,1.659,...
                   255.,0., 275.,0.
 FUNCTION HGHLTB = 1.,0.1, 30.,34., 64.,54., 85.,87., 114.,147., 142.,153., 177.,178., 205.,202., 240.,216., 275.,216.
 FUNCTION OBSWLTB= 1.,0., 30.,296., 64.,520., 85.,731., 114.,1071., 142.,1762., 177.,3164., 205.,3572., 240.,3267., 275.,3164.
 FUNCTION FabsTB = 1.,0., 30.,0.210, 64.,0.531, 85.,0.679, 114.,0.754, 142.,0.629, 177.,0.601, 205.,0.536, 240.,0.390, ...
                   255.,0., 275.,0.

**Crop Maize-mdP intercrop 2018_(Maize)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEM      = 1.435
* FUNCTION KAMTB  = 0.,0.234, 85.,0.467, 177.,0.467, 240.,0.467
* FUNCTION LAIMTB = 1.,0.01, 30.,1.127, 64.,3.247, 85.,3.304, 114.,3.198, 142.,3.198, 165.,0., 177.,0., 240.,0.
* FUNCTION HGHMTB = 1.,0.1, 30.,70., 64.,128., 85.,147., 114.,183., 142.,195., 177.,198., 178.,0., 240.,0.
* FUNCTION OBSWMTB= 1.,0., 30.,450., 64.,3637., 85.,5128., 114.,7727., 142.,11009., 177.,11187., 240.,11187.

**Crop Maize-mdP intercrop 2018_(mdP)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEL      = 0.560
* FUNCTION KALTB  = 1., 0.157, 30.,0.157, 64.,0.531, 85.,0.660, 114.,0.428, 142.,0.430, 177.,0.345, 205.,0.466, 240.,0.466
* FUNCTION LAILTB = 1.,0.001, 30.,0.113, 64.,0.161, 85.,0.993, 114.,2.130, 142.,2.359, 177.,2.590, 205.,1.344, 220.,0., 240.,0.
* FUNCTION HGHLTB = 1.,0.1, 30.,33., 64.,47., 85.,77., 114.,121., 142.,144., 177.,161., 205.,180., 240.,180.
* FUNCTION OBSWLTB= 1.,0., 30.,278., 64.,472., 85.,762., 114.,1354., 142.,1967., 177.,2667., 205.,2503., 240.,1842.
* FUNCTION FabsTB = 1.,0., 30.,0.262, 64.,0.606, 85.,0.730, 114.,0.766, 142.,0.669, 177.,0.522, 205.,0.367, 220.,0., 240.,0.

**Crop Maize-Lablab intercrop 2018_(Maize)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEM      = 1.435
* FUNCTION KAMTB  = 0.,0.234, 85.,0.467, 240.,0.467
* FUNCTION LAIMTB = 1.,0.01, 30.,1.421, 64.,3.032, 85.,3.598, 114.,2.868, 142.,2.868, 165.,0., 177.,0., 240.,0.
* FUNCTION HGHMTB = 1.,0.1, 30.,56., 64.,107., 85.,129., 114.,176., 142.,182., 177.,184., 178.,0., 240.,0.
* FUNCTION OBSWMTB= 1.,0., 30.,430., 64.,2727., 85.,3738., 114.,7356., 142.,10756., 177.,11165., 240.,11165.

**Crop Maize-Lablab intercrop 2018_(Lablab)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEL      = 0.505
* FUNCTION KALTB  = 0., 0.209, 64.,0.772, 240.,0.772
* FUNCTION LAILTB = 1.,0., 38.,0., 39.,0.001, 64.,0.084, 85.,1.227, 114.,2.634, 142.,3.204, 177.,2.762, 205.,2.258, 225.,0., 240.,0.
* FUNCTION HGHLTB = 1.,0., 38.,0., 39.,0.1, 64.,8., 85.,70., 114.,132., 142.,137., 177.,138., 205.,138., 240.,138.
* FUNCTION OBSWLTB= 1.,0., 38.,0., 39.,0., 64.,399., 85.,760., 114.,1363., 142.,2142., 177.,2531., 205.,3132., 240.,2105.
* FUNCTION FabsTB = 1.,0., 30.,0.120, 64.,0.467, 85.,0.688, 114.,0.819, 142.,0.905, 177.,0.925, 205.,0.852, 225.,0., 240.,0.
***********************************************************************************************************************************
*****2019
**Crop Maize-ldP intercrop 2019_(Maize)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEM      = 1.435
* FUNCTION KAMTB  = 1.,0.098, 27.,0.098, 55.,0.204, 83.,0.495, 120.,0.584, 153.,0.584
* FUNCTION LAIMTB = 1.,0.01, 27.,1.112, 55.,3.076, 83.,2.197, 120.,1.064, 153.,0., 265.,0.
* FUNCTION HGHMTB = 1.,0.1, 27.,49., 55.,136., 83.,179., 120.,191., 153.,191., 154.,0., 265.,0.
* FUNCTION OBSWMTB= 1.,0., 27.,463., 55.,994., 83.,3162., 120.,4997., 153.,5249., 154.,5249., 265.,5249.

**Crop Maize-ldP intercrop 2019_(ldP)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEL      = 0.487
* FUNCTION KALTB  = 1.,0.572, 27.,0.572, 55.,0.815, 83.,0.637, 120.,0.759, 153.,0.907, 181.,0.611, 208.,0.646, 237.,0.877,...
*                    265.,0.877
* FUNCTION LAILTB = 1.,0.001, 27.,0.061, 55.,0.195, 83.,0.484, 120.,0.964, 153.,1.822, 181.,3.010, 208.,1.050, 237.,0.698,...
*                   255.,0., 265.,0.
* FUNCTION HGHLTB = 1.,0.1, 27.,29., 55.,65., 83.,100., 120.,151., 153.,192., 181.,218., 208.,219., 237.,228., 265.,228.
* FUNCTION OBSWLTB= 1.,0., 27.,43., 55.,365., 83.,708., 120.,1382., 153.,2239., 181.,3033., 208.,3704., 237.,4321., 265.,4651.
* FUNCTION FabsTB = 1.,0., 27.,0.189, 55.,0.536, 83.,0.604, 120.,0.669, 153.,0.726, 181.,0.654, 208.,0.616, 237.,0.517, ...
*                   255.,0., 265.,0.

**Crop Maize-mdP intercrop 2019_(Maize)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEM      = 1.435
* FUNCTION KAMTB  = 1.,0.098, 27.,0.098, 55.,0.204, 83.,0.495, 120.,0.584, 153.,0.584
* FUNCTION LAIMTB = 1.,0.01, 27.,1.113, 55.,2.431, 83.,1.248, 120.,0.744, 153.,0., 237.,0.
* FUNCTION HGHMTB = 1.,0.1, 27.,56., 55.,123., 83.,170., 120.,185., 153.,185., 154.,0., 237.,0.
* FUNCTION OBSWMTB= 1.,0., 27.,333., 55.,1107., 83.,2654., 120.,4818., 153.,5032., 154.,5032., 237.,5032.

**Crop Maize-mdP intercrop 2019_(mdP)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEL      = 0.590
* FUNCTION KALTB  = 1.,0.741, 27.,0.741, 55.,0.749, 83.,0.445, 120.,0.746, 153.,0.786, 181.,0.616, 208.,0.571, 237.,0.571
* FUNCTION LAILTB = 1.,0.001, 27.,0.057, 55.,0.131, 83.,0.279, 120.,0.914, 153.,1.536, 181.,0.890, 208.,0.476, 225.,0., 237.,0.
* FUNCTION HGHLTB = 1.,0.1, 27.,31., 55.,60., 83.,82., 120.,126., 153.,153., 181.,175., 208.,175., 237.,175.
* FUNCTION OBSWLTB= 1.,0., 27.,49., 55.,371., 83.,689., 120.,1549., 153.,2503., 181.,3032., 208.,3128., 237.,3190.
* FUNCTION FabsTB = 1.,0., 27.,0.283, 55.,0.538, 83.,0.648, 120.,0.659, 153.,0.683, 181.,0.637, 237.,0.0

**Crop Maize-Lablab intercrop 2019_(Maize)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEM      = 1.435
* FUNCTION KAMTB  = 1.,0.098, 27.,0.098, 55.,0.204, 83.,0.495, 120.,0.584, 153.,0.584, 208.,0.584
* FUNCTION LAIMTB = 1.,0.01, 27.,0.987, 55.,2.116, 83.,2.124, 120.,1.696, 153.,0., 208.,0.
* FUNCTION HGHMTB = 1.,0.1, 27.,45., 55.,121., 83.,165., 120.,184., 153.,184., 154.,0., 208.,0.
* FUNCTION OBSWMTB= 1.,0., 27.,377., 55.,1310., 83.,3662., 120.,6217., 153.,6408., 208.,6408.

**Crop Maize-Lablab intercrop 2019_(Lablab)
* RUE expressed as g biomass/MJ(PAR)
* PARAM RUEL      = 0.737
* FUNCTION KALTB  = 0., 0.209, 64.,0.772, 208.,0.772
* FUNCTION LAILTB = 1.,0., 35.,0., 36.,0.001, 55.,0.027, 83.,0.128, 120.,0.351, 153.,0.702, 181.,0.328, 195.,0., 208.,0.
* FUNCTION HGHLTB = 1.,0., 35.,0., 36.,0.1, 55.,6., 83.,7., 120.,13., 153.,88., 181.,129., 208.,131.
* FUNCTION OBSWLTB= 1.,0., 35.,0., 36.,0., 55.,113., 83.,245., 120.,636., 153.,1149., 181.,1628., 208.,1856.
* FUNCTION FabsTB = 1.,0., 27.,0.262, 55.,0.606, 83.,0.730, 120.,0.766, 153.,0.669, 181.,0.522, 208.,0.

***********************************************************************************************************************************
* Light absorption
  PARAM RHO       = 0.07

*  Transpiration
  PARAM TRANSPCM  = 0.2; PM = 0.8
  PARAM TRANSPCL  = 0.3; PL = 0.65
* transpc = transpirationcoefficient in L water/g biomass; P = soil moisture depletion factor

* Soil water availability
  PARAM RdepthM   = 6.; RdepthL = 20.
* Rdepth is rooting depth in dm
  PARAM FieldCap  = 0.21; WiltingP = 0.13
* Soil characteristics

* Timer and print settings (275; MZ-ldP_2018)
  TIMER STTIME=1.;FINTIM=275.;DELT=1.;PRDEL=1.
* Timer and print settings (240; MZ-mdP & MZ-Lablab_2018 and MZ-mdP_2019 )
* TIMER STTIME=1.;FINTIM=240.;DELT=1.;PRDEL=1.
* Timer and print settings (265; MZ-ldP_2019)
* TIMER STTIME=1.;FINTIM=265.;DELT=1.;PRDEL=1.
* Timer and print settings (208; MZ-Lablab_2019)
* TIMER STTIME=1.;FINTIM=208.;DELT=1.;PRDEL=1.

  TRANSLATION_GENERAL DRIVER='EUDRIV'
 PRINT LAIM,LAIL,AVRAD,IABSM,IABSL,FabsTOT,FabsOBS,HEIGHTM,HEIGHTL,WsoilL1,WsoilL2,RATIOActPotM,RATIOActPotL,RTRANSPTOTact,...
       IABSAccM, IABSAccL,Wtransp, Wperc, Wrain, WTOTM,OBSWM,WTOTL,OBSWL, Balance


DYNAMIC
* 1. Maize Crop
* 1.a. Dry matter production (kg/ha)
  WTOTM      = INTGRL(WTOTMI,GROWTHM)
  GROWTHMpot = RUEM * IabsM * 10.
  GROWTHM    = RATIOActPotM * GROWTHMpot
* factor 10. relates to conversion from g/m2 to kg/ha

* 1.b. LAI-development and plant height
  LAIM       = AFGEN(LAIMTB, TIME)
  KAM        = AFGEN(KAMTB, TIME)
  HEIGHTM    = AFGEN(HGHMTB, TIME)
   LAITM     = MAX(((HEIGHTM-HEIGHTL)/NOTNUL(HEIGHTM))*LAIM, 0.)
   LAIBM     = LAIM-LAITM
* LAI separated into a top (T) and a bottom (B) layer

* 2. Legume Crop
* 2.a. Dry matter production (kg/ha)
  WTOTL      = INTGRL(WTOTLI,GROWTHL)
  GROWTHLpot = RUEL * IabsL * 10.
  GROWTHL    = RATIOActPotL * GROWTHLpot
* factor 10. relates to conversion from g/m2 to kg/ha

* 2.b. LAI-development
  LAIL      = AFGEN(LAILTB, TIME)
  KAL       = AFGEN(KALTB, TIME)
  HEIGHTL   = AFGEN(HGHLTB, TIME)
   LAITL    = MAX(((HEIGHTL-HEIGHTM)/NOTNUL(HEIGHTL))*LAIL, 0.)
   LAIBL    = LAIL-LAITL
* LAI separated into a top (T) and a bottom (B) layer

* 3. Light absorption and distribution
* IABS expressed as MJ(PAR) per m2 per day
* 3.a. Top-layer
  IabsTM    = (1.-RHO)*0.5*AVRAD*(1.-EXP(-KAM*LAITM))
  IabsTL    = (1.-RHO)*0.5*AVRAD*(1.-EXP(-KAL*LAITL))
  IMID      = (1.-RHO)*0.5*AVRAD*EXP(-KAM*LAITM-KAL*LAITL)
* Light intensity at the border between top and bottom layer; is the incoming radiation for the bottom layer.

* 3.b. Bottom-layer
*   IabsBTOT = IMID*(1.-EXP(-KAM*LAIBM-KAL*LAIBL))
*   IabsBM   = ((KAM*LAIBM)/NOTNUL(KAM*LAIBM+KAL*LAIBL))*IabsBTot
*   IabsBL   = ((KAL*LAIBL)/NOTNUL(KAM*LAIBM+KAL*LAIBL))*IabsBTot
* Upper lines, in case, also for the legume, the product K*LAI is calculated based on K and LAI
   IabsBTOT  = IMID*(1.-EXP(-KAM*LAIBM-KLAILeg))
   IabsBM    = ((KAM*LAIBM)/NOTNUL(KAM*LAIBM+KLAILeg))*IabsBTot
   IabsBL    = ((KLAILeg)/NOTNUL(KAM*LAIBM+KLAILeg))*IabsBTot
** If LAI is difficult to measure with legumes, a relation between shoot dry weight and the product of k*LAI can be derived.
** This product K.LAI can be directly derived from light absortption measurements (using Beer's law) in pure stands.

** the KLAI relation for ldP
   KLAILeg   = 0.692*1E-7*(OBSWL)**2 + 0.0005061*OBSWL
** the KLAI relation for mdP
*  KLAILeg   = 1.368*1E-7*(OBSWL)**2 + 0.000327*OBSWL
** the KLAI relation for Lablab
*  KLAILeg   = 2.05*1E-7*(OBSWL)**2 + 0.0005485*OBSWL

* 3.c. Summation per species
  IabsM      = IabsTM + IabsBM
  IabsL      = INSW(LAIM-0.001,FabsOBS*0.5*AVRAD,(IabsTL + IabsBL))
* Once maize is harvested, the light interception of the legume is calculated directly from the measured fraction light interception
  FabsTOT    = (IabsM+IabsL)/(0.5*AVRAD)
  FabsOBS    = AFGEN(FabsTB, TIME)
  IABSAccM   = INTGRL(NILL,IabsM)
  IABSAccL   = INTGRL(NILL,IabsL)

* FabsPMobs = AFGEN(FabsPMTB, TIME)
* FabsPLobs = AFGEN(FabsPLTB, TIME)

* 4. Transpiration
* From the top-layer
  WcriticalML1 = WsoilminL1 + (1.-PM)*(WsoilmaxL1-WsoilminL1)
  WcriticalLL1 = WsoilminL1 + (1.-PL)*(WsoilmaxL1-WsoilminL1)
  WcriticalLL2 = WsoilminL2 + (1.-PL)*(WsoilmaxL2-WsoilminL2)

  RATIOActPotM   = INSW(WsoilL1-WcriticalML1,(WsoilL1-WsoilminL1)/(WcriticalML1-WsoilminL1),1.)
  RATIOActPotLL1 = INSW(WsoilL1-WcriticalLL1,(WsoilL1-WsoilminL1)/(WcriticalLL1-WsoilminL1),1.)
  RATIOActPotLL2 = INSW(WsoilL2-WcriticalLL2,(WsoilL2-WsoilminL2)/(WcriticalLL2-WsoilminL2),1.)

  RTRANSPMpot    = (GROWTHMpot/10.)*TRANSPCM
  RTRANSPMact    = RATIOActPotM * RTRANSPMpot

  RTRANSPLpot    = (GROWTHLpot/10.)*TRANSPCL
  RTRANSPLL1pot  = RTRANSPLpot*(RdepthM/RdepthL)
  RTRANSPLL1act  = RATIOActPotLL1 * RTRANSPLL1pot
  RTRANSPL1act   = RTRANSPMact + RTRANSPLL1act

  RTRANSPLL2pot  = RTRANSPLpot*((RdepthL-RdepthM)/RdepthL)+(RTRANSPLL1pot-RTRANSPLL1act)
  RTRANSPLL2act  = RATIOActPotLL2 * RTRANSPLL2pot
  RTRANSPLact    = RTRANSPLL1act + RTRANSPLL2act

  RATIOActPotL   = RTRANSPLact/NOTNUL(RTRANSPLpot)
  RTRANSPTOTact  = RTRANSPL1act + RTRANSPLL2act

* 5. Soil water availability
  SOILvolrootL1  = 10. * 10. * RdepthM
* soil volume rootable zone in dm^3
  WSOILminL1     = SOILvolrootL1 * WiltingP
  WSOILmaxL1     = SOILvolrootL1 * FieldCap

  WSOILL1        = INTGRL(WSOILL1I, RWSOILL1)
    RWSOILL1     = RAIN - RTRANSPL1act - RpercL1
    RpercL1      = Max(0.,WSOILexpL1 - WSOILmaxL1)
     WSOILexpL1  = WSOILL1 + (RAIN - RTRANSPL1act)*DELT

  SOILvolrootL2 = 10. * 10. * (RdepthL-RdepthM)
* soil volume rootable zone in dm^3
  WSOILminL2    = SOILvolrootL2 * WiltingP
  WSOILmaxL2    = SOILvolrootL2 * FieldCap

  WSOILL2       = INTGRL(WSOILL2I, RWSOILL2)
    RWSOILL2    = RpercL1 - RTRANSPLL2act - RpercL2
    RpercL2     = Max(0.,WSOILexpL2 - WSOILmaxL2)
     WSOILexpL2 = WSOILL2 + (RpercL1 - RTRANSPLL2act)*DELT

* 6. Water-balance
  Wrain        = INTGRL(NILL, RAIN)
  Wtransp      = INTGRL(NILL, RTRANSPTOTact)
  Wperc        = INTGRL(NILL, RpercL2)

  BALANCE      = ((WSOILL1+WSOILL2) - (WSOILL1I+WSOILL2I))-(Wrain-Wtransp-Wperc)

 WEATHER WTRDIR='E:\PhD stuff\Academic stuff\DATA\Data analysis\Chapter 4\'
* WEATHER WTRDIR='C:\Users\basti001\Weather\TZ\'
 WEATHER CNTR='TZ';ISTN=2; IYEAR=2018
* WEATHER CNTR='TZ';ISTN=2; IYEAR=2019

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
  OBSWM   = AFGEN(OBSWMTB,TIME)
  OBSWL   = AFGEN(OBSWLTB,TIME)
END
