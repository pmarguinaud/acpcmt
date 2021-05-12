SUBROUTINE ADVPRCS (KIDIA, KFDIA, KGPBLKS,KLON, KTDIA, KFLEV,&
 & PT, PQ, PQL, PQI, PAUTOL, PAUTOI, &
 & PQR, PQS, PNEB, &
 & PCP, PR, PAPHI, PAPRSF, PDELP,&
 & PFPLSL, PFPLSN, PFPEVPL, PFPEVPN, PFPFPL, PFPFPN, PSEDIQL, PSEDIQN )  

! ========================================================

!   THIS ROUTINE PERFORMS THE VERTICAL ADVECTION OF
!   PRECIPITATION PARTICLES.
!   IT ALSO COMPUTES COLLECTION AND EVAPORATION PROCESSES.

! ========================================================

!   Auteur: Yves Bouteloup, CNRM/GMAP FROM ADVPRC

!   Date: 2006-06   

!     Modifications.
!     --------------
!     2006-10-30, F. Bouyssel : Introduction of RHEVAP and ZALPHA
!     2007-04-06, F. Bouyssel : Change in precipitation evaporation (LLEVAPX)
!     2008-01-24, Y. Bouteloup : Change in taking account of the melting (like evaporation for snow !)
! This is very important. Each disparition process for a species must be treated as this
!     2010-04-06, F. Bouyssel : Sedimentation speed for clouds
!     2010-04-30, Y. Bouteloup : Freezing of rain + some cleaning and simplifications
!     2010-12-03, F. Bouyssel : Removal of ZQFRZ=0=ZQFRZX
!     2011-06-08, O. Riviere: Introduction of LSMOOTHMELT to smooth melting
!     around 0°C
!      R. El Khatib 22-Jul-2014 Vectorizations
! ========================================================

! ---------------
! INPUT VARIABLES
! ---------------

! KIDIA, : DEBUT/FIN DES BOUCLES HORIZONTALES (IST,IEND DANS CPG).
! KFDIA  : START/END OF HORIZONTAL LOOP       (IST,IEND IN   CPG).
! KLON   : DIMENSION HORIZONTALE              (NPROMA   DANS CPG).
!        : HORIZONTAL DIMENSION               (NPROMA   IN   CPG).
! KTDIA  : INDICE DE DEPART DES BOUCLES VERTICALES.
!        : START OF THE VERTICAL LOOP IN THE PHYSICS.
! KLEV   : FIN BOUCLE VERTICALES, DIMENSION VERTICALE (NFLEVG DANS CPG).
!        : END OF VERTICAL LOOP, VERTICAL DIMENSION   (NFLEVG IN   CPG).

! PT     : TEMPERATURE.
!        : TEMPERATURE.
! PQ     : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
!        : SPECIFIC HUMIDITY OF WATER VAPOUR.
! PQL    : QUANTITE SPECIFIQUE D'EAU CONDENSEE LIQUIDE
!        : LIQUID CONDENSED WATER SPECIFIC HUMIDITY
! PQI    : QUANTITE SPECIFIQUE D'EAU CONDENSEE SOLIDE
!        : SOLID CONDENSED WATER SPECIFIC HUMIDITY
! PAUTOL : GENERATION DE PRECIPITATIONS A PARTIR DE L'EAU NUAGEUSE LIQ.
!        : GENERATION OF PRECIPITATION FROM LIQUID CLOUD WATER (ACMICRO). 
! PAUTOI : GENERATION DE PRECIPITATIONS A PARTIR DE L'EAU NUAGEUSE SOLIDE.
!        : GENERATION OF PRECIPITATION FROM SOLID CLOUD WATER (ACMICRO). 
! PQR    : QUANTITE SPECIFIQUE D'EAU PRECIPITANTE LIQUIDE.
!        : LIQUID PRECIPITATING WATER SPECIFIC HUMIDITY.
! PQS    : QUANTITE SPECIFIQUE D'EAU PRECIPITANTE SOLIDE.
!        : SOLID PRECIPITATING WATER SPECIFIC HUMIDITY.
! PNEB   : NEBULOSITE TOTALE
!        : TOTAL CLOUDINESS  
! PCP    : CHALEUR MASSIQUE A PRESSION CONSTANTE DE L'AIR.
!        : SPECIFIC HEAT AT CONSTANT PRESSURE FOR AIR.
! PR     : CONSTANTE DES GAZ POUR L'AIR.
!        : GAS CONSTANT FOR AIR.
! PAPHI  : GEOPOTENTIEL SUR DEMI-NIVEAUX.
!        : GEOPOTENTIAL ON HALF-LEVELS.
! PAPRSF : PRESSION SUR LES NIVEAUX PLEINS.
!        : PRESSURE ON FULL LEVELS.
! PDELP  : EPAISSEUR EN PRESSION DE LA COUCHE.
!        : LAYER THICKNESS IN PRESSURE UNITS.

! ---------------
! OUTPUT VARIABLES
! ---------------

! PFPLSL  : FLUX DE PRECIPITATION LIQUIDE (PLUIE).
!         : RAIN FLUX.
! PFPLSN  : FLUX DE PRECIPITATION SOLIDE  (NEIGE).
!         : ICE PRECIPITATION FLUX.
! PFPEVPL : FLUX ASSOCIE A L'EVAPORATION DES PRECIP.
!         : FLUX ASSOCIATED TO EVAPORATION OF PRECIPITATIONS.
! PFPEVPN : FLUX ASSOCIE A LA SUBLIMATION DES PRECIP.
!         : FLUX ASSOCIATED TO SUBLIMATION OF PRECIPITATIONS.
! PFPFPL  : FLUX DE GENERATION DE PRECIPITATIONS LIQUIDES.
!         : FLUX OF LIQUID PRECIPITATION GENERATION.
! PFPFPN  : FLUX DE GENERATION DE PRECIPITATIONS SOLIDES.
!         : FLUX OF SOLID PRECIPITATION GENERATION.
! PSEDIQL : FLUX SEDIMENTATION D'EAU LIQUIDE NUAGEUSE.
!         : FLUX SEDIMENTATION OF CLOUD LIQUID WATER.
! PSEDIQN : FLUX SEDIMENTATION D'EAU SOLIDE NUAGEUSE.
!         : FLUX SEDIMENTATION OF CLOUD SOLID WATER.

! ========================================================

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOMPHY    , ONLY : YRPHY
USE YOMPHY0   , ONLY : YRPHY0
USE YOMPHY2   , ONLY : YRPHY2
USE YOMCST    , ONLY : RG   , RV   , RTT  , RPI  ,&
 & RCS  , RCW  , RCPV , RLVTT, RLSTT, RETV , RALPW, RALPS,&
 & RALPD, RBETW, RBETS, RBETD, RGAMW, RGAMS, RGAMD


IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KGPBLKS,KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT     (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ     (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL    (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI    (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAUTOL (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAUTOI (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQR    (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQS    (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEB   (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCP    (KLON,KFLEV,KGPBLKS)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR     (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI  (KLON,0:KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP  (KLON,KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPLSL (KLON,0:KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPLSN (KLON,0:KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPEVPL(KLON,0:KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPEVPN(KLON,0:KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPFPL (KLON,0:KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPFPN (KLON,0:KFLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSEDIQL(KLON,0:KFLEV,KGPBLKS)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSEDIQN(KLON,0:KFLEV,KGPBLKS)

REAL(KIND=JPRB) :: ZEPS,ZFVELR,ZFVELS,ZTMELT,ZRHOW,ZNRHOW      &
 & , ZDVISC,ZSQTVIS,ZCDARV,ZRHOREF,ZEXP1,ZEXP4,ZEXP6           &
 & , ZPREF,ZCLEAR,ZKDIFF,ZFACT3,ZFACT4                    &
 & , ZSSATW,ZCONDT,ZDIFFV,ZCEV,ZCSU                   &
 & , ZSSATI,ZQR,ZQS                                &
 & , ZACCR,ZAGGR,ZRIMI                             &
 & , ZCOEFF1,ZCOEFF2,ZCOEFF2B,ZCOEFF3,ZCOEFF4,ZCOEFF5,ZCOEFF6  &
 & , ZNU1,ZNU2,ZTAU1,ZTAU2,ZSIGMA1,ZSIGMA2                     &
 & , ZFVENTR1,ZFVENTR2,ZFVENTS1,ZFVENTS2                       &
 & , ZLHFUS,ZSUBSA,ZEVAPPL,ZEVAPPN,ZINT1,ZQMLTX,ZQFRZX,ZQFRZ   &
 & , ZTQEVAPPL,ZTQEVAPPN,ZTCOLLL,ZTCOLLN,ZQFPFPL,ZQFPFPN,ZQMLT &
 & , ZQPRTOT1,ZQPSTOT1,ZQPSTOT2,ZQPRTOT2       &
 & , ZALPHA,ZDZS, ZP1, ZP2, ZP3, ZDZL, ZDZI, ZP1L, ZP2L, ZP1I, ZP2I
REAL(KIND=JPRB) :: ZWORK1(KLON,KGPBLKS), ZWORK2(KLON,KGPBLKS), ZWORK3(KLON,KGPBLKS), ZPOW1(KLON,KGPBLKS), ZPOW2(KLON,KGPBLKS)
REAL(KIND=JPRB) :: ZQPSTOT(KLON,KGPBLKS), ZDZ(KLON,KGPBLKS)
REAL(KIND=JPRB), EXTERNAL :: FCGENERALIZED_GAMMA

REAL(KIND=JPRB) :: ZRHO(KLON,KFLEV,KGPBLKS)                 &
 & , ZALTIH   (KLON,0:KFLEV,KGPBLKS)                        &
 & , ZDPSG    (KLON,KFLEV,KGPBLKS)  , ZDPSGDT  (KLON,KFLEV,KGPBLKS) &
 & , ZDELT    (KLON,KFLEV,KGPBLKS)  , ZEFFA    (KLON,KFLEV,KGPBLKS) &
 & , ZNS      (KLON,KFLEV,KGPBLKS)                          &
 & , ZCEV1    (KLON,KFLEV,KGPBLKS)  , ZCEV2    (KLON,KFLEV,KGPBLKS) &
 & , ZCSU1    (KLON,KFLEV,KGPBLKS)  , ZCSU2    (KLON,KFLEV,KGPBLKS) &
 & , ZCAGG    (KLON,KFLEV,KGPBLKS)  , ZCACC    (KLON,KFLEV,KGPBLKS) &
 & , ZCRIM    (KLON,KFLEV,KGPBLKS)  , ZFVEL    (KLON,KFLEV,KGPBLKS) &
 & , ZQL      (KLON,KFLEV,KGPBLKS)  , ZQI      (KLON,KFLEV,KGPBLKS) &
 & , ZQPR     (KLON,KFLEV,KGPBLKS)  , ZQPS     (KLON,KFLEV,KGPBLKS) &
 & , ZQSATW   (KLON,KFLEV,KGPBLKS)  , ZQSATI   (KLON,KFLEV,KGPBLKS) &
 & , ZAUTOL   (KLON,KFLEV,KGPBLKS)  , ZAUTOI   (KLON,KFLEV,KGPBLKS)
 

LOGICAL :: LLMELTS,LLFREEZ
LOGICAL :: LLEVAPX
INTEGER(KIND=JPIM) :: JLEV, JBLK,JLON

#include "fcttrm.func.h"

!$acc data present (PSEDIQN, PT)
!$acc data present (PFPLSL, PFPLSN, PNEB, PQ, PQI, PQL, PQR, PQS, PR, PSEDIQL)
!$acc data present (PAPHI, PAPRSF, PAUTOI, PAUTOL, PCP, PDELP, PFPEVPL, PFPEVPN, PFPFPL, PFPFPN)
!$acc data create (ZQL, ZQPR, ZQPS, ZQPSTOT, ZQSATI, ZQSATW, ZRHO, ZWORK1, ZWORK2, ZWORK3)
!$acc data create (ZDELT, ZDPSG, ZDPSGDT, ZDZ, ZEFFA, ZFVEL, ZNS, ZPOW1, ZPOW2, ZQI)
!$acc data create (ZALTIH, ZAUTOI, ZAUTOL, ZCACC, ZCAGG, ZCEV1, ZCEV2, ZCRIM, ZCSU1, ZCSU2)
! --------------------------------------------------------

!     CHECK RELIABILITY OF INPUT ARGUMENTS.



ZDZL   = YRPHY0%TFVL*YRPHY2%TSPHY
ZDZI   = YRPHY0%TFVI*YRPHY2%TSPHY

!- - - - - - - - - - - - - - -
IF (YRPHY2%TSPHY > 0.0_JPRB) THEN
!- - - - - - - - - - - - - - -

  LLMELTS = .TRUE.
  LLFREEZ = .TRUE.
  LLEVAPX = ( YRPHY0%REVASX /= 0.0_JPRB )

      ! ----------
      ! Constants      
      ! ----------

  ZEPS = 1.E-20_JPRB

      ! ----------------------------------------------------------
      ! COEFFICIENTS IN DISTRIBUTIONS OF PARTICLE SPEED AND MASS  
      ! ----------------------------------------------------------

  ZNU1 = 377.8_JPRB
  ZNU2 = 2.0_JPRB/3._JPRB
  ZTAU1 = 21._JPRB
  ZTAU2 = 0.5_JPRB
  ZSIGMA1 = 0.069_JPRB
  ZSIGMA2 = 2.0_JPRB

      ! ------------------------------------------------------
      ! COEFFICIENTS IN VENTILATION FACTOR FOR RAIN AND SNOW
      ! ------------------------------------------------------

  ZFVENTR1 = 0.78_JPRB
  ZFVENTR2 = 0.31_JPRB
  ZFVENTS1 = 0.65_JPRB
  ZFVENTS2 = 0.44_JPRB

      ! ------------
      ! FALL SPEEDS
      ! ------------

      
  ZFVELR = YRPHY0%TFVR
  ZFVELS = YRPHY0%TFVS
  
  ZTMELT = RTT 
  ZRHOW = 1000._JPRB
  ZNRHOW = YRPHY0%RNINTR * ZRHOW
  ZDVISC = 1.669E-05_JPRB
  ZSQTVIS = SQRT(ZDVISC)
  ZCDARV = 2.31E-02_JPRB * RV
  ZRHOREF = 1.2_JPRB
  ZEXP1 = 1.0_JPRB/3._JPRB
  ZEXP4 = 2.0_JPRB*ZEXP1
  ZEXP6 = 17._JPRB/24._JPRB
  ZPREF = 1.E+05_JPRB
  ZCOEFF1 = 12.695_JPRB * ZNU1 * FCGENERALIZED_GAMMA(3._JPRB+ZNU2) * YRPHY0%RACCEF &
   & / (4._JPRB * ZRHOW)  
!LOP  ZCOEFF2 = 0.0485_JPRB * ZTAU1 * FCGENERALIZED_GAMMA(3._JPRB+ZTAU2) * RRIMEF &
!LOP   & * RPI / (4._JPRB * (2.0_JPRB**(1.0_JPRB+ZTAU2/3._JPRB)) * ZSIGMA1) 
  ZCOEFF2 = 0.0485_JPRB * ZTAU1 * FCGENERALIZED_GAMMA(3._JPRB+ZTAU2) * YRPHY0%RRIMEF &
   & * RPI /  4._JPRB &
   & / (FCGENERALIZED_GAMMA(ZSIGMA2+1.0_JPRB)**((3._JPRB+ZTAU2)/(1.0_JPRB+ZSIGMA2))) &
   & / ZSIGMA1   
  ZCOEFF2B = ZCOEFF2 * YRPHY0%RAGGEF / YRPHY0%RRIMEF
  ZCOEFF3 = 2.0_JPRB * ZFVENTR1 * SQRT(RPI)
  ZCOEFF4 = 2.0_JPRB * RPI**((3._JPRB-ZNU2)/8._JPRB) * ZFVENTR2 * SQRT(ZNU1) &
   & * (ZRHOREF**0.2_JPRB) * FCGENERALIZED_GAMMA((ZNU2+5._JPRB)/2.0_JPRB)   
  ZCOEFF5 = 4._JPRB * ZFVENTS1 / (2.0_JPRB * ZSIGMA1)**ZEXP4 
  ZCOEFF6 = 5.784_JPRB * 4._JPRB * ZFVENTS2 * SQRT(ZTAU1) &
   & * (ZRHOREF**0.2_JPRB) * FCGENERALIZED_GAMMA((ZTAU2+5._JPRB)/2.0_JPRB) &
   & / (2.0_JPRB * ZSIGMA1)**((ZTAU2+5._JPRB)/6._JPRB)   

      ! ---------------
      ! Initializations
      ! ---------------

  !$acc parallel loop gang vector collapse (2) vector_length (KLON) private (JBLK, JLEV, JLON) default(none)
  DO JBLK = 1, KGPBLKS
  DO JLON = KIDIA, KFDIA
  DO JLEV = 0, KFLEV
    
      ZALTIH(JLON,JLEV,JBLK) = PAPHI(JLON,JLEV,JBLK) / RG 
    
  ENDDO
  ENDDO
  ENDDO


    ! ==========================
    ! COMPUTE DENSITY, THICKNESS
    ! ==========================

  !$acc parallel loop gang vector collapse (2) vector_length (KLON) private (JBLK, JLEV, JLON) default(none)
  DO JBLK = 1, KGPBLKS
  DO JLON = KIDIA, KFDIA
  DO JLEV = KTDIA, KFLEV
    
      ZDPSG(JLON,JLEV,JBLK) = PDELP(JLON,JLEV,JBLK) / RG
      ZDPSGDT(JLON,JLEV,JBLK) = ZDPSG(JLON,JLEV,JBLK) * YRPHY2%TSPHY
      ZDELT(JLON,JLEV,JBLK) = PT(JLON,JLEV,JBLK) - RTT
      ZRHO(JLON,JLEV,JBLK) = PAPRSF(JLON,JLEV,JBLK) / PR(JLON,JLEV,JBLK)&
       & / PT(JLON,JLEV,JBLK)  
      ZQPR(JLON,JLEV,JBLK) = PQR(JLON,JLEV,JBLK)
      ZQPS(JLON,JLEV,JBLK) = PQS(JLON,JLEV,JBLK)
      ZAUTOL(JLON,JLEV,JBLK) = PAUTOL(JLON,JLEV,JBLK) * ZDPSGDT(JLON,JLEV,JBLK)
      ZAUTOI(JLON,JLEV,JBLK) = PAUTOI(JLON,JLEV,JBLK) * ZDPSGDT(JLON,JLEV,JBLK)
            
  ENDDO
  ENDDO
  ENDDO
  

     ! ======================================
     ! OTHER INITIALIZATIONS FOR MICROPHYSICS
     ! ======================================

  !$acc parallel loop gang vector collapse (2) vector_length (KLON) private (JBLK, JLEV, JLON, ZALPHA, ZCEV, ZCLEAR, ZCONDT, ZCSU, ZDIFFV, ZFACT3, ZFACT4, ZKDIFF, ZSSATI, ZSSATW) default(none)
  DO JBLK = 1, KGPBLKS
  DO JLON = KIDIA, KFDIA
  DO JLEV=KTDIA,KFLEV
!   Isolate in a loop what may not vectorize:
    
      ZWORK1(JLON,JBLK)=FOEW(PT(JLON,JLEV,JBLK),0.0_JPRB)
      ZWORK2(JLON,JBLK)=FOEW(PT(JLON,JLEV,JBLK),1.0_JPRB)
      ZWORK3(JLON,JBLK)= ( ZRHOREF / ZRHO(JLON,JLEV,JBLK) )**0.4_JPRB
    
!   This loop should vectorize:
    

      ZALPHA=MAX(ZEPS,PQS(JLON,JLEV,JBLK))/MAX(ZEPS,PQR(JLON,JLEV,JBLK)+PQS(JLON,JLEV,JBLK))
      ZFVEL(JLON,JLEV,JBLK) = ZALPHA*ZFVELS + (1.0_JPRB - ZALPHA)*ZFVELR 
       ! -----------------------------------------------------------
       ! Efficiency for ice aggregation as a function of temperature.
       ! -----------------------------------------------------------
      ZEFFA(JLON,JLEV,JBLK) = EXP(0.025_JPRB * ZDELT(JLON,JLEV,JBLK))

       ! ---------------------------------------------------------
       ! Intercept parameter for ice as a function of temperature.
       ! ---------------------------------------------------------
      ZNS(JLON,JLEV,JBLK) = YRPHY0%RNINTS * EXP(-0.1222_JPRB * ZDELT(JLON,JLEV,JBLK))

      ZQL(JLON,JLEV,JBLK) = MAX(0.0_JPRB,PQL(JLON,JLEV,JBLK) &
       & -PAUTOL(JLON,JLEV,JBLK)*YRPHY2%TSPHY)
      ZQI(JLON,JLEV,JBLK) = MAX(0.0_JPRB,PQI(JLON,JLEV,JBLK) &
       & -PAUTOI(JLON,JLEV,JBLK)*YRPHY2%TSPHY)

      ZCLEAR = 1.0_JPRB - PNEB(JLON,JLEV,JBLK)
      ZKDIFF = 2.E-5_JPRB * ZPREF / PAPRSF(JLON,JLEV,JBLK)
      ZFACT3 = (ZSQTVIS * ZKDIFF)**ZEXP1
      ZFACT4 = RV * PT(JLON,JLEV,JBLK) / ZKDIFF

       ! -----------------------
       ! For evaporation of rain
       ! -----------------------
      ZQSATW(JLON,JLEV,JBLK) = FOQS(ZWORK1(JLON,JBLK)/PAPRSF(JLON,JLEV,JBLK))
      ZSSATW = 1.0_JPRB - PQ(JLON,JLEV,JBLK)/ZQSATW(JLON,JLEV,JBLK)

      ZCONDT = ( FOLH(PT(JLON,JLEV,JBLK),0.0_JPRB)/PT(JLON,JLEV,JBLK) )**2 /ZCDARV
      ZDIFFV = ZFACT4 / ZWORK1(JLON,JBLK)

      ZCEV = ZSSATW * ZCLEAR * YRPHY0%RNINTR &
       & / ZRHO(JLON,JLEV,JBLK) / (ZCONDT + ZDIFFV)  
      ZCEV = MAX(0.0_JPRB,ZCEV)
      ZCEV1(JLON,JLEV,JBLK) = ZCEV * ZCOEFF3 
      ZCEV2(JLON,JLEV,JBLK) = ZCEV * ZCOEFF4 / ZFACT3

       ! -----------------------
       ! For sublimation of snow
       ! -----------------------
      ZQSATI(JLON,JLEV,JBLK) = FOQS(ZWORK2(JLON,JBLK)/PAPRSF(JLON,JLEV,JBLK))
      ZSSATI = 1.0_JPRB - PQ(JLON,JLEV,JBLK)/ZQSATI(JLON,JLEV,JBLK)

      ZCONDT = ( FOLH(PT(JLON,JLEV,JBLK),1.0_JPRB)/PT(JLON,JLEV,JBLK) )**2 /ZCDARV
      ZDIFFV = ZFACT4 / ZWORK2(JLON,JBLK)

      ZCSU = ZSSATI * ZCLEAR * ZNS(JLON,JLEV,JBLK) &
       & / ZRHO(JLON,JLEV,JBLK) / (ZCONDT + ZDIFFV)  
      ZCSU = MAX(0.0_JPRB,ZCSU)
      ZCSU1(JLON,JLEV,JBLK) = ZCSU * ZCOEFF5
      ZCSU2(JLON,JLEV,JBLK) = ZCSU * ZCOEFF6 / ZFACT3

       ! ------------------------
       ! For collection processes
       ! ------------------------
      ZCACC(JLON,JLEV,JBLK) = ZCOEFF1 * ZWORK3(JLON,JBLK)
      ZCRIM(JLON,JLEV,JBLK) = ZCOEFF2 * ZWORK3(JLON,JBLK)
      ZCAGG(JLON,JLEV,JBLK) = ZCOEFF2B * ZWORK3(JLON,JBLK) * ZEFFA(JLON,JLEV,JBLK)

    
  ENDDO
  ENDDO
  ENDDO


    ! =============================================
    ! PERFORM STATISTICAL ADVECTION OF PRECIPITATION
    ! =============================================

    !-- -- -- -- -- --
  !$acc parallel loop gang vector collapse (2) vector_length (KLON) private (JBLK, JLEV, JLON, ZACCR, ZAGGR, ZDZS, ZEVAPPL, ZEVAPPN, ZINT1, ZLHFUS, ZP1, ZP1I, ZP1L, ZP2, ZP2I, ZP2L, ZP3, ZQFPFPL, ZQFPFPN, ZQFRZ, ZQFRZX, ZQMLT, ZQMLTX, ZQPRTOT1, ZQPRTOT2, ZQPSTOT1, ZQPSTOT2, ZQR, ZQS, ZRIMI, ZSUBSA, ZTCOLLL, ZTCOLLN, ZTQEVAPPL, ZTQEVAPPN) default(none)
  DO JBLK = 1, KGPBLKS
  DO JLON = KIDIA, KFDIA
  DO JLEV=KTDIA,KFLEV  
    !-- -- -- -- -- --


      ! =================================================
      ! First computation of total rain and snow which fall  
      ! through the curent level. Only 3 terms at this stage :
      ! 1 ==> Initial contents
      ! 2 ==> Flux from the upper level
      ! 3 ==> Autoconversion flux 
      
      ! In this version there is only one falling speed, depending of 
      ! the nature of the precipitation (like ADVPRC)
      ! =================================================

    
    
      ZDZ(JLON,JBLK)   = ZFVEL(JLON,JLEV,JBLK)*YRPHY2%TSPHY
    
      ZWORK3(JLON,JBLK) = MAX(0.0_JPRB,ZDPSG(JLON,JLEV,JBLK)*ZQPR(JLON,JLEV,JBLK) + &
       &              YRPHY2%TSPHY*(PFPLSL(JLON,JLEV-1,JBLK)) + ZAUTOL(JLON,JLEV,JBLK))
      ZQPSTOT(JLON,JBLK) = MAX(0.0_JPRB,ZDPSG(JLON,JLEV,JBLK)*ZQPS(JLON,JLEV,JBLK) + &
       &              YRPHY2%TSPHY*(PFPLSN(JLON,JLEV-1,JBLK)) + ZAUTOI(JLON,JLEV,JBLK))

!  New formulation which does not take into account initial contents
! This implies a total independence to CFL criteria therefore to the layers thickness

!      ZWORK3(JLON) = MAX(0.0_JPRB,TSPHY*(PFPLSL(JLON,JLEV-1))+ZAUTOL(JLON,JLEV))
!      ZQPSTOT(JLON) = MAX(0.0_JPRB,TSPHY*(PFPLSN(JLON,JLEV-1))+ZAUTOI(JLON,JLEV))

    

    

      ZQR = ZWORK3(JLON,JBLK) / ZDZ(JLON,JBLK)
      ZQS = ZQPSTOT(JLON,JBLK) / ZDZ(JLON,JBLK)
      
      IF (YRPHY%LEVAPP) THEN

        ZWORK1(JLON,JBLK) =  ZQR / ZNRHOW
        ZWORK2(JLON,JBLK) =  ZQS / ZNS(JLON,JLEV,JBLK)

      ENDIF

    

    IF (YRPHY%LEVAPP) THEN
      
        ZPOW1(JLON,JBLK)=ZCEV2(JLON,JLEV,JBLK)*ZWORK1(JLON,JBLK)**ZEXP6
        ZPOW2(JLON,JBLK)=ZCSU1(JLON,JLEV,JBLK)*ZWORK2(JLON,JBLK)**ZEXP4
      
    ENDIF

    

      ZTQEVAPPL = 0.0_JPRB
      ZTQEVAPPN = 0.0_JPRB
      ZQFPFPL   = 0.0_JPRB
      ZQFPFPN   = 0.0_JPRB
      ZTCOLLL   = 0.0_JPRB
      ZTCOLLN   = 0.0_JPRB
      ZQMLT     = 0.0_JPRB
      ZQMLTX    = 0.0_JPRB
      ZQFRZ     = 0.0_JPRB
      ZQFRZX    = 0.0_JPRB  
      ZACCR     = 0.0_JPRB

      IF (YRPHY%LEVAPP) THEN

           ! ----------------------------------------
           ! Evaporation/Sublimation of precipitation
           ! ----------------------------------------

        ZEVAPPL = ZCEV1(JLON,JLEV,JBLK)*SQRT(ZWORK1(JLON,JBLK)) + ZPOW1(JLON,JBLK)
        ZEVAPPN = ZPOW2(JLON,JBLK) + ZCSU2(JLON,JLEV,JBLK)*ZWORK2(JLON,JBLK)

        ZINT1 = 1.0_JPRB / MAX(ZEPS,ZEVAPPL+ZEVAPPN)

        IF (LLEVAPX) THEN
          ZSUBSA = YRPHY0%REVASX*ZINT1*(1.0_JPRB-EXP(-1.0_JPRB/(YRPHY0%REVASX*ZINT1)))
          ZEVAPPL = ZSUBSA*ZEVAPPL
          ZEVAPPN = ZSUBSA*ZEVAPPN
        ENDIF

        ZSUBSA = ZINT1 * ZEVAPPL * (ZQSATW(JLON,JLEV,JBLK) - PQ(JLON,JLEV,JBLK))
        ZTQEVAPPL = MAX(0.0_JPRB, MIN( ZWORK3(JLON,JBLK),  &
         & ZEVAPPL * ZDPSGDT(JLON,JLEV,JBLK), ZSUBSA * ZDPSG(JLON,JLEV,JBLK) ))   

        ZSUBSA = ZINT1 * ZEVAPPN * (ZQSATI(JLON,JLEV,JBLK) - PQ(JLON,JLEV,JBLK))
        ZTQEVAPPN = MAX(0.0_JPRB, MIN( ZQPSTOT(JLON,JBLK),  &
         & ZEVAPPN * ZDPSGDT(JLON,JLEV,JBLK), ZSUBSA * ZDPSG(JLON,JLEV,JBLK) ))   

      ENDIF

      ZQPRTOT1 = ZWORK3(JLON,JBLK) - ZTQEVAPPL
      ZQPSTOT1 = ZQPSTOT(JLON,JBLK) - ZTQEVAPPN

      ZQR = ZQPRTOT1 / ZDZ(JLON,JBLK)
      ZQS = ZQPSTOT1 / ZDZ(JLON,JBLK)

      IF (YRPHY%LCOLLEC) THEN

           ! ----------------------------------------
           ! Collection of cloud liquid water by rain
           ! ----------------------------------------

        ZACCR = ZQL(JLON,JLEV,JBLK)*(1.0_JPRB-EXP(-ZCACC(JLON,JLEV,JBLK)*ZQR*YRPHY2%TSPHY)) &
        &     * MAX(0.0_JPRB,SIGN(1.0_JPRB,PT(JLON,JLEV,JBLK)-RTT))
        

           ! -------------------------------
           ! Collection of cloud ice by snow
           ! -------------------------------
        ZAGGR = ZQI(JLON,JLEV,JBLK)*(1.0_JPRB-EXP(-ZCAGG(JLON,JLEV,JBLK)*ZQS*YRPHY2%TSPHY))

           ! ----------------------------------------
           ! Collection of cloud liquid water by snow
           ! ----------------------------------------
        ZRIMI = ZQL(JLON,JLEV,JBLK)*(1.0_JPRB-EXP(-ZCRIM(JLON,JLEV,JBLK)*ZQS*YRPHY2%TSPHY))

           ! ----------------------------
           ! Sum up collection processes
           ! ----------------------------
        ZTCOLLL = MAX(0.0_JPRB, MIN(ZACCR+ZRIMI,ZQL(JLON,JLEV,JBLK)) ) &
         & * ZDPSG(JLON,JLEV,JBLK)
        ZTCOLLN = MAX(0.0_JPRB, MIN(ZAGGR      ,ZQI(JLON,JLEV,JBLK)) ) &
         & * ZDPSG(JLON,JLEV,JBLK)

      ENDIF

      ZQPRTOT2  = ZWORK3(JLON,JBLK) + ZTCOLLL
      ZQPSTOT2  = ZQPSTOT(JLON,JBLK) + ZTCOLLN

      IF (LLMELTS) THEN

           ! ----------------------------
           ! Snow melting
           ! ----------------------------
        ZLHFUS = FOLH(PT(JLON,JLEV,JBLK),1.0_JPRB) - FOLH(PT(JLON,JLEV,JBLK),0.0_JPRB)
        ZQMLTX = ZDPSG(JLON,JLEV,JBLK) * PCP(JLON,JLEV,JBLK) &
         & * MAX(0.0_JPRB,ZDELT(JLON,JLEV,JBLK)) / ZLHFUS

         IF (.NOT. YRPHY%LSMOOTHMELT) THEN 
          ZQMLT = MIN ( ZQMLTX , ZQPSTOT2 - ZTQEVAPPN )
         ELSE
          ZQMLT=(ZQPSTOT2-ZTQEVAPPN)*(1+TANH(ZDELT(JLON,JLEV,JBLK)/YRPHY0%RSMOOTHMELT))/2.0_JPRB
         ENDIF
       ENDIF 
       IF (LLFREEZ) THEN  

           ! ----------------------------
           ! Rain freezing
           ! ----------------------------
        
        ZQFRZX = ZDPSG(JLON,JLEV,JBLK) * PCP(JLON,JLEV,JBLK) &
         & * MAX(0.0_JPRB,-ZDELT(JLON,JLEV,JBLK)) / ZLHFUS
        
        ZQFRZ = MIN ( ZQFRZX , ZQPRTOT2 - ZTQEVAPPL ) 
        
      ENDIF

      
      PFPEVPL(JLON,JLEV,JBLK) = PFPEVPL(JLON,JLEV-1,JBLK) &
       & + ( ZTQEVAPPL - ZQMLT + ZQFRZ) / YRPHY2%TSPHY
      PFPEVPN(JLON,JLEV,JBLK) = PFPEVPN(JLON,JLEV-1,JBLK) &
       & + ( ZTQEVAPPN + ZQMLT - ZQFRZ) / YRPHY2%TSPHY

      PFPFPL (JLON,JLEV,JBLK) = PFPFPL (JLON,JLEV-1,JBLK) &
       & + ( ZTCOLLL + ZAUTOL(JLON,JLEV,JBLK) ) / YRPHY2%TSPHY
      PFPFPN (JLON,JLEV,JBLK) = PFPFPN (JLON,JLEV-1,JBLK) &
       & + ( ZTCOLLN + ZAUTOI(JLON,JLEV,JBLK) ) / YRPHY2%TSPHY


           ! ----------------------------
           ! Computation of fundamental proportions
           ! needed by the statistical algorithm
           ! (only YB formulation !)
           ! ----------------------------

! Rain and snow           
      ZDZS = ZALTIH(JLON,JLEV-1,JBLK) - ZALTIH(JLON,JLEV,JBLK)
      ZP1  = MIN(1._JPRB , ZDZ(JLON,JBLK)/ZDZS)
      ZP2  = MAX(0._JPRB,1._JPRB - ZDZS/ZDZ(JLON,JBLK))
      ZP3  = (ZP1 + ZP2)/2.0_JPRB
! Cloud liquid water      
      ZP1L  = MIN(1._JPRB , ZDZL/ZDZS)
      ZP2L  = MAX(0._JPRB,1._JPRB - ZDZS/MAX(ZEPS,ZDZL))
! Cloud ice
      ZP1I  = MIN(1._JPRB , ZDZI/ZDZS)
      ZP2I  = MAX(0._JPRB,1._JPRB - ZDZS/MAX(ZEPS,ZDZI))

      
! WARNING ! : Dans cette version pour coller a ADVPRC il n'y a pas de traitement separe de la neige 
!             et de la pluie. Ceci serait difficile dans ADVPRC mais trivial dans ADVPRCS      
    ! ================================================================
    ! COMPUTE FLUX ASSOCIATED TO FALLING OF PRECIPITATION
    ! ================================================================

      PFPLSL(JLON,JLEV,JBLK) = (ZP1*ZDPSG(JLON,JLEV,JBLK)*ZQPR(JLON,JLEV,JBLK)      &
      &                 + ZP2*YRPHY2%TSPHY*PFPLSL(JLON,JLEV-1,JBLK)              &      
      &                 + ZP3*(ZAUTOL(JLON,JLEV,JBLK) + ZTCOLLL + ZQMLT)) &
      &                 * MAX(0.0_JPRB,                              &
      &                (1._JPRB - (ZTQEVAPPL+ZQFRZ)/MAX(ZEPS,ZQPRTOT2))) / YRPHY2%TSPHY
      

      PFPLSN(JLON,JLEV,JBLK) = (ZP1*ZDPSG(JLON,JLEV,JBLK)*ZQPS(JLON,JLEV,JBLK)      &
      &                 + ZP2*YRPHY2%TSPHY*PFPLSN(JLON,JLEV-1,JBLK)              &      
      &                 + ZP3*(ZAUTOI(JLON,JLEV,JBLK) + ZTCOLLN + ZQFRZ)) &
      &                 * MAX(0.0_JPRB,                              &
      &                (1._JPRB - (ZTQEVAPPN+ZQMLT)/MAX(ZEPS,ZQPSTOT2))) / YRPHY2%TSPHY

      PSEDIQL(JLON,JLEV,JBLK) = (ZP1L*ZDPSG(JLON,JLEV,JBLK)*ZQL(JLON,JLEV,JBLK)      &
       &                  + ZP2L*YRPHY2%TSPHY*PSEDIQL(JLON,JLEV-1,JBLK) ) / YRPHY2%TSPHY
       
      PSEDIQN(JLON,JLEV,JBLK) = (ZP1I*ZDPSG(JLON,JLEV,JBLK)*ZQI(JLON,JLEV,JBLK)      &
       &                  + ZP2I*YRPHY2%TSPHY*PSEDIQN(JLON,JLEV-1,JBLK) ) / YRPHY2%TSPHY
       
     ! JLON = KIDIA, KFDIA

    !-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  ENDDO
  ENDDO
  ENDDO
 ! LEV=1,KFLEV : end of statistical advection 
    !-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

!- - - - - - - - - - - - - - - - - - - - - - -
ENDIF  ! End of test on TSPHY > 0.0_JPRB
!- - - - - - - - - - - - - - - - - - - - - - -


!$acc end data
!$acc end data
!$acc end data
!$acc end data
!$acc end data
!$acc end data
END SUBROUTINE ADVPRCS
