!-----------------------------------------------------------------------
!$acc routine (ACPLUIZ) seq
SUBROUTINE ACPLUIZ( KIDIA, KFDIA, KLON, KTDIA, KLEV,&
 !-----------------------------------------------------------------------
 ! - INPUT -
 & PT, PQ, PQL, PQI, PQR, PQS,&
 & PDELP, PAPRSF, PCP, PR, PNEBS, PQCS, PNEB_CVPP, PQLI_CVPP,PQC_DET_PCMT,&
 & PTENDH, PTENDQ, LDADJCLD, PAPHI,&
 & PTS, PNEIJ, PLSM, PGM, PVETAF,&
 ! - OUTPUT -
 & PFCSQL, PFCSQN, PFPLSL, PFPLSN,&
 & PFPEVPL, PFPEVPN, PFPFPL, PFPFPN, PSEDIQL, PSEDIQN,KSTPT,KSTSZ,PSTACK )


#include "temp.h"

!**** *ACPLUIZ * - PROGNOSTIC CLOUD SCMEME (LOPEZ)

!**   Interface.
!     ----------
!        *CALL* *ACPLUIZ*

!-----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE./INPUT ARGUMENTS.
!     ------------------------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
!            : START OF HORIZONTAL LOOP 
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
!            : END OF HORIZONTAL LOOP
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
!            : HORIZONTAL DIMENSION  
! KTDIA      : INDICE DE DEPART DES BOUCLES VERTICALES. 
!            : START OF THE VERTICAL LOOP IN THE PHYSICS. 
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".
!            : END OF VERTICAL LOOP AND VERTICAL DIMENSION

! - 2D (1:KLEV)

! PT         : TEMPERATURE.
!            : TEMPERATURE.
! PQ         : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
!            : SPECIFIC HUMIDITY OF WATER VAPOUR.
! PQL        : HUMIDITE SPECIFIQUE DE L'EAU CONDENSEE LIQUIDE.
!            : SPECIFIC HUMIDITY OF LIQUID CONDENSATED WATER.
! PQI        : HUMIDITE SPECIFIQUE DE L'EAU CONDENSEE GLACE.
!            : SPECIFIC HUMIDITY OF SOLID CONDENSATED WATER.
! PQR        : PRECIPITATIONS LIQUIDES.
!            : LIQUID PECIPITATIONS.
! PQS        : PRECIPITATIONS SOLIDES.
!            : SOLID PECIPITATIONS.
! PDELP      : EPAISSEUR EN PRESSION DE LA COUCHE.
!            : LAYER THICKNESS IN PRESSURE UNITS.  
! PAPRSF     : PRESSION AUX NIVEAUX DES COUCHES.
!            : PRESSURE ON FULL LEVELS.
! PCP        : CHALEUR MASSIQUE A PRESSION CONSTANTE DE L'AIR.
!            : SPECIFIC HEAT AT CONSTANT PRESSURE FOR AIR.
! PR         : CONSTANTE DES GAZ POUR L'AIR.
!            : GAS CONSTANT FOR AIR.
! PNEBS      : NEBULOSITE PARTIELLE STRATIFORME.
!            : STRATIFORM FRACTIONAL CLOUDINESS.
! PQCS       : CONTENU "STRATIFORME" EN CONDENSAT NUAGEUX (LIQ. + SOL.).
!            : STRATIRORM CLOUD WATER (LIQUID + SOLID).
! PNEB_CVPP  : NEBULOSITE PARTIELLE CONVECTION PEU PROFONDE.
!            : SHALLOW CONVECTION FRACTIONAL CLOUDINESS.
! PQLI_CVPP  : CONTENU "CVPP" EN CONDENSAT NUAGEUX (LIQ. + SOL.).
!            : SHALLOW CONVECTION CLOUD WATER (LIQUID + SOLID).
! PQC_DET_PCMT: CONTENU EN CONDENSAT NUAGEUX DETRAINE PAR PCMT (LIQ. + SOL.).
!            : DEEP CONVECTION DETRAINED CLOUD WATER (LIQUID + SOLID).
! PTENDH     : TENDANCE D'ENTHALPIE.
!            : ENTHALPY TENDENCY.
! PTENDQ     : TENDANCE D'HUMIDITE SPECIFIQUE.
!            : SPECIFIC HUMIDITY TENDENCY.

! - 2D (0:KLEV)

! PAPHI      : GEOPOTENTIEL SUR DEMI-NIVEAUX.
!            : GEOPOTENTIAL ON HALF-LEVELS.

! - 1D (1:KLON) .
                                                                                
! PTS        : TEMPERATURE DE SURFACE.
!            : SURFACE TEMPERATURE.
! PNEIJ      : PROPORTION DE LA MAILLE RECOUVERTE DE NEIGE.
!            : SNOW FRACTION.
! PLSM       : INDICE TERRE/MER.
!            : LAND/SEA MASK.
! PGM        : FACTEUR D'ECHELLE.
!            : MAPPING FACTOR.
! PVETAF     : COORDONNEE VERTICALE ETA.
!            : VERTICAL COORDINATE ETA.

!-----------------------------------------------------------------------

! -   ARGUMENTS EN SORTIE.
!     ---------------------------

! - 2D (0:KLEV) .

! PFCSQN     : FLUX DE CONDENSATION LIE AUX RR STRATIFORMES NEIGEUSES.
!            : STRATIFORM CONDENSATION FLUX FOR ICE.
! PFCSQL     : FLUX DE CONDENSATION LIE AUX RR STRATIFORMES LIQUIDES.
!            : STRATIFORM CONDENSATION FLUX FOR LIQUID WATER.
! PFPLSL     : FLUX DE PRECIPITATION LIQUIDE (PLUIE).
!            : RAIN FLUX.
! PFPLSN     : FLUX DE PRECIPITATION SOLIDE  (NEIGE).
!            : ICE PRECIPITATION FLUX.
! PFPEVPL    : FLUX ASSOCIE A L'EVAPORATION DES PRECIP.
!            : FLUX ASSOCIATED TO EVAPORATION OF PRECIPITATIONS.
! PFPEVPN    : FLUX ASSOCIE A LA SUBLIMATION DES PRECIP.
!            : FLUX ASSOCIATED TO SUBLIMATION OF PRECIPITATIONS.
! PFPFPL     : FLUX DE GENERATION DE PRECIPITATIONS LIQUIDES.
!            : FLUX OF LIQUID PRECIPITATION GENERATION.
! PFPFPN     : FLUX DE GENERATION DE PRECIPITATIONS SOLIDES.
!            : FLUX OF SOLID PRECIPITATION GENERATION.
! PSEDIQL    : FLUX SEDIMENTATION D'EAU LIQUIDE NUAGEUSE.
!            : FLUX SEDIMENTATION OF CLOUD LIQUID WATER.
! PSEDIQN    : FLUX SEDIMENTATION D'EAU SOLIDE NUAGEUSE.
!            : FLUX SEDIMENTATION OF CLOUD SOLID WATER.

!-----------------------------------------------------------------------

!     Auteur.
!     -------
!         04-10, Francois Bouyssel

!     Modifications.
!     --------------
!         05-05, F.Bouyssel : Introduction of vertical coordinate eta
!         05-07, F.Bouyssel : New arguments from aplpar to acmicro
!         05-11, F.Bouyssel : Remove ZAUTO,ZQCMIC,ZQPMIC
!         06-01, F.Bouyssel : Introduction of PQS, PFPEVPL, PFPEVPN
!         06-01, F.Bouyssel : Microphysical adjustment at the beginning
!         06-04, F.Bouyssel : Key for Smith's adjustment LADJCLD 
!         06-06, Y.Bouteloup: Statistical sedimentation (ADVPRCS)
!         09-10, F.Bouyssel : Cleaning ADVPRC and ACNEBSM
!         10-04, Y.Bouteloup: No rain production by autoconversion in
!                             case of negative temperature
!         10-07, F.Bouyssel : Bug correction related with ZDPSGDT
!         10-12, F.Bouyssel : Introduction of LADJCLD
!         11-01, F.Bouyssel : Bug correction in case of negative temperature
!       2011-06, M. Jerczynski : some cleaning to meet norms
!       2014-04, JM Piriou : add detrained condensate from PCMT convection
!       2016-06, Y. Bouteloup : adjustment done as a function of a dummy logical argument, instead of global.

!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOMPHY    , ONLY : YRPHY
USE YOMPHY0   , ONLY : YRPHY0
USE YOMPHY1   , ONLY : YRPHY1
USE YOMPHY2   , ONLY : YRPHY2
USE YOMCST    , ONLY : RDT
USE YOMCST    , ONLY : RG   , RV   , RTT  , RPI  ,&
 & RCS  , RCW  , RCPV , RLVTT, RLSTT, RETV , RALPW, RALPS,&
 & RALPD, RBETW, RBETS, RBETD, RGAMW, RGAMS, RGAMD
USE YOMADVPRCS

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT     (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ     (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL    (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI    (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQR    (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQS    (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP  (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCP    (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR     (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PNEBS  (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQCS   (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEB_CVPP(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQLI_CVPP(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQC_DET_PCMT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTENDH (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTENDQ (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI  (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS    (KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEIJ  (KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSM   (KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGM   (KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVETAF(KLEV)

LOGICAL           ,INTENT(IN)    :: LDADJCLD

REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFCSQL (KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFCSQN (KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPLSL (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPLSN (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPEVPL(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPEVPN(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPFPL (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPFPN (KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSEDIQL(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSEDIQN(KLON,0:KLEV) 

INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT) :: PSTACK (KSTSZ)

REAL(KIND=JPRB) :: ZT
REAL(KIND=JPRB) :: ZQ
REAL(KIND=JPRB) :: ZAUTOL
REAL(KIND=JPRB) :: ZAUTOI
REAL(KIND=JPRB) :: ZQL
REAL(KIND=JPRB) :: ZQI
REAL(KIND=JPRB) :: ZFLUXCOR

REAL(KIND=JPRB) :: ZDPSGDT,ZICE,ZGDT,ZGDTI,ZIGEL,ZEPS1
INTEGER(KIND=JPIM) :: JLON,JLEV
REAL(KIND=JPRB) :: ZAUTOL_ACMI
REAL(KIND=JPRB) :: ZAUTOI_ACMI
REAL(KIND=JPRB) :: ZDELT_ACMI
REAL(KIND=JPRB) :: ZEFFA_ACMI
REAL(KIND=JPRB) :: ZQL_ACMI,ZQI_ACMI,ZCAUT_ACMI,&
 & ZALPH_ACMI,ZDUM_ACMI,ZQCR_ACMI,ZARG1_ACMI,ZARG2_ACMI,ZBETA_ACMI,ZFACICE_ACMI
REAL(KIND=JPRB) :: ZRHO_AD
REAL(KIND=JPRB) :: ZALTIH_AD
REAL(KIND=JPRB) :: ZDPSG_AD
REAL(KIND=JPRB) :: ZDPSGDT_AD
REAL(KIND=JPRB) :: ZDELT_AD
REAL(KIND=JPRB) :: ZEFFA_AD
REAL(KIND=JPRB) :: ZNS_AD
REAL(KIND=JPRB) :: ZCEV1_AD
REAL(KIND=JPRB) :: ZCEV2_AD
REAL(KIND=JPRB) :: ZCSU1_AD
REAL(KIND=JPRB) :: ZCSU2_AD
REAL(KIND=JPRB) :: ZCAGG_AD
REAL(KIND=JPRB) :: ZCACC_AD
REAL(KIND=JPRB) :: ZCRIM_AD
REAL(KIND=JPRB) :: ZFVEL_AD
REAL(KIND=JPRB) :: ZQL_AD
REAL(KIND=JPRB) :: ZQI_AD
REAL(KIND=JPRB) :: ZQPR_AD
REAL(KIND=JPRB) :: ZQPS_AD
REAL(KIND=JPRB) :: ZQSATW_AD
REAL(KIND=JPRB) :: ZQSATI_AD
REAL(KIND=JPRB) :: ZAUTOL_AD
REAL(KIND=JPRB) :: ZAUTOI_AD
REAL(KIND=JPRB) :: ZQPSTOT_AD
REAL(KIND=JPRB) :: ZDZ_AD
REAL(KIND=JPRB) :: ZWORK1_AD
REAL(KIND=JPRB) :: ZWORK2_AD
REAL(KIND=JPRB) :: ZWORK3_AD
REAL(KIND=JPRB) :: ZPOW1_AD
REAL(KIND=JPRB) :: ZPOW2_AD
REAL(KIND=JPRB) :: &
 &   ZCLEAR_AD,ZKDIFF_AD,ZFACT3_AD,ZFACT4_AD                         &
 & , ZSSATW_AD,ZCONDT_AD,ZDIFFV_AD,ZCEV_AD,ZCSU_AD                            &
 & , ZSSATI_AD,ZQR_AD,ZQS_AD                                            &
 & , ZACCR_AD,ZAGGR_AD,ZRIMI_AD                                         &
 & , ZLHFUS_AD,ZSUBSA_AD,ZEVAPPL_AD,ZEVAPPN_AD,ZINT1_AD,ZQMLTX_AD,ZQFRZX_AD,ZQFRZ_AD   &
 & , ZTQEVAPPL_AD,ZTQEVAPPN_AD,ZTCOLLL_AD,ZTCOLLN_AD,ZQFPFPL_AD,ZQFPFPN_AD,ZQMLT_AD &
 & , ZQPRTOT1_AD,ZQPSTOT1_AD,ZQPSTOT2_AD,ZQPRTOT2_AD                       &
 & , ZALPHA_AD,ZDZS_AD, ZP1_AD, ZP2_AD, ZP3_AD, ZDZL_AD, ZDZI_AD, ZP1L_AD, ZP2L_AD, ZP1I_AD, ZP2I_AD

#include "abor1.intfb.h"
#include "fctdoi.func.h"
#include "fcttrm.func.h"

!     ------------------------------------------------------------------

!     CHECK RELIABILITY OF INPUT ARGUMENTS.



!     ------------------------------------------------------------------

! - - - - - - - -
! ADJUSTMENT
! - - - - - - - -

init_stack ()


ZEPS1=1.E-12_JPRB

ZGDT=RG*YRPHY2%TSPHY
ZGDTI=1.0_JPRB/ZGDT

! - - - - - - - -
! AUTOCONVERSION
! - - - - - - - -

!     ------------------------------------------------------------------
!     CHECK RELIABILITY OF INPUT ARGUMENTS.
!     ------------------------------------------------------------------
! Define threshold for ice autoconversion as a function of temperature
! --------------------------------------------------------------------

ZARG1_ACMI = 2.0_JPRB*YRPHY0%RQICRMAX*(1.0_JPRB-0.999_JPRB)/(YRPHY0%RQICRMAX-YRPHY0%RQICRMIN)-1.0_JPRB
ZARG2_ACMI = 2.0_JPRB*(YRPHY0%RQICRMAX-1.5_JPRB*YRPHY0%RQICRMIN)/(YRPHY0%RQICRMAX-YRPHY0%RQICRMIN)-1.0_JPRB
ZARG1_ACMI = 0.5_JPRB*LOG(ABS((1.0_JPRB+ZARG1_ACMI)/(1.0_JPRB-ZARG1_ACMI)))
ZARG2_ACMI = 0.5_JPRB*LOG(ABS((1.0_JPRB+ZARG2_ACMI)/(1.0_JPRB-ZARG2_ACMI)))
ZALPH_ACMI = (ZARG1_ACMI - ZARG2_ACMI)/(YRPHY0%RQICRT2-YRPHY0%RQICRT1)
ZBETA_ACMI = ZARG1_ACMI - YRPHY0%RQICRT2 * ZALPH_ACMI
ZDZL_AD    = YRPHY0%TFVL*YRPHY2%TSPHY
ZDZI_AD    = YRPHY0%TFVI*YRPHY2%TSPHY

 

JLON = KIDIA

IF ( LDADJCLD ) THEN
  CALL ABOR1 ('UNEXPECTED LDADJCLD')
ENDIF
IF (YRPHY2%TSPHY <= 0.0_JPRB) THEN
  CALL ABOR1 ('UNEXPECTED YRPHY2%TSPHY')
ENDIF

DO JLEV = KTDIA, KLEV
    ZT = PT(JLON,JLEV)
    ZQ = PQ(JLON,JLEV)

! CLOUD OVERLAP FOR STRATIFORM AND SHALLOW CLOUDS
! -----------------------------------------------
                                                                                
    ZICE = FONICE(ZT)
    PQCS (JLON,JLEV) = PQCS(JLON,JLEV) + PQLI_CVPP(JLON,JLEV) + PQC_DET_PCMT(JLON,JLEV)
    PNEBS(JLON,JLEV) = MIN(1.0_JPRB-ZEPS1,MAX(PNEBS(JLON,JLEV),ZEPS1))
    PNEBS(JLON,JLEV) = MAX( PNEBS(JLON,JLEV) , PNEB_CVPP(JLON,JLEV) )
    ZQL   = PQCS(JLON,JLEV)*(1.0_JPRB-ZICE)
    ZQI   = PQCS(JLON,JLEV)*ZICE

    ZDELT_ACMI = ZT - RTT

! Efficiency for ice conversion as a function of temperature.
! -----------------------------------------------------------
    ZEFFA_ACMI = EXP(YRPHY0%RAUTSBET*ZDELT_ACMI)

! ---------------------------------------------------
! MICROPHYSICAL AUTOCONVERSION IN THE STRATIFORM CASE
! ---------------------------------------------------

! Compute in-cloud values
! -----------------------

    ZQL_ACMI = MAX(0.0_JPRB,ZQL/PNEBS(JLON,JLEV))
    ZQI_ACMI = MAX(0.0_JPRB,ZQI/PNEBS(JLON,JLEV))

! AUTOCONVERSION OF CLOUD LIQUID WATER INTO RAIN
! ----------------------------------------------

    ZCAUT_ACMI = YRPHY0%RAUTEFR
    ZDUM_ACMI = (1.0_JPRB-EXP(-ZCAUT_ACMI*YRPHY2%TSPHY)) * (ZQL_ACMI-YRPHY0%RQLCR) / YRPHY2%TSPHY
    ZAUTOL_ACMI = MAX(0.0_JPRB,ZDUM_ACMI)

! AUTOCONVERSION OF CLOUD ICE INTO PRECIPITATING ICE
! --------------------------------------------------

    ZCAUT_ACMI = YRPHY0%RAUTEFS * ZEFFA_ACMI
    ZQCR_ACMI = YRPHY0%RQICRMAX - (YRPHY0%RQICRMAX - YRPHY0%RQICRMIN) * 0.5_JPRB &
     & * (1.0_JPRB + TANH(ZALPH_ACMI * ZDELT_ACMI + ZBETA_ACMI))
    ZFACICE_ACMI = PLSM(JLON)*PNEIJ(JLON) + (1.0_JPRB-PLSM(JLON)) &
     & * MAX(0.0_JPRB,SIGN(1.0_JPRB,YRPHY1%TMERGL-PTS(JLON)))
    ZQCR_ACMI = ZQCR_ACMI * (1.0_JPRB-ZFACICE_ACMI*(1.0_JPRB-YRPHY0%RQICRSN))
    ZDUM_ACMI = (1.0_JPRB-EXP(-ZCAUT_ACMI*YRPHY2%TSPHY)) * (ZQI_ACMI-ZQCR_ACMI) / YRPHY2%TSPHY
    ZAUTOI_ACMI = MAX(0.0_JPRB,ZDUM_ACMI)
 
! TOTAL AUTOCONVERSION TERM
! -------------------------

    ZAUTOL = ZAUTOL_ACMI * PNEBS(JLON,JLEV)
    ZAUTOI = ZAUTOI_ACMI * PNEBS(JLON,JLEV)

! In case of negative temperature 
! no rain production by autoconversion 
! - - - - - - - - - - - - - - - - - -


    ZDPSGDT  = ZGDTI * PDELP(JLON,JLEV)
    ZIGEL = MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-PT(JLON,JLEV)))
    ZFLUXCOR = ZIGEL*ZAUTOL
    ZAUTOL = ZAUTOL - ZFLUXCOR
    ZAUTOI = ZAUTOI + ZFLUXCOR
    ZQL = ZQL - ZFLUXCOR*YRPHY2%TSPHY
    ZQI = ZQI + ZFLUXCOR*YRPHY2%TSPHY
    PFCSQL(JLON,JLEV) = PFCSQL(JLON,JLEV-1) &
     & + ( ZQL - PQL(JLON,JLEV) ) * ZDPSGDT
    PFCSQN(JLON,JLEV) = PFCSQN(JLON,JLEV-1) &
     & + ( ZQI - PQI(JLON,JLEV) ) * ZDPSGDT


! - - - - - - - - - - - - - - - - - - -
! FALLING OF PRECIPITATING PARTICLES,
! COLLECTION AND EVAPORATION PROCESSES.
! - - - - - - - - - - - - - - - - - - -

! --------------------------------------------------------

    ! ==========================
    ! COMPUTE DENSITY, THICKNESS
    ! ==========================

    
    ZDPSG_AD = PDELP(JLON,JLEV) / RG
    ZDPSGDT_AD = ZDPSG_AD * YRPHY2%TSPHY
    ZDELT_AD = ZT - RTT
    ZRHO_AD = PAPRSF(JLON,JLEV) / PR(JLON,JLEV)&
     & / ZT  
    ZQPR_AD = PQR(JLON,JLEV)
    ZQPS_AD = PQS(JLON,JLEV)
    ZAUTOL_AD = ZAUTOL * ZDPSGDT_AD
    ZAUTOI_AD = ZAUTOI * ZDPSGDT_AD
          
   ! ======================================
   ! OTHER INITIALIZATIONS FOR MICROPHYSICS
   ! ======================================

!   Isolate in a loop what may not vectorize:
  
    ZWORK1_AD=FOEW(ZT,0.0_JPRB)
    ZWORK2_AD=FOEW(ZT,1.0_JPRB)
    ZWORK3_AD= ( ZRHOREF / ZRHO_AD )**0.4_JPRB
  
!   This loop should vectorize:
  

    ZALPHA_AD=MAX(ZEPS,PQS(JLON,JLEV))/MAX(ZEPS,PQR(JLON,JLEV)+PQS(JLON,JLEV))
    ZFVEL_AD = ZALPHA_AD*ZFVELS + (1.0_JPRB - ZALPHA_AD)*ZFVELR 
     ! -----------------------------------------------------------
     ! Efficiency for ice aggregation as a function of temperature.
     ! -----------------------------------------------------------
    ZEFFA_AD = EXP(0.025_JPRB * ZDELT_AD)

     ! ---------------------------------------------------------
     ! Intercept parameter for ice as a function of temperature.
     ! ---------------------------------------------------------
    ZNS_AD = YRPHY0%RNINTS * EXP(-0.1222_JPRB * ZDELT_AD)

    ZQL_AD = MAX(0.0_JPRB,ZQL &
     & -ZAUTOL*YRPHY2%TSPHY)
    ZQI_AD = MAX(0.0_JPRB,ZQI &
     & -ZAUTOI*YRPHY2%TSPHY)

    ZCLEAR_AD = 1.0_JPRB - PNEBS(JLON,JLEV)
    ZKDIFF_AD = 2.E-5_JPRB * ZPREF / PAPRSF(JLON,JLEV)
    ZFACT3_AD = (ZSQTVIS * ZKDIFF_AD)**ZEXP1
    ZFACT4_AD = RV * ZT / ZKDIFF_AD

     ! -----------------------
     ! For evaporation of rain
     ! -----------------------
    ZQSATW_AD = FOQS(ZWORK1_AD/PAPRSF(JLON,JLEV))
    ZSSATW_AD = 1.0_JPRB - ZQ/ZQSATW_AD

    ZCONDT_AD = ( FOLH(ZT,0.0_JPRB)/ZT )**2 /ZCDARV
    ZDIFFV_AD = ZFACT4_AD / ZWORK1_AD

    ZCEV_AD = ZSSATW_AD * ZCLEAR_AD * YRPHY0%RNINTR &
     & / ZRHO_AD / (ZCONDT_AD + ZDIFFV_AD)  
    ZCEV_AD = MAX(0.0_JPRB,ZCEV_AD)
    ZCEV1_AD = ZCEV_AD * ZCOEFF3 
    ZCEV2_AD = ZCEV_AD * ZCOEFF4 / ZFACT3_AD

     ! -----------------------
     ! For sublimation of snow
     ! -----------------------
    ZQSATI_AD = FOQS(ZWORK2_AD/PAPRSF(JLON,JLEV))
    ZSSATI_AD = 1.0_JPRB - ZQ/ZQSATI_AD

    ZCONDT_AD = ( FOLH(ZT,1.0_JPRB)/ZT )**2 /ZCDARV
    ZDIFFV_AD = ZFACT4_AD / ZWORK2_AD

    ZCSU_AD = ZSSATI_AD * ZCLEAR_AD * ZNS_AD &
     & / ZRHO_AD / (ZCONDT_AD + ZDIFFV_AD)  
    ZCSU_AD = MAX(0.0_JPRB,ZCSU_AD)
    ZCSU1_AD = ZCSU_AD * ZCOEFF5
    ZCSU2_AD = ZCSU_AD * ZCOEFF6 / ZFACT3_AD

     ! ------------------------
     ! For collection processes
     ! ------------------------
    ZCACC_AD = ZCOEFF1 * ZWORK3_AD
    ZCRIM_AD = ZCOEFF2 * ZWORK3_AD
    ZCAGG_AD = ZCOEFF2B * ZWORK3_AD * ZEFFA_AD

  

  ! =============================================
  ! PERFORM STATISTICAL ADVECTION OF PRECIPITATION
  ! =============================================

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
  
    ZDZ_AD   = ZFVEL_AD*YRPHY2%TSPHY
  
    ZWORK3_AD = MAX(0.0_JPRB,ZDPSG_AD*ZQPR_AD + &
     &              YRPHY2%TSPHY*(PFPLSL(JLON,JLEV-1)) + ZAUTOL_AD)
    ZQPSTOT_AD = MAX(0.0_JPRB,ZDPSG_AD*ZQPS_AD + &
     &              YRPHY2%TSPHY*(PFPLSN(JLON,JLEV-1)) + ZAUTOI_AD)

!  New formulation which does not take into account initial contents
! This implies a total independence to CFL criteria therefore to the layers thickness

!      ZWORK3(JLON) = MAX(0.0_JPRB,TSPHY*(PFPLSL(JLON,JLEV-1))+ZAUTOL(JLON,JLEV))
!      ZQPSTOT(JLON) = MAX(0.0_JPRB,TSPHY*(PFPLSN(JLON,JLEV-1))+ZAUTOI(JLON,JLEV))

    ZQR_AD = ZWORK3_AD / ZDZ_AD
    ZQS_AD = ZQPSTOT_AD / ZDZ_AD
    
    IF (YRPHY%LEVAPP) THEN
      ZWORK1_AD =  ZQR_AD / ZNRHOW
      ZWORK2_AD =  ZQS_AD / ZNS_AD
    ENDIF

   IF (YRPHY%LEVAPP) THEN
       ZPOW1_AD=ZCEV2_AD*ZWORK1_AD**ZEXP6
       ZPOW2_AD=ZCSU1_AD*ZWORK2_AD**ZEXP4
   ENDIF

    ZTQEVAPPL_AD = 0.0_JPRB
    ZTQEVAPPN_AD = 0.0_JPRB
    ZQFPFPL_AD   = 0.0_JPRB
    ZQFPFPN_AD   = 0.0_JPRB
    ZTCOLLL_AD   = 0.0_JPRB
    ZTCOLLN_AD   = 0.0_JPRB
    ZQMLT_AD     = 0.0_JPRB
    ZQMLTX_AD    = 0.0_JPRB
    ZQFRZ_AD     = 0.0_JPRB
    ZQFRZX_AD    = 0.0_JPRB  
    ZACCR_AD     = 0.0_JPRB

    IF (YRPHY%LEVAPP) THEN
         ! ----------------------------------------
         ! Evaporation/Sublimation of precipitation
         ! ----------------------------------------
      ZEVAPPL_AD = ZCEV1_AD*SQRT(ZWORK1_AD) + ZPOW1_AD
      ZEVAPPN_AD = ZPOW2_AD + ZCSU2_AD*ZWORK2_AD

      ZINT1_AD = 1.0_JPRB / MAX(ZEPS,ZEVAPPL_AD+ZEVAPPN_AD)

      IF (LLEVAPX) THEN
        ZSUBSA_AD = YRPHY0%REVASX*ZINT1_AD*(1.0_JPRB-EXP(-1.0_JPRB/(YRPHY0%REVASX*ZINT1_AD)))
        ZEVAPPL_AD = ZSUBSA_AD*ZEVAPPL_AD
        ZEVAPPN_AD = ZSUBSA_AD*ZEVAPPN_AD
      ENDIF

      ZSUBSA_AD = ZINT1_AD * ZEVAPPL_AD * (ZQSATW_AD - ZQ)
      ZTQEVAPPL_AD = MAX(0.0_JPRB, MIN( ZWORK3_AD,  &
       & ZEVAPPL_AD * ZDPSGDT_AD, ZSUBSA_AD * ZDPSG_AD ))   

      ZSUBSA_AD = ZINT1_AD * ZEVAPPN_AD * (ZQSATI_AD - ZQ)
      ZTQEVAPPN_AD = MAX(0.0_JPRB, MIN( ZQPSTOT_AD,  &
       & ZEVAPPN_AD * ZDPSGDT_AD, ZSUBSA_AD * ZDPSG_AD ))   
    ENDIF

    ZQPRTOT1_AD = ZWORK3_AD - ZTQEVAPPL_AD
    ZQPSTOT1_AD = ZQPSTOT_AD - ZTQEVAPPN_AD

    ZQR_AD = ZQPRTOT1_AD / ZDZ_AD
    ZQS_AD = ZQPSTOT1_AD / ZDZ_AD

    IF (YRPHY%LCOLLEC) THEN

         ! ----------------------------------------
         ! Collection of cloud liquid water by rain
         ! ----------------------------------------
      ZACCR_AD = ZQL_AD*(1.0_JPRB-EXP(-ZCACC_AD*ZQR_AD*YRPHY2%TSPHY)) &
      &     * MAX(0.0_JPRB,SIGN(1.0_JPRB,ZT-RTT))

         ! -------------------------------
         ! Collection of cloud ice by snow
         ! -------------------------------
      ZAGGR_AD = ZQI_AD*(1.0_JPRB-EXP(-ZCAGG_AD*ZQS_AD*YRPHY2%TSPHY))
         ! ----------------------------------------
         ! Collection of cloud liquid water by snow
         ! ----------------------------------------
      ZRIMI_AD = ZQL_AD*(1.0_JPRB-EXP(-ZCRIM_AD*ZQS_AD*YRPHY2%TSPHY))
         ! ----------------------------
         ! Sum up collection processes
         ! ----------------------------
      ZTCOLLL_AD = MAX(0.0_JPRB, MIN(ZACCR_AD+ZRIMI_AD,ZQL_AD) ) &
       & * ZDPSG_AD
      ZTCOLLN_AD = MAX(0.0_JPRB, MIN(ZAGGR_AD      ,ZQI_AD) ) &
       & * ZDPSG_AD

    ENDIF

    ZQPRTOT2_AD  = ZWORK3_AD + ZTCOLLL_AD
    ZQPSTOT2_AD  = ZQPSTOT_AD + ZTCOLLN_AD

    IF (LLMELTS) THEN

         ! ----------------------------
         ! Snow melting
         ! ----------------------------
      ZLHFUS_AD = FOLH(ZT,1.0_JPRB) - FOLH(ZT,0.0_JPRB)
      ZQMLTX_AD = ZDPSG_AD * PCP(JLON,JLEV) &
       & * MAX(0.0_JPRB,ZDELT_AD) / ZLHFUS_AD

       IF (.NOT. YRPHY%LSMOOTHMELT) THEN 
        ZQMLT_AD = MIN ( ZQMLTX_AD , ZQPSTOT2_AD - ZTQEVAPPN_AD )
       ELSE
        ZQMLT_AD=(ZQPSTOT2_AD-ZTQEVAPPN_AD)*(1+TANH(ZDELT_AD/YRPHY0%RSMOOTHMELT))/2.0_JPRB
       ENDIF
     ENDIF 
     IF (LLFREEZ) THEN  

         ! ----------------------------
         ! Rain freezing
         ! ----------------------------
      
      ZQFRZX_AD = ZDPSG_AD * PCP(JLON,JLEV) &
       & * MAX(0.0_JPRB,-ZDELT_AD) / ZLHFUS_AD
      
      ZQFRZ_AD = MIN ( ZQFRZX_AD , ZQPRTOT2_AD - ZTQEVAPPL_AD ) 
      
    ENDIF
    
    PFPEVPL(JLON,JLEV) = PFPEVPL(JLON,JLEV-1) &
     & + ( ZTQEVAPPL_AD - ZQMLT_AD + ZQFRZ_AD) / YRPHY2%TSPHY
    PFPEVPN(JLON,JLEV) = PFPEVPN(JLON,JLEV-1) &
     & + ( ZTQEVAPPN_AD + ZQMLT_AD - ZQFRZ_AD) / YRPHY2%TSPHY

    PFPFPL (JLON,JLEV) = PFPFPL (JLON,JLEV-1) &
     & + ( ZTCOLLL_AD + ZAUTOL_AD ) / YRPHY2%TSPHY
    PFPFPN (JLON,JLEV) = PFPFPN (JLON,JLEV-1) &
     & + ( ZTCOLLN_AD + ZAUTOI_AD ) / YRPHY2%TSPHY
         ! ----------------------------
         ! Computation of fundamental proportions
         ! needed by the statistical algorithm
         ! (only YB formulation !)
         ! ----------------------------
! Rain and snow           
    ZDZS_AD = (PAPHI(JLON,JLEV-1) - PAPHI(JLON,JLEV))/RG
    ZP1_AD  = MIN(1._JPRB , ZDZ_AD/ZDZS_AD)
    ZP2_AD  = MAX(0._JPRB,1._JPRB - ZDZS_AD/ZDZ_AD)
    ZP3_AD  = (ZP1_AD + ZP2_AD)/2.0_JPRB
! Cloud liquid water      
    ZP1L_AD  = MIN(1._JPRB , ZDZL_AD/ZDZS_AD)
    ZP2L_AD  = MAX(0._JPRB,1._JPRB - ZDZS_AD/MAX(ZEPS,ZDZL_AD))
! Cloud ice
    ZP1I_AD  = MIN(1._JPRB , ZDZI_AD/ZDZS_AD)
    ZP2I_AD  = MAX(0._JPRB,1._JPRB - ZDZS_AD/MAX(ZEPS,ZDZI_AD))
    
! WARNING ! : Dans cette version pour coller a ADVPRC il n'y a pas de traitement separe de la neige 
!             et de la pluie. Ceci serait difficile dans ADVPRC mais trivial dans ADVPRCS      
  ! ================================================================
  ! COMPUTE FLUX ASSOCIATED TO FALLING OF PRECIPITATION
  ! ================================================================

    PFPLSL(JLON,JLEV) = (ZP1_AD*ZDPSG_AD*ZQPR_AD      &
    &                 + ZP2_AD*YRPHY2%TSPHY*PFPLSL(JLON,JLEV-1)              &      
    &                 + ZP3_AD*(ZAUTOL_AD + ZTCOLLL_AD + ZQMLT_AD)) &
    &                 * MAX(0.0_JPRB,                              &
    &                (1._JPRB - (ZTQEVAPPL_AD+ZQFRZ_AD)/MAX(ZEPS,ZQPRTOT2_AD))) / YRPHY2%TSPHY
    

    PFPLSN(JLON,JLEV) = (ZP1_AD*ZDPSG_AD*ZQPS_AD      &
    &                 + ZP2_AD*YRPHY2%TSPHY*PFPLSN(JLON,JLEV-1)              &      
    &                 + ZP3_AD*(ZAUTOI_AD + ZTCOLLN_AD + ZQFRZ_AD)) &
    &                 * MAX(0.0_JPRB,                              &
    &                (1._JPRB - (ZTQEVAPPN_AD+ZQMLT_AD)/MAX(ZEPS,ZQPSTOT2_AD))) / YRPHY2%TSPHY

    PSEDIQL(JLON,JLEV) = (ZP1L_AD*ZDPSG_AD*ZQL_AD      &
     &                  + ZP2L_AD*YRPHY2%TSPHY*PSEDIQL(JLON,JLEV-1) ) / YRPHY2%TSPHY
     
    PSEDIQN(JLON,JLEV) = (ZP1I_AD*ZDPSG_AD*ZQI_AD      &
     &                  + ZP2I_AD*YRPHY2%TSPHY*PSEDIQN(JLON,JLEV-1) ) / YRPHY2%TSPHY
     
   ! JLON = KIDIA, KFDIA

  !-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
ENDDO ! LEV=1,KFLEV : end of statistical advection 
    !-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

!- - - - - - - - - - - - - - - - - - - - - - -



END SUBROUTINE ACPLUIZ
