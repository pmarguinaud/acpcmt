!-----------------------------------------------------------------------
SUBROUTINE ACMICRO( KIDIA, KFDIA, KLON, KTDIA, KLEV,&
 !-----------------------------------------------------------------------
 ! - INPUT -
 & PNEBST, PT  , PQL , PQI , PTS , PNEIJ , PLSM,&
 ! - OUTPUT -
 & PAUTOL, PAUTOI, KSTACK, PSTACK, KPSTSZ, KKSTSZ, KPSTPT, KKSTPT )

!**** *ACMICRO * - CALCULS D'AUTOCONVERSION.
!                  AUTOCONVERSION COMPUTATIONS.

!     Sujet.
!     ------

!**   Interface.
!     ----------
!        *CALL* *ACMICRO*

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

! PNEBST     : NEBULOSITE PARTIELLE STRATIFORME.
!            : STRATIFORM FRACTIONAL CLOUDINESS.
! PT         : TEMPERATURE.
!            : TEMPERATURE.
! PQL        : CONTENU EN CONDENSAT NUAGEUX LIQUIDE.
!            : CLOUD WATER LIQUID.
! PQI        : CONTENU EN CONDENSAT NUAGEUX SOLIDE.
!            : CLOUD WATER SOLID.

! - 1D (1:KLON) .
                                                                                
! PTS        : TEMPERATURE DE SURFACE.
!            : SURFACE TEMPERATURE.
! PNEIJ      : PROPORTION DE LA MAILLE RECOUVERTE DE NEIGE.
!            : SNOW FRACTION.
! PLSM       : INDICE TERRE/MER.
!            : LAND/SEA MASK.
                                                                                
! -   ARGUMENTS EN SORTIE.
!     ---------------------------

! PAUTOL     : GENERATION DE PRECIPITATIONS A PARTIR DE L'EAU NUAGEUSE LIQ.
!            : GENERATION OF PRECIPITATION FROM LIQUID CLOUD WATER. 
! PAUTOI     : GENERATION DE PRECIPITATIONS A PARTIR DE L'EAU NUAGEUSE SOL.
!            : GENERATION OF PRECIPITATION FROM SOLID CLOUD WATER. 

!-----------------------------------------------------------------------

!     Auteur.
!     -------
!        99-10, Philippe Lopez
!               CNRM/GMME/RECYF

!     Modifications.
!     --------------
!      2002-01, P. Marquet Comments : 
!               here PAUTO is >0 when conversion occur, and the
!               contribution is "-PAUTO" in the "qc" equation.
!      2004-06, P. Marquet : introduce RAUTEFR for liquid->rain, with
!                            the old RAUTEFS valid for ice->snow
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!      2004-10, F. Bouyssel : Cleaning
!      2005-05, F. Bouyssel : More tunings in autoconvertion thresholds
!      2005-07, F. Bouyssel : Modification of ice autoconvertion thresholds
!                             over snow and sea ice (RQICRSN)
!      2006-01, F. Bouyssel : Remove ZSURF
!      2006-04, F. Bouyssel : Replace PQC by PQL,PQI

!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOMPHY0   , ONLY : YRPHY0
USE YOMPHY1   , ONLY : YRPHY1
USE YOMPHY2   , ONLY : YRPHY2
USE YOMCST    , ONLY : RTT

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEBST(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT    (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL   (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI   (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS   (KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEIJ (KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSM  (KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAUTOL(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAUTOI(KLON,KLEV)
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSTACK (KLON, KKSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KKSTSZ, KPSTPT, KKSTPT
 

REAL(KIND=JPRB) :: ZQL,ZQI,ZCAUT,&
 & ZALPH,ZDUM,ZQCR,ZARG1,ZARG2,ZBETA,ZFACICE

REAL(KIND=JPRB) :: ZAUTOL(KLON,KLEV), ZAUTOI(KLON,KLEV) &
 & , ZDELT(KLON,KLEV) , ZEFFA(KLON,KLEV)  
INTEGER(KIND=JPIM) :: JLON,JLEV
INTEGER(KIND=JPIM) :: IPSTPT_ZEFFA

INTEGER(KIND=JPIM) :: IPSTPT_ZDELT

INTEGER(KIND=JPIM) :: IPSTPT_ZAUTOI

INTEGER(KIND=JPIM) :: IPSTPT_ZAUTOL

INTEGER(KIND=JPIM) :: IPSTPT, IKSTPT


!     ------------------------------------------------------------------

!     CHECK RELIABILITY OF INPUT ARGUMENTS.



!     ------------------------------------------------------------------

! Define threshold for ice autoconversion as a function of temperature
! --------------------------------------------------------------------
 

IPSTPT = KPSTPT

IKSTPT = KKSTPT

IPSTPT_ZAUTOL = IPSTPT

IPSTPT = IPSTPT + KLEV

IPSTPT_ZAUTOI = IPSTPT

IPSTPT = IPSTPT + KLEV

IPSTPT_ZDELT = IPSTPT

IPSTPT = IPSTPT + KLEV

IPSTPT_ZEFFA = IPSTPT

IPSTPT = IPSTPT + KLEV

IF (IPSTPT > KPSTSZ) CALL ABOR1 ('IPSTPT > KPSTSZ')

IF (IKSTPT > KKSTSZ) CALL ABOR1 ('IKSTPT > KKSTSZ')
ZARG1 = 2.0_JPRB*YRPHY0%RQICRMAX*(1.0_JPRB-0.999_JPRB)/(YRPHY0%RQICRMAX-YRPHY0%RQICRMIN)-1.0_JPRB
ZARG2 = 2.0_JPRB*(YRPHY0%RQICRMAX-1.5_JPRB*YRPHY0%RQICRMIN)/(YRPHY0%RQICRMAX-YRPHY0%RQICRMIN)-1.0_JPRB
ZARG1 = 0.5_JPRB*LOG(ABS((1.0_JPRB+ZARG1)/(1.0_JPRB-ZARG1)))
ZARG2 = 0.5_JPRB*LOG(ABS((1.0_JPRB+ZARG2)/(1.0_JPRB-ZARG2)))
ZALPH = (ZARG1 - ZARG2)/(YRPHY0%RQICRT2-YRPHY0%RQICRT1)
ZBETA = ZARG1 - YRPHY0%RQICRT2 * ZALPH
 
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    PSTACK (JLON, IPSTPT_ZDELT+JLEV-(1)) = PT(JLON,JLEV) - RTT

! Efficiency for ice conversion as a function of temperature.
! -----------------------------------------------------------
    PSTACK (JLON, IPSTPT_ZEFFA+JLEV-(1)) = EXP(YRPHY0%RAUTSBET*&
    &  PSTACK (JLON, IPSTPT_ZDELT+JLEV-(1)))

  ENDDO
ENDDO

! ---------------------------------------------------
! MICROPHYSICAL AUTOCONVERSION IN THE STRATIFORM CASE
! ---------------------------------------------------

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA

! Compute in-cloud values
! -----------------------

    ZQL = MAX(0.0_JPRB,PQL(JLON,JLEV)/PNEBST(JLON,JLEV))
    ZQI = MAX(0.0_JPRB,PQI(JLON,JLEV)/PNEBST(JLON,JLEV))

! AUTOCONVERSION OF CLOUD LIQUID WATER INTO RAIN
! ----------------------------------------------

    ZCAUT = YRPHY0%RAUTEFR
    ZDUM = (1.0_JPRB-EXP(-ZCAUT*YRPHY2%TSPHY)) * (ZQL-YRPHY0%RQLCR) / YRPHY2%TSPHY
    PSTACK (JLON, IPSTPT_ZAUTOL+JLEV-(1)) = MAX(0.0_JPRB,ZDUM)

! AUTOCONVERSION OF CLOUD ICE INTO PRECIPITATING ICE
! --------------------------------------------------

    ZCAUT = YRPHY0%RAUTEFS * PSTACK (JLON, IPSTPT_ZEFFA+JLEV-(1))
    ZQCR = YRPHY0%RQICRMAX - (YRPHY0%RQICRMAX - YRPHY0%RQICRMIN) * 0.5_JPRB * (1.0_JPRB&
    &  + TANH(ZALPH * PSTACK (JLON, IPSTPT_ZDELT+JLEV-(1)) + ZBETA))
    ZFACICE = PLSM(JLON)*PNEIJ(JLON) + (1.0_JPRB-PLSM(JLON)) &
     & * MAX(0.0_JPRB,SIGN(1.0_JPRB,YRPHY1%TMERGL-PTS(JLON)))
    ZQCR = ZQCR * (1.0_JPRB-ZFACICE*(1.0_JPRB-YRPHY0%RQICRSN))
    ZDUM = (1.0_JPRB-EXP(-ZCAUT*YRPHY2%TSPHY)) * (ZQI-ZQCR) / YRPHY2%TSPHY
    PSTACK (JLON, IPSTPT_ZAUTOI+JLEV-(1)) = MAX(0.0_JPRB,ZDUM)
 
! TOTAL AUTOCONVERSION TERM
! -------------------------

    PAUTOL(JLON,JLEV) = PSTACK (JLON, IPSTPT_ZAUTOL+JLEV-(1)) * PNEBST(JLON,JLEV&
    & )
    PAUTOI(JLON,JLEV) = PSTACK (JLON, IPSTPT_ZAUTOI+JLEV-(1)) * PNEBST(JLON,JLEV&
    & )

  ENDDO 
ENDDO 


END SUBROUTINE ACMICRO
