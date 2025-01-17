! -----------------------------------------------------------------------
SUBROUTINE ACMTDDD( KIDIA,KFDIA,KLON,KTDIA,KLEV,KTRA,&
! -----------------------------------------------------------------------
! - INPUT  2D .
&KNCDN,KNLAB,&
&PALPH,PAPHIF,PAPRS,PAPRSF,&
&PDELP,PLH,PLNPR,PQ,PQLIC,PQLIS,&
&PQSAT,PQW,PR,PT,PTW,&
&PU,PV,PTRA,&
! - INPUT  1D .
&KLEVTEN,&
! - OUTPUT 2D .
&PBET,PDELA,PEPSI,PFORM,PQDN,&
&PQN2,PSDN,PSN2,PTN2,PUM,PVM,PTRAM,&
&PBCC_CD_FULL,&
! - OUTPUT 1D .
&KNLABD, KSTACK, PSTACK, KPSTSZ, KKSTSZ, KPSTPT, KKSTPT)

!**** *ACCVIMPD * - SCHEMA DE COURANTS DESCENDANTS A FLUX DE MASSE.


! Sujet.
! ------
!     - CALCUL DES FLUX DE CONDENSATION ET DE TRANSPORT CONVECTIFS
!       DESCENDANTS PAR UN SCHEMA A FLUX DE MASSE.

!     - MASS-FLUX DOWNDRAFT SCHEME, COMPUTING CONVECTIVE
!       CONDENSATION AND TRANSPORT FLUXES.

! **   Interface.
! ----------
! *CALL* *ACCVIMPD*

! -----------------------------------------------------------------------
! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
! "APLPAR" CODE.
! -----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE.
! -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
! KTDIA      : INDICE DE DEPART DES BOUCLES VERTICALES (NIVEL DU SOMMET
!            : DU NUAGE).
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".


! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
! CATEGORIE).


! KNCDN      : NIVEAU DU POINT DE CONDENSATION.
! KNLAB      : INDICE D'ACTIVITE CONVECTIVE (NUAGE SI EGAL A UN).

! - 2D (0:KLEV) .

! PAPRS      : PRESSION AUX DEMI-NIVEAUX.

! - 2D (1:KLEV) .

! PALPH      : LOG(PAPRS(JLEV)/PAPRSF(JLEV)) (POUR L'HYDROSTATIQUE).
! PAPHIF     : GEOPOTENTIEL AUX NIVEAUX DES COUCHES.
! PAPRSF     : PRESSION AUX NIVEAUX DES COUCHES.
! PDELP      : EPAISSEUR EN PRESSION DE LA COUCHE.
! PLH        : CHALEUR LATENTE A LA TEMPERATURE DE L'AIR.
! PLNPR      : LOG(PAPRS(JLEV)/PAPRS(JLEV-1)) (POUR L'HYDROSTATIQUE).
! PQ         : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
! PQLIC      : EAU LIQUIDE OU SOLIDE CONVECTIVE.
! PQLIS      : EAU LIQUIDE OU SOLIDE STRATIFORME.
! PQSAT      : HUMIDITE SPECIFIQUE DE SATURATION.
! PQW        : HUMIDITE SPECIFIQUE DU THERMOMETRE MOUILLE.
! PR         : CONSTANTE DES GAZ POUR L'AIR.
! PT         : TEMPERATURE.
! PTW        : TEMPERATURE DU THERMOMETRE MOUILLE.
! PU         : COMPOSANTE EN X DU VENT.
! PV         : COMPOSANTE EN Y DU VENT.

! - 1D .

! KLEVTEN    : NIVEAU MINIMUM DE THETAE
! -----------------------------------------------------------------------

! -   ARGUMENTS EN SORTIE.
! ---------------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .

! PFORM      : PROFIL DE FLUX DE MASSE.

! - 2D (1:KLEV) .

! PBET       : COEFFICIENT BETA DE LA VITESSE HORIZONTALE.
! PDELA      : COEFFICIENT DE DETRAINEMENT.
! PEPSI      : COEFFICIENT D'ENTRAINEMENT.
! PQDN       : HUMIDITE DE DETRAINEMENT DU COURANT DESCENDANT.
! PQN2       : HUMIDITE DU COURANT DESCENDANT.
! PSDN       : ENERGIE STATIQUE SECHE DE DETRAINEMENT DU C. DESCENDANT.
! PSN2       : ENERGIE STATIQUE SECHE DU C. DESCENDANT.
! PTN2       : TEMPERATURE DU C. DESCENDANT.
! PUM        : MOYENNE PONDEREE DE LA VITESSE U DEPUIS LA SURFACE.
! PVM        : MOYENNE PONDEREE DE LA VITESSE V DEPUIS LA SURFACE.
! PTRAM      : MOYENNE PONDEREE DE LA CONCENTRATION DE TRACEUR DEPUIS LA SURFACE.
! PBCC_CD_FULL : DERIVEE VERTICALE DE L'HUMIDITE SPECIFIQUE NUAGEUSE

! KNLABD     : INDICE DE COURANT DESCENDANT (C.D. SI EGAL A UN).

! -----------------------------------------------------------------------

! -   ARGUMENTS IMPLICITES.
! ---------------------

! COMMON/YOMPHY /
! COMMON/YOMCST /
! COMMON/YOMPHY0/
! COMMON/YOMPHY2/
! COMMON/FCTTRM /

! -----------------------------------------------------------------------

!     Externes.
!     ---------

!     Methode.
!     --------
!        SCHEMA A FLUX DE MASSE (J.F. GUEREMY)

!        MASS-FLUX SCHEME (J.F. GUEREMY)

!     Auteur.
!     -------
!        02-09, J.F. GUEREMY (� partir de accvimp cod� par J.F. GELEYN)

!     Modifications.
!     --------------
!        04-11, Calcul de la subsidence compensatoire avec le courant
!               ascendant - J.F. GUEREMY
!        06-01, Modif variables de detrainement dans la couche
!               seche - J.F. GUEREMY
!        06-02, P. Marquet : set the same (?) NAMELIST tuning parameter 
!                            as done by Alias/Gueremy in cy30 codes
!                            ZGAMA = 1.5_JPRB      -> GAMAP1
!                            ZKD   = 240.E-06_JPRB -> RKDN*TENTRX/TENTR
!        06-03, KDN renamed to RKDN - A.Alias
!        09-07, remove CDLOCK + some cleanings -  K. Yessad
!        09-10, modif indice de courant descendant et fonction isign - J.F. GUEREMY
!        10-08, correction vit verticale conv - J.F. GUEREMY
!        14-04, tracer transport - D. SAINT-MARTIN
!        15-09, BUGFIX-J.F Gu�r�my

! -----------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRB


USE YOMPHY   , ONLY : YRPHY
USE YOMCST   , ONLY : RG       ,RD       ,RV       ,RCPD     ,&
            &RCPV     ,RETV     ,RCW      ,RCS      ,RLVTT    ,&
            &RLSTT    ,RTT      ,RALPW    ,RBETW    ,RGAMW    ,&
            &RALPS    ,RBETS    ,RGAMS    ,RALPD    ,RBETD    ,&
            &RGAMD
USE YOMPHY0  , ONLY : YRPHY0

! -----------------------------------------------------------------------

IMPLICIT NONE

!     DUMMY ARRAYS
INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
INTEGER(KIND=JPIM), INTENT(IN) :: KLON
INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KTRA

INTEGER(KIND=JPIM), INTENT(IN) :: KNCDN(KLON,KLEV)
INTEGER(KIND=JPIM), INTENT(IN) :: KNLAB(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PALPH(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PAPHIF(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PAPRS(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PDELP(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PLH(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PLNPR(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PQ(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PQLIC(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PQLIS(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PQSAT(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PQW(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PR(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PT(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PTW(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PU(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PV(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN) :: PTRA(KLON,KLEV,KTRA)
INTEGER(KIND=JPIM), INTENT(IN) :: KLEVTEN(KLON)

REAL(KIND=JPRB), INTENT(OUT) :: PBET(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PDELA(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PEPSI(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PFORM(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PQDN(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PQN2(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PSDN(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PSN2(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PTN2(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PUM(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PVM(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT) :: PTRAM(KLON,KLEV,KTRA)
REAL(KIND=JPRB), INTENT(INOUT) :: PBCC_CD_FULL(KLON,KLEV)
INTEGER(KIND=JPIM), INTENT(OUT) :: KNLABD(KLON,KLEV)
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSTACK (KLON, KKSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KKSTSZ, KPSTPT, KKSTPT


! -----------------------------------------------------------------------

!     LOCAL ARRAYS



REAL(KIND=JPRB) :: ZGAMA, ZKD, ZRVMD, ZCPVMD, ZCPVMW, ZCPVMS&
    &, ZEPSO, ZENTR, ZDELTA, ZDCP, ZDELT, ZDELQ, ZEW&
    &, ZESP, ZQW, ZDQW, ZCPS, ZTD, ZTNSEC, ZTVN, ZTVE, ZFLO, ZFLOI&
    &, ZDELPF, ZENTRT, ZVVERDEN, ZVVERN, ZVVERF, ZENTRO, ZTEST&
    &, ZDELTAP, ZB

!     LOCAL SCALAR
INTEGER(KIND=JPIM) :: JLEV, JLON, ITOP, JIT, ICDN, IBDC, IVVER, IDOMDP&
    &, IKUO1, IKUO2, INUA, ILEVBDC, ISUM, ISDELTAP

INTEGER(KIND=JPIM) :: JTRA
INTEGER(KIND=JPIM) :: IPSTPT_ZBCC_CD_HALF

INTEGER(KIND=JPIM) :: IPSTPT_ZFMOD

INTEGER(KIND=JPIM) :: IPSTPT_ZDWNDP

INTEGER(KIND=JPIM) :: IPSTPT_ZVVER

INTEGER(KIND=JPIM) :: IPSTPT_ZVVERFM1

INTEGER(KIND=JPIM) :: IPSTPT_ZTN

INTEGER(KIND=JPIM) :: IPSTPT_ZTB

INTEGER(KIND=JPIM) :: IPSTPT_ZRVH

INTEGER(KIND=JPIM) :: IPSTPT_ZRBH

INTEGER(KIND=JPIM) :: IPSTPT_ZRBB

INTEGER(KIND=JPIM) :: IPSTPT_ZQN

INTEGER(KIND=JPIM) :: IPSTPT_ZQB

INTEGER(KIND=JPIM) :: IPSTPT_ZMIX

INTEGER(KIND=JPIM) :: IPSTPT_ZLH

INTEGER(KIND=JPIM) :: IPSTPT_ZCP

INTEGER(KIND=JPIM) :: IKSTPT_INIVBDC

INTEGER(KIND=JPIM) :: IPSTPT, IKSTPT



#include "fcttrm.func.h"


! -----------------------------------------------------------------------

! *
! ------------------------------------------------------------------

!     I - CONSTANTES AUXILIAIRES.
!         AUXILIARY CONSTANTS.

! PARAMETRE DE MASSE VIRTUELLE + 1.

IPSTPT = KPSTPT

IKSTPT = KKSTPT

IKSTPT_INIVBDC = IKSTPT

IKSTPT = IKSTPT + KLEV

IPSTPT_ZCP = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZLH = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZMIX = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZQB = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZQN = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZRBB = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZRBH = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZRVH = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZTB = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZTN = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZVVERFM1 = IPSTPT

IPSTPT = IPSTPT + 1

IPSTPT_ZVVER = IPSTPT

IPSTPT = IPSTPT + KLEV-0+1

IPSTPT_ZDWNDP = IPSTPT

IPSTPT = IPSTPT + KLEV-0+1

IPSTPT_ZFMOD = IPSTPT

IPSTPT = IPSTPT + KLEV-0+1

IPSTPT_ZBCC_CD_HALF = IPSTPT

IPSTPT = IPSTPT + KLEV-0+1

IF (IPSTPT > KPSTSZ) CALL ABOR1 ('IPSTPT > KPSTSZ')

IF (IKSTPT > KKSTSZ) CALL ABOR1 ('IKSTPT > KKSTSZ')
ZGAMA=YRPHY0%GAMAP1

! COEFFICIENT DE RESISTANCE.
ZKD=YRPHY0%RKDN*YRPHY0%TENTRX/YRPHY0%TENTR

ZRVMD=RV-RD
ZCPVMD=RCPV-RCPD
ZCPVMW=RCPV-RCW
ZCPVMS=RCPV-RCS

! *
! ------------------------------------------------------------------
!     II - CALCUL DES PARAMETRES DERIVES, CONSTANTES DE SECURITE (POUR
!     TROIS DES INTEGRALES VERTICALES DU SCHEMA).

!     COMPUTATION OF DERIVED PARAMETERS, SECURITY CONSTANTS (FOR
!     THREE OF THE VERTICAL INTEGRALS OF THE SCHEME).

ZEPSO=1.E-12_JPRB

! *
! ------------------------------------------------------------------
!     III - MISE A ZERO DE TOUS LES FLUX (POUR LE CAS SANS CONVECTION)

!           SETTING ALL FLUXES TO ZERO (FOR THE CASE WITHOUT CONVECTION)

! - TEMPORAIRE(S) 2D (1:KLEV) .

! ZFMOD      : FONCTION DE LA VITESE DU COURANT DESCENDANT.

DO JLEV=KTDIA-1,KLEV
  DO JLON=KIDIA,KFDIA

!      MISE A UN DE LA FONCTION DE MODULATION.
!      SETTING MODULATION FUNCTION TO ONE.

    PSTACK (JLON, IPSTPT_ZFMOD+JLEV-(0))=1.0_JPRB

  ENDDO
ENDDO

DO JLEV = 0, KLEV
DO JLON = KIDIA, KFDIA
PSTACK (JLON, IPSTPT_ZBCC_CD_HALF+JLEV-(0)) = 0._JPRB
ENDDO
ENDDO

DO JLEV = 1, KLEV
DO JLON = KIDIA, KFDIA
PBCC_CD_FULL(JLON,JLEV) = 0._JPRB
ENDDO
ENDDO


! *
! ------------------------------------------------------------------
!     IV - INITIALISATION A LA BASE. LE PROFIL DU COURANT DESCENDANT
!     SERA FINALEMENT CARACTERISE PAR SON HUMIDITE TOTALE ET SON ENERGIE
!     STATIQUE SECHE DE DETRAINEMENT AINSI QUE PAR UN INDICE DE STABILITE
!     (UN AUX DEUX BOUTS DE TOUTE COUCHE (DE COUCHE DU MODELE A COUCHE DU
!     MODELE ADJACENTE) ET ZERO AILLEURS) ET UN PROFIL NON ENCORE NORME
!     DU FLUX DE MASSE, TABLEAU DANS LEQUEL SERA ENSUITE RANGE LE
!     PRODUIT DU FLUX DE MASSE PAR LE PAS DE TEMPS (HOMOGENE A UNE
!     PRESSION).

!     BOTTOM INITIALISATION. THE DOWNDRAFT PROFILE WILL EVENTUALY BE
!     CHARACTERISED BY ITS TOTAL HUMIDITY AND ITS DETRAINING DRY STATIC
!     ENERGY AS WELL AS BY A STABILITY INDEX (ONE AT BOTH ENDS OF ANY
!     SLAB (FROM MODEL LAYER TO THE ADJACENT ONE) AND ZERO
!     ELSEWHERE) AND A YET UNNORMALIZED MASS FLUX PROFILE, ARRAY IN
!     WHICH THE PRODUCT OF THE MASS FLUX BY THE TIME STEP (QUANTITY
!     HOMOGENEOUS TO A PRESSURE) WILL AFTERWARDS BE WRITEN.


! - TEMPORAIRE(S) 2D (0:KLEV) .

! PFORM     : PROFIL DE FLUX DE MASSE.
!           : PROFILE OF MASS FLUX.

! - TEMPORAIRE(S) 2D (1:KLEV) .

! PSDN       : ENERGIE STATIQUE SECHE DE DETRAINEMENT DU COURANT DESCENDANT.
!            : DETRAINING DRY STATIC ENERGY OF THE DOWNDRAFT.
! PQDN       : HUMIDITE DE DETRAINEMENT DU COURANT DESCENDANT.
!            : DETRAINING DOWNDRAFT HUMIDITY.

DO JLON=KIDIA,KFDIA

!     CARACTERISTIQUES THERMODYNAMIQUES DU COURANT DESCENDANT.
!     THERMODYNAMIC CHARACTERISTICS OF THE DOWNDRAFT.

! - TEMPORAIRE(S) 1D .

! ZTN        : TEMPERATURE DU COURANT DESCENDANT.
!            : TEMPERATURE OF THE DOWNDRAFT.
! ZQN        : HUMIDITE SPECIFIQUE VAPEUR DU COURANT DESCENDANT.
!            : WATER VAPOUR SPECIFIC HUMIDITY OF THE DOWNDRAFT.


  PSTACK (JLON, IPSTPT_ZTN)=PTW(JLON,KTDIA)
  PSTACK (JLON, IPSTPT_ZQN)=PQW(JLON,KTDIA)
  PQN2(JLON,KTDIA)=PSTACK (JLON, IPSTPT_ZQN)
  PSN2(JLON,KTDIA)=(RCPD+ZCPVMD*PSTACK (JLON, IPSTPT_ZQN))*&
  &  PSTACK (JLON, IPSTPT_ZTN)+PAPHIF(JLON,KTDIA)
  PTN2(JLON,KTDIA)=PSTACK (JLON, IPSTPT_ZTN)

  ZTD=PT(JLON,KTDIA)*(1.0_JPRB+(PQLIC(JLON,KTDIA)-RETV*(PQSAT(JLON,KTDIA)&
   &-PQ(JLON,KTDIA)))/(1.0_JPRB+RETV*PLH(JLON,KTDIA)*PQSAT(JLON,KTDIA)&
   &/(RV*PT(JLON,KTDIA))))
  ZTD=YRPHY0%GCVADET*ZTD+(1.0_JPRB-YRPHY0%GCVADET)*PSTACK (JLON, IPSTPT_ZTN)
  IF (YRPHY%LNEIGE) THEN
    ZDELTA=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTD))
  ELSE
    ZDELTA=0.0_JPRB
  ENDIF
  ZEW=FOEW(ZTD,ZDELTA)
  ZESP=ZEW/PAPRSF(JLON,KTDIA)
  ZQW=FOQS(ZESP)
  ZQW=YRPHY0%GCVADET*ZQW+(1.0_JPRB-YRPHY0%GCVADET)*PSTACK (JLON, IPSTPT_ZQN)

  PQDN(JLON,KTDIA)=ZQW
  PSDN(JLON,KTDIA)=(RCPD+ZCPVMD*ZQW)*ZTD+PAPHIF(JLON,KTDIA)

!      CARACTERISTIQUES ACTIVES DU COURANT DESCENDANT.
! ACTIVE CHARACTERISTICS OF THE DOWNDRAFT.

! - TEMPORAIRE(S) 1D .

  KNLABD(JLON,KTDIA)=0
  PFORM(JLON,KTDIA-1)=0.0_JPRB

  PFORM(JLON,KLEV)=0.0_JPRB

! INIVBDC    : NIVEAU DE LA BASE DE LA DESCENDANCE CONVECTIVE.
! ZVVER      : VITESSE VERT. DE LA COLONNE CONVECTIVE AUX INTERCOUCHES.
! PEPSI      : ENTRAINEMENT.
! PDELA      : DETRAINEMENT.
! ZVVERFM1   : VITESSE VERT. DE LA COLONNE CONVECTIVE, AU NIVEAU MOINS.
! ZDWNDP     : DERIVEE VERT. DE VITESSE VERT.
! ZCAPE      : CAPE DU NIVEAU.

  KSTACK (JLON, IKSTPT_INIVBDC+KTDIA-(1))=KTDIA*KNLAB(JLON,KTDIA)*MAX(0,-ISIGN(1,&
  & -KNCDN(JLON,KTDIA)))
  PSTACK (JLON, IPSTPT_ZVVER+KTDIA-1-(0))=0.0_JPRB
  PSTACK (JLON, IPSTPT_ZVVER+KTDIA-(0))=0.0_JPRB
  PEPSI(JLON,KTDIA)=YRPHY0%TENTRX*PR(JLON,KTDIA)*PT(JLON,KTDIA)/PAPRSF(JLON,KTDIA)
  PDELA(JLON,KTDIA)=0.0_JPRB
  PSTACK (JLON, IPSTPT_ZVVERFM1)=0.0_JPRB
  PSTACK (JLON, IPSTPT_ZDWNDP+KTDIA-1-(0))=0.0_JPRB
  PSTACK (JLON, IPSTPT_ZDWNDP+KTDIA-(0))=0.0_JPRB

! -TEMPORAIRES 2D: VALEUR A LA SURFACE:

! PBET       : COEFFICIENT BETA DE LA VITESSE HORIZONTALE
! PUM        : MOYENNE PONDEREE DE LA VITESSE U DEPUIS LA SURFACE
! PVM        : MOYENNE PONDEREE DE LA VITESSE V DEPUIS LA SURFACE
! PTRAM      : MOYENNE PONDEREE DE LA CONCENTRATION DE TRACEUR DEPUIS LA SURFACE

  PBET(JLON,KTDIA)=1.0_JPRB
  PUM(JLON,KTDIA)=PU(JLON,KTDIA)
  PVM(JLON,KTDIA)=PV(JLON,KTDIA)

  DO JTRA=1,KTRA
    PTRAM(JLON,KTDIA,JTRA)=PTRA(JLON,KTDIA,JTRA)    
  ENDDO

ENDDO

! *
! ------------------------------------------------------------------
!     V - PREMIERE BOUCLE VERTICALE (VERS LE BAS) INCLUANT LE CALCUL
!     "EN ADIABATIQUE SATUREE" DU PROFIL DU COURANT DESCENDANT.

!     FIRST VERTICAL LOOP (DOWNWARDS) INCLUDING THE SATURATED
!     ADIABATIC TYPE CALCULATION OF THE DOWNDRAFT PROFILE.

!     INITIALISATION DU PARAMETRE QUI SERVIRA EVENTUELLEMENT A EVITER
!     DES CALCULS INUTILES EN HAUT DE L'ATMOSPHERE ET DEBUT DE LA
!     BOUCLE.
!     INITIALIZATION OF THE PARAMETER THAT SHALL EVENTUALLY HELP
!     AVOIDING UNNECESSARY COMPUTATIONS IN THE UPPER PART OF THE
!     ATMOSPHERE AND START OF THE VERTICAL LOOP.

ITOP=KTDIA
DO JLEV=KTDIA+1,KLEV

!     ADIABATIQUE SATUREE AVEC ENTRAINEMENT.
!     SATURATED ADIABAT WITH ENTRAINMENT.

  DO JLON=KIDIA,KFDIA

!     ENTRAINEMENT.
!     ENTRAINEMENT.

! - TEMPORAIRE(S) 1D .

! ZTB        : "ZTN" A LA BASE DE LA COUCHE EN INCLUANT L'ENTRAINEMENT.
!            : "ZTN" AT THE BOTTOM OF THE SLAB WITH ENTRAINMENT "IN".
! ZQB        : "ZQN" A LA BASE DE LA COUCHE EN INCLUANT L'ENTRAINEMENT.
!            : "ZQN" AT THE BOTTOM OF THE SLAB WITH ENTRAINMENT "IN".

    ZENTR=PEPSI(JLON,JLEV-1)*PAPRSF(JLON,JLEV-1)/(PR(JLON,JLEV-1)&
     &*PT(JLON,JLEV-1))
    ZENTR=(ZENTR+YRPHY0%TENTRX)
    PSTACK (JLON, IPSTPT_ZMIX)=ZENTR*(PAPHIF(JLON,JLEV-1)-PAPHIF(JLON,JLEV))
    PSTACK (JLON, IPSTPT_ZMIX)=PSTACK (JLON, IPSTPT_ZMIX)/(1.0_JPRB+&
    &  PSTACK (JLON, IPSTPT_ZMIX))
    PSTACK (JLON, IPSTPT_ZTB)=PSTACK (JLON, IPSTPT_ZTN)+&
    &  PSTACK (JLON, IPSTPT_ZMIX)*(PT(JLON,JLEV-1)-PSTACK (JLON, IPSTPT_ZTN))
    PSTACK (JLON, IPSTPT_ZQB)=PSTACK (JLON, IPSTPT_ZQN)+&
    &  PSTACK (JLON, IPSTPT_ZMIX)*(PQ(JLON,JLEV-1)-PSTACK (JLON, IPSTPT_ZQN))

!     ENTRAINEMENT DE LA VITESSE HORIZONTALE:
!     NOUVELLE PARAMETRISATION (KERSHAW & GREGORY).
!     MOMENTUM ENTRAINMENT:
!     NEW PARAMETERIZATION (KERSHAW & GREGORY).

    PBET(JLON,JLEV)=PBET(JLON,JLEV-1)*(1.0_JPRB-PSTACK (JLON, IPSTPT_ZMIX))
    PUM(JLON,JLEV)=PUM(JLON,JLEV-1) +(PSTACK (JLON, IPSTPT_ZMIX)*(PU(JLON,JLEV-1   &
    & )-PUM(JLON,JLEV-1)) +YRPHY0%TDDGP*(PU(JLON,JLEV)-PU(JLON,JLEV-1))) /(1.0_JPRB&
    & -PBET(JLON,JLEV))
    PVM(JLON,JLEV)=PVM(JLON,JLEV-1) +(PSTACK (JLON, IPSTPT_ZMIX)*(PV(JLON,JLEV-1   &
    & )-PVM(JLON,JLEV-1)) +YRPHY0%TDDGP*(PV(JLON,JLEV)-PV(JLON,JLEV-1))) /(1.0_JPRB&
    & -PBET(JLON,JLEV))

    DO JTRA=1,KTRA
      PTRAM(JLON,JLEV,JTRA)=PTRAM(JLON,JLEV-1,JTRA) +(PSTACK (JLON, IPSTPT_ZMIX)&
      & *(PTRA(JLON,JLEV-1,JTRA)-PTRAM(JLON,JLEV-1,JTRA)) +YRPHY0%TDDGP*(PTRA(  &
      & JLON,JLEV,JTRA)-PTRA(JLON,JLEV-1,JTRA))) /(1.0_JPRB-PBET(JLON,JLEV))
    ENDDO

!     COEFFICIENTS DE L'HYDROSTATIQUE DU PROFIL COURANT DESCENDANT.
!     HYDROSTATIC COEFFICIENTS FOR THE DOWNDRAFT PROFILE.

! - TEMPORAIRE(S) 1D .

! ZRBB       : POIDS DE "ZTB" DANS L'EPAISSEUR EN GEOPOTENTIEL.
!            : WEIGHT OF "ZTB" IN THE GEOPOTENTIAL THICKNESS.
! ZRBH       : POIDS DE "ZTN" A "ZQN=ZQB" DANS LA MEME EPAISSEUR.
!            : WEIGHT OF "ZTN" AT "ZQN=ZQB" IN THE SAME THICKNESS.
! ZRVH       : POUR LA CORRECTION DE VARIATION DE Q A ZRBH.
!            : FOR THE Q DEPENDENT CORRECTION OF ZRBH.

    PSTACK (JLON, IPSTPT_ZRBB)=-PALPH(JLON,JLEV-1)*(RD+ZRVMD*&
    &  PSTACK (JLON, IPSTPT_ZQB))
    PSTACK (JLON, IPSTPT_ZRBH)=(-PLNPR(JLON,JLEV)+PALPH(JLON,JLEV))*(RD+ZRVMD*&
    &  PSTACK (JLON, IPSTPT_ZQB))
    PSTACK (JLON, IPSTPT_ZRVH)=(-PLNPR(JLON,JLEV)+PALPH(JLON,JLEV))*RV

!     DANS LE CALCUL DE L'ADIABATIQUE SATUREE GCVADS PERMET UNE
!     TRANSITION CONTINUE ENTRE LE TRAITEMENT EQUIPRESSION ET
!     EQUIGEOPOTENTIEL.

!     TO COMPUTE ADIABATIC PROFILE GCVADS ALLOWS TO GO
!     CONTINUOUSLY FROM EQUIPRESSURE TO EQUIGEOPOTENTIAL APPROACH.

    PSTACK (JLON, IPSTPT_ZRBB)=(1.0_JPRB-YRPHY0%GCVADS)*                        &
    &  PSTACK (JLON, IPSTPT_ZRBB)+YRPHY0%GCVADS*(PAPHIF(JLON,JLEV) -PAPHIF(JLON,&
    &  JLEV-1))/PSTACK (JLON, IPSTPT_ZTB)  
    PSTACK (JLON, IPSTPT_ZRBH)=(1.0_JPRB-YRPHY0%GCVADS)*&
    &  PSTACK (JLON, IPSTPT_ZRBH)
    PSTACK (JLON, IPSTPT_ZRVH)=(1.0_JPRB-YRPHY0%GCVADS)*&
    &  PSTACK (JLON, IPSTPT_ZRVH)

!     CALCULS DEPENDANT DE L'OPTION NEIGE.
!     SNOW OPTION DEPENDENT CALCULATIONS.

    IF (YRPHY%LNEIGE) THEN
      ZDELTA=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-PSTACK (JLON, IPSTPT_ZTB)))
    ELSE
      ZDELTA=0.0_JPRB
    ENDIF

!     APPEL A LA FONCTION DEFINIE POUR LE CALCUL DE LA PSEUDO CHALEUR
!     LATENTE A LA BASE (PARMI TROIS PARAMETRES CARACTERISTIQUES).
!     CALL TO THE DEFINED FUNCTION FOR THE COMPUTATION OF THE PSEUDO
!     LATENT HEAT AT THE BOTTOM (AMONG THREE CHARACTERISTIC PARAMETERS).

! - TEMPORAIRE(S) 1D .

! ZLH        : VALEUR COURANTE DU PSEUDO LH DURANT LA BOUCLE DE NEWTON.
!            : RUNNING VALUE OF THE PSEUDO LH DURING NEWTON'S LOOP.
! ZCP        : COMME PZLH MAIS POUR LA CHALEUR MASSIQUE CP.
!            : AS PZLH BUT FOR THE SPECIFIC HEAT CP.

    PSTACK (JLON, IPSTPT_ZLH)= FOLH (PSTACK (JLON, IPSTPT_ZTB),ZDELTA)+&
    &  PSTACK (JLON, IPSTPT_ZRVH)*PSTACK (JLON, IPSTPT_ZTB)
    PSTACK (JLON, IPSTPT_ZCP)=RCPD+ZCPVMD*PSTACK (JLON, IPSTPT_ZQB)+&
    &  PSTACK (JLON, IPSTPT_ZRBH)
    ZDCP=PSTACK (JLON, IPSTPT_ZRVH)+ZCPVMW+ZDELTA*(ZCPVMS-ZCPVMW)

!     CHANGEMENT DE NIVEAU POUR OBTENIR LA PREMIERE EBAUCHE, POINT DE
!     DEPART DE LA BOUCLE DE NEWTON.
!     CHANGE OF LEVEL TO OBTAIN A FIRST GUESS AS STARTING POINT FOR THE
!     NEWTON LOOP.

    ZDELT=PT(JLON,JLEV)-PT(JLON,JLEV-1)
    PSTACK (JLON, IPSTPT_ZLH)=PSTACK (JLON, IPSTPT_ZLH)+ZDCP*ZDELT
    ZDELQ=-((PSTACK (JLON, IPSTPT_ZRBB)+PSTACK (JLON, IPSTPT_ZRBH))*&
    &  PSTACK (JLON, IPSTPT_ZTB)+PSTACK (JLON, IPSTPT_ZCP)*ZDELT)/  &
    &  PSTACK (JLON, IPSTPT_ZLH)
    PSTACK (JLON, IPSTPT_ZCP)=PSTACK (JLON, IPSTPT_ZCP)+ZDCP*ZDELQ
    PSTACK (JLON, IPSTPT_ZTN)=PSTACK (JLON, IPSTPT_ZTB)+ZDELT
    PSTACK (JLON, IPSTPT_ZQN)=PSTACK (JLON, IPSTPT_ZQB)+ZDELQ
  ENDDO

!     BOUCLE DE NEWTON.
!     NEWTON'S LOOP.

  DO JIT=1,YRPHY%NBITER
    DO JLON=KIDIA,KFDIA

!     CALCULS DEPENDANT DE L'OPTION NEIGE.
!     SNOW OPTION DEPENDENT COMPUTATIONS.

      IF (YRPHY%LNEIGE) THEN
        ZDELTA=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-PSTACK (JLON, IPSTPT_ZTN)))
      ELSE
        ZDELTA=0.0_JPRB
      ENDIF

!     CALCULS DE SATURATION UTILISANT LES FONCTIONS DEFINIES.
!     SATURATION CALCULATIONS USING DEFINED FUNCTIONS.

      ZEW= FOEW (PSTACK (JLON, IPSTPT_ZTN),ZDELTA)
      ZESP=ZEW/PAPRSF(JLON,JLEV)
      ZQW= FOQS (ZESP)
      ZDQW= FODQS (ZQW,ZESP, FODLEW (PSTACK (JLON, IPSTPT_ZTN),ZDELTA))

!     INCREMENTATIONS.
!     INCREMENTATIONS.

      ZDCP=PSTACK (JLON, IPSTPT_ZRVH)+ZCPVMW+ZDELTA*(ZCPVMS-ZCPVMW)
      ZDELQ=(ZQW-PSTACK (JLON, IPSTPT_ZQN))*PSTACK (JLON, IPSTPT_ZCP)/(&
      &  PSTACK (JLON, IPSTPT_ZCP)+PSTACK (JLON, IPSTPT_ZLH)*ZDQW)
      PSTACK (JLON, IPSTPT_ZCP)=PSTACK (JLON, IPSTPT_ZCP)+ZDCP*ZDELQ
      ZDELT=-ZDELQ*PSTACK (JLON, IPSTPT_ZLH)/PSTACK (JLON, IPSTPT_ZCP)
      PSTACK (JLON, IPSTPT_ZLH)=PSTACK (JLON, IPSTPT_ZLH)+ZDCP*ZDELT
      PSTACK (JLON, IPSTPT_ZQN)=PSTACK (JLON, IPSTPT_ZQN)+ZDELQ
      PSTACK (JLON, IPSTPT_ZTN)=PSTACK (JLON, IPSTPT_ZTN)+ZDELT
    ENDDO
  ENDDO

!     ADIABATIQUE SECHE.

  DO JLON=KIDIA,KFDIA
    ZCPS=RCPD*(1.0_JPRB-PSTACK (JLON, IPSTPT_ZQB))+RCPV*&
    &  PSTACK (JLON, IPSTPT_ZQB)
    ZTNSEC=(ZCPS-PSTACK (JLON, IPSTPT_ZRBB))*PSTACK (JLON, IPSTPT_ZTB)/(ZCPS+&
    &  PSTACK (JLON, IPSTPT_ZRBH))
    ICDN=MAX(0,-ISIGN(1,-KNCDN(JLON,JLEV)))
    PSTACK (JLON, IPSTPT_ZTN)=ICDN*PSTACK (JLON, IPSTPT_ZTN)+(1-ICDN)*ZTNSEC
    PSTACK (JLON, IPSTPT_ZQN)=ICDN*PSTACK (JLON, IPSTPT_ZQN)+(1-ICDN)*&
    &  PSTACK (JLON, IPSTPT_ZQB)
    PSTACK (JLON, IPSTPT_ZBCC_CD_HALF+JLEV-(0))=MAX(0._JPRB,                    &
    &  PSTACK (JLON, IPSTPT_ZQN)-PSTACK (JLON, IPSTPT_ZQB)) /(PAPRSF(JLON,JLEV-1&
    &  ) -PAPRSF(JLON,JLEV))
    PBCC_CD_FULL(JLON,JLEV-1)=0.5_JPRB*(           &
    &  PSTACK (JLON, IPSTPT_ZBCC_CD_HALF+JLEV-(0))+&
    &  PSTACK (JLON, IPSTPT_ZBCC_CD_HALF+JLEV-1-(0)))
  ENDDO

!     TEST DE STABILITE, RECTIFICATION EVENTUELLE DU NIVEAU SUPERIEUR,
!     CALCUL DU PARAMETRE DE PROFIL DU FLUX DE MASSE ET DES DEUX
!     INTEGRALES INTERVENANT DANS LA CONDITION DE FERMETURE "TYPE KUO".
!     STABILITY TEST, OCCASIONAL CORRECTION OF THE BOTTOM LAYER,
!     COMPUTATION OF THE MASS FLUX PROFILE PARAMETER AND OF THE TWO
!     INTEGRALS CONNECTED WITH THE "KUO-TYPE" CLOSURE.

  DO JLON=KIDIA,KFDIA
!     TEST DE DECLENCHEMENT.
!     CALCUL DE L'INTEGRALE DE LA FLOTTABILITE
!     DURANT LE PAS DE TEMPS ET TEST DE SON SIGNE.

    IBDC=MAX(0,-ISIGN(1,-KSTACK (JLON, IKSTPT_INIVBDC+JLEV-1-(1))))
    ZTVN=PSTACK (JLON, IPSTPT_ZTN)*(1.0_JPRB+RETV*PSTACK (JLON, IPSTPT_ZQN))
    ZTVE=PT(JLON,JLEV)*(1.0_JPRB+RETV*PQ(JLON,JLEV))
    ZFLO=(ZTVE-ZTVN)/ZTVE/ZTVE
    ZFLOI=RG*RG*PAPRSF(JLON,JLEV)/(RD*ZGAMA)
    ZFLO=ZFLOI*ZFLO

    ZDELPF=PDELP(JLON,JLEV)
    ZENTR=PEPSI(JLON,JLEV-1)
    ZENTRT=YRPHY0%TENTRX*PR(JLON,JLEV-1)*PT(JLON,JLEV-1)/PAPRSF(JLON,JLEV-1)
    ZENTR=ZENTR+ZENTRT

    ZVVERDEN=2.0_JPRB*(0.25_JPRB*(ZKD+ZENTR)+0.5_JPRB/ZDELPF)
    ZB=0.5_JPRB*(ZKD+ZENTR)*PSTACK (JLON, IPSTPT_ZVVER+JLEV-1-(0))
    ZDELTAP=ZB**2 +2.0_JPRB*ZVVERDEN*((ZFLO+(0.5_JPRB/ZDELPF-0.25_JPRB*(ZKD+&
    & ZENTR)) *PSTACK (JLON, IPSTPT_ZVVER+JLEV-1-(0))**2))
    ISDELTAP=NINT(MAX(0.0_JPRB,-SIGN(1.0_JPRB,-ZDELTAP)))
    ZDELTAP=ZDELTAP*ISDELTAP
    ZVVERN=(ZB+SQRT(ZDELTAP))/ZVVERDEN

    PSTACK (JLON, IPSTPT_ZVVER+JLEV-(0))=ZVVERN
    ZVVERF=0.5_JPRB*(PSTACK (JLON, IPSTPT_ZVVER+JLEV-(0))+&
    &  PSTACK (JLON, IPSTPT_ZVVER+JLEV-1-(0)))
    ZDELPF=PAPRSF(JLON,JLEV)-PAPRSF(JLON,JLEV-1)
    PSTACK (JLON, IPSTPT_ZDWNDP+JLEV-1-(0))=(ZVVERF-&
    &  PSTACK (JLON, IPSTPT_ZVVERFM1))/ZDELPF

    PSTACK (JLON, IPSTPT_ZVVERFM1)=ZVVERF

!     ENTRAINEMENT-DETRAINEMENT ORGANISES.
    IVVER=NINT(MAX(0.0_JPRB,-SIGN(1.0_JPRB,ZVVERF+0.0_JPRB)))
    IDOMDP=NINT(MAX(0.0_JPRB,SIGN(1.0_JPRB,&
    & PSTACK (JLON, IPSTPT_ZDWNDP+JLEV-1-(0)))))
    ZENTRO=IVVER/MAX(ZEPSO,ZVVERF)*PSTACK (JLON, IPSTPT_ZDWNDP+JLEV-1-(0))
    PEPSI(JLON,JLEV)=IDOMDP*ZENTRO
    PDELA(JLON,JLEV)=-(1-IDOMDP)*ZENTRO

    IKUO1=MAX(NINT(MAX(0.0_JPRB,-SIGN(1.0_JPRB,-                               &
    &  PSTACK (JLON, IPSTPT_ZVVER+JLEV-1-(0))+0.0_JPRB))) ,NINT(MAX(0.0_JPRB,- &
    & SIGN(1.0_JPRB,-PSTACK (JLON, IPSTPT_ZVVER+JLEV-(0))+0.0_JPRB))))
    ILEVBDC=IBDC*KSTACK (JLON, IKSTPT_INIVBDC+JLEV-1-(1))+(1-IBDC)*(KLEV+1)
    IKUO2=MAX(0,ISIGN(1,KLEVTEN(JLON)-ILEVBDC))
    KNLABD(JLON,JLEV)=IKUO1*IKUO2*IBDC

!     VALEURS FINALES DE DETRAINEMENT AVEC APPEL A LA FONCTION DEFINIE
!     POUR REVENIR A LA VERITABLE CHALEUR LATENTE.
!     FINAL DETRAINMENT VALUES WITH A CALL TO THE DEFINED FUNCTION TO
!     COME BACK TO THE TRUE LATENT HEAT.

    ZTD=PT(JLON,JLEV)*(1.0_JPRB+(PQLIC(JLON,JLEV)-RETV*(PQSAT(JLON,JLEV)&
     &-PQ(JLON,JLEV)))/(1.0_JPRB+RETV*PLH(JLON,JLEV)*PQSAT(JLON,JLEV)&
     &/(RV*PT(JLON,JLEV))))
    IF (YRPHY%LNEIGE) THEN
      ZDELTA=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTD))
    ELSE
      ZDELTA=0.0_JPRB
    ENDIF
    ZEW=FOEW (ZTD,ZDELTA)
    ZESP=ZEW/PAPRSF(JLON,JLEV)
    ZQW=FOQS(ZESP)
    ICDN=MAX(0,-ISIGN(1,-KNCDN(JLON,JLEV)))
    ZTD=ICDN*(YRPHY0%GCVADET*ZTD+(1.0_JPRB-YRPHY0%GCVADET)*&
    &  PSTACK (JLON, IPSTPT_ZTN))+(1-ICDN)*PSTACK (JLON, IPSTPT_ZTN)
    ZQW=ICDN*(YRPHY0%GCVADET*ZQW+(1.0_JPRB-YRPHY0%GCVADET)*&
    &  PSTACK (JLON, IPSTPT_ZQN))+(1-ICDN)*PSTACK (JLON, IPSTPT_ZQN)
    IF (YRPHY%LNEIGE) THEN
      ZDELTA=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTD))
    ELSE
      ZDELTA=0.0_JPRB
    ENDIF

    PQDN(JLON,JLEV)=ZQW
    PSDN(JLON,JLEV)=(RCPD+ZCPVMD*ZQW)*ZTD+PAPHIF(JLON,JLEV)
    PQN2(JLON,JLEV)=PSTACK (JLON, IPSTPT_ZQN)
    PSN2(JLON,JLEV)=(RCPD+ZCPVMD*PSTACK (JLON, IPSTPT_ZQN))*&
    &  PSTACK (JLON, IPSTPT_ZTN)+PAPHIF(JLON,JLEV)
    PTN2(JLON,JLEV)=PSTACK (JLON, IPSTPT_ZTN)

!     EN CAS DE COURANT DESCENDANT PLUS "CHAUD" QUE L'ENVIRONEMENT ON
!     REEGALISE  TEMPERATURE ET HUMIDITE SPECIFIQUES DU COURANT
!     DESCENDANT A CELLES DU THERMOMETRE MOUILLE DE L'ENVIRONEMENT.
!     IN CASE OF A "WARMER" DOWNDRAFT THAN THE ENVIRONMENT ONE SETS
!     BACK TEMPERATURE AND SPECIFIC HUMIDITY OF THE DOWNDRAFT TO
!     THAT OF THE WET BULB CHARACTERISTICS OF THE ENVIRONMENT.

    INUA=KNLABD(JLON,JLEV)

    ICDN=MAX(0,-ISIGN(1,-KNCDN(JLON,JLEV-1)))
    KSTACK (JLON, IKSTPT_INIVBDC+JLEV-(1))=MIN( &
    & KSTACK (JLON, IKSTPT_INIVBDC+JLEV-1-(1)),JLEV)*KNLAB(JLON,JLEV-1)*ICDN
    KSTACK (JLON, IKSTPT_INIVBDC+JLEV-(1))=INUA*&
    &  KSTACK (JLON, IKSTPT_INIVBDC+JLEV-(1))+(1-INUA)*JLEV
    KSTACK (JLON, IKSTPT_INIVBDC+JLEV-1-(1))=INUA*&
    &  KSTACK (JLON, IKSTPT_INIVBDC+JLEV-1-(1))

    PSTACK (JLON, IPSTPT_ZVVER+JLEV-(0))=INUA*&
    &  PSTACK (JLON, IPSTPT_ZVVER+JLEV-(0))

    PSTACK (JLON, IPSTPT_ZTN)=INUA*PSTACK (JLON, IPSTPT_ZTN)+(1-INUA)*PTW(JLON, &
    & JLEV)
    PSTACK (JLON, IPSTPT_ZQN)=INUA*PSTACK (JLON, IPSTPT_ZQN)+(1-INUA)*PQW(JLON, &
    & JLEV)

!     PROFIL DE FLUX DE MASSE.
!     MASS FLUX PROFILE.

    PFORM(JLON,JLEV-1)=KNLABD(JLON,JLEV)*PSTACK (JLON, IPSTPT_ZVVER+JLEV-1-(0))

    ZTEST=MAX(MAX(0.0_JPRB,-SIGN(1.0_JPRB,-PFORM(JLON,JLEV-1)+0.0_JPRB)) ,MAX(0.0_JPRB,&
    & -SIGN(1.0_JPRB,PSTACK (JLON, IPSTPT_ZFMOD+JLEV-2-(0))-1.0_JPRB+0.0_JPRB)))
    PSTACK (JLON, IPSTPT_ZFMOD+JLEV-1-(0))=(1.0_JPRB-ZTEST)+ZTEST*           &
    &  PSTACK (JLON, IPSTPT_ZFMOD+JLEV-2-(0)) *((PAPRS(JLON,KLEV)-PAPRS(JLON,&
    & JLEV -1)) /(PAPRS(JLON,KLEV)-PAPRS(JLON,JLEV-2))) **YRPHY0%GDDSDE
    PFORM(JLON,JLEV-1)=PFORM(JLON,JLEV-1)*PSTACK (JLON, IPSTPT_ZFMOD+JLEV-1-(0))

    PSTACK (JLON, IPSTPT_ZFMOD+JLEV-1-(0))=INUA*&
    &  PSTACK (JLON, IPSTPT_ZFMOD+JLEV-1-(0))+(1-INUA)*1.0_JPRB

  ENDDO

!     VERIFICATION DE LA PRESENCE D'AU MOINS UN COURANT DESCENDANT LE
!     LONG DU VECTEUR "JLON" ET MODIFICATION SI NECESSAIRE DE ITOP.
!     VERIFICATION OF THE PRESENCE OF AT LEAST ONE DOWNDRAFT ALONG THE
!     "JLON" VECTOR AND MODIFICATION OF ITOP IF NECESSARY.

  ISUM=0
  DO JLON=KIDIA,KFDIA
    ISUM=ISUM+KNLABD(JLON,JLEV)
  ENDDO

  IF ((ISUM == 0).AND.(ITOP == JLEV-1)) THEN
    ITOP=JLEV
  ENDIF

ENDDO


END SUBROUTINE ACMTDDD
