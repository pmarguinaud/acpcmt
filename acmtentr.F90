SUBROUTINE ACMTENTR ( KIDIA,KFDIA,KLON,KTDIA,KLEV,&
!-----------------------------------------------------------------------
! - INPUT  2D .
&PDELP,PPSIC,PPSIR,PUDAL,PDDAL,PENTR_U,PENTR_D,PDETR_U,PDETR_D,&
! - OUTPUT 2D .
&PFCED, KSTACK, PSTACK, KPSTSZ, KKSTSZ, KPSTPT, KKSTPT)

!**** *ACMTENTR * - FLUX DUE TO ENTRAINMENT AND DETRAINMENT PROCESSES.

!     Sujet.
!     ------

!**   Interface.
!     ----------
!        *CALL* *ACMTENTR*
!-----------------------------------------------------------------------

! - PHYSICS DIMENSIONNING PARAMETERS

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT..
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
! KTDIA      : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL).
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".


! - 2D (1:KLEV) .

! PDELP      : PRESSURE DIFFERENCE OVER THE LAYER

! - 2D (0:KLEV) .

! PFCED      : FLUX OF PSI DUE TO THE SUM OF ENTRAINEMENT AND DETRAINMENT PROCESSES.

!-----------------------------------------------------------------------

!     Method.
!     --------

!     Author.
!     -------
!        2011-04-19, J.M. Piriou.

!     Modifications.
!     --------------

!-----------------------------------------------------------------------

USE PARKIND1, ONLY: JPIM, JPRB

USE YOMCST   , ONLY : RG
USE YOMPHY2  , ONLY : YRPHY2

IMPLICIT NONE

INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
INTEGER(KIND=JPIM), INTENT(IN) :: KLON
INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA

REAL(KIND=JPRB), INTENT(IN)    :: PDELP(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PPSIC(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PPSIR(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PUDAL(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PDDAL(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PENTR_U(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PENTR_D(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PDETR_U(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PDETR_D(KLON,KLEV)

REAL(KIND=JPRB), INTENT(OUT)   :: PFCED(KLON,0:KLEV)
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSTACK (KLON, KKSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KKSTSZ, KPSTPT, KKSTPT


REAL(KIND=JPRB) :: ZINC,ZMINVALUE_ZINC,ZMAXVALUE_ZINC
INTEGER(KIND=JPIM) :: JLEV,JLON
INTEGER(KIND=JPIM) :: IPSTPT, IKSTPT




!-------------------------------------------------
! INITIALIZATION.
!-------------------------------------------------


IPSTPT = KPSTPT

IKSTPT = KKSTPT

IF (IPSTPT > KPSTSZ) CALL ABOR1 ('IPSTPT > KPSTSZ')

IF (IKSTPT > KKSTSZ) CALL ABOR1 ('IKSTPT > KKSTSZ')

DO JLEV = 0, KLEV
DO JLON = KIDIA, KFDIA
  PFCED(JLON,JLEV)=0._JPRB
ENDDO
ENDDO

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    
    ! PSIC INCREMENT.
    ZINC=((PENTR_U(JLON,JLEV)+PENTR_D(JLON,JLEV)) &
      & *PPSIR(JLON,JLEV)/MAX(1.E-4_JPRB,1._JPRB-PUDAL(JLON,JLEV)-PDDAL(JLON,JLEV)) &
      & -(PDETR_U(JLON,JLEV)+PDETR_D(JLON,JLEV)) &
      & *PPSIC(JLON,JLEV)/MAX(1.E-4_JPRB,PUDAL(JLON,JLEV)+PDDAL(JLON,JLEV)))*YRPHY2%TSPHY

    ! PROTECTION IN CASE OF LARGE TIME STEPS:
    ! TO AVOID INSTABILITIES
    ! THIS ZINC INCREMENT HAS TO BE SUCH THAT THE FINAL PSIC VALUE
    ! IN THE END OF TIME STEP WILL BE BETWEEN 0 AND (PSIC+PSIR).
    ! THEREFORE, THE INCREMENT HAS TO BE BETWEEN -PSIC AND PSIR.
    ZMINVALUE_ZINC=-PPSIC(JLON,JLEV)
    ZMAXVALUE_ZINC=PPSIR(JLON,JLEV)
    ZINC=MAX(ZMINVALUE_ZINC,MIN(ZMAXVALUE_ZINC,ZINC))
    PFCED(JLON,JLEV)=PFCED(JLON,JLEV-1)-PDELP(JLON,JLEV)/RG*ZINC/YRPHY2%TSPHY
  ENDDO
ENDDO


END SUBROUTINE ACMTENTR
