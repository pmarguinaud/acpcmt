!$acc routine (ACADVEC) seq
SUBROUTINE ACADVEC (KIDIA, KFDIA, KLON, KTDIA, KLEV, PDELP, PAPHI, PVAR, PVARW, PFVAR,KSTPT,KSTSZ,PSTACK)


#include "temp.h"

! ========================================================

!   Compute vertical flux of a variable with vertical speed PVARW

! ========================================================

!   Auteur: Yves Bouteloup, CNRM/GMAP

!   Date: 2010-12   

!     Modifications.
!     --------------
!         2012-06-22, J.M. Piriou: if LADVLIM=.TRUE. minimise p2 to the value such that 
!                     the transport scheme cannot transport beyond a distance of w*deltat, in a time-step.

! ========================================================

! ---------------
! INPUT VARIABLES
! ---------------

! KIDIA, : START OF HORIZONTAL LOOP 
! KFDIA  : END OF HORIZONTAL LOOP     
! KLON   : HORIZONTAL DIMENSION
! KTDIA : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL).
! KLEV  : VERTICAL DIMENSION
! PDELP  : LAYER THICKNESS IN PRESSURE UNITS.
! PAPHI  : GEOPOTENTIAL ON HALF-LEVELS.
! PVAR   : INPUT VARIABLE ON FULL LEVEL
! PVARW  : VERTICAL SPEED OF VARIABLE PVAR ON FULL LEVEL (positive from top to bottom)


! ---------------
! OUTPUT VARIABLE
! ---------------

! PFVAR  : FLUX OF VARIABLE PVAR ON HALF LEVEL (Positive from top to bottom)

! ========================================================

USE PARKIND1  ,ONLY : JPIM     ,JPRB

USE YOMPHY0   , ONLY : YRPHY0
USE YOMPHY2   , ONLY : YRPHY2
USE YOMCST    , ONLY : RG


IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)     :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)     :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)     :: KLON 
INTEGER(KIND=JPIM), INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)     :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)     :: PVAR (KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)     :: PVARW(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)     :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)     :: PAPHI(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)    :: PFVAR(KLON,0:KLEV) 

INTEGER(KIND=JPIM),INTENT(IN)     :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)     :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT)  :: PSTACK (KSTSZ)
REAL(KIND=JPRB) :: ZP1, ZEPS

REAL(KIND=JPRB) :: ZDZL
REAL(KIND=JPRB) :: ZDZV
REAL(KIND=JPRB) :: ZP2
       
temp (REAL(KIND=JPRB), ZFDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFUP, (KLON,0:KLEV))



INTEGER(KIND=JPIM) :: JLEV, JLON
REAL(KIND=JPRB) :: ZLN_NEGLIG,ZUSCFL

! --------------------------------------------------------

!     CHECK RELIABILITY OF INPUT ARGUMENTS.

#include "abor1.intfb.h"

! --------------------------------------------------------

init_stack ()

alloc (ZFUP)
alloc (ZFDN)


JLON = KIDIA

DO JLEV = 0, KLEV
    ZFDN(JLON,JLEV) = 0.0_JPRB
    ZFUP(JLON,JLEV) = 0.0_JPRB
ENDDO

!- - - - - - - - - - - - - - -
IF (YRPHY2%TSPHY > 0.0_JPRB) THEN
!- - - - - - - - - - - - - - -

! Some intialisation
  ZEPS = 1.E-20_JPRB
  ZLN_NEGLIG=LOG(1.0E-2_JPRB)

! First loop : From top to bottom
  DO JLEV = KTDIA, KLEV-1
    ZDZV = MAX(0._JPRB,-PVARW(JLON,JLEV))*YRPHY2%TSPHY
    ZDZL = (PAPHI(JLON,JLEV-1) - PAPHI(JLON,JLEV))/RG
    
    IF(YRPHY0%LADVLIM) THEN
      ZUSCFL=ZDZL/MAX(ZEPS,ZDZV)
      ZP2  = MAX(0._JPRB,MIN(EXP(ZLN_NEGLIG*ZUSCFL),1._JPRB - ZUSCFL))
    ELSE
      ZP2  = MAX(0._JPRB,1._JPRB - ZDZL/MAX(ZEPS,ZDZV))
    ENDIF
    ZP1  = MIN(1._JPRB , ZDZV/ZDZL)
    ZFDN(JLON,JLEV) = ZP1*PDELP(JLON,JLEV)/RG*PVAR(JLON,JLEV)/YRPHY2%TSPHY &
      &+ ZP2*ZFDN(JLON,JLEV-1)
  ENDDO    

! Second loop : From bottom to top
  DO JLEV = KLEV, KTDIA+1, -1
    ZDZV = MAX(0._JPRB,PVARW(JLON,JLEV))*YRPHY2%TSPHY
    ZDZL = (PAPHI(JLON,JLEV-1) - PAPHI(JLON,JLEV))/RG
    IF(YRPHY0%LADVLIM) THEN
        ZUSCFL=ZDZL/MAX(ZEPS,ZDZV)
        ZP2  = MAX(0._JPRB,MIN(EXP(ZLN_NEGLIG*ZUSCFL),1._JPRB - ZUSCFL))
    ELSE
        ZP2  = MAX(0._JPRB,1._JPRB - ZDZL/MAX(ZEPS,ZDZV))
    ENDIF
    ZP1 = MIN(1._JPRB , ZDZV/ZDZL)
    ZFUP(JLON,JLEV-1) = ZP1*PDELP(JLON,JLEV)/RG*PVAR(JLON,JLEV)/YRPHY2%TSPHY &
      &+ ZP2*ZFUP(JLON,JLEV)
  ENDDO    
!- - - - - - - - - - - - - - - - - - - - - - -
ENDIF  ! End of test on TSPHY > 0.0_JPRB
!- - - - - - - - - - - - - - - - - - - - - - -

!  Final loop construction of net flux
DO JLEV = KTDIA, KLEV-1
    PFVAR(JLON,JLEV) = ZFDN(JLON,JLEV) - ZFUP(JLON,JLEV) 
ENDDO   

! --------------------------------------------------------


END SUBROUTINE ACADVEC
