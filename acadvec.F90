SUBROUTINE ACADVEC (KIDIA, KFDIA, KGPBLKS,KLON, KTDIA, KLEV, PDELP, PAPHI, PVAR, PVARW, PFVAR)

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
INTEGER(KIND=JPIM),INTENT(IN)     :: KGPBLKS,KLON 
INTEGER(KIND=JPIM), INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)     :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)     :: PVAR (KLON,KLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)     :: PVARW(KLON,KLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)     :: PDELP(KLON,KLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)     :: PAPHI(KLON,0:KLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(OUT)    :: PFVAR(KLON,0:KLEV,KGPBLKS) 

REAL(KIND=JPRB) :: ZP1, ZP2(KLON,KGPBLKS), ZDZV(KLON,KGPBLKS), ZDZL(KLON,KGPBLKS), ZEPS

REAL(KIND=JPRB) :: ZFUP(KLON,0:KLEV,KGPBLKS), ZFDN(KLON,0:KLEV,KGPBLKS), ZALTIH(KLON,0:KLEV,KGPBLKS)       
REAL(KIND=JPRB) :: ZDPSG(KLON,KLEV,KGPBLKS)


INTEGER(KIND=JPIM) :: JLEV, JBLK,JLON
REAL(KIND=JPRB) :: ZLN_NEGLIG,ZUSCFL

!$acc data present (PAPHI, PDELP, PFVAR, PVAR, PVARW)
!$acc data create (ZALTIH, ZDPSG, ZDZL, ZDZV, ZFDN, ZFUP, ZP2)
! --------------------------------------------------------

!     CHECK RELIABILITY OF INPUT ARGUMENTS.



! --------------------------------------------------------

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLEV = 0, KLEV
DO JLON = KIDIA, KFDIA
  
    ZFDN(JLON,JLEV,JBLK) = 0.0_JPRB
    ZFUP(JLON,JLEV,JBLK) = 0.0_JPRB
  
ENDDO
ENDDO
ENDDO
!$acc end kernels


!- - - - - - - - - - - - - - -
IF (YRPHY2%TSPHY > 0.0_JPRB) THEN
!- - - - - - - - - - - - - - -

! Some intialisation
  !$acc kernels
  DO JBLK = 1, KGPBLKS
  DO JLEV = KTDIA, KLEV
  DO JLON = KIDIA, KFDIA
    
      ZDPSG(JLON,JLEV,JBLK) = PDELP(JLON,JLEV,JBLK) / RG
            
  ENDDO
  ENDDO
  ENDDO
  !$acc end kernels
  
  !$acc kernels
  DO JBLK = 1, KGPBLKS
  DO JLEV = KTDIA-1, KLEV
  DO JLON = KIDIA, KFDIA
    
      ZALTIH(JLON,JLEV,JBLK) = PAPHI(JLON,JLEV,JBLK) / RG 
    
  ENDDO
  ENDDO
  ENDDO
  !$acc end kernels

  ZEPS = 1.E-20_JPRB
  ZLN_NEGLIG=LOG(1.0E-2_JPRB)


! First loop : From top to bottom

  !$acc kernels
  DO JBLK = 1, KGPBLKS
  DO JLEV = KTDIA, KLEV-1
  DO JLON = KIDIA, KFDIA
    
      ZDZV(JLON,JBLK) = MAX(0._JPRB,-PVARW(JLON,JLEV,JBLK))*YRPHY2%TSPHY
      ZDZL(JLON,JBLK) = ZALTIH(JLON,JLEV-1,JBLK) - ZALTIH(JLON,JLEV,JBLK)
    
    IF(YRPHY0%LADVLIM) THEN
      
        ZUSCFL=ZDZL(JLON,JBLK)/MAX(ZEPS,ZDZV(JLON,JBLK))
        ZP2(JLON,JBLK)  = MAX(0._JPRB,MIN(EXP(ZLN_NEGLIG*ZUSCFL),1._JPRB - ZUSCFL))
      
    ELSE
      
        ZP2(JLON,JBLK)  = MAX(0._JPRB,1._JPRB - ZDZL(JLON,JBLK)/MAX(ZEPS,ZDZV(JLON,JBLK)))
      
    ENDIF
    
      ZP1  = MIN(1._JPRB , ZDZV(JLON,JBLK)/ZDZL(JLON,JBLK))
      ZFDN(JLON,JLEV,JBLK) = ZP1*ZDPSG(JLON,JLEV,JBLK)*PVAR(JLON,JLEV,JBLK)/YRPHY2%TSPHY &
        &+ ZP2(JLON,JBLK)*ZFDN(JLON,JLEV-1,JBLK)
    
  ENDDO
  ENDDO
  ENDDO
  !$acc end kernels
    

! Second loop : From bottom to top

  !$acc kernels
  DO JBLK = 1, KGPBLKS
  DO JLEV = KLEV, KTDIA+1, -1
  DO JLON = KIDIA, KFDIA
    
      ZDZV(JLON,JBLK) = MAX(0._JPRB,PVARW(JLON,JLEV,JBLK))*YRPHY2%TSPHY
      ZDZL(JLON,JBLK) = ZALTIH(JLON,JLEV-1,JBLK) - ZALTIH(JLON,JLEV,JBLK)
    
    IF(YRPHY0%LADVLIM) THEN
      
        ZUSCFL=ZDZL(JLON,JBLK)/MAX(ZEPS,ZDZV(JLON,JBLK))
        ZP2(JLON,JBLK)  = MAX(0._JPRB,MIN(EXP(ZLN_NEGLIG*ZUSCFL),1._JPRB - ZUSCFL))
      
    ELSE
      
        ZP2(JLON,JBLK)  = MAX(0._JPRB,1._JPRB - ZDZL(JLON,JBLK)/MAX(ZEPS,ZDZV(JLON,JBLK)))
      
    ENDIF
    
      ZP1 = MIN(1._JPRB , ZDZV(JLON,JBLK)/ZDZL(JLON,JBLK))
      ZFUP(JLON,JLEV-1,JBLK) = ZP1*ZDPSG(JLON,JLEV,JBLK)*PVAR(JLON,JLEV,JBLK)/YRPHY2%TSPHY &
        &+ ZP2(JLON,JBLK)*ZFUP(JLON,JLEV,JBLK)
    
  ENDDO
  ENDDO
  ENDDO
  !$acc end kernels
    


!- - - - - - - - - - - - - - - - - - - - - - -
ENDIF  ! End of test on TSPHY > 0.0_JPRB
!- - - - - - - - - - - - - - - - - - - - - - -

!  Final loop construction of net flux

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLEV = KTDIA, KLEV-1
DO JLON = KIDIA, KFDIA
  
    PFVAR(JLON,JLEV,JBLK) = ZFDN(JLON,JLEV,JBLK) - ZFUP(JLON,JLEV,JBLK) 
  
ENDDO
ENDDO
ENDDO
!$acc end kernels
   

! --------------------------------------------------------


!$acc end data
!$acc end data
END SUBROUTINE ACADVEC
