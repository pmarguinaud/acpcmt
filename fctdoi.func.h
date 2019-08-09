!*   ------------------------------------------------------------------
!
!     FONCTION DE LA PARTITION EAU/GLACE
!     ICE/LIQUID WATER PARTITION FUNCTION
!
REAL(KIND=JPRB) :: PTARG1, FONICE
!
FONICE ( PTARG1 ) = 1.0_JPRB - EXP ( - (RTT-MIN(RTT,PTARG1))**2 &
  & * (1.0_JPRB/(2.0_JPRB*(RDT*YRPHY0%RDTFAC)**2)) )
!*
!     -----------------------------------------------------------------
