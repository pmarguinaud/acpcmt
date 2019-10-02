MODULE YOMRIP

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

!     --------------------------------------------------------------------------------
!     Date and timestep related variables.
!     Values may be different for the different models run under the OOPS layer.
!     In particuliar, we find there all date and timestep variables updated in UPDTIM.
!     --------------------------------------------------------------------------------

TYPE :: TRIP
REAL(KIND=JPRB) :: RSTATI
END TYPE TRIP

!     --------------------------------------------------------------------------------

TYPE(TRIP) :: YRRIP 

!     --------------------------------------------------------------------------------

!$acc declare create(YRRIP)
END MODULE YOMRIP
