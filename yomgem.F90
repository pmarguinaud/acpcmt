MODULE YOMGEM

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

TYPE :: TGEM

REAL(KIND=JPRB) :: RDELXN
END TYPE TGEM

TYPE(TGEM) :: YRGEM 

!     ------------------------------------------------------------------

END MODULE YOMGEM
