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

!$acc declare create(YRGEM)
END MODULE YOMGEM
