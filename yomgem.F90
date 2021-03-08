MODULE YOMGEM


#include "create.h"

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

TYPE :: TGEM

REAL(KIND=JPRB) :: RDELXN
END TYPE TGEM

TYPE(TGEM) :: YRGEM 

create (YRGEM)
!     ------------------------------------------------------------------

END MODULE YOMGEM
