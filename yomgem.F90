#include "attr_mod.h"
MODULE YOMGEM

USE PARKIND1, ONLY : JPIM, JPRB

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

TYPE :: TGEM

REAL(KIND=JPRB) :: RDELXN
END TYPE TGEM

TYPE(TGEM) ATTR_MOD :: YRGEM 

!     ------------------------------------------------------------------

END MODULE YOMGEM
