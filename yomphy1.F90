MODULE YOMPHY1


#include "create.h"

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

TYPE TPHY1
REAL(KIND=JPRB) :: TMERGL

END TYPE TPHY1

TYPE(TPHY1) :: YRPHY1 

create (YRPHY1)
!     ------------------------------------------------------------------
END MODULE YOMPHY1
