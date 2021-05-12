MODULE YOMPHY2


#include "create.h"

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

TYPE TPHY2
REAL(KIND=JPRB) :: TSPHY
END TYPE TPHY2

TYPE(TPHY2) :: YRPHY2 

create (YRPHY2)
!     ------------------------------------------------------------------
END MODULE YOMPHY2
