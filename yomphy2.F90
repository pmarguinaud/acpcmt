#include "attr_mod.h"
MODULE YOMPHY2

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

TYPE TPHY2
REAL(KIND=JPRB) :: TSPHY
END TYPE TPHY2

TYPE(TPHY2) ATTR_MOD :: YRPHY2 

!     ------------------------------------------------------------------
END MODULE YOMPHY2
