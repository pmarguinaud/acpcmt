#include "attr_mod.h"
MODULE YOMPHY1

USE PARKIND1  ,ONLY : JPIM     ,JPRB

IMPLICIT NONE

SAVE

TYPE TPHY1
REAL(KIND=JPRB) :: TMERGL

END TYPE TPHY1

TYPE(TPHY1) ATTR_MOD :: YRPHY1 

!     ------------------------------------------------------------------
END MODULE YOMPHY1
