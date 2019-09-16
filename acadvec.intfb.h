INTERFACE
#include "attr.h"
SUBROUTINE ACADVEC (KIDIA, KFDIA, KLON, KTDIA, KLEV, PDELP, PAPHI, PVAR, PVARW, PFVAR, &
                  & KSTACK, PSTACK, KPSTSZ, KKSTSZ, KPSTPT, KKSTPT)
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMPHY0   , ONLY : YRPHY0
USE YOMPHY2   , ONLY : YRPHY2
USE YOMCST    , ONLY : RG
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)     :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)     :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)     :: KLON
INTEGER(KIND=JPIM), INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)     :: KLEV
REAL(KIND=JPRB)   ,INTENT(IN)     :: PDELP(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)     :: PAPHI(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)     :: PVAR (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)     :: PVARW(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)    :: PFVAR(KLON,0:KLEV)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSTACK (KLON, KKSTSZ)
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KKSTSZ, KPSTPT, KKSTPT
END SUBROUTINE ACADVEC
END INTERFACE
