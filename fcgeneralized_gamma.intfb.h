INTERFACE
!$acc routine (FCGENERALIZED_GAMMA) seq
SUBROUTINE FCGENERALIZED_GAMMA (PX, PY)  

USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMCST  ,ONLY : RPI

REAL(KIND=JPRB), INTENT(IN)                     :: PX
REAL(KIND=JPRB), INTENT(OUT)                    :: PY

END SUBROUTINE FCGENERALIZED_GAMMA

END INTERFACE
