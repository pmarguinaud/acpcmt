INTERFACE
SUBROUTINE ACMICRO( KIDIA, KFDIA, KLON, KTDIA, KLEV,&
 !-----------------------------------------------------------------------
 ! - INPUT -
 & PNEBST, PT  , PQL , PQI , PTS , PNEIJ , PLSM,&
 ! - OUTPUT -
 & PAUTOL, PAUTOI, KSTACK, PSTACK, KPSTSZ, KKSTSZ, KPSTPT, KKSTPT )
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMPHY0   , ONLY : YRPHY0
USE YOMPHY1   , ONLY : YRPHY1
USE YOMPHY2   , ONLY : YRPHY2
USE YOMCST    , ONLY : RTT
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEBST(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT    (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL   (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI   (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS   (KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEIJ (KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSM  (KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAUTOL(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PAUTOI(KLON,KLEV)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSTACK (KLON, KKSTSZ)
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KKSTSZ, KPSTPT, KKSTPT
END SUBROUTINE ACMICRO
END INTERFACE
