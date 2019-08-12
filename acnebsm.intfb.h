INTERFACE
SUBROUTINE ACNEBSM ( KIDIA, KFDIA, KLON, KTDIA, KLEV,&
 !-----------------------------------------------------------------------
 ! - INPUT -
 & PT, PQ, PQL, PQI, &
 & PAPHI, PAPRSF, PCP, PR, &
 & PGM, PVETAF, &
 ! - OUTPUT -
 & PQCS, PNEBS, PRHCRI, PICE, PQSATS, KSTACK, PSTACK, KPSTSZ, KKSTSZ )
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMCST    , ONLY : RD   , RV   , RTT  , RDT  ,&
 & RCS  , RCW  , RCPV , RLVTT, RLSTT, RETV , RALPW, RALPS,&
 & RALPD, RBETW, RBETS, RBETD, RGAMW, RGAMS, RGAMD
USE YOMPHY0  , ONLY : YRPHY0
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT    (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ    (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL   (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI   (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI (KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCP   (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR    (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGM   (KLON)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PVETAF(KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQCS  (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNEBS (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT)   :: PRHCRI(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT)   :: PICE  (KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT)   :: PQSATS(KLON,KLEV)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSTACK (KLON, KKSTSZ)
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KKSTSZ
END SUBROUTINE ACNEBSM
END INTERFACE
