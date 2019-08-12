INTERFACE
SUBROUTINE FPCINCAPE(KIDIA,KFDIA,KLON,KLEV,KLEVST,PT,PRP,PQV,PCAPE,PCIN,KLCL,KFCL,KLNB)
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMCAPE  , ONLY :  NCAPEITER,NETAPES  ,GCAPERET
USE YOMCST   , ONLY :  RTT      ,RDAY     ,REPSM    ,RETV     ,&
 & RCW      ,REA      ,RD       ,RV       ,RCPD     ,RCPV     ,&
 & RCS      ,RLVTT    ,RLSTT    ,RBETS    ,RALPW    ,RBETW    ,&
 & RGAMW    ,RALPS    ,RGAMS    ,RALPD    ,RBETD    ,RGAMD    ,&
 & RG
USE YOMFPC , ONLY : RENTRA
USE YOMTOPH, ONLY : YRTOPH
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVST
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRP(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQV(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCAPE(KLON)
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCIN(KLON)
INTEGER(KIND=JPIM)   ,INTENT(OUT)   :: KLCL(KLON)
INTEGER(KIND=JPIM)   ,INTENT(OUT)   :: KFCL(KLON)
INTEGER(KIND=JPIM)   ,INTENT(OUT)   :: KLNB(KLON)
END SUBROUTINE FPCINCAPE
END INTERFACE
