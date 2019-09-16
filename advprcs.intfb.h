INTERFACE
#include "attr.h"
SUBROUTINE ADVPRCS (KIDIA, KFDIA, KLON, KTDIA, KFLEV,&
 & PT, PQ, PQL, PQI, PAUTOL, PAUTOI, &
 & PQR, PQS, PNEB, &
 & PCP, PR, PAPHI, PAPRSF, PDELP,&
 & PFPLSL, PFPLSN, PFPEVPL, PFPEVPN, PFPFPL, PFPFPN, PSEDIQL, PSEDIQN, KSTACK, PSTACK, KPSTSZ, KKSTSZ, KPSTPT, KKSTPT )
USE PARKIND1  ,ONLY : JPIM     ,JPRB
USE YOMPHY    , ONLY : YRPHY
USE YOMPHY0   , ONLY : YRPHY0
USE YOMPHY2   , ONLY : YRPHY2
USE YOMCST    , ONLY : RG   , RV   , RTT  , RPI  ,&
 & RCS  , RCW  , RCPV , RLVTT, RLSTT, RETV , RALPW, RALPS,&
 & RALPD, RBETW, RBETS, RBETD, RGAMW, RGAMS, RGAMD
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIA
INTEGER(KIND=JPIM),INTENT(IN)    :: KFLEV
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT     (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ     (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQL    (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQI    (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAUTOL (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAUTOI (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQR    (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQS    (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNEB   (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCP    (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR     (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI  (KLON,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP  (KLON,KFLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPLSL (KLON,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPLSN (KLON,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPEVPL(KLON,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPEVPN(KLON,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPFPL (KLON,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFPFPN (KLON,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSEDIQL(KLON,0:KFLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PSEDIQN(KLON,0:KFLEV)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSTACK (KLON, KKSTSZ)
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KKSTSZ, KPSTPT, KKSTPT
END SUBROUTINE ADVPRCS
END INTERFACE
