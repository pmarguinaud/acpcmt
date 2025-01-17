INTERFACE
SUBROUTINE ACMTUD ( KIDIA,KFDIA,KLON,KTDIA,KLEV,KTRA,&
!-----------------------------------------------------------------------
! - INPUT  2D .
& PALPH,PAPHI,PAPHIF,PAPRS,PAPRSF,PCP,PLH,PR,&
& PDELP,PLNPR,&
& PQ,PQI,PQL,PQLIS,PQSAT,PQW,&
& PRDELP,PCVGQ,PT,PTW,PU,PV,PTKE,PTRA,PEVAPO,&
! - INPUT 1D .
& PGM, PTS, PQS,&
! - OUTPUT 2D .
& PDIFCQ,PDIFCQL,PDIFCQI,PDIFCS,PDIFCTH,&
& PFCCQL,PFCCQI,PSTRCU,PSTRCV,PSTRCTRA,&
& PFHMLTS,PFHEVPP,PFPEVPCL,PFPEVPCN,&
& PFPLCL,PFPLCN,PMF_UP,&
& PNEBC,PQLIC,PTU,PQU,PSU,KNLAB,PENTR_U,PDETR_U,PENTR_D,PDETR_D,&
! - OUTPUT 1D .
& KNND,PCAPE,PALF,PALF_CAPE,PALF_CVGQ,&
! - INPUT/OUTPUT 2D .
& PUDAL,PUDOM,PDDAL,PDDOM, KSTACK, PSTACK, KPSTSZ, KKSTSZ, KPSTPT, KKSTPT)
USE PARKIND1, ONLY : JPIM, JPRB
USE YOMPHY   , ONLY : YRPHY
USE YOMCST   , ONLY : RATM       ,RG       ,RKAPPA   ,&
  & RD       ,RV       ,RCPD     ,&
  & RCPV     ,RETV     ,RCW      ,RCS      ,RLVTT    ,&
  & RLSTT    ,RTT      ,RALPW    ,RBETW    ,RGAMW    ,&
  & RALPS    ,RBETS    ,RGAMS    ,RALPD    ,RBETD    ,&
  & RGAMD    ,RPI      ,RA       ,&
  & RDT
USE YOMPHY0  , ONLY : YRPHY0
USE YOMPHY2  , ONLY : YRPHY2
USE YOMCT0   , ONLY : LELAM
USE YOMDIM   , ONLY : YRDIM
USE YEMGEO   , ONLY : YREGEO
USE YOMCT3   , ONLY : NSTEP
USE YOMSCM   , ONLY : NFRSCM
USE YOMRIP   , ONLY : YRRIP
USE YOMGEM   , ONLY : YRGEM
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KLON
INTEGER(KIND=JPIM), INTENT(IN) :: KTDIA
INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
INTEGER(KIND=JPIM), INTENT(IN) :: KTRA
REAL(KIND=JPRB), INTENT(IN)    :: PALPH(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PAPHI(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PAPHIF(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PAPRS(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PCP(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PLH(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PR(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PDELP(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PLNPR(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PQ(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PQI(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PQL(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PQLIS(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PQSAT(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PQW(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PRDELP(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PCVGQ(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PT(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PTW(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PU(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PV(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PTKE(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PTRA(KLON,KLEV,KTRA)
REAL(KIND=JPRB), INTENT(IN)    :: PEVAPO(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PGM(KLON)
REAL(KIND=JPRB), INTENT(IN)    :: PTS(KLON)
REAL(KIND=JPRB), INTENT(IN)    :: PQS(KLON)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCQ(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCQL(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCQI(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCS(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCTH(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFCCQL(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFCCQI(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PSTRCU(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PSTRCV(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PSTRCTRA(KLON,0:KLEV,KTRA)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFHMLTS(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFHEVPP(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPEVPCL(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPEVPCN(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPLCL(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPLCN(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PMF_UP(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PNEBC(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PQLIC(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PTU(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PQU(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PSU(KLON,KLEV)
INTEGER(KIND=JPIM), INTENT(INOUT) :: KNLAB(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PENTR_U(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PDETR_U(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PENTR_D(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PDETR_D(KLON,KLEV)
INTEGER(KIND=JPIM), INTENT(INOUT) :: KNND(KLON)
REAL(KIND=JPRB), INTENT(OUT)   :: PCAPE(KLON)
REAL(KIND=JPRB)   , INTENT(INOUT) :: PALF(KLON)
REAL(KIND=JPRB), INTENT(OUT)   :: PALF_CAPE(KLON)
REAL(KIND=JPRB), INTENT(OUT)   :: PALF_CVGQ(KLON)
REAL(KIND=JPRB), INTENT(INOUT) :: PUDAL(KLON,KLEV)
REAL(KIND=JPRB), INTENT(INOUT) :: PUDOM(KLON,KLEV)
REAL(KIND=JPRB), INTENT(INOUT) :: PDDAL(KLON,KLEV)
REAL(KIND=JPRB), INTENT(INOUT) :: PDDOM(KLON,KLEV)
INTEGER(KIND=JPIM),INTENT(OUT)   :: KSTACK (KLON, KKSTSZ)
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ, KKSTSZ, KPSTPT, KKSTPT
END SUBROUTINE ACMTUD
END INTERFACE
