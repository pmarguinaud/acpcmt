INTERFACE
SUBROUTINE ACPCMT ( KIDIA,KFDIA,KLON,KTDIA,KLEV,KTRA,&
  !-----------------------------------------------------------------------
  ! - INPUT  2D .
  & PALPH,PAPHI,PAPHIF,PAPRS,PAPRSF,PCP,PLH,PR,&
  & PDELP,PLNPR,&
  & PQ,PQI,PQL,PRR,PS,PQLIS,PQSAT,PQW,&
  & PRDELP,PCVGQ,PT,PTW,PU,PV,PTKE,PTRA,&
  & PQLCONV, PQICONV , PQRCONV, PQSCONV, &
  ! - INPUT 1D .
  & PGM, PTS, PQS, PNEIJ, PLSM, PVETAF, &
  ! - OUTPUT 2D .
  & PDIFCQ,PDIFCQL,PDIFCQI,PDIFCQLC,PDIFCQIC,PDIFCS,PDIFCTH,&
  & PFCCQL,PFCCQN,PSTRCU,PSTRCV,PSTRCTRA,&
  & PFIMCC,PFPEVPCL,PFPEVPCN,&
  & PFPLCL,PFPLCN,PMF_UP,&
  & PFPFPCL,PFPFPCN,&
  & PFEDQLC, PFEDQIC, PFEDQRC, PFEDQSC,&
  & PFCNEGQLC,PFCNEGQIC,PFCNEGQRC,PFCNEGQSC, &
  & KNLAB,&
  & PNEBC,PQLIC,PTU,PQU,PQC_DET_PCMT,PCSGC, &
  ! - OUTPUT 1D .
  & KNND, PCAPE,PAIPCMT,PALF_CAPE,PALF_CVGQ,&
  ! - INPUT/OUTPUT 2D .
  & PUDAL,PUDOM,PDDAL,PDDOM)
USE PARKIND1, ONLY : JPIM, JPRB
USE YOMCST   , ONLY : RG, RCPD, RCS, RCW, RALPD, RALPS,RALPW,RBETD,RBETS, &
  & RBETW,RCPV,RDT,RETV,RGAMD,RGAMS,RGAMW,RLSTT,RLVTT,RTT,RV
USE YOMPHY0  , ONLY : YRPHY0
USE YOMPHY2  , ONLY : YRPHY2
USE YOMPHY   , ONLY : YRPHY
USE YOMSCM   , ONLY : NFRSCM
USE YOMCT3   , ONLY : NSTEP
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
REAL(KIND=JPRB), INTENT(IN)    :: PRR(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PS(KLON,KLEV)
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
REAL(KIND=JPRB), INTENT(IN)    :: PTRA(KLON, KLEV, KTRA)
REAL(KIND=JPRB), INTENT(IN)    :: PQLCONV(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PQICONV(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PQRCONV(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PQSCONV(KLON,KLEV)
REAL(KIND=JPRB), INTENT(IN)    :: PGM(KLON)
REAL(KIND=JPRB), INTENT(IN)    :: PTS(KLON)
REAL(KIND=JPRB), INTENT(IN)    :: PQS(KLON)
REAL(KIND=JPRB), INTENT(IN)    :: PNEIJ(KLON)
REAL(KIND=JPRB), INTENT(IN)    :: PLSM(KLON)
REAL(KIND=JPRB), INTENT(IN)    :: PVETAF(KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCQ(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCQL(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCQI(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCQLC(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCQIC(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCS(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PDIFCTH(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFCCQL(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFCCQN(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PSTRCU(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PSTRCV(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PSTRCTRA(KLON, 0:KLEV, KTRA)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFIMCC(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPEVPCL(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPEVPCN(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPLCL(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPLCN(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PMF_UP(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPFPCL(KLON,0:KLEV)
REAL(KIND=JPRB), INTENT(INOUT)   :: PFPFPCN(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFEDQLC(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFEDQIC(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFEDQRC(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFEDQSC(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFCNEGQLC(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFCNEGQIC(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFCNEGQRC(KLON,0:KLEV)
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PFCNEGQSC(KLON,0:KLEV)
INTEGER(KIND=JPIM), INTENT(INOUT) :: KNLAB(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PNEBC(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PQLIC(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PTU(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PQU(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PQC_DET_PCMT(KLON,KLEV)
REAL(KIND=JPRB), INTENT(OUT)   :: PCSGC(KLON,KLEV)
INTEGER(KIND=JPIM), INTENT(INOUT) :: KNND(KLON)
REAL(KIND=JPRB), INTENT(OUT)   :: PCAPE(KLON)
REAL(KIND=JPRB), INTENT(OUT)   :: PAIPCMT(KLON)
REAL(KIND=JPRB), INTENT(OUT)   :: PALF_CAPE(KLON)
REAL(KIND=JPRB), INTENT(OUT)   :: PALF_CVGQ(KLON)
REAL(KIND=JPRB), INTENT(INOUT) :: PUDAL(KLON,KLEV)
REAL(KIND=JPRB), INTENT(INOUT) :: PUDOM(KLON,KLEV)
REAL(KIND=JPRB), INTENT(INOUT) :: PDDAL(KLON,KLEV)
REAL(KIND=JPRB), INTENT(INOUT) :: PDDOM(KLON,KLEV)
END SUBROUTINE ACPCMT
END INTERFACE