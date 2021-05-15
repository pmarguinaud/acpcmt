!OPTIONS XOPT(NOEVAL)
SUBROUTINE FPCINCAPE(KIDIA,KFDIA,KGPBLKS,KLON,KLEV,KLEVST,PT,PRP,PQV,PCAPE,PCIN,KLCL,KFCL,KLNB)

! --------------------------------------------------------------
! **** *FPCINCAPE* COMPUTE CAPE AND CIN.
! --------------------------------------------------------------
! SUBJECT:
!    ROUTINE COMPUTING CAPE AND CIN  

! INTERFACE:
!    *CALL* *FPCINCAPE*

! --------------------------------------------------------------
! -   INPUT ARGUMENTS
!     ---------------

! - DIMENSIONING

! KIDIA      : FIRST INDEX OF LOOPS
! KFDIA     : LAST INDEX OF LOOPS
! KLON   : DEPTH OF THE VECTORIZATION ARRAYS
! KLEV     : END OF VERTICAL LOOP AND VERTICAL DIMENSION

! - VARIABLES
! KLEVST   : LEVEL FROM WHICH PARCEL IS RAISED
! PT       : TEMPERATURE (K)
! PRP      : PRESSURE (PA)
! PQV      : WATER VAPOUR SPECIFIC HUMIDITY (NO DIM)

! --------------------------------------------------------------
! -   OUTPUT ARGUMENTS
!     ---------------
! - VARIABLES
! PCAPE    : CAPE - CONVECTIVE AVAILABLE POTENTIAL ENERGY (J/KG)
!                   (POTENTIALLY AVAILABLE CONVECTIVE KINETIC ENERGY)
! PCIN     : CIN - CONVECTIVE INHIBITION (J/KG)
! KLCL     : CONDENSATION LEVEL
! KFCL     : FREE CONVECTION LEVEL
! KLNB     : LEVEL OF NEUTRAL BUOYANCY

! --------------------------------------------------------------
! -   IMPLICITE ARGUMENTS
!     -------------------
! YOMCAPE 
! YOMCST
! FCTTRM
! FCTAST
! FCTTIM

! --------------------------------------------------------------
! EXTERNALS:

! METHOD:

!      THE PARCEL IS RAISED FROM LEVEL (LO which is equal KLEVST) 
!      TO THE LEVEL OF CONDENSATION (LC),
!      FURTHER TO ITS LEVEL OF FREE CONVECTION (LFC), WHERE THE 
!      PARCEL BECOMES BUOYANT,
!      THEN FURTHER TO THE LEVEL OF NEUTRAL BUOYANCY (LNB), WHERE THE
!      PARCEL BECOMES UNBUOYANT.

!      ALL COMPUTATIONS ARE DONE WITHOUT ENTRAINMENT OF ENVIRONMENTAL AIR.

!      CIN IS MASS SPECIFIC ENERGY TO RAISE THE PARCEL FROM 
!          from LO to LC further to LFC.
!          ONLY THE SUM OF NEGATIVE TERMS.
!      CAPE IS MASS SPECIFIC ENERGY  PROVIDED BY THE RAISE OF THE PARCEL
!          from LFC to LNB.
!          ONLY THE SUM OF POSITIVE TERMS.

! AUTEUR/AUTHOR:   2001-03, J.M. PIRIOU, N. PRISTOV.

! MODIFICATIONS:
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!    2010-02-04  J.M. Piriou. Bug correction: derivative of qs(T,p) from the Newton loops.
!    2010-02-11  J.M. Piriou. Bug correction: JLON dimension of ZFDERQS0 array.
!    2010-02-17  J.M. Piriou. Protections in case of cold temperature, low pressure, etc.
!    2010-02-17  J.M. Piriou. First Newton loop in a single DO loop.
!    2010-02-17  J.M. Piriou. Compute CAPE only below the convective reference level (NTCVIM).
! --------------------------------------------------------------

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

INTEGER(KIND=JPIM),INTENT(IN)    :: KGPBLKS,KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVST 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRP(KLON,KLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQV(KLON,KLEV,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCAPE(KLON,KGPBLKS) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCIN(KLON,KGPBLKS) 
INTEGER(KIND=JPIM)   ,INTENT(OUT)   :: KLCL(KLON,KGPBLKS)
INTEGER(KIND=JPIM)   ,INTENT(OUT)   :: KFCL(KLON,KGPBLKS)
INTEGER(KIND=JPIM)   ,INTENT(OUT)   :: KLNB(KLON,KGPBLKS)
INTEGER(KIND=JPIM)      :: IPREVIOUS_NULL_ZRT(KLON,KGPBLKS)
INTEGER(KIND=JPIM) :: JLEV,JBLK,JLON,JIT,JETAPES
REAL(KIND=JPRB) :: ZDLOG(KLON,KGPBLKS),ZBUOY,ZTV1,ZTV2,ZQV2,ZRT(KLON,KGPBLKS),ZT2,ZDT(KLON,KGPBLKS),ZST,ZDELARG,ZL,ZCP, &
 & ZFDERQS,ZADD,ZDERL,ZRDLOG,ZZQV
REAL(KIND=JPRB) :: ZBUOYPREC(KLON,KGPBLKS),ZQS(KLON,KGPBLKS),ZQV(KLON,KGPBLKS), ZQV1(KLON,KGPBLKS), &
 & ZT(KLON,KGPBLKS), ZT1(KLON,KGPBLKS),ZQL(KLON,KGPBLKS),ZQI(KLON,KGPBLKS),ZSQV(KLON,KGPBLKS), &
 & ZSQL(KLON,KGPBLKS),ZSQI(KLON,KGPBLKS),ZTDEPART(KLON,KGPBLKS),ZLOG(KLON,KGPBLKS), &
 & ZPARRIVEE(KLON,KGPBLKS),ZPDEPART(KLON,KGPBLKS),ZQVDEPART(KLON,KGPBLKS),ZZT(KLON,KGPBLKS)  
REAL(KIND=JPRB) :: ZFDERFOLH0(KLON,KGPBLKS),ZDELARG0(KLON,KGPBLKS),ZL0(KLON,KGPBLKS),ZCP0(KLON,KGPBLKS)
REAL(KIND=JPRB) :: ZFDERQS0, ZDZ, ZZFOEW(KLON,KGPBLKS)
REAL(KIND=JPRB) :: ZMAXT,ZMINT,ZMINDERI,ZMINQ,ZMAXQ
REAL(KIND=JPRB) :: ZTIN(KLON,KLEV,KGPBLKS) 
REAL(KIND=JPRB) :: ZQVIN(KLON,KLEV,KGPBLKS) 

!---------

!  FUNCTIONS
#include "fctast.func.h"
#include "fcttrm.func.h"
#include "fcttim.func.h"



!$acc data present (KFCL, KLCL, KLNB, PCAPE, PCIN, PQV, PRP, PT)
!$acc data create (ZSQL, ZSQV, ZT, ZT1, ZTDEPART, ZTIN, ZZFOEW, ZZT)
!$acc data create (ZPDEPART, ZQI, ZQL, ZQS, ZQV, ZQV1, ZQVDEPART, ZQVIN, ZRT, ZSQI)
!$acc data create (IPREVIOUS_NULL_ZRT, ZBUOYPREC, ZCP0, ZDELARG0, ZDLOG, ZDT, ZFDERFOLH0, ZL0, ZLOG, ZPARRIVEE)
!-------------------------------------------------
! INITIALIZE DEFAULT VALUES.
!-------------------------------------------------

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

PCIN(JLON,JBLK)=0.0_JPRB

ENDDO
ENDDO
!$acc end kernels

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

PCAPE(JLON,JBLK)=0.0_JPRB

ENDDO
ENDDO
!$acc end kernels

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

KLCL(JLON,JBLK)=-1

ENDDO
ENDDO
!$acc end kernels

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

KFCL(JLON,JBLK)=-1

ENDDO
ENDDO
!$acc end kernels

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

KLNB(JLON,JBLK)=-1

ENDDO
ENDDO
!$acc end kernels


ZMINT=150._JPRB
ZMAXT=400._JPRB
ZMINDERI=1000._JPRB
ZMINQ=1.E-07_JPRB
ZMAXQ=1.0_JPRB-ZMINQ

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLEV=1,KLEV
DO JLON = KIDIA, KFDIA
  
    ZTIN(JLON,JLEV,JBLK)=MAX(ZMINT,MIN(ZMAXT,PT(JLON,JLEV,JBLK)))
    ZQVIN(JLON,JLEV,JBLK)=MAX(ZMINQ,MIN(ZMAXQ,PQV(JLON,JLEV,JBLK)))
  
ENDDO
ENDDO
ENDDO
!$acc end kernels


!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

ZBUOYPREC(JLON,JBLK)=0.0_JPRB

ENDDO
ENDDO
!$acc end kernels

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

ZT (JLON,JBLK)=ZTIN (JLON,KLEVST,JBLK)

ENDDO
ENDDO
!$acc end kernels

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

ZQV(JLON,JBLK)=ZQVIN(JLON,KLEVST,JBLK)

ENDDO
ENDDO
!$acc end kernels

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

ZQL(JLON,JBLK)=0.0_JPRB

ENDDO
ENDDO
!$acc end kernels

!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

ZQI(JLON,JBLK)=0.0_JPRB

ENDDO
ENDDO
!$acc end kernels


!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLON = KIDIA, KFDIA

IPREVIOUS_NULL_ZRT(JLON,JBLK)=999999

ENDDO
ENDDO
!$acc end kernels


!$acc kernels
DO JBLK = 1, KGPBLKS
DO JLEV=KLEVST,YRTOPH%NTCVIM+1,-1
DO JLON = KIDIA, KFDIA
  
    !
    !-------------------------------------------------
    ! SATURATION SPECIFIC HUMIDITY.
    !-------------------------------------------------
    !
    ZT(JLON,JBLK)=MAX(ZMINT,MIN(ZMAXT,ZT(JLON,JBLK)))
    ZQS(JLON,JBLK)=FOQS(FOEW(ZT(JLON,JBLK),MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZT(JLON,JBLK))))/PRP(JLON,JLEV,JBLK))  
    ZDLOG(JLON,JBLK)=LOG(PRP(JLON,MIN(KLEVST,JLEV+1),JBLK)/PRP(JLON,JLEV,JBLK)) * MAX(0,-SIGN(1,JLEV-KLEVST))  
    !
  
  
    !
    !-------------------------------------------------
    ! PRESSURE AND BUOYANCY.
    !-------------------------------------------------
    !
    ZQV(JLON,JBLK)=MAX(ZMINQ,MIN(ZMAXQ,ZQV(JLON,JBLK)))
    ZTV1=ZT(JLON,JBLK)*(1.0_JPRB+ZQV(JLON,JBLK)/(1.0_JPRB-ZQV(JLON,JBLK))*RV/RD)/ &
     & (1.0_JPRB+ZQV(JLON,JBLK)/ &
     & (1.0_JPRB-ZQV(JLON,JBLK))+ZQL(JLON,JBLK)/(1.0_JPRB-ZQL(JLON,JBLK))+ZQI(JLON,JBLK)/(1.0_JPRB-ZQI(JLON,JBLK)))  
    ZTV2=ZTIN(JLON,JLEV,JBLK)*(1.0_JPRB+ZQVIN(JLON,JLEV,JBLK)/(1.0_JPRB-ZQVIN(JLON,JLEV,JBLK))*RV/RD)/&
     & (1.0_JPRB+ZQVIN(JLON,JLEV,JBLK)/(1.0_JPRB-ZQVIN(JLON,JLEV,JBLK)) )  
    ZBUOY=(ZTV1/ZTV2-1.0_JPRB)*(RD+(RV-RD)*ZQVIN(JLON,JLEV,JBLK))*ZTIN(JLON,JLEV,JBLK)
    !
    !-------------------------------------------------
    ! CIN AND CAPE INTEGRALS.
    !-------------------------------------------------
    !
    ZRT(JLON,JBLK)=0.5_JPRB*(ZBUOY+ZBUOYPREC(JLON,JBLK))*ZDLOG(JLON,JBLK)
    !------------------------------------------
    ! CUMULATE CAPE IF POSITIVE CONTRIBUTION.
    !------------------------------------------
    PCAPE(JLON,JBLK)=PCAPE(JLON,JBLK)+MAX(0.0_JPRB,ZRT(JLON,JBLK))
    !------------------------------------------
    ! CUMULATE CIN IF NEGATIVE CONTRIBUTION AND BELOW LFC.
    !------------------------------------------
    IF(PCAPE(JLON,JBLK) == 0.0_JPRB) PCIN(JLON,JBLK)=PCIN(JLON,JBLK)+MIN(0.0_JPRB,ZRT(JLON,JBLK))
    ZBUOYPREC(JLON,JBLK)=ZBUOY
    !
    !-------------------------------------------------
    ! IF THE PARCEL IS SUPERSATURATED, SUPERSATURATION IS REMOVED.
    ! THIS IS DONE THROUGH AN ISOBARIC TRANSFORMATION FROM (ZT,ZQV)
    ! TO (ZT1,ZQV1).
    !       Solution for T
    !            f(T)=cp*(T-T0)+L*(q-q0)=0
    !            with constraint q=qs(T,p0),
    !       it is solved by the method of Newton, iteration
    !       T --> T-f(T)/f'(T), with starting point ZT.
    !-------------------------------------------------
    !
    ZT1(JLON,JBLK)=ZT(JLON,JBLK)
  
  
    ZQV1(JLON,JBLK)=FOQS(FOEW(ZT1(JLON,JBLK),MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZT1(JLON,JBLK))))/PRP(JLON,JLEV,JBLK))  
    IF ((ZQV1(JLON,JBLK)<=ZQV(JLON,JBLK)).AND.(KLCL(JLON,JBLK)==-1)) THEN
      KLCL(JLON,JBLK)=JLEV
    ENDIF
    IF ((PCAPE(JLON,JBLK) > 0.0_JPRB).AND.(KFCL(JLON,JBLK)==-1)) THEN
      KFCL(JLON,JBLK)=JLEV
    ENDIF
    IF ((ZRT(JLON,JBLK) <= 0.0_JPRB).AND.(JLEV<KFCL(JLON,JBLK)).AND.(JLEV<IPREVIOUS_NULL_ZRT(JLON,JBLK)-1)) THEN
      KLNB(JLON,JBLK)=JLEV
    ENDIF
    IF (ZRT(JLON,JBLK) <= 0.0_JPRB) THEN
      IPREVIOUS_NULL_ZRT(JLON,JBLK)=JLEV
    ENDIF
  

  DO JIT=1,NCAPEITER
    
      !
      !-------------------------------------------------
      ! LATENT HEAT
      !-------------------------------------------------
      !
      ZDELARG0(JLON,JBLK)=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZT1(JLON,JBLK)))
      ZL0(JLON,JBLK)=FOLH(ZT1(JLON,JBLK),ZDELARG0(JLON,JBLK))
      ZFDERFOLH0(JLON,JBLK)=-RV*(RGAMW+ZDELARG0(JLON,JBLK)*RGAMD)
      ZCP0(JLON,JBLK)=RCPD*(1.0_JPRB-ZQV1(JLON,JBLK))+RCPV*ZQV1(JLON,JBLK)
      !
      !-------------------------------------------------
      ! Newton's loop to solve the supersaturation.
      !-------------------------------------------------
      !
    
    
      ZZFOEW(JLON,JBLK)=FOEW(ZT1(JLON,JBLK),ZDELARG0(JLON,JBLK))
    
    
      ZFDERQS0=FODQS(ZQV1(JLON,JBLK),ZZFOEW(JLON,JBLK)/PRP(JLON,JLEV,JBLK),FODLEW(ZT1(JLON,JBLK),ZDELARG0(JLON,JBLK)))
      ZT1(JLON,JBLK)=MAX(ZMINT,MIN(ZMAXT,ZT1(JLON,JBLK)-(ZCP0(JLON,JBLK)*&
       & (ZT1(JLON,JBLK)-ZT(JLON,JBLK))+ZL0(JLON,JBLK)*(ZQV1(JLON,JBLK)-ZQV(JLON,JBLK)))&
       & /MAX(ZMINDERI,ZCP0(JLON,JBLK)+((RCPV-RCPD)*(ZT1(JLON,JBLK)-ZT(JLON,JBLK))+ZL0(JLON,JBLK))* ZFDERQS0 &
       & +(ZQV1(JLON,JBLK)-ZQV(JLON,JBLK))*ZFDERFOLH0(JLON,JBLK))))
      ZDELARG0(JLON,JBLK)=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZT1(JLON,JBLK)))
    
    
      ZZFOEW(JLON,JBLK)=FOEW(ZT1(JLON,JBLK),ZDELARG0(JLON,JBLK))
    
    
      !
      !-------------------------------------------------
      ! SATURATION SPECIFIC HUMIDITY
      !-------------------------------------------------
      !
      ZQV1(JLON,JBLK)=FOQS(ZZFOEW(JLON,JBLK)/PRP(JLON,JLEV,JBLK))  
    
  ENDDO

  
    ZADD=GCAPERET*(ZQV(JLON,JBLK)-ZQV1(JLON,JBLK))
                        
    ZSQL(JLON,JBLK)=ZQL(JLON,JBLK)+ZADD*MAX(0.0_JPRB,SIGN(1.0_JPRB,ZT1(JLON,JBLK)-RTT))
    ZSQI(JLON,JBLK)=ZQI(JLON,JBLK)+ZADD*MAX(0.0_JPRB,-SIGN(1.0_JPRB,ZT1(JLON,JBLK)-RTT))
    !
    !-------------------------------------------------
    ! MOIST ADIABATIC ASCENT.
    ! TRANSFORMATION FROM (ZT1,ZQV1) TO (ZT2,ZQV2).
    !      Solution for T
    !             f(T)=cp*(T-T0)+L*(q-q0)+phi-phi0=0
    !      either f(T)=cp*(T-T0)+L*(q-q0)-R*T*log(p/p0)=0
    !            with constraint q=qs(T,p), knowing that q0=qs(T0,p0)
    !       it is solved by the method of Newton, iteration
    !       T --> T-f(T)/f'(T), with starting point ZT1.
    !-------------------------------------------------
    !
    ZZT(JLON,JBLK)=ZT1(JLON,JBLK)
  
  !-------------------------------------------------
  ! CALCULATION IS DIVIDED IN MORE STEPS IN ORDER
  ! TO GET MORE PRECISE VALUES
  !-------------------------------------------------
  DO JETAPES=1,NETAPES
    
      ZPDEPART(JLON,JBLK) =PRP(JLON,JLEV,JBLK)+(PRP(JLON,JLEV-1,JBLK)-PRP(JLON,JLEV,JBLK))*REAL(JETAPES-1)/REAL(NETAPES)  
      ZPARRIVEE(JLON,JBLK)=PRP(JLON,JLEV,JBLK)+(PRP(JLON,JLEV-1,JBLK)-PRP(JLON,JLEV,JBLK))*REAL(JETAPES)/REAL(NETAPES)  
      ZTDEPART(JLON,JBLK)=ZZT(JLON,JBLK)
    
    
      ZQVDEPART(JLON,JBLK)=FOQS(FOEW(ZTDEPART(JLON,JBLK),MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTDEPART(JLON,JBLK)))) &
       & /ZPDEPART(JLON,JBLK))
      ZLOG(JLON,JBLK)=LOG(ZPARRIVEE(JLON,JBLK)/ZPDEPART(JLON,JBLK))
    
    DO JIT=1,NCAPEITER
      
        ZZFOEW(JLON,JBLK)=FOEW(ZZT(JLON,JBLK),MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZZT(JLON,JBLK))))
      
      
        !
        !-------------------------------------------------
        ! SATURATION SPECIFIC HUMIDITY
        !-------------------------------------------------
        !
        ZZQV=FOQS(ZZFOEW(JLON,JBLK)/ZPARRIVEE(JLON,JBLK))  
        !
        !-------------------------------------------------
        ! LATENT HEAT
        !-------------------------------------------------
        !
        ZDELARG=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTDEPART(JLON,JBLK)))
        !
        ! CALCULATION WHERE IT IS CONSIDERED THAT LATENT HEAT RELEASE
        ! FROM CONDENSATION IS ABSORBTED ONLY BY THE GASEOUS PORTION 
        ! OF THE PARCEL, AND NOT BY THE CONDENSATE
        !
        ZL=FOLH(ZZT(JLON,JBLK),ZDELARG)  
        ZDERL=-RV*(RGAMW+ZDELARG*RGAMD)
        !
        !-------------------------------------------------
        ! Newton's loop to solve the moist adiabatic ascent.
        !-------------------------------------------------
        !
        ZRDLOG=(RD+(RV-RD)*ZZQV)*ZLOG(JLON,JBLK)
        ZCP=RCPD*(1.0_JPRB-ZZQV)+RCPV*ZZQV 
        ZFDERQS=FODQS(ZZQV,ZZFOEW(JLON,JBLK)/ZPARRIVEE(JLON,JBLK) &
          & ,FODLEW(ZZT(JLON,JBLK),MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZZT(JLON,JBLK)))))
        ZZT(JLON,JBLK)=MAX(ZMINT,MIN(ZMAXT,ZZT(JLON,JBLK)-(ZCP*(ZZT(JLON,JBLK)-ZTDEPART(JLON,JBLK)) &
         & +ZL*(ZZQV-ZQVDEPART(JLON,JBLK))-ZZT(JLON,JBLK) &
         & *ZRDLOG)/MAX(ZMINDERI,ZCP+((RCPV-RCPD)*(ZZT(JLON,JBLK)-ZTDEPART(JLON,JBLK)) &
         & +ZL-ZLOG(JLON,JBLK)*ZZT(JLON,JBLK)*(RV-RD))*ZFDERQS &
         & +(ZZQV-ZQVDEPART(JLON,JBLK))*ZDERL-ZRDLOG)))
            ! JLON
    ENDDO     ! JIT
  ENDDO   !JETAPES
  
  
    ZZFOEW(JLON,JBLK)=FOEW(ZZT(JLON,JBLK),MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZZT(JLON,JBLK))))
    !
    !-------------------------------------------------
    ! DRY ADIABATIC ASCENT.
    !-------------------------------------------------
    !
    ZDT(JLON,JBLK)=ZT(JLON,JBLK)*(PRP(JLON,JLEV-1,JBLK)/PRP(JLON,JLEV,JBLK))**((RD+(RV-RD) &
     & *ZQV(JLON,JBLK))/(RCPD*(1.0_JPRB-ZQV(JLON,JBLK))+RCPV*ZQV(JLON,JBLK)))  
    !
  
  
    ZT2=ZZT(JLON,JBLK)

    ZQV2=FOQS(ZZFOEW(JLON,JBLK)/PRP(JLON,JLEV-1,JBLK))

    ZADD=GCAPERET*(ZQV1(JLON,JBLK)-ZQV2)*MAX(0.0_JPRB,SIGN(1.0_JPRB,ZQV1(JLON,JBLK)-ZQV2))
    ZSQL(JLON,JBLK)=ZSQL(JLON,JBLK)+ZADD*MAX(0.0_JPRB,SIGN(1.0_JPRB,ZT2-RTT))
    ZSQI(JLON,JBLK)=ZSQI(JLON,JBLK)+ZADD*MAX(0.0_JPRB,-SIGN(1.0_JPRB,ZT2-RTT))
    !
    !-------------------------------------------------
    ! UPDATE PARCEL STATE. The result of moist adiabatic ascent.
    !-------------------------------------------------
    !
    ZST=ZT2
    ZSQV(JLON,JBLK)=ZQV2
    !-------------------------------------------------
    !  VALUES FROM DRY ADIABATIC OR MOIST ADIABATIC ASCENT ARE CHOSEN 
    !-------------------------------------------------
    !
    ZADD=MAX(0.0_JPRB,SIGN(1.0_JPRB,ZQV(JLON,JBLK)-ZQS(JLON,JBLK)))
    ZT(JLON,JBLK)=ZST*ZADD + ZDT(JLON,JBLK)*(1.0_JPRB-ZADD)
    ZQV(JLON,JBLK)=ZSQV(JLON,JBLK)*ZADD + ZQV(JLON,JBLK)*(1.0_JPRB-ZADD)
    ZQL(JLON,JBLK)=ZSQL(JLON,JBLK)*ZADD
    ZQI(JLON,JBLK)=ZSQI(JLON,JBLK)*ZADD
    !-----------------------------------------------------
    ! Entrainement
    !-----------------------------------------------------
    IF (RENTRA > 0.0_JPRB) THEN
      ZDZ=(PRP(JLON,JLEV,JBLK)-PRP(JLON,JLEV-1,JBLK))/(0.5*(PRP(JLON,JLEV,JBLK)+ &
        & PRP(JLON,JLEV-1,JBLK)))*(RD+(RV-RD)*(0.5*(ZQVIN(JLON,JLEV,JBLK)+ &
        & (ZQVIN(JLON,JLEV-1,JBLK)))))*0.5*(ZTIN(JLON,JLEV,JBLK)+(ZTIN(JLON,JLEV-1,JBLK)))/RG
      IF(ZT(JLON,JBLK) > ZTIN(JLON,JLEV-1,JBLK)) THEN
        ZT(JLON,JBLK)=MAX(ZTIN(JLON,JLEV-1,JBLK),ZT(JLON,JBLK)+RENTRA*ZDZ*(ZTIN(JLON,JLEV-1,JBLK)-ZT(JLON,JBLK)))
      ELSE
        ZT(JLON,JBLK)=MIN(ZTIN(JLON,JLEV-1,JBLK),ZT(JLON,JBLK)+RENTRA*ZDZ*(ZTIN(JLON,JLEV-1,JBLK)-ZT(JLON,JBLK)))
      ENDIF
      IF(ZQV(JLON,JBLK) > ZQVIN(JLON,JLEV-1,JBLK)) THEN
        ZQV(JLON,JBLK)=MAX(ZQVIN(JLON,JLEV-1,JBLK),ZQV(JLON,JBLK)+RENTRA*ZDZ*(ZQVIN(JLON,JLEV-1,JBLK)-ZQV(JLON,JBLK)))
      ELSE
        ZQV(JLON,JBLK)=MIN(ZQVIN(JLON,JLEV-1,JBLK),ZQV(JLON,JBLK)+RENTRA*ZDZ*(ZQVIN(JLON,JLEV-1,JBLK)-ZQV(JLON,JBLK)))
      ENDIF
    ENDIF
                       ! JLON
ENDDO
ENDDO
ENDDO
!$acc end kernels
                     !JLEV


!$acc end data
!$acc end data
!$acc end data
!$acc end data
END SUBROUTINE FPCINCAPE
