!OPTIONS XOPT(NOEVAL)
!$acc routine (FPCINCAPE) seq
SUBROUTINE FPCINCAPE(KIDIA,KFDIA,KLON,KLEV,KLEVST,PT,PRP,PQV,PCAPE,PCIN,KLCL,KFCL,KLNB,KSTPT,KSTSZ,PSTACK)


#include "temp.h"

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

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEVST 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PRP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCAPE 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCIN 
INTEGER(KIND=JPIM)   ,INTENT(OUT)   :: KLCL
INTEGER(KIND=JPIM)   ,INTENT(OUT)   :: KFCL
INTEGER(KIND=JPIM)   ,INTENT(OUT)   :: KLNB
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTSZ
INTEGER(KIND=JPIM),INTENT(IN)    :: KSTPT
REAL (KIND=JPRB)   ,INTENT(INOUT) :: PSTACK (KSTSZ)

INTEGER(KIND=JPIM) :: IPREVIOUS_NULL_ZRT
INTEGER(KIND=JPIM) :: JLEV,JLON,JIT,JETAPES
REAL(KIND=JPRB) :: ZBUOY,ZTV1,ZTV2,ZQV2,ZT2,ZST,ZDELARG,ZL,ZCP, &
 & ZFDERQS,ZADD,ZDERL,ZRDLOG,ZZQV
REAL(KIND=JPRB) :: ZDT
REAL(KIND=JPRB) :: ZRT
REAL(KIND=JPRB) :: ZDLOG
  
REAL(KIND=JPRB) :: ZZT
REAL(KIND=JPRB) :: ZQVDEPART
REAL(KIND=JPRB) :: ZPDEPART
REAL(KIND=JPRB) :: ZPARRIVEE
REAL(KIND=JPRB) :: ZLOG
REAL(KIND=JPRB) :: ZTDEPART
REAL(KIND=JPRB) :: ZSQI
REAL(KIND=JPRB) :: ZSQL
REAL(KIND=JPRB) :: ZSQV
REAL(KIND=JPRB) :: ZQI
REAL(KIND=JPRB) :: ZQL
REAL(KIND=JPRB) :: ZT1
REAL(KIND=JPRB) :: ZT
REAL(KIND=JPRB) :: ZQV1
REAL(KIND=JPRB) :: ZQV
REAL(KIND=JPRB) :: ZQS
REAL(KIND=JPRB) :: ZBUOYPREC

REAL(KIND=JPRB) :: ZCP0
REAL(KIND=JPRB) :: ZL0
REAL(KIND=JPRB) :: ZDELARG0
REAL(KIND=JPRB) :: ZFDERFOLH0
REAL(KIND=JPRB) :: ZFDERQS0, ZDZ
REAL(KIND=JPRB) :: ZZFOEW
REAL(KIND=JPRB) :: ZMAXT,ZMINT,ZMINDERI,ZMINQ,ZMAXQ
 
temp (REAL(KIND=JPRB), ZTIN, (KLON,KLEV))
 

temp (REAL(KIND=JPRB), ZQVIN, (KLON,KLEV))
!---------

!  FUNCTIONS
#include "abor1.intfb.h"
#include "fctast.func.h"
#include "fcttrm.func.h"
#include "fcttim.func.h"



!-------------------------------------------------
! INITIALIZE DEFAULT VALUES.
!-------------------------------------------------

init_stack ()

alloc (ZTIN)
alloc (ZQVIN)



JLON = KIDIA

PCIN=0.0_JPRB


PCAPE=0.0_JPRB


KLCL=-1


KFCL=-1


KLNB=-1


ZMINT=150._JPRB
ZMAXT=400._JPRB
ZMINDERI=1000._JPRB
ZMINQ=1.E-07_JPRB
ZMAXQ=1.0_JPRB-ZMINQ

DO JLEV=1,KLEV
  
    ZTIN(JLON,JLEV)=MAX(ZMINT,MIN(ZMAXT,PT(JLON,JLEV)))
    ZQVIN(JLON,JLEV)=MAX(ZMINQ,MIN(ZMAXQ,PQV(JLON,JLEV)))
  
ENDDO


ZBUOYPREC=0.0_JPRB


ZT =ZTIN (JLON,KLEVST)


ZQV=ZQVIN(JLON,KLEVST)


ZQL=0.0_JPRB


ZQI=0.0_JPRB



IPREVIOUS_NULL_ZRT=999999


DO JLEV=KLEVST,YRTOPH%NTCVIM+1,-1
  
    !
    !-------------------------------------------------
    ! SATURATION SPECIFIC HUMIDITY.
    !-------------------------------------------------
    !
    ZT=MAX(ZMINT,MIN(ZMAXT,ZT))
    ZQS=FOQS(FOEW(ZT,MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZT)))/PRP(JLON,JLEV))  
    ZDLOG=LOG(PRP(JLON,MIN(KLEVST,JLEV+1))/PRP(JLON,JLEV)) * MAX(0,-SIGN(1,JLEV-KLEVST))  
    !
  
  
    !
    !-------------------------------------------------
    ! PRESSURE AND BUOYANCY.
    !-------------------------------------------------
    !
    ZQV=MAX(ZMINQ,MIN(ZMAXQ,ZQV))
    ZTV1=ZT*(1.0_JPRB+ZQV/(1.0_JPRB-ZQV)*RV/RD)/ &
     & (1.0_JPRB+ZQV/ &
     & (1.0_JPRB-ZQV)+ZQL/(1.0_JPRB-ZQL)+ZQI/(1.0_JPRB-ZQI))  
    ZTV2=ZTIN(JLON,JLEV)*(1.0_JPRB+ZQVIN(JLON,JLEV)/(1.0_JPRB-ZQVIN(JLON,JLEV))*RV/RD)/&
     & (1.0_JPRB+ZQVIN(JLON,JLEV)/(1.0_JPRB-ZQVIN(JLON,JLEV)) )  
    ZBUOY=(ZTV1/ZTV2-1.0_JPRB)*(RD+(RV-RD)*ZQVIN(JLON,JLEV))*ZTIN(JLON,JLEV)
    !
    !-------------------------------------------------
    ! CIN AND CAPE INTEGRALS.
    !-------------------------------------------------
    !
    ZRT=0.5_JPRB*(ZBUOY+ZBUOYPREC)*ZDLOG
    !------------------------------------------
    ! CUMULATE CAPE IF POSITIVE CONTRIBUTION.
    !------------------------------------------
    PCAPE=PCAPE+MAX(0.0_JPRB,ZRT)
    !------------------------------------------
    ! CUMULATE CIN IF NEGATIVE CONTRIBUTION AND BELOW LFC.
    !------------------------------------------
    IF(PCAPE == 0.0_JPRB) PCIN=PCIN+MIN(0.0_JPRB,ZRT)
    ZBUOYPREC=ZBUOY
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
    ZT1=ZT
  
  
    ZQV1=FOQS(FOEW(ZT1,MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZT1)))/PRP(JLON,JLEV))  
    IF ((ZQV1<=ZQV).AND.(KLCL==-1)) THEN
      KLCL=JLEV
    ENDIF
    IF ((PCAPE > 0.0_JPRB).AND.(KFCL==-1)) THEN
      KFCL=JLEV
    ENDIF
    IF ((ZRT <= 0.0_JPRB).AND.(JLEV<KFCL).AND.(JLEV<IPREVIOUS_NULL_ZRT-1)) THEN
      KLNB=JLEV
    ENDIF
    IF (ZRT <= 0.0_JPRB) THEN
      IPREVIOUS_NULL_ZRT=JLEV
    ENDIF
  

  DO JIT=1,NCAPEITER
    
      !
      !-------------------------------------------------
      ! LATENT HEAT
      !-------------------------------------------------
      !
      ZDELARG0=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZT1))
      ZL0=FOLH(ZT1,ZDELARG0)
      ZFDERFOLH0=-RV*(RGAMW+ZDELARG0*RGAMD)
      ZCP0=RCPD*(1.0_JPRB-ZQV1)+RCPV*ZQV1
      !
      !-------------------------------------------------
      ! Newton's loop to solve the supersaturation.
      !-------------------------------------------------
      !
    
    
      ZZFOEW=FOEW(ZT1,ZDELARG0)
    
    
      ZFDERQS0=FODQS(ZQV1,ZZFOEW/PRP(JLON,JLEV),FODLEW(ZT1,ZDELARG0))
      ZT1=MAX(ZMINT,MIN(ZMAXT,ZT1-(ZCP0*(ZT1-ZT)+ZL0*(ZQV1-ZQV))&
       & /MAX(ZMINDERI,ZCP0+((RCPV-RCPD)*(ZT1-ZT)+ZL0)* ZFDERQS0 &
       & +(ZQV1-ZQV)*ZFDERFOLH0)))
      ZDELARG0=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZT1))
    
    
      ZZFOEW=FOEW(ZT1,ZDELARG0)
    
    
      !
      !-------------------------------------------------
      ! SATURATION SPECIFIC HUMIDITY
      !-------------------------------------------------
      !
      ZQV1=FOQS(ZZFOEW/PRP(JLON,JLEV))  
    
  ENDDO

  
    ZADD=GCAPERET*(ZQV-ZQV1)
                        
    ZSQL=ZQL+ZADD*MAX(0.0_JPRB,SIGN(1.0_JPRB,ZT1-RTT))
    ZSQI=ZQI+ZADD*MAX(0.0_JPRB,-SIGN(1.0_JPRB,ZT1-RTT))
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
    ZZT=ZT1
  
  !-------------------------------------------------
  ! CALCULATION IS DIVIDED IN MORE STEPS IN ORDER
  ! TO GET MORE PRECISE VALUES
  !-------------------------------------------------
  DO JETAPES=1,NETAPES
    
      ZPDEPART =PRP(JLON,JLEV)+(PRP(JLON,JLEV-1)-PRP(JLON,JLEV))*REAL(JETAPES-1)/REAL(NETAPES)  
      ZPARRIVEE=PRP(JLON,JLEV)+(PRP(JLON,JLEV-1)-PRP(JLON,JLEV))*REAL(JETAPES)/REAL(NETAPES)  
      ZTDEPART=ZZT
    
    
      ZQVDEPART=FOQS(FOEW(ZTDEPART,MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTDEPART))) &
       & /ZPDEPART)
      ZLOG=LOG(ZPARRIVEE/ZPDEPART)
    
    DO JIT=1,NCAPEITER
      
        ZZFOEW=FOEW(ZZT,MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZZT)))
      
      
        !
        !-------------------------------------------------
        ! SATURATION SPECIFIC HUMIDITY
        !-------------------------------------------------
        !
        ZZQV=FOQS(ZZFOEW/ZPARRIVEE)  
        !
        !-------------------------------------------------
        ! LATENT HEAT
        !-------------------------------------------------
        !
        ZDELARG=MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTDEPART))
        !
        ! CALCULATION WHERE IT IS CONSIDERED THAT LATENT HEAT RELEASE
        ! FROM CONDENSATION IS ABSORBTED ONLY BY THE GASEOUS PORTION 
        ! OF THE PARCEL, AND NOT BY THE CONDENSATE
        !
        ZL=FOLH(ZZT,ZDELARG)  
        ZDERL=-RV*(RGAMW+ZDELARG*RGAMD)
        !
        !-------------------------------------------------
        ! Newton's loop to solve the moist adiabatic ascent.
        !-------------------------------------------------
        !
        ZRDLOG=(RD+(RV-RD)*ZZQV)*ZLOG
        ZCP=RCPD*(1.0_JPRB-ZZQV)+RCPV*ZZQV 
        ZFDERQS=FODQS(ZZQV,ZZFOEW/ZPARRIVEE &
          & ,FODLEW(ZZT,MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZZT))))
        ZZT=MAX(ZMINT,MIN(ZMAXT,ZZT-(ZCP*(ZZT-ZTDEPART) &
         & +ZL*(ZZQV-ZQVDEPART)-ZZT &
         & *ZRDLOG)/MAX(ZMINDERI,ZCP+((RCPV-RCPD)*(ZZT-ZTDEPART) &
         & +ZL-ZLOG*ZZT*(RV-RD))*ZFDERQS &
         & +(ZZQV-ZQVDEPART)*ZDERL-ZRDLOG)))
            ! JLON
    ENDDO     ! JIT
  ENDDO   !JETAPES
  
  
    ZZFOEW=FOEW(ZZT,MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZZT)))
    !
    !-------------------------------------------------
    ! DRY ADIABATIC ASCENT.
    !-------------------------------------------------
    !
    ZDT=ZT*(PRP(JLON,JLEV-1)/PRP(JLON,JLEV))**((RD+(RV-RD) &
     & *ZQV)/(RCPD*(1.0_JPRB-ZQV)+RCPV*ZQV))  
    !
  
  
    ZT2=ZZT

    ZQV2=FOQS(ZZFOEW/PRP(JLON,JLEV-1))

    ZADD=GCAPERET*(ZQV1-ZQV2)*MAX(0.0_JPRB,SIGN(1.0_JPRB,ZQV1-ZQV2))
    ZSQL=ZSQL+ZADD*MAX(0.0_JPRB,SIGN(1.0_JPRB,ZT2-RTT))
    ZSQI=ZSQI+ZADD*MAX(0.0_JPRB,-SIGN(1.0_JPRB,ZT2-RTT))
    !
    !-------------------------------------------------
    ! UPDATE PARCEL STATE. The result of moist adiabatic ascent.
    !-------------------------------------------------
    !
    ZST=ZT2
    ZSQV=ZQV2
    !-------------------------------------------------
    !  VALUES FROM DRY ADIABATIC OR MOIST ADIABATIC ASCENT ARE CHOSEN 
    !-------------------------------------------------
    !
    ZADD=MAX(0.0_JPRB,SIGN(1.0_JPRB,ZQV-ZQS))
    ZT=ZST*ZADD + ZDT*(1.0_JPRB-ZADD)
    ZQV=ZSQV*ZADD + ZQV*(1.0_JPRB-ZADD)
    ZQL=ZSQL*ZADD
    ZQI=ZSQI*ZADD
    !-----------------------------------------------------
    ! Entrainement
    !-----------------------------------------------------
    IF (RENTRA > 0.0_JPRB) THEN
      ZDZ=(PRP(JLON,JLEV)-PRP(JLON,JLEV-1))/(0.5*(PRP(JLON,JLEV)+ &
        & PRP(JLON,JLEV-1)))*(RD+(RV-RD)*(0.5*(ZQVIN(JLON,JLEV)+ &
        & (ZQVIN(JLON,JLEV-1)))))*0.5*(ZTIN(JLON,JLEV)+(ZTIN(JLON,JLEV-1)))/RG
      IF(ZT > ZTIN(JLON,JLEV-1)) THEN
        ZT=MAX(ZTIN(JLON,JLEV-1),ZT+RENTRA*ZDZ*(ZTIN(JLON,JLEV-1)-ZT))
      ELSE
        ZT=MIN(ZTIN(JLON,JLEV-1),ZT+RENTRA*ZDZ*(ZTIN(JLON,JLEV-1)-ZT))
      ENDIF
      IF(ZQV > ZQVIN(JLON,JLEV-1)) THEN
        ZQV=MAX(ZQVIN(JLON,JLEV-1),ZQV+RENTRA*ZDZ*(ZQVIN(JLON,JLEV-1)-ZQV))
      ELSE
        ZQV=MIN(ZQVIN(JLON,JLEV-1),ZQV+RENTRA*ZDZ*(ZQVIN(JLON,JLEV-1)-ZQV))
      ENDIF
    ENDIF
                       ! JLON
ENDDO                     !JLEV


END SUBROUTINE FPCINCAPE
