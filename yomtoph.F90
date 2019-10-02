MODULE YOMTOPH

USE PARKIND1  ,ONLY : JPIM     ,JPRB
! This module implements the routines for a simple manual deepcopy. 
  use openacc

IMPLICIT NONE

SAVE

TYPE TTOPH
!*
!     ------------------------------------------------------------------
!     Top limits of parametrization call
!     we have one by parameterization,
!              ETXXXX : Top pressure
!              NTXXXX : Corresponding level in standard atmosphere

!       ETQSAT,ETDIFU,ETCOEF,ETDRAG,ETCVIM,ETPLUI,ETRADI,ETNEBU
!       ETOZON,ETDRME,ETCOET,ETAJUC,NTAJUC
!       NTQSAT,NTDIFU,NTCOEF,NTDRAG,NTCVIM,NTPLUI,NTRADI,NTNEBU
!       NTOZON,NTDRME,NTCOET
!
!       1D model MUSC Same bottom pressure for the NUDGING level 
!               for T, Qv and Wind :
!       ETRELAX, NTRELAX

!     Constants for Mesospheric drag parameterization

!       XDRMUK  : Mesospheric drag coefficient for U and V
!       XDRMUX  : Maximum mesospheric drag coefficient for U and V
!       XDRMUP  : Bottom pressure for U and V
!       XDRMTK  : Mesospheric drag coefficient for T
!       XDRMTX  : Maximum mesospheric drag coefficient for T
!       XDRMTP  : Bottom pressure for T
!       XDRMQK  : Mesospheric drag coefficient for Q
!       XDRMQP  : Bottom pressure for Q
!       XDRMQX  : Maximum mesospheric drag coefficient for Q

!       RMESOU(NFLEVG) : Vertical mesospheric drag profil for U and V
!       RMESOT(NFLEVG) : Vertical mesospheric drag profil for T
!       RMESOQ(NFLEV)  : Vertical mesospheric drag profil for Q

!       RFMESOQ   : Mesospheric water reference

!       RCLX      : Multiplicator for Cl term in ozone photochemistry

!       TPSCLIM   : Temperature threshold for activation of heterogeneous
!                   chemistry (polar stratospheric clouds temperature formation)

REAL(KIND=JPRB),ALLOCATABLE:: RMESOU(:)
REAL(KIND=JPRB),ALLOCATABLE:: RMESOT(:)
REAL(KIND=JPRB),ALLOCATABLE:: RMESOQ(:)

REAL(KIND=JPRB) :: RFMESOQ

INTEGER(KIND=JPIM) :: NTQSAT
INTEGER(KIND=JPIM) :: NTDIFU
INTEGER(KIND=JPIM) :: NTCOEF
INTEGER(KIND=JPIM) :: NTDRAG
INTEGER(KIND=JPIM) :: NTCVIM
INTEGER(KIND=JPIM) :: NTPLUI
INTEGER(KIND=JPIM) :: NTRADI
INTEGER(KIND=JPIM) :: NTNEBU
INTEGER(KIND=JPIM) :: NTOZON
INTEGER(KIND=JPIM) :: NTDRME
INTEGER(KIND=JPIM) :: NTCOET
INTEGER(KIND=JPIM) :: NTAJUC
INTEGER(KIND=JPIM) :: NTRELAXT
INTEGER(KIND=JPIM) :: NTRELAXQ
INTEGER(KIND=JPIM) :: NTRELAXU
REAL(KIND=JPRB) :: ETQSAT
REAL(KIND=JPRB) :: ETDIFU
REAL(KIND=JPRB) :: ETCOEF
REAL(KIND=JPRB) :: ETDRAG
REAL(KIND=JPRB) :: ETCVIM
REAL(KIND=JPRB) :: ETPLUI
REAL(KIND=JPRB) :: ETRADI
REAL(KIND=JPRB) :: ETNEBU
REAL(KIND=JPRB) :: ETOZON
REAL(KIND=JPRB) :: ETDRME
REAL(KIND=JPRB) :: ETCOET
REAL(KIND=JPRB) :: ETAJUC
REAL(KIND=JPRB) :: ETRELAXT
REAL(KIND=JPRB) :: ETRELAXQ
REAL(KIND=JPRB) :: ETRELAXU
REAL(KIND=JPRB) :: XDRMUK
REAL(KIND=JPRB) :: XDRMUX
REAL(KIND=JPRB) :: XDRMUP
REAL(KIND=JPRB) :: XDRMTK
REAL(KIND=JPRB) :: XDRMTX
REAL(KIND=JPRB) :: XDRMTP
REAL(KIND=JPRB) :: XDRMQK
REAL(KIND=JPRB) :: XDRMQP
REAL(KIND=JPRB) :: XDRMQX

REAL(KIND=JPRB) :: RCLX

REAL(KIND=JPRB) :: TPSCLIM

END TYPE TTOPH

TYPE(TTOPH) :: YRTOPH 
!$acc declare create(YRTOPH)
!     ------------------------------------------------------------------

! debugging flag, when on it enforces data transfers in acc_manual_copyin / acc_manual_copyout, 
! even if the data is already / still present, respectively
  logical,save:: acc_debug_on=.false.

! these generic interfaces allow using the same name in calls in the main code, no matter 
! which type is passed

! deep copyin of the derived type variable
  interface acc_manual_copyin
     module procedure copyin_ttoph
  end interface acc_manual_copyin

! deep delete of the derived type variable
  interface acc_manual_delete
     module procedure delete_ttoph
  end interface acc_manual_delete

! deep copyout of the derived type variable
  interface acc_manual_copyout
     module procedure copyout_ttoph
  end interface acc_manual_copyout

! deep update host of the derived type variable
  interface acc_manual_upd_host
     module procedure upd_host_ttoph
  end interface acc_manual_upd_host

! deep update device of the derived type variable
  interface acc_manual_upd_dev
     module procedure upd_dev_ttoph
  end interface acc_manual_upd_dev

! shallow update host of the derived type variable. Only the immediate members are transferred,
! but members which are themselves of derived type are left untouched
  interface acc_manual_upd_host_shallow
     module procedure upd_host_ttoph_shallow
  end interface acc_manual_upd_host_shallow

! shallow update device of the derived type variable. Only the immediate members are transferred,
! but members which are themselves of derived type are left untouched
  interface acc_manual_upd_dev_shallow
     module procedure upd_dev_ttoph_shallow
  end interface acc_manual_upd_dev_shallow

contains

!============================================
! copyin routines
  subroutine copyin_ttoph(ttoph_var)
    type(ttoph):: ttoph_var

! If debugging flag is set, enforce a data transfer even if data is already present.
! Note hat we're using the 'shallow' variant of the update routines here, because we only want to
! update the members on the current level of the type. As this copyin routine calls back into the
! generic interface for the lower level derived type members, these will call corresponding 
! shallow updates on their respective levels as well. This avoids doing a full, deep update here,
! and then subsequently issuing transfers of the lower level data later, which would only 
! duplicate those transfers of the lower level derived type members.
    if(acc_debug_on)then
       call acc_manual_upd_dev_shallow(ttoph_var)
    endif

!$acc enter data copyin(ttoph_var)
!$acc enter data copyin(ttoph_var%RMESOU,ttoph_var%RMESOT,ttoph_var%RMESOQ)

  end subroutine copyin_ttoph


!============================================
!delete routines
  subroutine delete_ttoph(ttoph_var)
    type(ttoph):: ttoph_var

!$acc exit data delete(ttoph_var%RMESOU,ttoph_var%RMESOT,ttoph_var%RMESOQ)
!$acc exit data delete(ttoph_var)
  end subroutine delete_ttoph


!============================================
!copyout routines
  subroutine copyout_ttoph(ttoph_var)
    type(ttoph):: ttoph_var

! if debugging flag is set, enforce a data transfer even if data is already present 
! Note hat we're using the 'shallow' variant of the update routines here, because we only want to
! update the members on the current level of the type. As this copyin routine calls back into the
! generic interface for the lower level derived type members, these will call corresponding 
! shallow updates on their respective levels as well. This avoids doing a full, deep update here,
! and then subsequently issuing transfers of the lower level data later, which would only 
! duplicate those transfers of the lower level derived type members.

!    if(acc_debug_on)then
!       call acc_manual_upd_hostv_shallow(ttoph_var)
!    endif

!$acc exit data copyout(ttoph_var%RMESOU,ttoph_var%RMESOT,ttoph_var%RMESOQ)
!$acc exit data copyout(ttoph_var)
  end subroutine copyout_ttoph


!============================================
!update host routines. These are the 'deep' variants, which traverse the full type tree and
! transfer lower level derived type data as well.
  subroutine upd_host_ttoph(ttoph_var)
   type(ttoph):: ttoph_var

! update all the scalar members

!$acc update host(ttoph_var%RFMESOQ)
!$acc update host(ttoph_var%NTQSAT)
!$acc update host(ttoph_var%NTDIFU)
!$acc update host(ttoph_var%NTCOEF)
!$acc update host(ttoph_var%NTDRAG)
!$acc update host(ttoph_var%NTCVIM)
!$acc update host(ttoph_var%NTPLUI)
!$acc update host(ttoph_var%NTRADI)
!$acc update host(ttoph_var%NTNEBU)
!$acc update host(ttoph_var%NTOZON)
!$acc update host(ttoph_var%NTDRME)
!$acc update host(ttoph_var%NTCOET)
!$acc update host(ttoph_var%NTAJUC)
!$acc update host(ttoph_var%NTRELAXT)
!$acc update host(ttoph_var%NTRELAXQ)
!$acc update host(ttoph_var%NTRELAXU)
!$acc update host(ttoph_var%ETQSAT)
!$acc update host(ttoph_var%ETDIFU)
!$acc update host(ttoph_var%ETCOEF)
!$acc update host(ttoph_var%ETDRAG)
!$acc update host(ttoph_var%ETCVIM)
!$acc update host(ttoph_var%ETPLUI)
!$acc update host(ttoph_var%ETRADI)
!$acc update host(ttoph_var%ETNEBU)
!$acc update host(ttoph_var%ETOZON)
!$acc update host(ttoph_var%ETDRME)
!$acc update host(ttoph_var%ETCOET)
!$acc update host(ttoph_var%ETAJUC)
!$acc update host(ttoph_var%ETRELAXT)
!$acc update host(ttoph_var%ETRELAXQ)
!$acc update host(ttoph_var%ETRELAXU)
!$acc update host(ttoph_var%XDRMUK)
!$acc update host(ttoph_var%XDRMUX)
!$acc update host(ttoph_var%XDRMUP)
!$acc update host(ttoph_var%XDRMTK)
!$acc update host(ttoph_var%XDRMTX)
!$acc update host(ttoph_var%XDRMTP)
!$acc update host(ttoph_var%XDRMQK)
!$acc update host(ttoph_var%XDRMQP)
!$acc update host(ttoph_var%XDRMQX)
!$acc update host(ttoph_var%RCLX)
!$acc update host(ttoph_var%TPSCLIM)
!update the content of arrays, leaving the pointers intact
!$acc update host(ttoph_var%RMESOU)
!$acc update host(ttoph_var%RMESOT)
!$acc update host(ttoph_var%RMESOQ)


  end subroutine upd_host_ttoph


!============================================
!update host routines. These are the 'shallow' variants, which only transfer data of intrinsic
! types at the current level, but do not process lower level derived type data.
  subroutine upd_host_ttoph_shallow(ttoph_var)
   type(ttoph):: ttoph_var

! update all the scalar members
!$acc update host(ttoph_var%RFMESOQ)
!$acc update host(ttoph_var%NTQSAT)
!$acc update host(ttoph_var%NTDIFU)
!$acc update host(ttoph_var%NTCOEF)
!$acc update host(ttoph_var%NTDRAG)
!$acc update host(ttoph_var%NTCVIM)
!$acc update host(ttoph_var%NTPLUI)
!$acc update host(ttoph_var%NTRADI)
!$acc update host(ttoph_var%NTNEBU)
!$acc update host(ttoph_var%NTOZON)
!$acc update host(ttoph_var%NTDRME)
!$acc update host(ttoph_var%NTCOET)
!$acc update host(ttoph_var%NTAJUC)
!$acc update host(ttoph_var%NTRELAXT)
!$acc update host(ttoph_var%NTRELAXQ)
!$acc update host(ttoph_var%NTRELAXU)
!$acc update host(ttoph_var%ETQSAT)
!$acc update host(ttoph_var%ETDIFU)
!$acc update host(ttoph_var%ETCOEF)
!$acc update host(ttoph_var%ETDRAG)
!$acc update host(ttoph_var%ETCVIM)
!$acc update host(ttoph_var%ETPLUI)
!$acc update host(ttoph_var%ETRADI)
!$acc update host(ttoph_var%ETNEBU)
!$acc update host(ttoph_var%ETOZON)
!$acc update host(ttoph_var%ETDRME)
!$acc update host(ttoph_var%ETCOET)
!$acc update host(ttoph_var%ETAJUC)
!$acc update host(ttoph_var%ETRELAXT)
!$acc update host(ttoph_var%ETRELAXQ)
!$acc update host(ttoph_var%ETRELAXU)
!$acc update host(ttoph_var%XDRMUK)
!$acc update host(ttoph_var%XDRMUX)
!$acc update host(ttoph_var%XDRMUP)
!$acc update host(ttoph_var%XDRMTK)
!$acc update host(ttoph_var%XDRMTX)
!$acc update host(ttoph_var%XDRMTP)
!$acc update host(ttoph_var%XDRMQK)
!$acc update host(ttoph_var%XDRMQP)
!$acc update host(ttoph_var%XDRMQX)
!$acc update host(ttoph_var%RCLX)
!$acc update host(ttoph_var%TPSCLIM)

!update the content of arrays, leaving the pointers intact
!$acc update host(ttoph_var%RMESOU)
!$acc update host(ttoph_var%RMESOT)
!$acc update host(ttoph_var%RMESOQ)

 end subroutine upd_host_ttoph_shallow


!============================================
!update device routines. These are the 'deep' variants, which traverse the full type tree and
! transfer lower level derived type data as well.
  subroutine upd_dev_ttoph(ttoph_var)
   type(ttoph):: ttoph_var

!$acc update device(ttoph_var%RFMESOQ) if_present
!$acc update device(ttoph_var%NTQSAT) if_present
!$acc update device(ttoph_var%NTDIFU) if_present
!$acc update device(ttoph_var%NTCOEF) if_present
!$acc update device(ttoph_var%NTDRAG) if_present
!$acc update device(ttoph_var%NTCVIM) if_present
!$acc update device(ttoph_var%NTPLUI) if_present
!$acc update device(ttoph_var%NTRADI) if_present
!$acc update device(ttoph_var%NTNEBU) if_present
!$acc update device(ttoph_var%NTOZON) if_present
!$acc update device(ttoph_var%NTDRME) if_present
!$acc update device(ttoph_var%NTCOET) if_present
!$acc update device(ttoph_var%NTAJUC) if_present
!$acc update device(ttoph_var%NTRELAXT) if_present
!$acc update device(ttoph_var%NTRELAXQ) if_present
!$acc update device(ttoph_var%NTRELAXU) if_present
!$acc update device(ttoph_var%ETQSAT) if_present
!$acc update device(ttoph_var%ETDIFU) if_present
!$acc update device(ttoph_var%ETCOEF) if_present
!$acc update device(ttoph_var%ETDRAG) if_present
!$acc update device(ttoph_var%ETCVIM) if_present
!$acc update device(ttoph_var%ETPLUI) if_present
!$acc update device(ttoph_var%ETRADI) if_present
!$acc update device(ttoph_var%ETNEBU) if_present
!$acc update device(ttoph_var%ETOZON) if_present
!$acc update device(ttoph_var%ETDRME) if_present
!$acc update device(ttoph_var%ETCOET) if_present
!$acc update device(ttoph_var%ETAJUC) if_present
!$acc update device(ttoph_var%ETRELAXT) if_present
!$acc update device(ttoph_var%ETRELAXQ) if_present
!$acc update device(ttoph_var%ETRELAXU) if_present
!$acc update device(ttoph_var%XDRMUK) if_present
!$acc update device(ttoph_var%XDRMUX) if_present
!$acc update device(ttoph_var%XDRMUP) if_present
!$acc update device(ttoph_var%XDRMTK) if_present
!$acc update device(ttoph_var%XDRMTX) if_present
!$acc update device(ttoph_var%XDRMTP) if_present
!$acc update device(ttoph_var%XDRMQK) if_present
!$acc update device(ttoph_var%XDRMQP) if_present
!$acc update device(ttoph_var%XDRMQX) if_present
!$acc update device(ttoph_var%RCLX) if_present
!$acc update device(ttoph_var%TPSCLIM) if_present

!update the content of arrays, leaving the pointers intact
!$acc update device(ttoph_var%RMESOU) if_present
!$acc update device(ttoph_var%RMESOT) if_present
!$acc update device(ttoph_var%RMESOQ) if_present

  end subroutine upd_dev_ttoph


!============================================
!update device routines. These are the 'shallow' variants, which only transfer data of intrinsic
! types at the current level, but do not process lower level derived type data.
  subroutine upd_dev_ttoph_shallow(ttoph_var)
   type(ttoph):: ttoph_var

!$acc update device(ttoph_var%RFMESOQ) if_present
!$acc update device(ttoph_var%NTQSAT) if_present
!$acc update device(ttoph_var%NTDIFU) if_present
!$acc update device(ttoph_var%NTCOEF) if_present
!$acc update device(ttoph_var%NTDRAG) if_present
!$acc update device(ttoph_var%NTCVIM) if_present
!$acc update device(ttoph_var%NTPLUI) if_present
!$acc update device(ttoph_var%NTRADI) if_present
!$acc update device(ttoph_var%NTNEBU) if_present
!$acc update device(ttoph_var%NTOZON) if_present
!$acc update device(ttoph_var%NTDRME) if_present
!$acc update device(ttoph_var%NTCOET) if_present
!$acc update device(ttoph_var%NTAJUC) if_present
!$acc update device(ttoph_var%NTRELAXT) if_present
!$acc update device(ttoph_var%NTRELAXQ) if_present
!$acc update device(ttoph_var%NTRELAXU) if_present
!$acc update device(ttoph_var%ETQSAT) if_present
!$acc update device(ttoph_var%ETDIFU) if_present
!$acc update device(ttoph_var%ETCOEF) if_present
!$acc update device(ttoph_var%ETDRAG) if_present
!$acc update device(ttoph_var%ETCVIM) if_present
!$acc update device(ttoph_var%ETPLUI) if_present
!$acc update device(ttoph_var%ETRADI) if_present
!$acc update device(ttoph_var%ETNEBU) if_present
!$acc update device(ttoph_var%ETOZON) if_present
!$acc update device(ttoph_var%ETDRME) if_present
!$acc update device(ttoph_var%ETCOET) if_present
!$acc update device(ttoph_var%ETAJUC) if_present
!$acc update device(ttoph_var%ETRELAXT) if_present
!$acc update device(ttoph_var%ETRELAXQ) if_present
!$acc update device(ttoph_var%ETRELAXU) if_present
!$acc update device(ttoph_var%XDRMUK) if_present
!$acc update device(ttoph_var%XDRMUX) if_present
!$acc update device(ttoph_var%XDRMUP) if_present
!$acc update device(ttoph_var%XDRMTK) if_present
!$acc update device(ttoph_var%XDRMTX) if_present
!$acc update device(ttoph_var%XDRMTP) if_present
!$acc update device(ttoph_var%XDRMQK) if_present
!$acc update device(ttoph_var%XDRMQP) if_present
!$acc update device(ttoph_var%XDRMQX) if_present
!$acc update device(ttoph_var%RCLX) if_present
!$acc update device(ttoph_var%TPSCLIM) if_present

!update the content of arrays, leaving the pointers intact
!$acc update device(ttoph_var%RMESOU) if_present
!$acc update device(ttoph_var%RMESOT) if_present
!$acc update device(ttoph_var%RMESOQ) if_present

 end subroutine upd_dev_ttoph_shallow



!     ------------------------------------------------------------------
END MODULE YOMTOPH
