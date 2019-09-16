#include "attr_mod.h"
MODULE YOMDIM

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

TYPE :: TDIM

!*    Dimensions of model working arrays

! === COLLOCATION GRID OF THE DYNAMICS ========================================

! NDGLG  : number of rows of latitudes
! NDGLL  : number of rows of latitudes for which this process is
!          performing Fourier Space calculations
! NDGNH  : number of rows in the northern hemisphere
! NDGSUR : number of additional rows at each pole for horizontal
!          interpolations.
! NDGSAG = -NDGSUR+1
! NDGSAL = Local version of NDGSAG.
! NDGSAH = 1-YRSL%NSLWIDE in DM version.
! NDGSAFPH=1-YRFP%NSLWIDE in DM version.
! NDGENG = NDGLG+NDGSUR
! NDGENL = Number of latitude rows for which this process has grid
!          point calculations to perform.
! NDGENH = NDGENL+YRSL%NSLWIDE in DM version.
! NDGENFPH=NDGENL+YRFP%NSLWIDE in DM version.
! NDGUNG : first row of the area of interest in Aladin
!        = NDGSAG in the global model
! NDGUXG : last  row of the area of interest in Aladin
!        = NDGENG in the global model
! NDGUNL : local first row in C+I zone in distributed memory Aladin
! NDGUXL : local last row in C+I zone in distributed memory Aladin
! NDLON  : length of a row of latitude near equator
! NDSUR1 : over dimensioning of NDLON for technical reasons (at least 2)
! NSTENCILWIDE : max stencil width / 2, default = 2
! NDLSUR = NDLON+NDSUR1
! NDLSM  = NDLSUR-1
! NDLUNG : first meridian of the area of interest in Aladin
!        = 1 in the global model
! NDLUXG : last  meridian of the area of interest in Aladin
!        = NDLON in the global model
! NDLUNL : local first meridian in C+I zone in distributed memory Aladin
! NDLUXL : local last meridian in C+I zone in distributed memory Aladin
! NPROMA : working dimension for grid-point computations
! NPROMA9: working dimension for grid-point computations specifically at t9
! NPROMM : working dimension for Meteo-France physics computations
! NPROMM9 : working dimension for Meteo-France physics computations specifically at t9
! NPROMNH: working dimension for arrays used only in the non hydrostatic model
! NPROMNH9: working dimension for t9 arrays used only in the non hydrostatic model
! NPROMVC: working dimension for t0 or t1 g.p. arrays used only when LVERCOR=T
! NPROMVC9: working dimension for t9 g.p. arrays used only when LVERCOR=T 
! NPROMDLW : working dimension for some g.p. arrays used only when LRWSDLW=T
! NPROMDLR : working dimension for some g.p. arrays used only when LRWSDLR=T
! NPROMDLR2: working dimension for some g.p. arrays used only when LRWSDLR2=T
! NPROMDLG : working dimension for some g.p. arrays used only when LRWSDLG=T
! NGPBLKS: number of grid point NPROMA-blocks.
! LOPTPROMA : .TRUE. NPROMA will be optimised
!           : .FALSE. NPROMA will not be optimised (forced by
!           : negative NPROMA in namelist)

INTEGER(KIND=JPIM) :: NDLON
INTEGER(KIND=JPIM) :: NSMAX

END TYPE TDIM

TYPE(TDIM) ATTR_MOD :: YRDIM 

!     ------------------------------------------------------------------

END MODULE YOMDIM
