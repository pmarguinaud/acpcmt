MODULE YOMCT3


#include "create.h"

USE PARKIND1  ,ONLY : JPIM

IMPLICIT NONE

SAVE

!     ------------------------------------------------------------------

!*    Control variables for the model  (changed at level 3)

! NSTEP       : current timestep of model
INTEGER(KIND=JPIM) :: NSTEP

create (NSTEP)
!     ------------------------------------------------------------------
END MODULE YOMCT3
