MODULE YOMADVPRCS


#include "create.h"

USE PARKIND1

LOGICAL :: LLMELTS,LLFREEZ
create (LLFREEZ)
create (LLMELTS)
LOGICAL :: LLEVAPX
create (LLEVAPX)
REAL (KIND=JPRB) :: ZPREF, ZEPS,  &
 & ZFVELR,ZFVELS,ZTMELT,ZRHOW,ZNRHOW,                        &
 & ZNU1,ZNU2,ZTAU1,ZTAU2,ZSIGMA1,ZSIGMA2,                    &
 & ZDVISC,ZSQTVIS,ZCDARV,ZRHOREF,ZEXP1,ZEXP4,ZEXP6,          &
 & ZCOEFF1,ZCOEFF2,ZCOEFF2B,ZCOEFF3,ZCOEFF4,ZCOEFF5,ZCOEFF6, &
 & ZFVENTR1,ZFVENTR2,ZFVENTS1,ZFVENTS2                    


create (ZFVENTS2)
create (ZFVENTS1)
create (ZFVENTR2)
create (ZFVENTR1)
create (ZCOEFF6)
create (ZCOEFF5)
create (ZCOEFF4)
create (ZCOEFF3)
create (ZCOEFF2B)
create (ZCOEFF2)
create (ZCOEFF1)
create (ZEXP6)
create (ZEXP4)
create (ZEXP1)
create (ZRHOREF)
create (ZCDARV)
create (ZSQTVIS)
create (ZDVISC)
create (ZSIGMA2)
create (ZSIGMA1)
create (ZTAU2)
create (ZTAU1)
create (ZNU2)
create (ZNU1)
create (ZNRHOW)
create (ZRHOW)
create (ZTMELT)
create (ZFVELS)
create (ZFVELR)
create (ZEPS)
create (ZPREF)
END MODULE
