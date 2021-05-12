#FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -vec-report3 -fPIC  -align array64byte
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX -ftz  -fast-transcendentals

FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -fPIC
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
OPT_FRTFLAGS = -fp-model source -g -O0 -ip -check bounds -debug full

FC = ifort $(FRTFLAGS) $(OPT_FRTFLAGS)
#FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I161150/ifort $(FRTFLAGS) -g -O0
#FC = pgf90 -mp -byteswapio -Mlarge_arrays -Minfo=mp -mp -O0 -acc -ta=tesla:managed,lineinfo -Minfo=accel,all,intensity,ccff -DUSE_ACC
FC = pgf90 -mp -byteswapio -Mlarge_arrays -Minfo=mp -mp -O0 -acc -ta=tesla:lineinfo -Minfo=accel,all,intensity,ccff -DUSE_ACC

all: wrap_acpcmt.x

MODULES=parkind1.o yemgeo.o yomcst.o yomct0.o yomct3.o yomdim.o yomgem.o yomphy.o yomphy0.o yomphy2.o yomrip.o yomscm.o load_mod.o yomcape.o yomfpc.o yomphy1.o yomtoph.o xrd_unix_env.o xrd_getoptions.o

xrd_unix_env.o: xrd_unix_env.F90 parkind1.o
	$(FC) -c xrd_unix_env.F90

xrd_getoptions.o: xrd_unix_env.o xrd_getoptions.F90 parkind1.o
	$(FC) -c xrd_getoptions.F90

fcgeneralized_gamma.o: fcgeneralized_gamma.F90 $(MODULES)
	$(FC) -c fcgeneralized_gamma.F90

load_mod.o: load_mod.F90 parkind1.o
	$(FC) -c load_mod.F90

yomrip.o: yomrip.F90 parkind1.o
	$(FC) -c yomrip.F90

yomphy.o: yomphy.F90 parkind1.o
	$(FC) -c yomphy.F90

acmtud.o: acmtud.F90 $(MODULES)
	$(FC) -c acmtud.F90

acadvec.o: acadvec.F90 $(MODULES)
	$(FC) -c acadvec.F90

acpcmt_load_all.o: acpcmt_load_all.F90 $(MODULES)
	$(FC) -c acpcmt_load_all.F90

acnebxrs.o: acnebxrs.F90 $(MODULES)
	$(FC) -c acnebxrs.F90

yomphy1.o: yomphy1.F90 parkind1.o
	$(FC) -c yomphy1.F90

wrap_acpcmt.o: wrap_acpcmt.F90 $(MODULES)
	$(FC) -c wrap_acpcmt.F90

acpcmt.o: acpcmt.F90 $(MODULES)
	$(FC) -c acpcmt.F90

yomfpc.o: yomfpc.F90 parkind1.o
	$(FC) -c yomfpc.F90

yomcape.o: yomcape.F90 parkind1.o
	$(FC) -c yomcape.F90

yomphy2.o: yomphy2.F90 parkind1.o
	$(FC) -c yomphy2.F90

fpcincape.o: fpcincape.F90 $(MODULES)
	$(FC) -c fpcincape.F90

acnebsm.o: acnebsm.F90 $(MODULES)
	$(FC) -c acnebsm.F90

yomdim.o: yomdim.F90 parkind1.o
	$(FC) -c yomdim.F90

acpluiz.o: acpluiz.F90 $(MODULES)
	$(FC) -c acpluiz.F90

acmtentr.o: acmtentr.F90 $(MODULES)
	$(FC) -c acmtentr.F90

acmicro.o: acmicro.F90 $(MODULES)
	$(FC) -c acmicro.F90

yomtoph.o: yomtoph.F90 parkind1.o
	$(FC) -c yomtoph.F90

yomct0.o: yomct0.F90 parkind1.o
	$(FC) -c yomct0.F90

yomscm.o: yomscm.F90 parkind1.o
	$(FC) -c yomscm.F90

advprcs.o: advprcs.F90 $(MODULES)
	$(FC) -c advprcs.F90

yomcst.o: yomcst.F90 parkind1.o
	$(FC) -c yomcst.F90

yemgeo.o: yemgeo.F90 parkind1.o
	$(FC) -c yemgeo.F90

yomct3.o: yomct3.F90 parkind1.o
	$(FC) -c yomct3.F90

parkind1.o: parkind1.F90 
	$(FC) -c parkind1.F90

yomphy0.o: yomphy0.F90 parkind1.o
	$(FC) -c yomphy0.F90

acmtddd.o: acmtddd.F90 $(MODULES)
	$(FC) -c acmtddd.F90

yomgem.o: yomgem.F90 parkind1.o
	$(FC) -c yomgem.F90

wrap_acpcmt.x: yomrip.o yomphy.o acmtud.o acadvec.o acpcmt_load_all.o acnebxrs.o yomphy1.o wrap_acpcmt.o acpcmt.o yomfpc.o yomcape.o yomphy2.o fpcincape.o acnebsm.o yomdim.o acpluiz.o acmtentr.o acmicro.o yomtoph.o yomct0.o yomscm.o advprcs.o yomcst.o yemgeo.o yomct3.o parkind1.o yomphy0.o acmtddd.o yomgem.o fcgeneralized_gamma.o load_mod.o xrd_unix_env.o xrd_getoptions.o
	$(FC) -o wrap_acpcmt.x yomrip.o yomphy.o acmtud.o acadvec.o acpcmt_load_all.o acnebxrs.o yomphy1.o wrap_acpcmt.o acpcmt.o yomfpc.o yomcape.o yomphy2.o fpcincape.o acnebsm.o yomdim.o acpluiz.o acmtentr.o acmicro.o yomtoph.o yomct0.o yomscm.o advprcs.o yomcst.o yemgeo.o yomct3.o parkind1.o yomphy0.o acmtddd.o yomgem.o fcgeneralized_gamma.o load_mod.o xrd_unix_env.o xrd_getoptions.o

clean:
	\rm -f *.o *.x *.mod *.xml *.optrpt

