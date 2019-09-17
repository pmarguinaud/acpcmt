#FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -vec-report3 -fPIC  -align array64byte
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX -ftz  -fast-transcendentals

FRTFLAGS = -convert big_endian -assume byterecl -traceback -qopenmp -qopenmp-threadprivate compat -vec-report3 -fPIC
#OPT_FRTFLAGS = -fp-model source -g -O2 -ip -xAVX
OPT_FRTFLAGS = -fp-model source -g -O2 -ip -check bounds -debug full

#FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I161150/ifort $(FRTFLAGS) $(OPT_FRTFLAGS)
#FC = /home/gmap/mrpm/marguina/install/gmkpack_support/wrapper/I161150/ifort $(FRTFLAGS) -g -O0

TOP=..

ifeq ($(ARCH), CPU) 
FC = pgf90 -DCPU  -mp -byteswapio -Mlarge_arrays -I$(TOP) -g
endif

ifeq ($(ARCH), GPU)
FC = pgf90 -DGPU  -mp -byteswapio -Mlarge_arrays -Mcuda -Mcuda=lineinfo -I$(TOP)
endif

all: wrap_acpcmt.x

MODULES=parkind1.o yemgeo.o yomcst.o yomct0.o yomct3.o yomdim.o yomgem.o yomphy.o yomphy0.o yomphy2.o yomrip.o yomscm.o load_mod.o yomcape.o yomfpc.o yomphy1.o yomtoph.o xrd_getoptions.o xrd_unix_env.o

xrd_getoptions.o: $(TOP)/xrd_getoptions.F90 xrd_unix_env.o parkind1.o
	$(FC) -c $(TOP)/xrd_getoptions.F90

xrd_unix_env.o: $(TOP)/xrd_unix_env.F90 parkind1.o
	$(FC) -c $(TOP)/xrd_unix_env.F90

fcgeneralized_gamma.o: $(TOP)/fcgeneralized_gamma.F90 $(MODULES)
	$(FC) -c $(TOP)/fcgeneralized_gamma.F90

load_mod.o: $(TOP)/load_mod.F90 parkind1.o
	$(FC) -c $(TOP)/load_mod.F90

yomrip.o: $(TOP)/yomrip.F90 parkind1.o
	$(FC) -c $(TOP)/yomrip.F90

yomphy.o: $(TOP)/yomphy.F90 parkind1.o
	$(FC) -c $(TOP)/yomphy.F90

acmtud.o: $(TOP)/acmtud.F90 $(MODULES)
	$(FC) -c $(TOP)/acmtud.F90

acadvec.o: $(TOP)/acadvec.F90 $(MODULES)
	$(FC) -c $(TOP)/acadvec.F90

acpcmt_load_all.o: $(TOP)/acpcmt_load_all.F90 $(MODULES)
	$(FC) -c $(TOP)/acpcmt_load_all.F90

acnebxrs.o: $(TOP)/acnebxrs.F90 $(MODULES)
	$(FC) -c $(TOP)/acnebxrs.F90

yomphy1.o: $(TOP)/yomphy1.F90 parkind1.o
	$(FC) -c $(TOP)/yomphy1.F90

wrap_acpcmt.o: $(TOP)/wrap_acpcmt.F90 $(MODULES)
	$(FC) -c $(TOP)/wrap_acpcmt.F90

acpcmt.o: $(TOP)/acpcmt.F90 $(MODULES)
	$(FC) -c $(TOP)/acpcmt.F90

yomfpc.o: $(TOP)/yomfpc.F90 parkind1.o
	$(FC) -c $(TOP)/yomfpc.F90

yomcape.o: $(TOP)/yomcape.F90 parkind1.o
	$(FC) -c $(TOP)/yomcape.F90

yomphy2.o: $(TOP)/yomphy2.F90 parkind1.o
	$(FC) -c $(TOP)/yomphy2.F90

acnebsm.o: $(TOP)/acnebsm.F90 $(MODULES)
	$(FC) -c $(TOP)/acnebsm.F90

yomdim.o: $(TOP)/yomdim.F90 parkind1.o
	$(FC) -c $(TOP)/yomdim.F90

acpluiz.o: $(TOP)/acpluiz.F90 $(MODULES)
	$(FC) -c $(TOP)/acpluiz.F90

acmtentr.o: $(TOP)/acmtentr.F90 $(MODULES)
	$(FC) -c $(TOP)/acmtentr.F90

acmicro.o: $(TOP)/acmicro.F90 $(MODULES)
	$(FC) -c $(TOP)/acmicro.F90

yomtoph.o: $(TOP)/yomtoph.F90 parkind1.o
	$(FC) -c $(TOP)/yomtoph.F90

yomct0.o: $(TOP)/yomct0.F90 parkind1.o
	$(FC) -c $(TOP)/yomct0.F90

yomscm.o: $(TOP)/yomscm.F90 parkind1.o
	$(FC) -c $(TOP)/yomscm.F90

advprcs.o: $(TOP)/advprcs.F90 $(MODULES)
	$(FC) -c $(TOP)/advprcs.F90

yomcst.o: $(TOP)/yomcst.F90 parkind1.o
	$(FC) -c $(TOP)/yomcst.F90

yemgeo.o: $(TOP)/yemgeo.F90 parkind1.o
	$(FC) -c $(TOP)/yemgeo.F90

yomct3.o: $(TOP)/yomct3.F90 parkind1.o
	$(FC) -c $(TOP)/yomct3.F90

parkind1.o: $(TOP)/parkind1.F90 
	$(FC) -c $(TOP)/parkind1.F90

yomphy0.o: $(TOP)/yomphy0.F90 parkind1.o
	$(FC) -c $(TOP)/yomphy0.F90

yomgem.o: $(TOP)/yomgem.F90 parkind1.o
	$(FC) -c $(TOP)/yomgem.F90

abor1.o: $(TOP)/abor1.F90
	$(FC) -c $(TOP)/abor1.F90

run_acpcmt.o: $(TOP)/run_acpcmt.F90
	$(FC) -c $(TOP)/run_acpcmt.F90

wrap_acpcmt.x: yomrip.o yomphy.o acmtud.o acadvec.o acpcmt_load_all.o acnebxrs.o yomphy1.o wrap_acpcmt.o acpcmt.o yomfpc.o yomcape.o yomphy2.o acnebsm.o yomdim.o acpluiz.o acmtentr.o acmicro.o yomtoph.o yomct0.o yomscm.o advprcs.o yomcst.o yemgeo.o yomct3.o parkind1.o yomphy0.o yomgem.o fcgeneralized_gamma.o load_mod.o abor1.o xrd_unix_env.o xrd_getoptions.o run_acpcmt.o
	$(FC) -o wrap_acpcmt.x yomrip.o yomphy.o acmtud.o acadvec.o acpcmt_load_all.o acnebxrs.o yomphy1.o wrap_acpcmt.o acpcmt.o yomfpc.o yomcape.o yomphy2.o acnebsm.o yomdim.o acpluiz.o acmtentr.o acmicro.o yomtoph.o yomct0.o yomscm.o advprcs.o yomcst.o yemgeo.o yomct3.o parkind1.o yomphy0.o yomgem.o fcgeneralized_gamma.o load_mod.o abor1.o xrd_unix_env.o xrd_getoptions.o run_acpcmt.o

clean:
	\rm -f *.o *.x *.mod *.xml *.optrpt

