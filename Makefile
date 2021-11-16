.SUFFIXES:      (.SUFFIXES) .f90 .f

## Compilers and flags ##
FC      = gfortran
FCFLAGS = -g 

## Linker and flags ##
LD      = $(FC)
LDFLAGS = -g 
LIBS    =

## Compilation directives ##

.f90.o:
	$(FC) -c $(FCFLAGS) $*.f90

MOBJECTS = main.o

BOBJECTS=nrtype.o\
	phys_consts.o\
	Laguerre_fast.o\
	g_mvt.o\
	WCA.o\
	dielectric.o\
	diel_models.o

OBJECTS =  $(BOBJECTS) $(MOBJECTS)

## Executable ##
CMD =  diel_const

## Make directives ##

$(CMD): $(OBJECTS)
		$(LD) -o $(CMD) $(LDFLAGS) $(OBJECTS) $(LIBS)

clean:
		/bin/rm *.o *.mod

distclean:
		/bin/rm *.o *.mod $(CMD)
