#####################################################
###                                               ###
###     Makefile for 1D planet climate model      ###
###                                               ###
###         Duncan H. Forgan 2/8/2010             ###
###       				          ###
###                                               ###
#####################################################

# Compiler variables for WKMR/DHF:
FC     = gfortran

# For big endian files generated from stacpolly use these flags
#FFLAGS = -O3 -fPIC -frecord-marker=4 -fconvert=swap -fdefault-real-8

# For files generated from stacpolly use these flags
FFLAGS = -O3 -frecord-marker=4 -fdefault-real-8  

# For files generated on seaforth use these flags
#FFLAGS = -O3 -frecord-marker=4  -Wall

# Create object files:
%.o: %.f
	$(FC) $(FFLAGS) -c $<
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

SOURCESAF90 = datamodule.f90 main.f90 calc_params.f90 initial.f90 integrate.f90 output.f90 timestep.f90
OBJECTSA    = $(SOURCESAF90:.f90=.o)

# Create executable files:
build: planet_1DEBM

planet_1DEBM:  $(OBJECTSA)
	$(FC) $(FFLAGS) -o $@ $(OBJECTSA)

# Clean statements:
clean: 
	\rm *.o *.mod planet_1DEBM

# End Makefile
