#####################################################
###                                               ###
###     Makefile for 1D planet climate model      ###
###                                               ###
###         Duncan H. Forgan 2/8/2010             ###
###       				          ###
###                                               ###
#####################################################

# Compiler variables:
FC     = gfortran

FFLAGS = -O3 -frecord-marker=4 -fdefault-real-8  

# Create object files:
%.o: %.f
	$(FC) $(FFLAGS) -c $<
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Source files (.F90)
SOURCESAF90 = datamodule.f90 main.f90 adjustpCO2_spiegel.f90 calc_params.f90 initial.f90 integrate.f90 output.f90 timestep.f90 WK97cooling.f90
OBJECTSA    = $(SOURCESAF90:.f90=.o)

# Create executable files:
build: planet_1DEBM

planet_1DEBM:  $(OBJECTSA)
	$(FC) $(FFLAGS) -o $@ $(OBJECTSA)

# Executables for other useful codes

time_average: time_average.f90
	 $(FC) $(FFLAGS) time_average.f90 -o time_average
	rm -f time_average.o

equilibrium: equilibrium.f90
	 $(FC) $(FFLAGS) equilibrium.f90 -o equilibrium
	rm -f equilibrium.o

# Clean statements:
clean: 
	\rm *.o *.mod planet_1DEBM equilibrium time_average

# End Makefile
