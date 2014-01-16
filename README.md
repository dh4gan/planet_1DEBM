This program is designed to produce simple estimates of how the temperature on an Earthlike planet varies as a function of latitude.  This is achieved by solving a 1D diffusion equation based on input stellar radiation, planetary atmospheric albedo and optical depth, amongst other properties such as the planet's ocean fraction and its orbit.

An explanation of the algorithms can be found in Spiegel et al (2008), Astrophysical Journal vol 681, issue 2, pp 1609-1623
(Free link available at http://arxiv.org/abs/0711.4856)

The code is compiled using the Makefile (HEALTH WARNING: tested against gfortran but not others) by typing 'make'

Two other codes are supplied, compiled by typing:

'make time_average' to produce a code which averages snapshots from the simulation
'make equilibrium' to produce a code which calculates the input albedo, cooling and other functions as a function of temperature for plotting

Currently, no plotting scripts are supplied with this code!
