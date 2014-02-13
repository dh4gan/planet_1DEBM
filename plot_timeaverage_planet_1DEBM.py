# Written 17/1/14 by dh4gan
# Code reads in a sample of snapshots from EBM and plots them

import matplotlib.pyplot as plt
import numpy as np
from string import split

# File order
# 0 x
# 1 latitude
# 2 T
# 3 C
# 4 Q
# 5 IR
# 6 albedo
# 7 insolation
# 8 tau
# 9 ice fraction
# 10 habitability index
# 11 Radian Half Day Length


# Set up tuples and dictionaries

variablekeys = ("x","lat", "T", "C", "Q","IR","Albedo", "S", 
                "tau", "ice","hab", "H")
variablenames = ("x", r"$\lambda$", "T (K)", "C (/)", r"Net Heating ($erg\,s^{-1}\, cm^{-2}$)",  
                 r"IR Cooling ($erg\,s^{-1}\, cm^{-2}$)",r"Albedo", r"Mean Insolation ($erg\,s^{-1}\, cm^{-2}$)", 
                 "Optical Depth", r"$f_{ice}$","Habitability Index", "Half Day Length (rad)")                  
variablecolumns = (0,1,2,3,4,5,6,7,8,9,10,11)

nvar = len(variablekeys)

namedict = {}
coldict = {}

for i in range(len(variablekeys)):
    namedict[variablekeys[i]] = variablenames[i]
    coldict[variablekeys[i]] = variablecolumns[i]


# Open log file and read contents

prefix = raw_input("What is the run prefix? ")
t_interval = input ("What is the averaging interval (years)?")

initialdump = input("Which dump to start from? ")

# Read the header of the first file to find the dump frequency
inputfile = prefix+'.'+str(initialdump)
f = open(inputfile, 'r')
line = f.readline()

numbers = split(line)

nlat = int(numbers[0])
dumpfreq=float(numbers[4])        

print "Dumps made every ",dumpfreq, "years "

nfiles = np.int(t_interval/dumpfreq)+1

print "This corresponds to ",nfiles, "files"


# Define x axis as latitude (always)
xkey = 'lat'
ix = coldict[xkey]

# Pick variable to time average
print "Which variable is to be time averaged?"

for i in range(len(variablekeys)):
    if(i!=ix): print variablekeys[i],":\t \t", namedict[variablekeys[i]]

keyword = raw_input("Enter appropriate keyword:   ")

ykey = keyword
iy = coldict[keyword]

outputfile = ykey+'_timeaveraged_'+str(t_interval)+'yr.ps'

alldata = []

for i in range(nfiles):
    
    idump = initialdump + i
    
    # Read in data
    inputfile = prefix+'.'+str(idump)        
    
    data = np.genfromtxt(inputfile,skiprows=1)
    
    if(i==0):
        xdata = data[:,ix]
    
    data = data[:,iy]
    # Add to totals
    alldata.append(data)

# Calculate mean, standard deviation

mean= np.zeros((alldata[0].shape))
sd= np.zeros((alldata[0].shape))

nrows = alldata[0].shape[0]


for idump in range(nfiles):    
    mean[:] = mean[:] + alldata[idump][:]

mean = mean/nfiles

for idump in range(nfiles):    
        sd[:] = sd[:] + (mean[:]-alldata[idump][:])**2
        
sd = sd/(nfiles-1)
sd = np.sqrt(sd)

print "Mean and sd calculated"

# Make figure

fig1 = plt.figure()
ax = fig1.add_subplot(111)
ax.set_xlabel(namedict[xkey], fontsize = 16)
ax.set_ylabel(namedict[ykey], fontsize = 16)
ax.plot(xdata,mean)

fig1.savefig(outputfile, format='ps')
