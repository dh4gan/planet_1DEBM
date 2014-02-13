# Written 17/1/14 by dh4gan
# Code reads in log files from EBM and plots them

import matplotlib.pyplot as plt
import numpy as np

# File order
# 0 time
# 1 step
# 2 deltat/yr
# 3 r
# 4 phi
# 5 meanT
# 6 mean IR
# 7 mean S
# 8 mean Q
# 9 max T
# 10 min T
# 11 space averaged habitability index


# Set up tuples and dictionaries

variablekeys = ("t","step", "deltat", "r", "phi","meanT",  "meanIR", "meanS","meanQ", 
                "maxT", "minT", "meanhab")
variablenames = ("Time (yr)", "step", "$\Delta t$ (yr) ", "r (AU)", "$\phi$", "mean T (K)",r"Mean Cooling ($erg\,s^{-1}\, cm^{-2}$)",r"Mean Insolation ($erg\,s^{-1}\, cm^{-2}$)",
                  r"Net Heating ($erg\,s^{-1}\, cm^{-2}$)",  "maximum T (K)", "minimum T (K)",  "Habitability Index")                 
variablecolumns = (0,1,2,3,4,5,6,7,8,9,10,11)

namedict = {}
coldict = {}

for i in range(len(variablekeys)):
    namedict[variablekeys[i]] = variablenames[i]
    coldict[variablekeys[i]] = variablecolumns[i]


# Open log file and read contents

prefix = raw_input("What is the run prefix ? ")

inputfile = prefix+'.log'

print "Which variable for the x-axis?"

for i in range(len(variablekeys)):
    print variablekeys[i],":\t \t \t", namedict[variablekeys[i]]

keyword = raw_input("Enter appropriate keyword:   ")

xkey = keyword
ix = coldict[keyword]

print "Which variable for the y-axis?"

for i in range(len(variablekeys)):
    if(i!=ix): print variablekeys[i],":\t \t", namedict[variablekeys[i]]

keyword = raw_input("Enter appropriate keyword:   ")

ykey = keyword
iy = coldict[keyword]

# Read in data

print 'Reading ',inputfile

data = np.genfromtxt(inputfile)

# Find maximum and minimum time limits to plot

xmin = np.amin(data[:,ix])
xmax = np.amax(data[:,ix])

ymin = np.amin(data[:,iy])
ymax = np.amax(data[:,iy])


print 'Data xmin, xmax: ', xmin, ' , ',xmax
print 'Data ymin, ymax: ', ymin, ' , ',ymax

newlimits = raw_input("Set new x limits? (y/n) ")
if newlimits=='y':
    xmin = input("What is the minimum x value? ")
    xmax = input("What is the maximum x value? ")


    # Find minimum and maximum values of y in this range

    ymin = 1.0e30
    ymax = -1.0e30

    for i in range(data.shape[0]):
        if data[i,ix]>xmin and data[i,ix]<xmax:
            if data[i,iy] < ymin: ymin = data[i,iy]
            if data[i,iy] > ymax: ymax = data[i,iy]
    

    print 'New y limits: ',ymin, ymax

outputfile = ykey+'_vs_'+xkey+'.ps'

# Make figure

fig1 = plt.figure()
ax = fig1.add_subplot(111)
ax.set_xlabel(namedict[xkey], fontsize = 16)
ax.set_ylabel(namedict[ykey], fontsize = 16)
ax.set_xlim(xmin,xmax)
ax.set_ylim(ymin,ymax)
ax.plot(data[:,ix],data[:,iy])

fig1.savefig(outputfile, format='ps')

# Make a compare T figure for the hell of it

xkey = 't'
ix = coldict[xkey]
mincol = coldict['minT']
maxcol = coldict['maxT']
meancol = coldict['meanT']

xmin = np.amin(data[:,ix])
xmax = np.amax(data[:,ix])

print 'Data xmin, xmax: ', xmin, ' , ',xmax
print 'Data ymin, ymax: ', ymin, ' , ',ymax

newlimits = raw_input("Set new x limits? (y/n) ")
if newlimits=='y':
    xmin = input("What is the minimum x value? ")
    xmax = input("What is the maximum x value? ")

    # Find minimum and maximum values of y in this range

    ymin = 1.0e30
    ymax = -1.0e30

    for i in range(data.shape[0]):
        if data[i,ix]>xmin and data[i,ix]<xmax:
            if data[i,iy] < ymin: ymin = data[i,iy]
            if data[i,iy] > ymax: ymax = data[i,iy]
    

    print 'New y limits: ',ymin, ymax


globalTmax = np.amax(data[:,maxcol][data[:,ix]>xmin])
globalTmin = np.amin(data[:,mincol][data[:,ix]>xmin])

fig1 = plt.figure()
ax = fig1.add_subplot(111)
ax.set_xlabel(namedict[xkey], fontsize = 16)
ax.set_ylabel('T(K)', fontsize = 16)
ax.set_ylim(globalTmin, globalTmax)
ax.set_xlim(xmin,xmax)
ax.plot(data[:,ix],data[:,mincol], label='minimum T')
ax.plot(data[:,ix],data[:,maxcol], label='maximum T')
ax.plot(data[:,ix],data[:,meancol], label='mean T')

ax.legend(loc='upper left')
fig1.savefig('compareT_'+prefix+'.ps', format='ps')

