################################################
# Sample command file for a multiple job run   #
################################################

Executable              =/path/to/full_d0geant_batrun 
input                   =fort.9
output                  =full_d0geant_batrun.out.$(Cluster).$(Process)
error                   =full_d0geant_batrun.error.$(Cluster).$(Process)

image_size = 40M
notification = ERROR

initialdir	=/path/to/your/area/$(Process)
queue 100


