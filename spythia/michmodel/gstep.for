
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	function Gstep(part_id)

c	This step function returns 1 if the squark PART_ID is coupled, 0
c	if not. (Coupled meaning Q>Msquark).

	implicit none
	integer part_id
	real*8 Gstep
        include 'D0$SPYTHIA$INC:DIFFEQ.INC'

	if (gflog(part_id)) then
	    Gstep=1.0
	else
	    Gstep=0.0
	endif

c	Gstep=1.0

	return
	end
