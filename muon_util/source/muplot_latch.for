	subroutine muplot_latch(l1wamus,l1samus,l15low,l15high,ihist)

c	This subroutine decodes the two L1 words and the two L1. words
c	and fills 3 histograms for each bit (CCT or OTC latch) which fired.
c	The histogram is ordered so that NORTH and SOUTH are on opposite
c	sides of CENTRAL. Each region is ordered so that the TOP is towards
c	the middle and the BOTTOM is towards the outside.
c	
c	0-3 	4-7	8-11	12-15	16-19	20-23	24-27	28-31
c	SN	ON	WN	CF-E	CF-W	WS	OS	SS
c
c	Created:  31-Aug-94	Paul Quintas
c	Modified: 26-Oct-94	Paul Quintas	Add entry mubook_latch

	integer l1wamus, l1samus, l15low, l15high, ihist, jhist
	integer ibit, idum
	logical ok, btest

	do ibit = 0, 7
	   ok = btest(l1wamus,ibit)	! WAMUS CENTRAL
	   if (ok.and.ibit.le.5) call hfill(ihist,ibit+14.,0.,1.)
	   if (ok.and.ibit.ge.6) call hfill(ihist,ibit+ 6.,0.,1.)
	   ok = btest(l15low,ibit)	! WAMUS CENTRAL
	   if (ok.and.ibit.le.5) call hfill(ihist+1,ibit+14.,0.,1.)
	   if (ok.and.ibit.ge.6) call hfill(ihist+1,ibit+ 6.,0.,1.)
	   ok = btest(l15high,ibit)	! WAMUS CENTRAL
	   if (ok.and.ibit.le.5) call hfill(ihist+2,ibit+14.,0.,1.)
	   if (ok.and.ibit.ge.6) call hfill(ihist+2,ibit+ 6.,0.,1.)
	enddo
	
	do idum = 0, 3
	   ibit = 10 + 2*idum		! WAMUS NORTH
	   ok = btest(l1wamus,ibit)
	   if (ok) call hfill(ihist,11.-idum,0.,1.)
	   ibit = 8 + idum		! WAMUS NORTH
	   ok = btest(l15low,ibit)
	   if (ok) call hfill(ihist+1,11.-idum,0.,1.)
	   ibit = 8 + idum		! WAMUS NORTH
	   ok = btest(l15high,ibit)
	   if (ok) call hfill(ihist+2,11.-idum,0.,1.)

	   ibit = 10 + 2*idum + 1	! WAMUS SOUTH
	   ok = btest(l1wamus,ibit)
	   if (ok) call hfill(ihist,idum+20.,0.,1.)
	   ibit = 16 + idum		! WAMUS SOUTH
	   ok = btest(l15low,ibit)
	   if (ok) call hfill(ihist+1,idum+20.,0.,1.)
	   ibit = 16 + idum		! WAMUS SOUTH
	   ok = btest(l15high,ibit)
	   if (ok) call hfill(ihist+2,idum+20.,0.,1.)

	   ibit = 2*idum		! OVERLAP NORTH
	   ok = btest(l1samus,ibit)
	   if (ok) call hfill(ihist,7.-idum,0.,1.)
	   ibit = 12 + idum		! OVERLAP NORTH
	   ok = btest(l15low,ibit)
	   if (ok) call hfill(ihist+1,7.-idum,0.,1.)
	   ibit = 12 + idum		! OVERLAP NORTH
	   ok = btest(l15high,ibit)
	   if (ok) call hfill(ihist+2,7.-idum,0.,1.)

	   ibit = 2*idum + 1		! OVERLAP SOUTH
	   ok = btest(l1samus,ibit)
	   if (ok) call hfill(ihist,idum+24.,0.,1.)
	   ibit = 20 + idum		! OVERLAP SOUTH
	   ok = btest(l15low,ibit)
	   if (ok) call hfill(ihist+1,idum+24.,0.,1.)
	   ibit = 20 + idum		! OVERLAP SOUTH
	   ok = btest(l15high,ibit)
	   if (ok) call hfill(ihist+2,idum+24.,0.,1.)

	   ibit = 10 + 2*idum		! SAMUS NORTH
	   ok = btest(l1samus,ibit)
	   if (ok) call hfill(ihist,3.-idum,0.,1.)
	   ibit = 24 + idum 		! SAMUS NORTH
	   ok = btest(l15low,ibit)
	   if (ok) call hfill(ihist+1,3.-idum,0.,1.)
	   ibit = 24 + idum 		! SAMUS NORTH
	   ok = btest(l15high,ibit)
	   if (ok) call hfill(ihist+2,3.-idum,0.,1.)

	   ibit = 10 + 2*idum + 1	! SAMUS SOUTH
	   ok = btest(l1samus,ibit)
	   if (ok) call hfill(ihist,idum+28.,0.,1.)
	   ibit = 28 + idum		! SAMUS SOUTH
	   ok = btest(l15low,ibit)
	   if (ok) call hfill(ihist+1,idum+28.,0.,1.)
	   ibit = 28 + idum		! SAMUS SOUTH
	   ok = btest(l15high,ibit)
	   if (ok) call hfill(ihist+2,idum+28.,0.,1.)
	enddo

	return
	entry mubook_latch(jhist)

	call hbook1(jhist,'CCT LATCHES BY REGION',32,-0.5,31.5,0.)
	call hbook1(jhist+1,'OTC-HI LATCHES BY REGION',32,-0.5,31.5,0.)
	call hbook1(jhist+2,'OTC-LO LATCHES BY REGION',32,-0.5,31.5,0.)

	return
	end
