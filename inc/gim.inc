C
C.....................................................................
C
C	     These common blocks contain the global geometry 
C	information used to trace particles through a detector
C	or to make a ray tracing computer graphics pictures of
C	some object.  All these variables are initialized once
C	at the beginning and never touched again.  The routines
C	GEOMETRY_WRITE and GEOMETRY_READ read and write this
C	information onto a file for safekeeping.  They also write
C	and read some global variables which are listed in the
C	comments below but which are not actually stored in the
C	common blocks.  The "arrays" SUBLATT(I,L) [I(3)] and 
C	TRANS(I,L) are actually integer functions which simulate
C	a vector of three dimensional arrays.  These things 
C	are globally available to any program.  They are created
C	by another program {si0:[saul.util.dstruct]vecarray.mor}.
C	     At the beginning of this piece of code are some PARAMETER
C	statements which adjust the amount of memory reserved for
C	various things.  I expect that the default settings are
C	adequate for most purposes.  S. Youssef.
C
C
C	Here is a description of these global variables:
C
C	N_LATTICE		The number of 'LATTICES' which 
C				describe the different objects to be
C				simulated.  The number of lattices must
C				not exceed MAX_LATTICE+1.
C
C	M_LATTICE		Is the maximum lattice number allowed
C				by the array bounds.
C
C	M_CV_EDGE		Is the maximum number of edges allowed
C				in a single lattice.
C
C	CS(L)			Coordinate system type of the lattice
C				"L".  (0<L<21).  0 means cartesian
C				coordinates, 1 means cylindrical 
C				coordinates, 2 means spherical 
C				coordinates, 3 means bicycle
C				coordinates.  Other integers are not
C				defined at the moment (and not allowed).
C
C

C
C
C
C
C	BOUND(j,c,L)		for each lattice L, for each of the
C				coordinates 1<c<=3, a list of
C				values of the coordinates.
C
C	BL(c,L)			for c=1,2 or 3, the range of the 
C				corresponding value(s) of the first
C				argument in BOUND(j,c,L).  The range
C				is given by 1<=j<BL(c,L).
C
C	LCYL(L)			for a cylindrical lattice L, 
C				SF_AUX(j,LCYL(L)) j=0,....BL(2,L)
C				CF_AUX(j,LCYL(L)) j=0,....BL(2,L)
C				give the sine and cosine of the
C				usual cylindrical angle.  if the 
C				lattice L is not cylindrical 
C				(i.e. doesn't have CS(L)=1) then
C				LCYL(L) = 0.
C
C	SF_AUX(j,LC)		an auxilliary array which gives
C				sine phi values that correspond
C				to the list BOUND(j,2,L) for 
C				j=1,...BL(2,L)-1.  LC = LCYL(L).
C
C	CF_AUX(j,LC)		this is the same as SF_AUX except
C				that the cosine is stored instead
C				of the sine.
C
C	R2_AUX(j,LC)		an auxilliary array which gives the
C				R**2 values so they don't have to 
C				be recomputed all the time.
C
C

C
C
C
C	TRANS(I,L)		For all I(3) s.t. I(1)=1,..BL(1,L)-1,
C				for all I(2)=1,...BL(2,L)-1, for all
C				I(3)=1,...BL(3,L)-1, for each lattice
C				L, TRANS(I,L) is true iff the 
C				corresponding cv is "opaque".  This 
C				is for making rudimentary pictures.
C
C	SUBLATT(I,L)		for the same arguments as TRANS,
C				SUBLATT gives the "sublattice" number
C				for cv I in lattice L.  For the meaning
C				of this, refer to the ray casting paper.
C
C	DISP(3,L1,L2)		The position of the origin of L1 w.r.t.
C				the origin of lattice L2.
C
C	TILT(3,3,L1,L2)		The rotation vector to line up the 
C				z axis of lattice L1 with lattice L2.
C
C	R_LARGE			R_LARGE is the half length of a large
C				cube called CUBE that contains all the
C				volumes of interest.  This CUBE is 
C				set up in lattice 0 which is a 
C				cartesian lattice at the top of the
C				lattice tree (has no predecessors).
C				R_LARGE should not be made much larger
C				than necessary to avoid precision 
C				problems.
C
C	ORIENTATION(L)		This is an extra helpful vector which
C				contains an orientation number for
C				each coordinate system of each lattice.
C				the orientation number of two lattices
C				must be the same iff the coordinate
C				systems are of the same type and there
C				is no rotation or translation between
C				them.



	INTEGER MAX_LATTICE,MAX_CV_EDGE
	PARAMETER (MAX_LATTICE=50)	
	PARAMETER (MAX_CV_EDGE=500)	
					

	COMMON / GEOMETRY /
     &					N_LATTICE,
     &					M_LATTICE,
     &					M_CV_EDGE,
     &					CS(0:MAX_LATTICE),
     &					BOUND(MAX_CV_EDGE,3,
     &				              0:MAX_LATTICE),
     &					BL(3,0:MAX_LATTICE),
     &					LCYL(0:MAX_LATTICE),
     &					SF_AUX(0:MAX_CV_EDGE,
     &					       0:MAX_LATTICE),
     &					CF_AUX(0:MAX_CV_EDGE,
     &					       0:MAX_LATTICE),
     &                                  R2_AUX(0:MAX_CV_EDGE,
     &					       0:MAX_LATTICE),
     &					DISP(3,0:MAX_LATTICE,
     &					       0:MAX_LATTICE),
     &                                  TILT(3,3,0:MAX_LATTICE,
     &						 0:MAX_LATTICE),
     &					R_LARGE,
     &					ORIENTATION(0:MAX_LATTICE)

	INTEGER CS,N_LATTICE,BL,LCYL,ORIENTATION,M_LATTICE,M_CV_EDGE
	REAL BOUND,SF_AUX,CF_AUX,R2_AUX,DISP,TILT,R_LARGE
	DATA M_LATTICE /MAX_LATTICE/, M_CV_EDGE /MAX_CV_EDGE/
C
C.....................................................................
C
