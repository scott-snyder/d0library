C===================================================================
C  COMMON /TRACE/ contains information about the current particle status
C  when a detector dispatching subroutine is called by INTRPT
c
      REAL
     1  X1LOC(4),       
     1	X2LOC(4),       
C  X1LOC and X2LOC are local entrance and exit points
C  XnLOC are 4-vectors. If JCSYS=0 then CARTESIAN coordinates
C  X(1)=x, X(2)=y, X(3)=z, and X(4)=0.
C  Else if JCSYS=1 then CYLINDRICAL coordinates
C  X(1)=r, X(2)=cos(phi), X(3)=sin(phi), and X(4)=z.
C  XnGLO are always cartesian coordinates
     2  X1GLO(3),       
     2  X2GLO(3),       
     3  XORIG(3),       
     3  XDEST(3),       
     4  DXLES,          
     4	ELS		
C
      INTEGER
     1  CSYS,           
     1  IDET,           
     1  IMATT,          
     2  ICOORD(3)       
C
	LOGICAL
     1    QRSTRT       
C
      COMMON/TRACE/X1LOC,X2LOC,X1GLO,X2GLO,IDET,IMATT,ICOORD,DXLES,
     1             XORIG,XDEST,ELS,QRSTRT,CSYS
C==================================================================
C
