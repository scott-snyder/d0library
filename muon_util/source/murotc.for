      SUBROUTINE MUROTC( IADD, X,Y,Z, DX, DY, DZ, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return correction by rotation matrix for
C-                         muon hit position. If survey version geometry
C-                         not exist for the module, return IER=1.
C-
C-   Inputs  : IADD     : address of wire
C-             X,Y,Z    : hit position, not corrected by rotation
C-   Outputs : DX,DY,DZ : hit position correction deifference by rotation
C-             IER      : ERROR CODE, 0-OK, 1-Outputs is not valid
C-
C-   Controls: None
C-
C-   Created  28-JAN-1992   Atsushi Taketani
C-
C-   To get corrected postion, (X,Y,Z) + (DX,DY,DZ)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER  IADD,NMOD,NPLN,NWIR,JERR, IER, NBUF, IBUF
      REAL     X,Y,Z,DX,DY,DZ
      CHARACTER*4 HSHAPE
      INTEGER  NSPAR
      REAL     SPAR(3),XPAR(3),ROTM(3,3),VOFF
      REAL     DLTGLB(3), DLTLOC(3), GLB(3)
      REAL     INDEX, DET
      INTEGER  NERROR
      REAL     INVROT(3,3,16)
      DATA INVROT/
     1   0., 0., 1.,  -1., 0., 0.,   0.,-1., 0.,
     2   0., 0.,-1.,  -1., 0., 0.,   0., 1., 0.,
     3   1., 0., 0.,   0., 0., 1.,   0.,-1., 0.,
     4   1., 0., 0.,   0., 0.,-1.,   0., 1., 0.,
     5   0., 0.,-1.,   1., 0., 0.,   0.,-1., 0.,
     6   0., 0., 1.,   1., 0., 0.,   0., 1., 0.,
     7  -1., 0., 0.,   0., 0.,-1.,   0.,-1., 0.,
     8  -1., 0., 0.,   0., 0., 1.,   0., 1., 0.,
     9   0., 0.,-1.,   0.,-1., 0.,  -1., 0., 0.,
     A   0., 1., 0.,   0., 0.,-1.,  -1., 0., 0.,
     B   0., 0., 1.,   0., 1., 0.,  -1., 0., 0.,
     C   0.,-1., 0.,   0., 0., 1.,  -1., 0., 0.,
     D   0., 0., 1.,   0.,-1., 0.,   1., 0., 0.,
     E   0., 1., 0.,   0., 0., 1.,   1., 0., 0.,
     F   0., 0.,-1.,   0., 1., 0.,   1., 0., 0.,
     G   0.,-1., 0.,   0., 0.,-1.,   1., 0., 0./
      INTEGER GZMGEO,LMGEO,OINDEX,IROW,ICOL
      REAL    ROTI(3,3)
C----------------------------------------------------------------------
      IER = 0               ! Reset error flag
C
      CALL MUADD(IADD,NMOD,NPLN,NWIR,JERR)  ! translate wire address
      IF ( JERR.NE.0 ) THEN
        IER = 1
        GOTO 999
      END IF
C
      LMGEO = GZMGEO(NMOD)                  ! ideal rotation matrix
      IF ( IC(LMGEO+8).EQ.0 ) THEN
        IER = 1
        GOTO 999
      END IF
C                                           ! get cell position and etc.
      CALL MUCELL( NMOD,NPLN,NWIR,HSHAPE,NSPAR,SPAR,XPAR,ROTM,
     +                VOFF,NBUF,IBUF)
C
      DLTGLB(1) = X - XPAR(1)               ! relative hit position
      DLTGLB(2) = Y - XPAR(2)               ! from wire center
      DLTGLB(3) = Z - XPAR(3)
C
      OINDEX = IC(LMGEO+12)                 ! ideal rotation matrix
      DO 100 ICOL=1,3
      DO 100 IROW=1,3
  100   ROTI(ICOL,IROW) = INVROT(ICOL,IROW,OINDEX)
C
C global -> local on ideal world 
      CALL VMATR( DLTGLB, ROTI, DLTLOC, 3, 3)
C loacl -> global on real world
      CALL VMATR(DLTLOC,ROTM,DLTGLB,3,3)     
C wire_center + (drift,pad)
      CALL VADD( DLTGLB,XPAR,GLB,3)
C get corrections
      DX = GLB(1) - X
      DY = GLB(2) - Y
      DZ = GLB(3) - Z
C
  999 RETURN
      END
