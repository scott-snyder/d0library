      SUBROUTINE MUROTG( IROT,XYZL,XYZG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return global rotated coordinates for
C-                         for a given module geometry IROT.
C-
C-   Inputs  : IROT     : Module orientation (1-16) see MGEO.
C-             XYZL(3)  : Local coordinate system
C-   Outputs : XYZG(3)  : Global coordinate directions
C-
C-   Controls: None
C-
C-   Created  7-MAY-1995   M Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IROT
      REAL     XYZL(3),XYZG(3)
      INTEGER  IROW,ICOL
      REAL     ROTMU(3,3,16)
      DATA ROTMU/
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
C----------------------------------------------------------------------
C
      DO IROW=1,3
        XYZG(IROW) = 0.0
        DO ICOL=1,3
          XYZG(IROW) = XYZG(IROW) + ROTMU(ICOL,IROW,IROT)*XYZL(ICOL)
        END DO
      END DO
C
  999 RETURN
      END
