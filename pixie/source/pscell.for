      SUBROUTINE PSCELL(IVIEW,XPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw cell of SAMUS tube
C-
C-   Inputs  : IVIEW: View number; 1 = Y-Z view, 3 = X-Z view
C-             XPAR: coordinate of center of tube in global coordinates
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-MAY-1991,  S. Hagopian
C-   Updated  27-JAN-1993   Vladimir Glebov  ! Delete rotation matrix 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IVIEW
      REAL XPAR(3) 
C-----------------------------------------------------------------
C--------------------------------------------------------------------
C----------------------------------------------------------------------
      IF(IVIEW.EQ.1)THEN     
        CALL PXMARK('MAG',4,XPAR(3),XPAR(2),XPAR(1))
      ELSE
        CALL PXMARK('MAG',4,XPAR(3),XPAR(1),XPAR(2))
      ENDIF
  999 RETURN
      END
