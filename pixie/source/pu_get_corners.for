      SUBROUTINE PU_GET_CORNERS(XCOORD1,YCOORD1,XCOORD2,YCOORD2,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the coordinates of two corners in
C-   the coordinates of the screen (-1,1) X (-1,1) and mark the corners.
C-
C-   Inputs  : XCOORD1,YCOORD1  [R]     1st point
C-             XCOORD2,YCOORD2  [R]     2nd point
C-             IER              [I]     0 if points are ok.
C-   Outputs :
C-   Controls:
C-
C-   Created  15-NOV-1990   Harrison B. Prosper, Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XCOORD1, YCOORD1, XCOORD2, YCOORD2
      INTEGER IER
      INTEGER ICH
      REAL    CROSSIZE
      PARAMETER( CROSSIZE = 0.25 )
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
C
C ****  Set view-port to occupy whole screen
C
      CALL JVPORT( XCVPRT-XMVPRT, XCVPRT+XMVPRT,
     &             YCVPRT-YMVPRT, YCVPRT+YMVPRT )
C
C ****  Map 2 x 2 square to the view-port
C
      CALL JWINDO( -1., 1., -1., 1. )
      CALL JVUPNT(0.,0.,0.)
      CALL JNORML(0.,0.,-1.)
C
C ****  Enclose primitives within a temporary segment
C
      CALL JOPEN
      CALL PXCOLR('FOR')
C
C ****  Get first point 
C
      CALL PULOCA( 0., 0., ICH, XCOORD1,YCOORD1 )
C
C ****  Draw cross
C
      CALL JMOVE( XCOORD1-CROSSIZE, YCOORD1 )
      CALL JDRAW( XCOORD1+CROSSIZE, YCOORD1 )
      CALL JMOVE( XCOORD1, YCOORD1-CROSSIZE )
      CALL JDRAW( XCOORD1, YCOORD1+CROSSIZE )
C
C ****  Get second point
C
      CALL PULOCA( XCOORD1, YCOORD1, ICH, XCOORD2, YCOORD2 )
C
C ****  Draw cross
C
      CALL JMOVE( XCOORD2-CROSSIZE, YCOORD2 )
      CALL JDRAW( XCOORD2+CROSSIZE, YCOORD2 )
      CALL JMOVE( XCOORD2, YCOORD2-CROSSIZE )
      CALL JDRAW( XCOORD2, YCOORD2+CROSSIZE )
      CALL JCLOSE
C
C ****  Check for sensible points
C
      IF( (XCOORD1 .EQ. XCOORD2) .OR.
     &    (YCOORD1 .EQ. YCOORD2) ) THEN
        IER =-1
      ELSE
        IER = 0
      ENDIF
  999 RETURN
      END
