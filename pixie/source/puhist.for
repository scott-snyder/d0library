      SUBROUTINE PUHIST( NBIN, Y, COLOR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a plot ( Y(j) vs J ) in the current window,
C-               with max value VALMAX, on NBIN bins, with color COLOR
C-
C-   Inputs  : NBIN   [I]   : number of bins
C-             Y      [R]   : bin content
C-             COLOR  [C*3] : Color choosen
C-   Outputs : none ( screen )
C-
C-   Created  21-DEC-1987   Olivier Callot
C-   UPDATED   9-JAN-1990   Lupe Howell Color table implemented
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:PXPARA.INC'

      INTEGER NBIN,  MINVAL, MAXVAL, ISEGM
      CHARACTER*(*) COLOR
      REAL    Y(*)
      REAL    XWMIN, XWMAX, YWMIN, YWMAX
      REAL    XVMIN, XVMAX, YVMIN, YVMAX, YSIZE
      INTEGER J
      CHARACTER*3 MLAB
C----------------------------------------------------------------------
      YWMIN = 0.
      YWMAX = 0.
      DO 10 J = 1, NBIN
        IF ( Y(J) .GT. YWMAX ) THEN
          YWMAX = Y(J)
        ELSEIF ( Y(J) .LT. YWMIN ) THEN
          YWMIN = Y(J)
        ENDIF
   10 CONTINUE
      XWMIN = -20.
      XWMAX = FLOAT( NBIN ) + 1.
      MINVAL = NINT(-YWMIN )
      MAXVAL = NINT( YWMAX )
      IF ( YWMAX - YWMIN .LT. 5.) THEN
        IF ( YWMIN .EQ. 0. ) THEN
          YWMAX = 5.
        ELSE
          YWMIN = -5.
        ENDIF
      ENDIF
      CALL JWINDO(XWMIN,XWMAX,YWMIN,YWMAX)
      CALL PUOPEN
      CALL PXCOLR(COLOR)
      CALL JMOVE(XWMIN,0.)
      CALL JDRAW(XWMAX,0.)
      CALL JMOVE(0.,YWMAX)
      CALL JDRAW(0.,YWMIN)
C
C ****  Char size is 4 bins ( reserved 20 in x ) and 20% of full hight.
C ****  ( if viewport is less than .2 ( max = 2. ) in height, saturate the
C ****  height if bigger viewport ) centered in upper right
C
      CALL J4RGET( 2, XVMIN, XVMAX, YVMIN, YVMAX )
      YSIZE = .20 * ( YWMAX-YWMIN )
      IF( YVMAX-YVMIN .GT. .2 ) YSIZE = YSIZE * .2 / ( YVMAX-YVMIN )
      CALL JSIZE( 4., .2*(YWMAX-YWMIN) )
      IF ( YWMAX .GT. 0. ) THEN
        WRITE( MLAB, 1000 ) MAXVAL
 1000   FORMAT(I3)
        CALL JJUST( 3, 3)
        CALL JMOVE( -4., YWMAX )
        CALL J3STRG( MLAB )
      ENDIF
      IF ( YWMIN .LT. 0. ) THEN
        WRITE( MLAB, 1000 ) MINVAL
        CALL JJUST( 3, 1)
        CALL JMOVE( -4., YWMIN )
        CALL J3STRG( MLAB )
      ENDIF
      CALL JMOVE(0.,0.)
C
      CALL JDRAW( 0., Y(1) )
      DO 33 J = 1, NBIN-1
        IF ( Y(J+1) .NE. Y(J) ) THEN
          CALL JDRAW( FLOAT(J), Y(J))
          CALL JDRAW( FLOAT(J), Y(J+1))
        ENDIF
   33 CONTINUE
      CALL JDRAW( FLOAT(NBIN), Y(NBIN))
      CALL JDRAW( FLOAT(NBIN), 0. )
C
      CALL JRCLOS
  999 RETURN
      END
