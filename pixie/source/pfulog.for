      SUBROUTINE PFULOG( NBIN, Y, COLOR1, COLOR2, Z, TZ, NZ, YMIN )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a LOG plot ( Y(j) vs J ) in the current window,
C-               with max value MAXVAL, on NBIN bins, with color COLOR
C-               and mark it with DI3000 markers at selected points along 
C-               the horizontal axis.
C-
C-   Inputs  : NBIN   [I]   : number of bins
C-             Y      [R]   : bin content
C-             COLOR1 [C*3] : Color choosen for axes
C-             COLOR2 [C*3] : Color choosen for input and markers
C-             Z      [R]   : axis marker locations 
C-                            (used to mark hits by FDC)
C-             TZ     [I]   : type of marker desired (2='+',4='o',5='x')
C-             NZ     [I]   : number of axis markers
C-             YMIN   [R]   : minimum power of 10 to put on plot
C-   Outputs : none ( screen )
C-
C-   Created  20-APR-1990   Jeffrey Bantly    based on PUHIST.FOR
C-   Updated  15-OCT-1990   Susan K. Blessing  Created PFULOG.FOR
C-                          to make plots with a log scale
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER J
      INTEGER NBIN, NZ, MAXVAL, ISEGM
      INTEGER TZ(*)
C
      REAL Y(*),Z(*)
      REAL LOGY(512)
      REAL XWMIN, XWMAX, YWMIN, YWMAX
      REAL XVMIN, XVMAX, YVMIN, YVMAX, YSIZE
      REAL YMIN
C
      CHARACTER*(*) COLOR1, COLOR2
      CHARACTER*5 MLAB
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
      IF ( YWMAX - YWMIN .LT. 5.) THEN
        IF ( YWMIN .EQ. 0. ) THEN
          YWMAX = 5.
        ELSE
          YWMIN = -5.
        ENDIF
      ENDIF
      MAXVAL = NINT( YWMAX )
C
C MAKE WINDOW LOG SIZE
      YWMIN = 0.
      YWMAX = LOG(YWMAX) - YMIN
C
      CALL JWINDO(XWMIN,XWMAX,YWMIN,YWMAX)
      CALL PUOPEN
      CALL PXCOLR(COLOR1)
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
      CALL JSIZE( 2., .2*(YWMAX-YWMIN) )
      IF ( YWMAX .GT. 0. ) THEN
        IF (MAXVAL.GT.99999) MAXVAL = 99999
        WRITE( MLAB, 1000 ) MAXVAL
 1000   FORMAT(I5)
        CALL JJUST( 3, 3)
        CALL JMOVE( -4., YWMAX )
        CALL J3STRG( MLAB )
      ENDIF
      CALL JMOVE(0.,0.)
C
      CALL PXCOLR(COLOR2)
      DO J= 1, NBIN
        IF (Y(J).GT.0.) THEN
          LOGY(J) = LOG(Y(J)) - YMIN
        ELSE
          LOGY(J) = YWMIN - 10.
        END IF
      END DO
      IF ( LOGY(1) .NE. YWMIN ) CALL JDRAW( 0.,LOGY(1) )
      DO 33 J = 1, NBIN-1
        IF ( LOGY(J+1) .NE. LOGY(J) ) THEN
          CALL JDRAW( FLOAT(J), LOGY(J))
          CALL JDRAW( FLOAT(J), LOGY(J+1))
        ENDIF
   33 CONTINUE
      IF ( LOGY(NBIN) .NE. YWMIN ) THEN
        CALL JDRAW( FLOAT(NBIN), LOGY(NBIN))
        CALL JDRAW( FLOAT(NBIN), YWMIN )
      ENDIF
C
      CALL JJUST(2,2)
      CALL JSIZE( .02, .01*(YWMAX-YWMIN) )
      DO 34 J = 1, NZ
        CALL JCMARK( TZ(J) )
        CALL JMARK( Z(J),0. )
   34 CONTINUE
      CALL JRCLOS
  999 RETURN
      END
