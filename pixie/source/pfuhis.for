      SUBROUTINE PFUHIS( NBIN, Y, COLOR1, COLOR2, Z, TZ, NZ )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a plot ( Y(j) vs J ) in the current window,
C-               with max value VALMAX, on NBIN bins, with color COLOR
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
C-   Outputs : none ( screen )
C-
C-   Created  20-APR-1990   Jeffrey Bantly    based on PUHIST.FOR
C-   Updated  30-APR-1991   Jeffrey Bantly  cleanup and add checks
C-   Updated   8-NOV-1991   Robert E. Avery   use MX_HIT_WIRE from FDPARA.
C-   Updated  25-JUN-1992   Robert E. Avery  Use flage for SET SCALE. 
C-   Updated   7-JUL-1992   Tacy Joffe-Minor  use HITS_PER_WIRE 
C-   Updated  10-AUG-1992   Robert E. Avery  Don't really need HITS_PER_WIRE.
C-   Updated  18-AUG-1992   Robert E. Avery  Fix up pulse subtraction 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'

      INTEGER NBIN, NZ, MINVAL, MAXVAL, ISEGM, J, VSCALE
      INTEGER TZ(*)
      INTEGER HITS_PER_WIRE,IER
C
      REAL    Y(*),Z(*)
      REAL    XWMIN, XWMAX, YWMIN, YWMAX
      REAL    XVMIN, XVMAX, YVMIN, YVMAX, YSIZE
      REAL    YDIFF
C
      CHARACTER*(*) COLOR1, COLOR2
      CHARACTER*5 MLAB
C
      LOGICAL LSCALE,FLGVAL
C----------------------------------------------------------------------
      IF (NZ.LT.50) THEN                 ! recompute scales
        LSCALE = FLGVAL('PF_SET_SCALE')
        IF(LSCALE) CALL PUGETV('ELECT VERT SCALE',VSCALE)
        YWMIN = 0.
        YWMAX = 0.
        DO 10 J = 1, NBIN
          IF ( Y(J) .GT. YWMAX ) THEN
            YWMAX = Y(J)
          ELSEIF ( Y(J) .LT. YWMIN ) THEN
            YWMIN = Y(J)
          ENDIF
   10   CONTINUE
C
        IF(LSCALE) THEN
          YWMAX = VSCALE
          MAXVAL = VSCALE
        ENDIF
C
        XWMIN = -20.
        XWMAX = FLOAT( NBIN ) + 1.
        IF ( YWMAX - YWMIN .LT. 5.) THEN
          IF ( YWMIN .EQ. 0. ) THEN
            YWMAX = 5.
          ELSE
            YWMIN = -5.
          ENDIF
        ENDIF
C
        MINVAL = NINT(-YWMIN ) 
        MAXVAL = NINT( YWMAX )
C
      ENDIF
C
      YDIFF=YWMAX-YWMIN
      CALL JWINDO(XWMIN,XWMAX,YWMIN-(YDIFF*0.11),(YWMAX+5.))
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
      IF (NBIN.LT.256) THEN
        CALL JSIZE( 2., .2*(YWMAX-YWMIN) )
      ELSE
        CALL JSIZE( 4., .2*(YWMAX-YWMIN) )
      END IF
      IF ( YWMAX .GT. 0. ) THEN
        IF (MAXVAL.GT.99999) MAXVAL = 99999
        WRITE( MLAB, 1000 ) MAXVAL
 1000   FORMAT(I5)
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
      CALL PXCOLR(COLOR2)
      IF ( Y(1) .NE. 0.0 ) CALL JDRAW( 0., Y(1) )
      DO J = 1, NBIN-1
        IF ( Y(J) .EQ. 0.0 ) CALL JMOVE( FLOAT(J), Y(J) )
        IF ( Y(J+1) .NE. Y(J) ) THEN
          CALL JDRAW( FLOAT(J), Y(J))
          CALL JDRAW( FLOAT(J), Y(J+1))
        ENDIF
      ENDDO
      IF ( Y(NBIN) .NE. 0.0 ) THEN
        CALL JDRAW( FLOAT(NBIN), Y(NBIN))
        CALL JDRAW( FLOAT(NBIN), 0. )
      ENDIF
C
      IF (NZ.LE.0) GOTO 200
      IF (NZ.GE.100) GOTO 200
      CALL JJUST(2,2)
      CALL JSIZE( 8., .25*(YWMAX-YWMIN) )
      CALL PXCOLR('CYA')
      DO J = 1, MIN(NZ,MX_HIT_WIRE)
        CALL JMOVE( Z(J),0.)
        IF(TZ(J).EQ.999 ) THEN          ! not on segment
          CALL J3STRG('+')
        ELSEIF(TZ(J).EQ.99 ) THEN       ! not on labelled segment
          CALL J3STRG('?')
        ELSEIF(TZ(J).EQ.1 ) THEN        ! first segment encountered
          CALL J3STRG('X')
        ELSEIF(TZ(J).EQ.2 ) THEN        ! second segment encountered
          CALL J3STRG('Y')
        ELSEIF(TZ(J).EQ.3 ) THEN        ! etc
          CALL J3STRG('I')
        ELSEIF(TZ(J).EQ.4 ) THEN
          CALL J3STRG('V')
        ELSEIF(TZ(J).EQ.5 ) THEN
          CALL J3STRG('A')
        ELSEIF(TZ(J).EQ.6 ) THEN
          CALL J3STRG('*')
        ELSEIF(TZ(J).EQ.7 ) THEN
          CALL J3STRG('U')
        ELSEIF(TZ(J).EQ.8 ) THEN
          CALL J3STRG('T')
        ELSEIF(TZ(J).EQ.9 ) THEN
          CALL J3STRG('W')
        ELSEIF(TZ(J).EQ.10 ) THEN
          CALL J3STRG('O')
        ENDIF
      ENDDO
C
  200 CONTINUE
C
      CALL JRCLOS
C----------------------------------------------------------------------
  999 RETURN
      END
