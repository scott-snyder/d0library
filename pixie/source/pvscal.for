      SUBROUTINE PVSCAL( NBIN, UPSIDN, COLOR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a scale for an FADC plot.  Every 10 bins is
C-                         marked with a tick, every 50 bins a double tick, and
C-                         every 100 bins a quadruple tick mark is drawn.
C-
C-   Inputs  : NBIN   [I] = number of fadc bins
C-             UPSIDN [I] = 1 for upward ticks, -1 for downward
C-             COLOR  [C*3] = chosen color
C-   Outputs : none (screen)
C-
C-   Created  14-NOV-1989   Peter Grudberg from PUHIST
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER NBIN, MINVAL, MAXVAL, UPSIDN, MXFADC
      CHARACTER*(*) COLOR
      PARAMETER ( MXFADC = 1024 )
      REAL    Y(MXFADC)
      REAL    XWMIN, XWMAX, YWMIN, YWMAX
      INTEGER J, UPDOWN, BIN
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Fill array Y with tickmarks:
C
        CALL VZERO( Y, MXFADC )
        DO BIN = 10, MXFADC, 10
          Y(BIN) = 2.
        ENDDO
        DO BIN = 50, MXFADC, 50
          Y(BIN) = Y(BIN) * 2.
        ENDDO
        DO BIN = 100, MXFADC, 100
          Y(BIN) = Y(BIN) * 2.
        ENDDO
      ENDIF
C
C ****  Draw the scale:
C
      YWMIN = 0.
      YWMAX = 0.
      IF ( UPSIDN .EQ. -1 ) THEN
        UPDOWN = -1
      ELSE
        UPDOWN = 1
      ENDIF
      DO 10 J = 1, NBIN
        IF ( UPDOWN*Y(J) .GT. YWMAX ) THEN
          YWMAX = UPDOWN*Y(J)
        ELSEIF ( UPDOWN*Y(J) .LT. YWMIN ) THEN
          YWMIN = UPDOWN*Y(J)
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
      CALL JMOVE(0.,0.)
C
      CALL JDRAW( 0., UPDOWN*Y(1) )
      DO 33 J = 1, NBIN-1
        IF ( UPDOWN*Y(J+1) .NE. UPDOWN*Y(J) ) THEN
          CALL JDRAW( FLOAT(J), UPDOWN*Y(J))
          CALL JDRAW( FLOAT(J), UPDOWN*Y(J+1))
        ENDIF
   33 CONTINUE
      CALL JDRAW( FLOAT(NBIN), UPDOWN*Y(NBIN))
      CALL JDRAW( FLOAT(NBIN), 0. )
C
      CALL JRCLOS
  999 RETURN
      END
