      SUBROUTINE PFISDR_FDC(PHI1,PHI2,PHI3,PHI4,IVIEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the Isajet tracks in R-Z view
C-                         from vertex to point of entry into FDC
C-
C-   Inputs  : PHI1,PHI2,PHI3,PHI4
C-   Outputs : draws Isajet tracks
C-
C-   Created  24-JUL-1989   Jeffrey Bantly
C-   Updated  25-JAN-1992   Robert E. Avery   Check phi bounds with call to
C-       PFPHICHK. Upper half of plot is PHI1-PHI2.
C-   Updated   7-FEB-1992   Robert E. Avery  Change colour. Also fix some 
C-      bugs in drawing secondary tracks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FDLTRK.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER  IVIEW, HALF, GZFGEH, GZFITR
      INTEGER  ITRK, NUMTRK, DUM, IERR
      INTEGER  RSIGN, PFPHICHK
      REAL     PHI1,PHI2,PHI3,PHI4
      REAL     XVER,YVER,RVER,ZVER,RIN,XIN,YIN,ZIN,PRCENT,YXRATI
      REAL     ZMIN,ZMAX,TRKDAT(9),DIR
      CHARACTER*4 TRKNUM
      CHARACTER*4 TRKCLR
      DATA TRKCLR /'CYA '/
C----------------------------------------------------------------------
C
C   Find the limits of the FDC RMIN, ZMIN, RMAX, ZMAX in the geometry
C   banks
C
      LFGEH = GZFGEH()
      IF ( LFGEH .LE. 0 ) GOTO 999
      ZMIN = ( C(LFGEH+8) - C(LFGEH+5) )
      ZMAX = ( C(LFGEH+8) + C(LFGEH+5) )
C
C   Find FITR bank
C
      LFITR = GZFITR()
      IF ( LFITR .LE. 5 ) GOTO 999
C
C  Get colour
C
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA('FDC COLR ISAJET',TRKCLR)
      CALL EZRSET
      CALL PXCOLR(TRKCLR)
C
C  Calculate track position for drawing.
C
      NUMTRK = IQ( LFITR + 1 )
      DO 10 ITRK = 1, NUMTRK
        CALL GTFITR(ITRK,TRKDAT)
        IF( TRKDAT(5) .LE. HALFPI ) DIR = 1.    ! Half 1
        IF( TRKDAT(5) .GE. HALFPI ) DIR = -1.   ! Half 0
        XVER = TRKDAT(1)                ! Track vertex
        YVER = TRKDAT(2)                ! Track vertex
        RVER = SQRT(XVER**2 + YVER**2)
        ZVER = TRKDAT(3)                ! Track vertex
C
        ZMIN = ABS(ZMIN) * DIR           ! Front edge of FDC
        IF ( DIR*(ZMIN -ZVER) .LT. 0.0 ) THEN
          ZIN = ABS(ZMAX) * DIR         ! Back edge of FDC
        ELSE 
          ZIN = ZMIN                     ! Track position at entry into FDC
        ENDIF
        XIN  = (ABS(ZIN-ZVER) * TAN(TRKDAT(5)) ) * COS(TRKDAT(4))
        YIN  = (ABS(ZIN-ZVER) * TAN(TRKDAT(5)) ) * SIN(TRKDAT(4))
        XIN  = XVER + XIN * DIR
        YIN  = YVER + YIN * DIR
        RIN  = ((XIN**2.)+(YIN**2.))**.5
C
        RSIGN = PFPHICHK(XIN,YIN,PHI1,PHI2,PHI3,PHI4) 
        RIN  = RSIGN*RIN
        IF ( RSIGN.EQ.0 ) THEN
          GOTO 10
        ENDIF
        IF ( ABS(RVER) .GT. 1.0 ) THEN
          RSIGN = PFPHICHK(XVER,YVER,PHI1,PHI2,PHI3,PHI4) 
        ENDIF
        RVER = RSIGN*RVER
C
C  Draw track with 'x' placed at vertex
C 
        IF(IVIEW.EQ.1) THEN
          CALL JMOVE(ZVER,XVER)
          CALL J3STRG('x')
          CALL JDRAW(ZIN,XIN)
        ELSEIF(IVIEW.EQ.2) THEN
          CALL JMOVE(ZVER,YVER)
          CALL J3STRG('x')
          CALL JDRAW(ZIN,YIN)
        ELSEIF(IVIEW.EQ.3) THEN
          CALL JMOVE(ZVER,RVER)
          CALL J3STRG('x')
          CALL JDRAW(ZIN,RIN)
        ELSEIF(IVIEW.EQ.4) THEN
          CALL JMOVE(XVER,YVER)
          CALL J3STRG('x')
          CALL JDRAW(XIN,YIN)
        ENDIF
C
C  Draw track number at end of track
C
        IF(TRKDAT(8) .LE. 9999.) THEN
          WRITE(TRKNUM,100) INT(TRKDAT(8))
  100     FORMAT(I4)
          PRCENT=0.8
          YXRATI=2.0
          CALL JJUST(2,2)
          IF(IVIEW.EQ.1) THEN
            CALL PUVSTR(ZIN-(4.*DIR),XIN,PRCENT,YXRATI,TRKNUM)
          ELSEIF(IVIEW.EQ.2) THEN
            CALL PUVSTR(ZIN-(4.*DIR),YIN,PRCENT,YXRATI,TRKNUM)
          ELSEIF(IVIEW.EQ.3) THEN
            CALL PUVSTR(ZIN-(4.*DIR),RIN,PRCENT,YXRATI,TRKNUM)
          ELSEIF(IVIEW.EQ.4) THEN
            CALL PUVSTR(XIN-(4.*DIR),YIN,PRCENT,YXRATI,TRKNUM)
          ENDIF
        ENDIF
C
   10 CONTINUE                          ! End of loop over tracks
C----------------------------------------------------------------------
  999 RETURN
      END
