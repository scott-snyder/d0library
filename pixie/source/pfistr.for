      SUBROUTINE PFISTR(HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the Isajet tracks that intersect one
C-                         Half of the FDC from vertex to point of entry
C-
C-   Inputs  : HALF - FDC Half
C-   Outputs : draws Isajet tracks
C-   Controls: 
C-
C-   Created  16-MAR-1989   Jeffrey Bantly
C-   Updated  13-DEC-1991   Robert E. Avery  Fix bug in Secondary track 
C-      drawing. Take out error message if no isajet track.
C-   Updated   7-FEB-1992   Robert E. Avery  Change colour. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FDLTRK.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER  HALF, GZFGEH, GZFITR, GZISAE
      INTEGER  ITRK, NUMTRK, DUM, LKISAE, IERR
      REAL     XVER,YVER,XIN,YIN,PRCENT,YXRATI
      REAL     ZMIN,TRKDAT(9),DIR,RADIUS
      CHARACTER*4 TRKNUM
      CHARACTER*4 TRKCLR
      DATA TRKCLR /'CYA '/
C----------------------------------------------------------------------
C
C   Check if Isajet banks present
C
      LKISAE=GZISAE()
      IF( LKISAE .LE. 0 ) GOTO 999
      LFITR = GZFITR()
      IF ( LFITR .LE. 0 ) GOTO 999
C
C   Find the limits of the FDC ZMIN in the geometry
C   banks
C
      LFGEH = GZFGEH()
      IF ( LFGEH .LE. 0 ) GOTO 999
      DIR = FLOAT((-1)**(HALF+1))
      ZMIN = DIR * ( C(LFGEH+8) - C(LFGEH+5) )
C
C  Get colour
C
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA('FDC COLR ISAJET',TRKCLR)
      CALL EZRSET
      CALL PXCOLR(TRKCLR)
C
C   Find FITR bank, take track info, and plot track 
C
      NUMTRK = IQ( LFITR + 1 )
      DO 10 ITRK = 1, NUMTRK
        CALL GTFITR(ITRK,TRKDAT)
        IF( HALF .EQ. 0 .AND. TRKDAT(5) .LE. HALFPI ) GOTO 10
        IF( HALF .EQ. 1 .AND. TRKDAT(5) .GE. HALFPI ) GOTO 10
        XVER = TRKDAT(1)
        YVER = TRKDAT(2)
        XIN  = ( ABS(ZMIN-TRKDAT(3)) * TAN(TRKDAT(5)) ) * COS(TRKDAT(4))
        YIN  = ( ABS(ZMIN-TRKDAT(3)) * TAN(TRKDAT(5)) ) * SIN(TRKDAT(4))
        XIN  = XVER + XIN * DIR
        YIN  = YVER + YIN * DIR
        RADIUS = ((XIN**2.)+(YIN**2.))**.5
        IF( RADIUS .GT. 70. ) GOTO 10
        CALL JMOVE(XVER,YVER)
        CALL JDRAW(XIN,YIN)
C
        IF(TRKDAT(8) .LE. 9999.) THEN
          WRITE(TRKNUM,100) INT(TRKDAT(8))
  100     FORMAT(I4)
          PRCENT=0.6
          YXRATI=2.0
          CALL JJUST(2,2)
          CALL PUVSTR(XIN,YIN,PRCENT,YXRATI,TRKNUM)
        ENDIF
C
   10 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
