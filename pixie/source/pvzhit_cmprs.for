      SUBROUTINE PVZHIT_CMPRS(LAYER,SECTOR,IFVHIT,DRWNOZ,
     &                        PHI1,PHI2,PHI3,PHI4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw VTX hits in R-Z view using compressed 
C-                         hits bank VHIT
C-
C-   Inputs  : LAYER: VTX layer number
C-             SECTOR: VTX sector number
C-             IFVHIT: = 0, no hit drawing
C-                     = 1, draw hits from traks on wires 0 and 7
C-                     = 2, draw hits from traks on all wires
C-                     = 3, draw all hits
C-             DRWNOZ: flag to draw unmatched Z hits
C-             PHI1, PHI2: phi road limits for upper part of chamber
C-             PHI3, PHI4: phi road limits for lower part of chamber
C-   Outputs : none
C-
C-   Created  18-MAR-1993   Alexandre Zinchenko (after PDZHIT_CMPRS)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      LOGICAL FLDRAW
      INTEGER LAYER, SECTOR, DRWNOZ, IFVHIT
      INTEGER LVHIT, GZVHIT, NHIT, JPOINT, IP
      INTEGER IHIT, WORDS(3), IH, MARK, MIRR
      INTEGER LAY, SEC, WIRE, JHIT, IENDS, ISIDE, ONTRK, IZTRK
      REAL    XPOS(2), YPOS(2), ZPOS, AREA
      REAL    PHI1, PHI2, PHI3, PHI4, HITPHI, RHPOS, ZERR
      DATA ZERR /2./
C----------------------------------------------------------------------
C  
      IF (IFVHIT.EQ.0) GO TO 999
      LVHIT = GZVHIT()
      IF (LVHIT .LE. 0) GOTO 999
C
      CALL VHITPT(LAYER,SECTOR,JPOINT,NHIT)
      IF (JPOINT .LE. 0) GOTO 999
      IP = JPOINT + 1
C
      DO 100 IHIT = 1, NHIT
        CALL UCOPY(IQ(LVHIT+IP),WORDS,3)
        CALL GTVHIT(-1,WORDS,LAY,SEC,WIRE,JHIT,IENDS,
     &              ONTRK,ISIDE,IZTRK,XPOS,YPOS,ZPOS,AREA)
        IF (ZPOS .GT. 99.0) GOTO 101
        IF (LAYER .EQ. LAY .AND. SECTOR .EQ. SEC) THEN
          IF (IFVHIT.EQ.1.AND..NOT.(WIRE.EQ.0.OR.WIRE.EQ.7)) 
     &        GO TO 101
          MARK = 2
          IF (ONTRK.EQ.1) THEN
            MIRR = 1
            CALL PXCOLR('CYA')
          ELSE
            IF (IFVHIT.LT.3) GO TO 101
            MIRR = 2
            CALL PXCOLR('RED')
          ENDIF
          IF (IENDS.NE.3) THEN ! no Z-match
            IF (DRWNOZ.EQ.0) GO TO 101
C            CALL PXCOLR('YEL')
            MARK = 1            
          ENDIF
          CALL JCMARK(MARK)
          DO 200 IH = 1, MIRR
            FLDRAW = .FALSE.
            HITPHI = ATAN2(YPOS(IH),XPOS(IH))
            RHPOS = SQRT(YPOS(IH)**2+XPOS(IH)**2)
            IF (HITPHI .LT. 0.) HITPHI = HITPHI + TWOPI
            IF (HITPHI.GE.PHI1.AND.HITPHI.LE.PHI2) THEN
              FLDRAW = .TRUE.
            ELSE IF (HITPHI.GE.PHI3.AND.HITPHI.LE.PHI4) THEN
              FLDRAW = .TRUE.
              RHPOS = -RHPOS
            ENDIF
            IF (FLDRAW) THEN
C              CALL JJUST(2,2)
C              CALL JMARK(ZPOS,RHPOS)
              CALL JMOVE(ZPOS-ZERR/2.,RHPOS)
              CALL JDRAW(ZPOS+ZERR/2.,RHPOS)
            ENDIF
  200     CONTINUE
        ENDIF
  101   IP = IP + IQ(LVHIT + 3)
  100 CONTINUE
C
  999 RETURN
      END
