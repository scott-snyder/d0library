      SUBROUTINE PVXYHT_CMPRS(LAYER,SECTOR,NPLOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw hits in X-Y (R-PHI) plane using compressed
C-                         hits bank
C-
C-   Inputs  : LAYER: VTX layer number
C-             SECTOR: VTX sector number
C-             NPLOT: see below
C-   Outputs : NPLOT: number of plotted hits
C-
C-   Created  16-MAR-1993   Alexandre Zinchenko (after PDXYHT_CMPRS for CDC)
C-   Updated  10-MAY-1993   A. Zinchenko - change color to BLUE at the end
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAYER, SECTOR, NPLOT
      INTEGER LVHIT, GZVHIT, NHIT, LVALS, GZVALS, IPWIR, J
      INTEGER IP, JPOINT
      INTEGER IHIT, WORDS(3)
      INTEGER LAY, SEC, WIRE, JHIT, IENDS, ISIDE, ONTRK, IZTRK
      REAL    XPOS(2), YPOS(2), ZPOS, AREA
      REAL    XHPOS, YHPOS, CPHIW, SPHIW, SIZDIS
C
      DATA SIZDIS / 0.05 /
C----------------------------------------------------------------------
      LVHIT = GZVHIT()
      IF (LVHIT .LE. 0) GOTO 999
C
      LVALS = LC( LC( LC( LSVTX-5 ) -(LAYER+1) ) -(SECTOR+1) )
      CPHIW = C( LVALS+3 )
      SPHIW = C( LVALS+4 )
C
      CALL VHITPT(LAYER,SECTOR,JPOINT,NHIT)
      IF (JPOINT.LE.0) GO TO 999
      IP = JPOINT + 1
      DO 100 IHIT = 1, NHIT
        CALL UCOPY(IQ(LVHIT+IP),WORDS,3)
        CALL GTVHIT(-1,WORDS,LAY,SEC,WIRE,JHIT,IENDS,
     &              ONTRK,ISIDE,IZTRK,XPOS,YPOS,ZPOS,AREA)
        IF (LAYER .EQ. LAY .AND. SECTOR .EQ. SEC) THEN
          NPLOT = NPLOT + 1
          IF (ONTRK .EQ. 1) THEN
            CALL PXCOLR('CYA')
            XHPOS = XPOS(1) - 0.5*SIZDIS*CPHIW
            YHPOS = YPOS(1) - 0.5*SIZDIS*SPHIW
            CALL JMOVE(XHPOS,YHPOS)
            XHPOS = XHPOS + SIZDIS * CPHIW
            YHPOS = YHPOS + SIZDIS * SPHIW
            CALL JDRAW(XHPOS,YHPOS)
            CALL JCMARK(1)
            CALL JJUST(2,2)
            CALL JMARK(XPOS(1),YPOS(1))
          ELSE
            CALL PXCOLR('MAG')
            DO 200 J = 1, 2
              XHPOS = XPOS(J) - 0.5*SIZDIS*CPHIW
              YHPOS = YPOS(J) - 0.5*SIZDIS*SPHIW
              CALL JMOVE(XHPOS,YHPOS)
              XHPOS = XHPOS + SIZDIS * CPHIW
              YHPOS = YHPOS + SIZDIS * SPHIW
              CALL JDRAW(XHPOS,YHPOS)
  200       CONTINUE
          ENDIF
        ENDIF
        IP = IP + IQ(LVHIT+3)
  100 CONTINUE
      CALL PXCOLR('BLU')
C
  999 RETURN
      END
