      SUBROUTINE PDXYHT_CMPRS(LAYER,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw hits in X-Y (R-PHI) plane using compressed
C-                         hits bank
C-
C-   Inputs  : LAYER: CDC layer number
C-             SECTOR: CDC sector number
C-   Outputs : none
C-
C-   Created   9-DEC-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAYER, SECTOR
      INTEGER LDHIT, GZDHIT, NHIT, LDALS, GZDALS, IPWIR, J
      INTEGER IP, JPOINT
      INTEGER IHIT, WORDS(2)
      INTEGER LAY, SEC, WIRE, JHIT, ISIDE, ONTRK, IZTRK
      REAL    XPOS(2), YPOS(2), ZPOS, AREA
      REAL    DDRIFT, DDIS
      REAL    XHPOS, YHPOS, XWIR, YWIR, CPHIW, SPHIW, SIZDIS
C
      DATA SIZDIS / 0.2 /
C----------------------------------------------------------------------
      LDHIT = GZDHIT()
      IF (LDHIT .LE. 0) GOTO 999
C
      CALL DHITPT(LAYER,SECTOR,JPOINT,NHIT)
      IF (JPOINT .LE. 0) GOTO 999
      IP = JPOINT + 1
      LDALS = GZDALS(LAYER,SECTOR)
      CPHIW = C(LDALS+3)
      SPHIW = C(LDALS+4)
C
      DO 100 IHIT = 1, NHIT
        CALL UCOPY(IQ(LDHIT+IP),WORDS,2)
        CALL GTDHIT(-1,WORDS,LAY,SEC,WIRE,JHIT,ONTRK,ISIDE,
     &                  IZTRK,XPOS,YPOS,ZPOS,AREA)
        IF (LAYER .EQ. LAY .AND. SECTOR .EQ. SEC) THEN
          IPWIR = LDALS + 6 + IC(LDALS+6) * WIRE
          XWIR = C(IPWIR+1)
          YWIR = C(IPWIR+2)
          IF (CPHIW .NE. 0.0) THEN
            DDRIFT = ABS((XPOS(1)-XWIR)/CPHIW)
          ELSE
            DDRIFT = 0.0
          ENDIF
          IF (ONTRK .EQ. 1) THEN
            DDIS = (-1)**ISIDE * DDRIFT - .5*SIZDIS
            XHPOS = XWIR + DDIS * CPHIW
            YHPOS = YWIR + DDIS * SPHIW
            CALL JMOVE(XHPOS,YHPOS)
            XHPOS = XHPOS + SIZDIS * CPHIW
            YHPOS = YHPOS + SIZDIS * SPHIW
            CALL JDRAW(XHPOS,YHPOS)
            CALL JCMARK(1)
            CALL JJUST(2,2)
            CALL JMARK(XPOS(1),YPOS(1))
          ELSE
            DO 200 J = 1, 2
              DDIS = (-1)**J * DDRIFT - .5*SIZDIS
              XHPOS = XWIR + DDIS * CPHIW
              YHPOS = YWIR + DDIS * SPHIW
              CALL JMOVE(XHPOS,YHPOS)
              XHPOS = XHPOS + SIZDIS * CPHIW
              YHPOS = YHPOS + SIZDIS * SPHIW
              CALL JDRAW(XHPOS,YHPOS)
  200       CONTINUE
          ENDIF
        ENDIF
        IP = IP + IQ(LDHIT+3)
  100 CONTINUE
C
  999 RETURN
      END
