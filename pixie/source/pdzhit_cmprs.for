      SUBROUTINE PDZHIT_CMPRS(LAYER,SECTOR,IFDWIR,PHI1,PHI2,PHI3,PHI4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw CDC hits in R-Z view using compressed 
C-                         hits bank DHIT
C-
C-   Inputs  : LAYER: CDC layer number
C-             SECTOR: CDC sector number
C-             IFDWIR: flag to draw wires
C-             PHI1, PHI2: phi road limits for upper part of chamber
C-             PHI3, PHI4: phi road limits for lower part of chamber
C-   Outputs : none
C-
C-   Created  11-DEC-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LAYER, SECTOR, IFDWIR
      INTEGER LDHIT, GZDHIT, LDALS, GZDALS, IPWIR, NHIT
      INTEGER JPOINT, IP, IWIR
      INTEGER IHIT, WORDS(2)
      INTEGER LAY, SEC, WIRE, JHIT, ISIDE, ONTRK, IZTRK
      REAL    XPOS(2), YPOS(2), ZPOS, AREA
      REAL    PHI1, PHI2, PHI3, PHI4, HITPHI
      REAL    RPOS(0:6), XWIR, YWIR
      LOGICAL DRAWHT
C----------------------------------------------------------------------
C  
      LDHIT = GZDHIT()
      IF (LDHIT .LE. 0) GOTO 999
C
      CALL DHITPT(LAYER,SECTOR,JPOINT,NHIT)
      IF (JPOINT .LE. 0) GOTO 999
      IP = JPOINT + 1
      LDALS = GZDALS(LAYER,SECTOR)
      DO 200 IWIR = 0, 6, 6
        IPWIR = LDALS + 6 + IC(LDALS+6) * IWIR
        XWIR = C(IPWIR+1)
        YWIR = C(IPWIR+2)
        RPOS(IWIR) = SQRT(XWIR ** 2 + YWIR **2)
  200 CONTINUE
C
      DO 100 IHIT = 1, NHIT
        CALL UCOPY(IQ(LDHIT+IP),WORDS,2)
        CALL GTDHIT(-1,WORDS,LAY,SEC,WIRE,JHIT,ONTRK,ISIDE,
     &                  IZTRK,XPOS,YPOS,ZPOS,AREA)
        IF (ZPOS .GT. 99.0) GOTO 101
        IF (LAYER .EQ. LAY .AND. SECTOR .EQ. SEC) THEN
          IF (WIRE .EQ. 0 .OR. WIRE .EQ. 6) THEN
            DRAWHT = .FALSE.
            HITPHI = ATAN2(YPOS(1),XPOS(1))
  300       IF (HITPHI .LT. 0.) HITPHI = HITPHI + TWOPI
            IF (HITPHI .GE. PHI1 .AND. HITPHI .LE. PHI2) THEN
              DRAWHT = .TRUE.
              CALL PDZHIT(ZPOS,RPOS(WIRE),IFDWIR)
            ELSE
              IF (HITPHI .GE. PHI3 .AND. HITPHI .LE. PHI4) THEN
                DRAWHT = .TRUE.
                CALL PDZHIT(ZPOS,-RPOS(WIRE),IFDWIR)
              ENDIF
            ENDIF
            IF (.NOT. DRAWHT .AND. ONTRK .EQ. 0) THEN
              DRAWHT = .TRUE.
              HITPHI = ATAN2(YPOS(2),XPOS(2))
              GOTO 300
            ENDIF
          ENDIF
        ENDIF
  101   IP = IP + IQ(LDHIT + 3)
  100 CONTINUE
C
  999 RETURN
      END
