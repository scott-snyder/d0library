      SUBROUTINE PDHITS_3D
c----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw CDC hits in 3D
C-                         if compressed hits bank exist, use compressed 
C-                         hits banks, otherwise use normal hits bank
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  12-DEC-1991   Qizhong Li-Demarteau
C-   Updated   2-JUL-1992   Qizhong Li-Demarteau  added choice for draw_hits
C-                             draw_hits=1 all hits are same color
C-                             draw_hits=2 mark hit on track
C-                             draw_hits=3 mark mirror hits (+phi purple;
C-                                                          (-phi green)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAYER, SECTOR, NHIT
      INTEGER LCDCH, GZCDCH, LDHIT, GZDHIT, LDALS, GZDALS, LDRFT
      INTEGER NBWIR, KPWIRE(0:6), NHITS(0:6), LHIT, IWIRE, IPWIR, KP
      INTEGER DRAW_HITS, LRCP
      INTEGER IHIT, WORDS(2)
      INTEGER LAY, SEC, WIRE, JHIT, ISIDE, ONTRK, IZTRK,IER
      REAL    XPOS(2), YPOS(2), ZPOS, AREA
      REAL    XWIR, YWIR, CPHIW, SPHIW, DDIS, SIZE
      PARAMETER( SIZE = 0.3 )
      LOGICAL OK,EZERROR
C----------------------------------------------------------------------
C
C ****  Getting drawing parameter
C
      CALL EZPICK('PX_ZTRAKSDIS_RCP')      
      IF ( .NOT. EZERROR(IER) ) THEN
        CALL PUGETV('ZTRAKS DRAW HITS  ',DRAW_HITS)
        CALL EZRSET
      ENDIF
C
      IF (DRAW_HITS .LE. 0) THEN
        CALL EZLOC('PX_ZTRAKSDIS_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL EZPICK('PX_CDCDIS_RCP')      
          IF ( .NOT. EZERROR(IER) ) THEN
            CALL PUGETV('CDC DRAW 3D HITS', DRAW_HITS)
            CALL EZRSET
          ENDIF
        ENDIF
      ENDIF
      IF (DRAW_HITS .LE. 0) GOTO 999
C
C  draw CDC hits
C
      LCDCH = GZCDCH()
      IF (LCDCH .LE. 0) GOTO 999
C
      CALL PUOPEN
      CALL PXCOLR('MAG')
C
C   use compressed hits bank, if it exists
C
      LDHIT = GZDHIT()
      IF (LDHIT .LE. 0) GOTO 200
      CALL GTDHIT(0,WORDS,LAY,SEC,WIRE,JHIT,ONTRK,ISIDE,
     &                  IZTRK,XPOS,YPOS,ZPOS,AREA)
      NHIT = WORDS(1)
      DO 100 IHIT = 1, NHIT
        CALL GTDHIT(IHIT,WORDS,LAY,SEC,WIRE,JHIT,ONTRK,ISIDE,
     &                  IZTRK,XPOS,YPOS,ZPOS,AREA)
        IF (WIRE .EQ. 0 .OR. WIRE .EQ. 6) THEN
          IF (ZPOS .LT. 99.0) THEN
            IF (XPOS(1) .LT. 99.0 .AND. YPOS(1) .LT. 99.0) THEN
              CALL PZDRAW_3DHIT(XPOS(1),YPOS(1),ZPOS,SIZE)
              IF (DRAW_HITS .GT. 1 .AND. ONTRK .GT. 0) THEN
                CALL PXCOLR('CYA')
                CALL PZDRAW_3DHIT(XPOS(1),YPOS(1),ZPOS,SIZE)
                CALL PXCOLR('MAG')
              ENDIF
            ENDIF
            IF (ONTRK .EQ. 0) THEN
              IF (XPOS(2) .LT. 99.0 .AND. YPOS(2) .LT. 99.0) THEN
                IF (DRAW_HITS .GT. 2) CALL PXCOLR('GRE')
                CALL PZDRAW_3DHIT(XPOS(2),YPOS(2),ZPOS,SIZE)
                CALL PXCOLR('MAG')
              ENDIF
            ENDIF
          ENDIF
        ENDIF
  100 CONTINUE
      GOTO 900
C
C  use normal hits bank, if it exists
C
  200 CONTINUE
      LDRFT = LC(LDGEH - 3)
      DO 300 LAYER = 0, 3
        DO 400 SECTOR = 0, 31
          CALL ZGDSEC(LAYER, SECTOR, NBWIR, KPWIRE(0), NHITS(0), LHIT)
          IF (LHIT .GT. 0) THEN
            LDALS = GZDALS(LAYER, SECTOR)
            IF (LDALS .LE. 0) GOTO 900
            CPHIW = C(LDALS+3)
            SPHIW = C(LDALS+4)
            DO 500 IWIRE = 0, 6, 6
              IPWIR = LDALS + 6 + IC(LDALS+6) * IWIRE
              XWIR = C(IPWIR+1)
              YWIR = C(IPWIR+2)
              DO 600 IHIT = 1, NHITS(IWIRE)
                KP = KPWIRE(IWIRE) + (IHIT-1)*LHIT + 2
                DDIS = Q(KP) - C(LDRFT+26+IWIRE)
                XPOS(1) = XWIR + DDIS * CPHIW
                YPOS(1) = YWIR + DDIS * SPHIW
                ZPOS = Q(KP+2)
                CALL PZDRAW_3DHIT(XPOS(1),YPOS(1),ZPOS,SIZE)
                DDIS = Q(KP+1) - C(LDRFT+26+IWIRE)
                XPOS(2) = XWIR + DDIS * CPHIW
                YPOS(2) = YWIR + DDIS * SPHIW
                IF (DRAW_HITS .GT. 2) CALL PXCOLR('GRE')
                CALL PZDRAW_3DHIT(XPOS(2),YPOS(2),ZPOS,SIZE)
                CALL PXCOLR('MAG')
  600         CONTINUE
  500       CONTINUE
          ELSE
            GOTO 400
          ENDIF
  400   CONTINUE
  300 CONTINUE
C
  900 CALL PUCLOSE
C
  999 RETURN
      END
