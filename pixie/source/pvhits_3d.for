      SUBROUTINE PVHITS_3D
c----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw VTX hits in 3D
C-                         if compressed hits bank exist, use compressed 
C-                         hits banks, otherwise use normal hits bank
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  14-JUL-1992  Tom Trippe  (patterned after PDHITS 2-JUL-1992
C-                                        version from Qizhong Li-Demarteau)
C-   Modified 23-SEP-1992  Tom Trippe - plot only 3d hits, reduce SIZE
C-   Modified 20-OCT-1992  Tom Trippe - correct bug in uncompressed hits sec.
C-
C-                             draw_hits=1 all hits are same color
C-                             draw_hits=2 mark hit on track
C-                             draw_hits=3 mark mirror hits (+phi purple;
C-                                                          (-phi green)
C-   Modified 05-NOV-1992  Nobu Oshima - remove DRAW_HITS checking for new
C-                                       ZTRAKS structure.
C    Modified 27-June-1994 Danilo Puseljic Added call to VCHT_UNPACK to 
C                                          enable hit display from VCHT bank 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LAYER, SECTOR, SECMAX, NHIT
      INTEGER LVTXH, GZVTXH, LVHIT, GZVHIT, LVALS, GZVALS, LVRFT
      INTEGER NBWIR, KPWIRE(0:7), NHITS(0:7), LHIT, IWIRE, IPWIR, KP
      INTEGER IHIT, WORDS(3)
      INTEGER LAY, SEC, WIRE, JHIT, IENDS, ONTRK, ISIDE, IZTRK
      INTEGER LVCHT, GZVCHT, ISTAT
      REAL    XPOS(2), YPOS(2), ZPOS, AREA
      REAL    XWIR, YWIR, CPHIW, SPHIW, DDIS, SIZE
      PARAMETER( SIZE = 0.15 )
C----------------------------------------------------------------------
C
C -- Draw VTX hits
C
      LVTXH = GZVTXH()
      IF (LVTXH .LE. 0) GOTO 999
C
      CALL PUOPEN
      CALL PXCOLR('MAG')
C
C -- Use VHIT hit banks, if they exist or ....
C
      LVHIT = GZVHIT()
      IF (LVHIT .LE. 0) GOTO 200
      CALL GTVHIT(0,WORDS,LAY,SEC,WIRE,JHIT,IENDS,
     &                  ONTRK,ISIDE,IZTRK,XPOS,YPOS,ZPOS,AREA)
      NHIT = WORDS(1)

      DO 100 IHIT = 1, NHIT
        CALL GTVHIT(IHIT,WORDS,LAY,SEC,WIRE,JHIT,IENDS,
     &                  ONTRK,ISIDE,IZTRK,XPOS,YPOS,ZPOS,AREA)
        IF (IENDS .EQ. 3) THEN

          IF (ZPOS .LT. 99.0) THEN

            IF (XPOS(1) .LT. 99.0 .AND. YPOS(1) .LT. 99.0) THEN
              CALL PZDRAW_3DHIT(XPOS(1),YPOS(1),ZPOS,SIZE)
              IF (ONTRK .GT. 0) THEN
                CALL PXCOLR('CYA')
                CALL PZDRAW_3DHIT(XPOS(1),YPOS(1),ZPOS,SIZE)
                CALL PXCOLR('MAG')
              ENDIF
            ENDIF

            IF (ONTRK .EQ. 0) THEN
              IF (XPOS(2) .LT. 99.0 .AND. YPOS(2) .LT. 99.0) THEN
                CALL PZDRAW_3DHIT(XPOS(2),YPOS(2),ZPOS,SIZE)
                CALL PXCOLR('MAG')
              ENDIF
            ENDIF

          ENDIF

        ENDIF

  100 CONTINUE   ! end loop over hits

      GOTO 900   ! all done go to the end
C
C -- VSEC banks if they exist and if not reconstruct them from VCHT 
C    banks if they are found
  200 CONTINUE

      LVRFT = LC(LVGEH - 3)
      DO 300 LAYER = 0, 2
        SECMAX = 31
        IF (LAYER .EQ. 0) SECMAX = 15

        DO 400 SECTOR = 0, SECMAX
          CALL ZGVSEC(LAYER, SECTOR, NBWIR, KPWIRE(0), NHITS(0), LHIT)

          IF (LHIT .LE. 0) THEN
            LVCHT=GZVCHT()
            IF(LVCHT .NE. 0)THEN
              CALL VCHT_UNPACK(LAYER,SECTOR,ISTAT)
              CALL ZGVSEC(LAYER,SECTOR,NBWIR,KPWIRE(0),NHITS(0),LHIT)
              IF (LHIT .LE. 0) GOTO 400
            ELSE
              GOTO 400        ! not enough info to do hits
            ENDIF
          ENDIF

          LVALS = GZVALS(LAYER, SECTOR)
          IF (LVALS .LE. 0) GOTO 900
          CPHIW = C(LVALS+3)
          SPHIW = C(LVALS+4)

          DO 500 IWIRE = 0, 7
            IPWIR = LVALS + 6 + IC(LVALS+6) * IWIRE
            XWIR = C(IPWIR+1)
            YWIR = C(IPWIR+2)

            DO 600 IHIT = 1, NHITS(IWIRE)
              KP = KPWIRE(IWIRE) + (IHIT-1)*LHIT + 2
              IENDS = IBITS(IQ(KP+8),0,2)
              IF (IENDS .NE. 3) GO TO 600
              DDIS = Q(KP) - C(LVRFT+31+IWIRE) * (-1**SECTOR)
              XPOS(1) = XWIR + DDIS * CPHIW
              YPOS(1) = YWIR + DDIS * SPHIW
              ZPOS = Q(KP+2)
              CALL PZDRAW_3DHIT(XPOS(1),YPOS(1),ZPOS,SIZE)
              DDIS = Q(KP+1) - C(LVRFT+31+IWIRE) * (-1**SECTOR)
              XPOS(2) = XWIR + DDIS * CPHIW
              YPOS(2) = YWIR + DDIS * SPHIW
              CALL PZDRAW_3DHIT(XPOS(2),YPOS(2),ZPOS,SIZE)
              CALL PXCOLR('MAG')
  600       CONTINUE                ! end loop over hits

  500     CONTINUE             ! end loop over wires

  400   CONTINUE            ! end loop over sectors

  300 CONTINUE         ! end loop over layers
C
C -- End of job pixie stuff
C
  900 CALL PUCLOSE
C
  999 RETURN
      END
