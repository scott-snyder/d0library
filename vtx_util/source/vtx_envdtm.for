      SUBROUTINE VTX_ENVDTM(DFAC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Udate the SCALE factor in the DTM banks.  On the
C-               first call, save initial values of SCALE, on subsequent calls,
C-               update this value
C-
C-   Inputs  : DFAC -- envirnmental correction factor for SCALE
C-   Outputs :
C-   Controls:
C-
C-   Created  24-DEC-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC' 
      INCLUDE 'D0$LINKS:IZVTMW.LINK'
      INCLUDE 'D0$LINKS:IZVDTM.LINK'
c I/O
      REAL DFAC
c Locals:
      LOGICAL FIRST,DONE(0:31)
      INTEGER ERR,NCAT
      INTEGER LVTMW,LVDTM,ITEMS,OFFSET
      INTEGER LAYER,SECTOR,CATEG,NUM_CATEG(0:2),NSEC(0:2)
      REAL    SCALE,SSAVE(0:95),SsAVE0(0:95)
      REAL    VDVEL(0:2)
      INTEGER LAST_RUN
c Externals:
      INTEGER RUNNO
c Data
      DATA  FIRST/.TRUE./
      DATA   NSEC/15,31,31/
      DATA LAST_RUN/-1/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('NUM_CATEG',NUM_CATEG,ERR)
        CALL EZGET('VTX_TO_CANARY',VDVEL,ERR)
        CALL EZRSET
      ENDIF
C
C ****  Save values of SCALE on first call (SSAVE0).  If run number has 
C ****  changed, call VTX_ENVINI again
C
      IF (RUNNO() .NE. LAST_RUN) THEN
        CALL VTX_ENVINI(SCALE)
        DO LAYER = 0,2
          DO CATEG = 0,NUM_CATEG(LAYER)-1
            DONE(CATEG) = .FALSE.
          ENDDO
          NCAT = 0
          LVTMW = LC(LVTMH - IZVTMW - LAYER)
          ITEMS = IC(LVTMW+3)
          OFFSET = LVTMW + 6 + 8*ITEMS*IC(LVTMW+5)
          DO SECTOR = 0,NSEC(LAYER)
            CATEG = IC(OFFSET+SECTOR)
            IF (.NOT. DONE(CATEG)) THEN
              DONE(CATEG) = .TRUE.
              NCAT = NCAT + 1
              LVDTM = LC( LVTMW - (IZVDTM+CATEG) )
              IF (LAST_RUN .LT. 0) SSAVE0(32*LAYER + CATEG) = C(LVDTM+6)
              SSAVE(32*LAYER + CATEG) = 
     &          SSAVE0(32*LAYER + CATEG)*SCALE/VDVEL(LAYER)
              IF (NCAT .GE. NUM_CATEG(LAYER)) GO TO 10
            ENDIF
          ENDDO
   10   ENDDO
        LAST_RUN = RUNNO()
      ENDIF
C
C ****  Normal entry
C
      DO LAYER = 0,2
        DO CATEG = 0,NUM_CATEG(LAYER)-1
          DONE(CATEG) = .FALSE.
        ENDDO
        NCAT = 0
        LVTMW = LC(LVTMH - IZVTMW - LAYER)
        ITEMS = IC(LVTMW+3)
        OFFSET = LVTMW + 6 + 8*ITEMS*IC(LVTMW+5)
        DO SECTOR = 0,NSEC(LAYER)
          CATEG = IC(OFFSET+SECTOR)
          IF (.NOT. DONE(CATEG)) THEN
            DONE(CATEG) = .TRUE.
            NCAT = NCAT + 1
            LVDTM = LC( LVTMW - (IZVDTM+CATEG) )
            DO WHILE (LVDTM .GT. 0)
              C(LVDTM+6) = SSAVE(32*LAYER+CATEG)*DFAC
              LVDTM = LC(LVDTM)
            ENDDO
            IF (NCAT .GE. NUM_CATEG(LAYER)) GO TO 50
          ENDIF
        ENDDO
   50 ENDDO
  999 RETURN
      END
