      FUNCTION DISA_MARK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Mark CDC ISAJET track in DITR bank as being
C-      matched with reconstructed CDC track if it is the closest
C-      matched track defined by having the smallest direction cosine
C-      between the CDC and ISAJET track.
C-
C-   Returned value  : .TRUE. (always)
C-   Inputs  : isajet and cdc banks.
C-   Outputs : Fills repeated word 9 of DITR banks
C-
C-   Created  3-MAY-1994   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL DISA_MARK
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER GZISAE,LISAE
      INTEGER GZDTRH,LDTRH
      INTEGER GZDITR,LDITR
      INTEGER IER
      INTEGER ISATRK,NISATRK
      INTEGER ICDCTRK ,NCDCTRK, ICDC_BEST
      INTEGER ICDCDAT(26)
      INTEGER LBASE
C
      REAL    ISADAT(9)
      REAL    CDCDAT(26)
      REAL    X_CDC,Y_CDC,Z_CDC
      REAL    X_VTX,Y_VTX,Z_VTX
      REAL    PHI_CDC, PHI_ISA
      REAL    THE_CDC, THE_ISA
      REAL    DELTA_TRK_CUT, DELTA
      REAL    DELTA_BEST, DRCOS
C
      LOGICAL FIRST
      EQUIVALENCE (ICDCDAT,CDCDAT)
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      DISA_MARK = .FALSE.
C
C ****  Check on ISAJET information
C
      LISAE = GZISAE()
      IF ( LISAE .LE. 0 ) GOTO 999
C
C ****  Check on CDC track information
C
      LDTRH = GZDTRH()
      IF ( LDTRH .LE. 0 ) GOTO 999
      NCDCTRK = IQ(LDTRH+2)
      IF ( NCDCTRK .LE. 0 ) GOTO 999
C
C ****  Check on CDC ISAJET information
C
      LDITR = GZDITR()
      IF ( LDITR .LE. 0 ) GOTO 999
      NISATRK = IQ( LDITR + 1 )
      IF ( NISATRK .LE. 0 ) GOTO 999
C
C ****  Get cut values
C
      IF ( FIRST ) THEN
          CALL EZPICK('DTRAKS_RCP')
          CALL EZGET('DELTA_TRK_CUT',DELTA_TRK_CUT,IER)
          CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
C ****  Loop on ISAJET tracks
C
      DO ISATRK=  1, NISATRK
        CALL GTDITR(ISATRK, ISADAT)
        X_VTX = ISADAT(1)
        Y_VTX = ISADAT(2)
        Z_VTX = ISADAT(3)
        PHI_ISA = ISADAT(4)
        THE_ISA = ISADAT(5)
C
        DELTA_BEST = 0.
        ICDC_BEST = -1
C
C ****  Loop on CDC reconstructed tracks (DTRK)
C
        DO  ICDCTRK =  1, NCDCTRK
          CALL GTDTRK(ICDCTRK,CDCDAT)
          X_CDC = CDCDAT(7)
          Y_CDC = CDCDAT(8)
          Z_CDC = CDCDAT(11)
          PHI_CDC = CDCDAT(6)
          THE_CDC = CDCDAT(9)
C
C ****  Define best match as COS(ANGLE)>.9
C ****  (note that other definitions are possible)
C
          DELTA = DRCOS(PHI_CDC,THE_CDC,PHI_ISA,THE_ISA)
C
          IF ( DELTA .GT. DELTA_TRK_CUT ) THEN
            IF ( DELTA .GT. DELTA_BEST
     &          .OR. (ICDC_BEST.EQ.-1) ) THEN
              ICDC_BEST   =  ICDCTRK
              DELTA_BEST =  DELTA
            ENDIF
          ENDIF
        ENDDO
C
C ****  Mark DITR bank with CDC track number associated with it.
C
        LBASE = LDITR + 2 + IQ(LDITR+2)*(ISATRK-1)
        IF ( ICDC_BEST.GT.0 ) THEN
          Q(LBASE+9) = ICDC_BEST
        ELSE
          Q(LBASE+9) = -999
        ENDIF
      ENDDO
C
      DISA_MARK = .TRUE.
  999 RETURN
      END
