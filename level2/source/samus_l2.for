      SUBROUTINE SAMUS_L2(ITRACKS,PTTRACK,ETATRACK,PHITRACK,SIDETRACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SAMUS level 2 main subroutine
C-
C-   Inputs  : PARAM_SET_NUMBER : Number in series of parameters to use.
C-             HARDWARE : Mask with bit set for Level-1 trigger which
C-                        started this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when TOOL wants event passed
C-             EXTRA_FLAG :  Not used
C-   Controls: None
C-
C-   Created  24-OKT-1992 Alexsei Volkov
C-   Updated  27-OKT-1992 Alexsei Volkov
C-   Modified 03-Nov-1992 Tom Diehl
C-   Updated  13-NOV-1992   Alexander Efimov: change SAMUS banks
C-                                            structure
C-   Updated  06-DEC-1992 Alexsei Volkov
C-   MODIFIED 07-DEC-1992 DIEHL added ESUM
C-   Updatted 07-DEC-1992 Alexsei changed P to Eta
C-   MODIFIED 08-DEC-1992 DIEHL removed ESUM; made this a MUON_L2 subtool
C-                              included SIDETRACK 
C-   Modified 26-APR-1993 Diehl Check if SAHTFL already done.
C-   Modified 19-FEB-1994 Denisov corrected for Ib: GZ and comments
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      LOGICAL SAMUS_L2_SIDE
C
      INTEGER IDUMMY
      REAL ETAMU_PHYSICS,ETAMU_DETECTOR,PHIMU
      INTEGER KEYT
      INTEGER N_PLANES
      PARAMETER (N_PLANES=3)
      INTEGER LSAMT, LSAHS
      INTEGER DIR, NST, IND, IST, IPL, N
      INTEGER LTRG1, LTRG2, ITR, SAMUS_END(2)
      INTEGER ITRACKS               !DIEHL
      REAL PTTRACK(48),ETATRACK(48),SIDETRACK(48)          !DIEHL
      REAL PT(24), ETA(24), THETA(24), PHI(24)
      INTEGER IERR, NHTMX
      LOGICAL OK, FIRST
      DATA FIRST /.TRUE./
      SAVE FIRST, NHTMX
      REAL    PHITRACK(48)
C
      INTEGER EVENT_NUM_LOW, EVENT_NUM_HIGH
      INTEGER LAST_EVENT_NUM_LOW, LAST_EVENT_NUM_HIGH
      INTEGER LMTRH,GZMTRH,GZSAMT,GZSAHS
C
      DATA LAST_EVENT_NUM_LOW/0/, LAST_EVENT_NUM_HIGH/0/
C----------------------------------------------------------------------
      EVENT_NUM_LOW  = IQ(LHEAD+7)
      EVENT_NUM_HIGH = IQ(LHEAD+8)
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('NHTMX', NHTMX, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C---- This if loop called only once per event -----------------
C
      IF (EVENT_NUM_LOW .NE. LAST_EVENT_NUM_LOW .OR.
     $   EVENT_NUM_HIGH .NE. LAST_EVENT_NUM_HIGH) THEN
C
        LAST_EVENT_NUM_LOW  = EVENT_NUM_LOW
        LAST_EVENT_NUM_HIGH = EVENT_NUM_HIGH
C
        ITRACKS = 0
C
C...  Check L1 SAMUS trigger side
C
        CALL SAMUS_L2_SIDE(SAMUS_END) 
        IF(SAMUS_END(1).NE.0.OR.SAMUS_END(2).NE.0) THEN 
C
C...    CREATION OF SAMUS BANKS AND FILLING WITH RAW DATA
C
          LMTRH = GZMTRH()
          IF(LMTRH.NE.0) THEN
            LSAMT = GZSAMT()
            IF(LSAMT.EQ.0) THEN
              CALL BKSAMT(LSAMT)
              LSAHS = GZSAHS()
              KEYT = 1
              CALL SAHTFL(OK,KEYT)
            ENDIF
          ELSE
            CALL ERRMSG('NO LMTRH BANK IN SAMUS_L2.','L2_MUON',' ','W')
            OK = .FALSE.
          ENDIF
C
C...   EVENT PROCESSING
C
          IF (OK) THEN
            DO 1000 DIR = 1, 2              ! SAMUS_END LOOP
	      IF (SAMUS_END(DIR).GE.1) THEN
                NST = 3 * DIR - 2
                DO IST = NST, NST+2       
                  DO IPL = 1, N_PLANES      ! CHECK "FIRE" EVENTS
                    IND = 3 * (IST - 1) + IPL
                    N = IQ(LSAHS+IND)
                    IF (N .EQ. 0 .OR. N .GT. NHTMX) GO TO 1000   !HIT MULT CUT
                  END DO
C
C...   TRIPLETS SEARCH
C
                  CALL SATG1A (IST, LTRG1)
                  IF (LTRG1 .LE. 0) GO TO 1000
                END DO
C
C...   TRACKS SEARCH
C
                CALL SATG2A (DIR, LTRG2, PT, ETA, THETA, PHI)
                IF (LTRG2 .GT. 0) THEN
                  DO ITR = 1, LTRG2
                    ITRACKS = ITRACKS + 1
                    PTTRACK(ITRACKS) = PT(ITR)
                    ETATRACK(ITRACKS) = ETA(ITR)
                    PHITRACK(ITRACKS) = PHI(ITR)
                    SIDETRACK(ITRACKS) = DIR
                  END DO
                END IF
              END IF           !SAMUS_END  ENDIF
 1000       CONTINUE           !SAMUS_SIDE LOOP END
          END IF               !"OK"
        END IF                 !ANOTHER SAMUS_END CHECK
      ENDIF                    !ONCE PER EVENT LOOP END
C----------------------------------------------------------------------
      RETURN
      END
