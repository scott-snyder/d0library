C+
      SUBROUTINE SAMTRK (DIR, KEYTRG, NTRACKS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : S/R for SAMUS tracks reconstruction.
C-
C-   Inputs  : DIR - SAMUS direction number.( 1 - North ; 2 - South)
C-   Outputs : KEYTRG - (+1) if Level_2 is OK for this event, else (-1),
C-             NTRACKS - number of founded tracks.
C-   Controls: none.
C-
C-   Created  11-AUG-1992   Alexander Efimov
C-   Updated   9-FEB-1994   Alexander Efimov
C-   Updated   8-JUN-1994   Andrei Mayorov  skip full tracking at LEVEL 2
C-   Updated  12-DEC-1994   Andrei Mayorov  update call to SATRMC
C-   Updated  20-JAN-1995   Andrei Mayorov  change call to SATG2A
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, KEYTRG, NTRACKS
      INTEGER N_PLANES
      PARAMETER (N_PLANES=3)
      INTEGER GZSAHS, GZSATN, GZSATS
      INTEGER LSAHS, LTRK, NTRA, NTRB
      INTEGER LST, IST, IPL, IND
      INTEGER LTRG1, LTRG2
      INTEGER NHTMX, NTRG2
      SAVE    NHTMX, NTRG2, FIRST
      INTEGER IERR, N, THIS_EVENT, LAST_EVENT(2)
      CHARACTER*4 THIS_PATH, FILT_PATH
      LOGICAL FIRST, FILTER
      DATA FIRST /.TRUE./
      DATA FILT_PATH /'FILT'/
      DATA LAST_EVENT / 0, 0 /
C
C ****  initializing
C
      LSAHS = GZSAHS()
      IF (LSAHS .LE. 0) GO TO 999
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('NHTMX', NHTMX, IERR)
        CALL EZGET  ('NTRG2', NTRG2, IERR)
        CALL PATHGT(THIS_PATH)
        FILTER = THIS_PATH.EQ.FILT_PATH
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      IF (DIR .EQ. 1) THEN
        LTRK = 1 + GZSATN()
      ELSE
        LTRK = 1 + GZSATS()
      END IF
      IQ(LTRK) = 0
      KEYTRG = -1
      NTRACKS = 0
C
C **** only track once per event (temporary PQ 17-jun-94)
C **** further modify -- once per side (dir=1,2)
      THIS_EVENT = IQ(LHEAD+7)
      IF (THIS_EVENT.EQ.LAST_EVENT(DIR)) THEN
        RETURN
      ELSE
        LAST_EVENT(DIR) = THIS_EVENT
      ENDIF
C
C ****  check "fire" events
C
      LST = 3 * DIR - 2
      DO IST = LST, LST+2
        DO IPL = 1, N_PLANES
          IND = 3 * (IST - 1) + IPL
          N = IQ(LSAHS+IND)
          IF (N .EQ. 0 .OR. N .GT. NHTMX) GO TO 999
        END DO
      END DO
C
C ****  check Level 1 trigger
C
      DO IST = LST, LST+2
        CALL SATG1A (IST, LTRG1)
        IF (LTRG1 .LE. 0) GO TO 999
      END DO
C
C ****  check Level 2 trigger
C
      CALL SATG2A (DIR, LTRG2)
      IF (LTRG2 .EQ. 0 .OR. LTRG2 .GT. NTRG2) GO TO 999
      KEYTRG = +1
C
C ****  full tracks reconstruction
C
      IF(FILTER) THEN
        NTRACKS=LTRG2
      ELSE
        CALL SAMTRB (DIR, NTRB)
        IF (NTRB .EQ. 0) GO TO 999
        CALL SAMTRA (DIR, NTRA)
        IF (NTRA .EQ. 0) GO TO 999
        CALL SATRLN (DIR, NTRACKS)
        CALL SATRMC (DIR,NTRACKS)
      END IF
      RETURN
C
  999 CONTINUE
      IQ(LTRK) = 0
      RETURN
      END
