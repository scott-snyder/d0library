      SUBROUTINE FDEDX(HALF,LADDER,THETA,IONIZATION,IONERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate dE/dx for an FDC track.
C-
C-   Inputs  : HALF
C-             LADDER
C-             THETA
C-   Outputs : IONIZATION
C-             IONERR
C-   Controls:
C-
C-   Created  17-JAN-1992   Susan K. Blessing
C-   Modified 06-DEC-1994   Yi-Cheng Liu : Change NTOT*TRUNCT to
C-                          NINT(NTOT*TRUNCT) for IBM machines.
c-                          ( NTOT*TRUNCT is so close to an exact
C-                          integer that it truncates differently on the 
C-                          ibm relative to vax and sgi. - Drew Baden )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IONTOT,IONDEX(34)
      INTEGER IER,NEL,NWORDS
      INTEGER ISECTD,IADD,NHITS
      INTEGER NCUT
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER IHITD,SECTD
      INTEGER IHIT,HIT,NTOT
      INTEGER LAYER,LADDER(0:2),MODULE
      INTEGER STATUS,USED,USED2,GTFSEG,IDUM
C
      REAL THETA
      REAL QHIT(18),HITION(34)
      REAL IONIZATION,IONAVG,IONERR,TRUNCT,IONSCALE
      REAL CONT(62)
      REAL FSECTD,FIADD,FNHITS
      EQUIVALENCE (ISECTD,FSECTD),(IADD,FIADD),(NHITS,FNHITS)
C
      LOGICAL FIRST
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TRUNCT',TRUNCT,IER)
        CALL EZGET('IONSCALE',IONSCALE,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      NTOT = 0
      CALL VZERO(HITION,34)
C
C  Loop through segments on ladder and get hits assigned to them.
C
      DO 100 LAYER = 0,2
        IF (LADDER(LAYER).EQ.0) GO TO 100
C
        MODULE = LAYER+3*HALF
C Do not include segments which have been used twice in calculation.
        STATUS = GTFSEG(MODULE,LADDER(LAYER),CONT)
        USED = IBITS(STATUS,2,1)
        USED2 = IBITS(STATUS,3,1)
        IF (USED2.EQ.1) GO TO 100
C
C Get contents of segment in LAYER
C
        IDUM = GTFSEG(MODULE,LADDER(LAYER),CONT)
        FSECTD=CONT(1)
        FIADD=CONT(2)
        FNHITS=CONT(3)                    ! number of hits in segment
        CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        IHITD = ISECTD/1000
        IF (ABS(IHITD).NE.0) THEN
          SECTD = SECTOR+(ISECTD/ABS(ISECTD))
        ENDIF
C
C  Loop through hits on segment and get their coordinates
C
        DO 200 HIT = 1,NHITS
          IF ( (HIT .GE. ABS(IHITD)) .AND. (IHITD .NE. 0) ) THEN
            SECTOR = SECTD
          ENDIF
          IF (UNIT.LE.0) THEN
            IHIT = CONT(11+HIT)
            CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT,NEL,NWORDS,QHIT)
          ELSE
            IHIT = CONT(19+HIT)
            CALL GTFPSC(HALF,SECTOR,'HIT',IHIT,NEL,NWORDS,QHIT)
          END IF
C
          NTOT = NTOT+1
          HITION(NTOT) = QHIT(7)
C
  200   CONTINUE                        ! End of loop over hits in layer
  100 CONTINUE                          ! End of loop over layers in track
C
      IONIZATION = 0.
      IONERR = 0.
      IONTOT = 0
      CALL SRTFLT(HITION,NTOT,IONDEX)
      NCUT = NINT(NTOT*TRUNCT)
      DO 101 HIT = 1, NCUT
        IONIZATION = IONIZATION+HITION(HIT)
        IONERR = IONERR+HITION(HIT)**2.
        IONTOT = IONTOT+1
  101 CONTINUE
      IF (IONTOT.GT.0) THEN
        IONIZATION = IONIZATION/IONTOT
        IONERR = IONERR/(IONTOT**2.) - IONIZATION**2.
        IONERR = SQRT(ABS(IONERR))
      ENDIF
C
      IONIZATION = ABS(IONIZATION*COS(THETA))  ! ionization, angle corrected
      IONERR = ABS(IONERR*COS(THETA))  ! ionization error, angle corrected
C
  999 RETURN
      END
