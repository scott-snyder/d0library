      SUBROUTINE SAHPRO(NSTA,NSEC,NCEL,ITIME,DIST)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Convert raw ADC hits to processed hits
C-
C-    Input  :  NSTA     - Station number
C-              NSEC     - Section number
C-              NCEL     - Cell address
C-              ITIME(2) - Raw ADC values (2 tubes)
C-
C-    Output :  DIST(2)  - Drift distance for each time
C-
C-    Created :  25-SEP-93  M. Fortner
C-   Modified :  4/94 MF  drop hits that fail SADIST
C-               11/94 MF remove geometry calls
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NSTA, NSEC, NCEL, ITIME(2)
      INTEGER MC, MUDVER
      INTEGER I, IPED, C1, C2, IWIR, ITUBE,IERR
      REAL DIST(2)
      REAL FACT
      REAL SADIST
      EXTERNAL SADIST, MUDVER
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA FACT/1.5/, C1/3500/, C2/3000/
C
C                Get initial parameters
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('PEDCUT', IPED, IERR)
        CALL EZRSET
        MC = MUDVER(0)
        IF (MC.GT.0) FIRST = .FALSE.
        MC = MC/10
        IF (MC.EQ.2) MC=0
      ENDIF
C
C                Get wire address
C
      DIST(1) = -999.
      DIST(2) = -999.
      IWIR = NCEL*2 + 1
      IF (MOD(NSEC,3) .EQ. 2) IWIR=IWIR-16
      IF (IWIR .LE. 0) GO TO 999               ! It may be for U plane
      DO 10 I=1,2                              ! Tube loop
        ITUBE = IWIR + I - 1
C
C                Calculate time to distance
C
        DIST(I) = -1.
        IF (ITIME(I).LE.IPED) GOTO 10
        IF (MC .NE. 0) THEN
          DIST(I) = FACT*(C1-ITIME(I))/C2
        ELSE
          DIST(I) = SADIST(NSTA,NSEC,ITUBE,ITIME(I))
        ENDIF
   10 CONTINUE
C
  999 CONTINUE
      RETURN
      END
