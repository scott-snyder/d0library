      SUBROUTINE FTSGPAR(HALF,QUAD,SECTOR,IWIRE,ILR,IHIT,
     &  THETA,PHI,NDELAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate a Theta segments theta and phi angle.
C-
C-   Inputs  : HALF,QUAD,SECTOR,IWIRE = Logical Address location
C-             ILR  = left/right ambiguity soluiton
C-             IHIT = number of hit on IWIRE
C-   Outputs : THETA, PHI = angles in D0 coordinates
C-             NDELAY = number of delay line hits used (0, 1, or 2)
C-
C-   Created  18-JUN-1990   Jeffrey Bantly
C-   Updated  29-APR-1991   Jeffrey Bantly  cleanup, use new RCP,PARAMS 
C-   Updated  13-MAY-1991   Susan K. Blessing  Add NDELAY to output
C-                                             parameters
C-   Updated  26-NOV-1991   Susan K. Blessing  Check if PHI is gt twopi
C-    and correct if necessary.
C-   Updated  18-MAR-1993   Susan K. Blessing  Correct calculation of 
C-    theta.  Drift distance should be taken into account.
C-   Updated  18-OCT-1993   Robert E. Avery  Reduce size of  QHIT array.
C-    Check delayline existance by looking at status word.
C-    (Pointer's don't exist for GEAN hits).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,QUAD,SECTOR
      INTEGER IWIRE,ILR,IHIT
      INTEGER LHIT,NEL,NWORDS
      INTEGER LKFTSC,GZFTSC
      INTEGER NDELAY
      INTEGER STATUS 
C
      REAL THETA,PHI,DELAY,EDELAY
      REAL XC,YC,ZC,RC
      REAL RHIT,PHI1,PHI2,RADTB
      REAL DRIFT
      LOGICAL BTEST
C
      REAL QHIT(18)
      INTEGER IQHIT(18)
      EQUIVALENCE (IQHIT,QHIT)
C
C----------------------------------------------------------------------
C
      THETA = 0.
      PHI = 0.
      NDELAY = 0
      LKFTSC = GZFTSC(HALF,QUAD,SECTOR)
      IF (LKFTSC.LT.5) GO TO 999
      LHIT = IQ(LKFTSC+4+IQ(LKFTSC+2)+(ILR/2))+
     &                         ((IHIT-1)*IQ(LKFTSC+3))-1
      CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',LHIT,NEL,NWORDS,QHIT)
C
      DRIFT = QHIT(ILR+2)
C Sector 1 drifts in the opposite direction
      IF (SECTOR.EQ.1) DRIFT = -DRIFT
C
      IF (IWIRE.EQ.0) THEN             ! wire 0 carries delay line info
        DELAY = QHIT(4)
        EDELAY = QHIT(6)
        STATUS = IQHIT(9)
C Find number of delay hits used
        IF (BTEST(STATUS,0).AND.BTEST(STATUS,1)) THEN
          NDELAY = 2
        ELSE IF (BTEST(STATUS,0).OR.BTEST(STATUS,1)) THEN
          NDELAY = 1
        ELSE
          NDELAY = 0
        END IF
      ELSE
        DELAY = 0.
        EDELAY = 0.
      ENDIF
      IF (EDELAY .GT. 10.) EDELAY = 9.9
C
      CALL GTFALH(HALF,0,QUAD,SECTOR,IWIRE,XC,YC,ZC)
      IF (ZC.EQ. 0.0) GO TO 999       ! no wire location info - skip
      RC = SQRT(XC**2+YC**2)
C      RHIT = SQRT(XC**2+YC**2+DELAY**2)       ! could be done better
      RHIT = SQRT((RC+DRIFT)**2 + DELAY**2)
      PHI1 = ATAN2(YC,XC)
      PHI2 = ATAN2(DELAY,RC)
      PHI = PHI1 + PHI2
      IF (YC.LT.0.) PHI = PHI+TWOPI
      IF (PHI.LT.0.) PHI = PHI+TWOPI
      IF (PHI.GT.TWOPI) PHI = PHI - TWOPI
      THETA = ATAN2(RHIT,ZC)
C
C----------------------------------------------------------------------
  999 RETURN
      END
