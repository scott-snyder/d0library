      SUBROUTINE L2_EM_TRANS_CUT(JENRG_BIN,JETA_BIN,IETA,CC,EM3,
     &  ESH3,ESH5,ESH7,TIGHT,IFAILED,TRANS,GOOD_REGION,
     &  SIGMA3,S3,SH13,SH35,SH57,ESH2,ESH4,SH24,SIGMA5,S3HIG)
C----------------------------------------------------------------------
C-
C-   Purpose And Methods : check transverse shape of EM shower
C-   Inputs  : JENRG_BIN energy bin number of candidate
C-             JETA_BIN  eta bin of candidate
C-             IETA  IETAC of candidate
C-             CC = .TRUE. if the candidate is in the CC
C-             EM3(-2:2,-2:2)  local array in EM3 space around candiate
C-                      (not filled for COARSE_EM3)
C-             ESH3,ESH5,ESH7 are energies in 3x3, 5x5, and (for forward) 7x7
C-                around the candidate
C               WARNING: when COARSE_EM3, the meaning changes from EM3 space to
C-                readout tower space
C-             CUTS from l2_em_stp.inc
C-   Outputs : IFAILED = 0   a success; else indicates reason for failure
C-          quantities derived from inputs:
C-             SIGMA3,SIGMA5 <r> in 3x3 and 5x5 EM3 cells around peak
C-             ESH2,ESH4 energy in 2x2 and 4x4 EM3 cells around peak
C-             S3 computed Energy-dependent central value for cut on SIGMA3
C-             RAij is E(jxj)/E(ixi) - 1; cuts are on these quantities
C-              these variables returned to fill L2EM
C-   Controls: CC = .FALSE. do fewer shape cuts in EC
C-             TIGHT = .FALSE. only cut on 1 variable each in EC or CC
C-             COARSE_EM3 = .TRUE. far forward, do fewer still
C-                EM3 was not filled for this case; ESH3, ESH5 and ESH7 are
C-             GOOD_REGION  .FALSE. if in a compromised area: don't cut
C-             TRANS     = .FALSE. if don't want any cutting done
C-
C-   CreAted 15-SEP-1990   Yi  XiA
C-   Updated  16-DEC-1991   James T. Linnemann  pull cuts out of filling
C-                                              redo loops; merge EC, CC more
C-   Updated   8-FEB-1992   James T. Linnemann  debugging by Scott Snyder;
C-                    report computed variables for ease of checking
C-   Updated  25-FEB-1992  Yi  Xia put more results as output to fill L2EM
C-   Updated  26-JUL-1992  James T. McKinley use binned cuts for 5X5-3X3/3X3
C-                          cuts in EC only (IETA.LT.31), high side cut only,
C-                          cut not done in CC.
C-   Updated   7-AUG-1992   James T. Linnemann   TIGHT: separate primary from
C-                            secondary cuts
C-   Updated   2-SEP-1992  James T. McKinley calculate all variables and then
C-                         do cuts in order to fill L2EM bank with all variables
C-   Updated  25-SEP-1992  James T. McKinley  fix 7x7-5x5/5x5 cut bug, was
C-                         doing cut in CC!
C-   Updated  28-SEP-1992  James T. McKinley use binned cuts for 4x4-2x2/2x2
C-   Updated  29-SEP-1992  James T. McKinley use binned cuts for 5x5-3x3/3x3
C-                         as secondary cut in the CC, cut was not doing
C-                         anything unbinned (turned off in L2_EM.RCP).
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      INCLUDE 'D0$INC:L2_EM_STP.INC'
C
      INTEGER JENRG_BIN,JETA_BIN,IETA,IFAILED
      REAL EM3(-2:2,-2:2)
      INTEGER I,J,ESTEP,PSTEP   !indices and steps in eta, phi space
      REAL ESH2,ESH3,ESH4,ESH5,ESH7,SIGMA3,SIGMA5,SH13,SH24,SH35,SH57
      REAL AVIETA,AVIPHI,GOLD,S3,S3LOW,S3HIG
      LOGICAL CC,EC,COARSE_EM3,TIGHT,TRANS,GOOD_REGION
C
C----------------------------------------------------------------------
C CALCULATE ALL CUT VARIABLES
C----------------------------------------------------------------------
C
C...calculate EM3 1X1/3X3, 5x5-3x3/3x3 and 7x7-5x5/5x5
C
      IFAILED = 0                     !innocent until proven guilty
      IF (ESH3 .GT. 0) THEN           ! divide by zero protection
        SH13 = EM3(0,0)/ESH3
      ELSE
        IFAILED = 52
        GOTO 990
      ENDIF
      IF (SH13 .LT. 0) SH13 = 0
      IF (ESH3 .GT. 0) THEN           ! divide by zero protection
        SH35 = (ESH5-ESH3)/ESH3
      ELSE
        IFAILED = 52
        GOTO 990
      ENDIF
      IF (SH35 .LT. 0) SH35 = 0
      IF (ESH5 .GT. 0) THEN           ! divide by zero protection
        SH57 = (ESH7 - ESH5)/ESH5
      ELSE
        IFAILED = 55
        GOTO 990
      ENDIF
      IF (SH57 .LT. 0) SH57 = 0
C
C...calculate <r> in 3x3 em3 around peak
      AVIETA = 0.
      AVIPHI = 0.
      DO I = -1,1
        DO J = -1,1
          AVIETA = FLOAT(I)*EM3(I,J) + AVIETA
          AVIPHI = FLOAT(J)*EM3(I,J) + AVIPHI
        ENDDO
      ENDDO
      IF(ESH3 .GT. 0)THEN             ! divide by zero protection
        AVIETA = AVIETA/ESH3
        AVIPHI = AVIPHI/ESH3
        SIGMA3 = 0
        DO I = -1,1
          DO J = -1,1
            SIGMA3 = SIGMA3 + SQRT((FLOAT(J)-AVIPHI)**2 +
     &        (FLOAT(I)-AVIETA)**2)*EM3(I,J)/ESH3
          ENDDO
        ENDDO
      ELSE
        IFAILED = 52
        GOTO 990
      ENDIF
C
C...calculate expected value, based on SH13 to remove position dependence
      S3 = -(A2*SH13**2 + B1*SH13 + C0)
      S3LOW = S3 - SS3
      S3HIG = S3 + SS3
C
C    Radius of showers in 5x5 em3
C
      IF(ESH5 .GT. 0)THEN                 ! divide by zero protection
        AVIETA = 0.
        AVIPHI = 0.
        DO I = -2,2
          DO J = -2,2
            AVIETA = FLOAT(I)*EM3(I,J)/ESH5 + AVIETA
            AVIPHI = FLOAT(J)*EM3(I,J)/ESH5 + AVIPHI
          ENDDO
        ENDDO
        SIGMA5 = 0
        DO I = -2,2
          DO J = -2,2
            SIGMA5 = SIGMA5 + SQRT((FLOAT(J)-AVIPHI)**2 +
     &        (FLOAT(I)-AVIETA)**2)*EM3(I,J)/ESH5
          ENDDO
        ENDDO
      ELSE
        IFAILED = 55
        GOTO 990
      ENDIF
C
C...<r> in 5x5 - <r> in 3x3
      GOLD = SIGMA5 - SIGMA3
C
C...  pick 2x2 as having peak + highest eta, phi neighbors
      ESTEP = 1
      IF(EM3(-1,0).GT.EM3(1,0)) ESTEP = -1
      PSTEP = 1
      IF(EM3(0,-1).GT.EM3(0,1)) PSTEP = -1
C...get 2x2 energy
      ESH2= EM3(0,0) + EM3(ESTEP,0) + EM3(0,PSTEP) + EM3(ESTEP,PSTEP)
C...and 4x4
      ESH4 = 0.
      DO I = -ESTEP, 2*ESTEP, ESTEP
        DO J = -PSTEP, 2*PSTEP, PSTEP
          ESH4 = ESH4 + EM3(I,J)
        ENDDO
      ENDDO
      IF (ESH4.LT.ESH2.OR.ESH2.LE.0) THEN     ! divide by zero protection
        SH24 = 0.
      ELSE
        SH24 = (ESH4 - ESH2)/ESH2
      ENDIF
C
C---------------------------------------------------------------------------
C NOW DO CUTS
C---------------------------------------------------------------------------
C
      IF( .NOT. GOOD_REGION ) GOTO 999  ! bug out if in undefined region
      IF( .NOT. TRANS ) GOTO 999   ! skip transverse cuts if requested
C
      IF(IABS(IETA).GE.33) GO TO 999  !automatic pass when far forward
      COARSE_EM3 = IABS(IETA).GE.26   !not all cells in 5x5 are fine
C
C...variables for primary cuts: 5x5-3x3 or 7x7-5x5 in EC
C
C
C...for CC, the primary variable is <r> in 5x5 - 3x3; cuts on components are
C   secondary variables, so defer cuts.
C
C...primary cut in CC is on 5x5 - 3x3 shape
      IF (CC) THEN
        IFAILED = 61  !CC
        IF ( GOLD .GT. EMCUTS(10,JENRG_BIN,JETA_BIN)) GO TO 990
      ELSEIF (IABS(IETA).LT.31) THEN
          IFAILED = 54    !EC
          IF ( SH35 .GT. EMCUTS(11,JENRG_BIN,JETA_BIN)) GO TO 990
      ELSE
          IFAILED = 56    !EC
          IF (SH57 .LT. ESIZE(5) .OR. SH57 .GT. ESIZE(6)) GO TO 990
      ENDIF
C
C...now have cut on primary variables.  See if want to cut some more
      IF (.NOT.TIGHT) GO TO 999
      IF (CC) THEN
C
C...and 5x5-3x3 in CC
        IFAILED = 53  !CC, use binned cuts
        IF ( SH35 .GT. EMCUTS(11,JENRG_BIN,JETA_BIN) ) GO TO 990
C
C...compare 2x2 with surrounding 4x4 square
C
        IFAILED = 57  !do shape cuts in subsquares, use binned cuts
        IF ( SH24 .GT. EMCUTS(12,JENRG_BIN,JETA_BIN) )GO TO 990
C
C...1x1 vs. 3x3
        IFAILED = 58
        IF(SH13 .LT. EM3L) GO TO 990
C
C...cut on sigma3 by itself
C
        IFAILED = 59
        IF (SIGMA3 .LT. S3LOW .OR. SIGMA3 .GT. S3HIG) GO TO 990
C
C...cut on sigma5 by itself
        IFAILED = 60
        IF (SIGMA5 .GT. ESIZE(8)) GO TO 990
C
C...can also cut on <r> 5x5-3x3 in EC
      ELSE
        IFAILED = 62  !EC this time
        IF ( GOLD .GT. EMCUTS(10,JENRG_BIN,JETA_BIN)) GO TO 990
      ENDIF
C
C
  999 IFAILED = 0 !success
  990 RETURN
      END
