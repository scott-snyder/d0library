      LOGICAL FUNCTION TRD_BADRUN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  : TRUE if run bad
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-DEC-1994   A. ZYLBERSTEJN
C-   Updated  14-MAY-1995   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILIM
      INCLUDE 'D0$INC:ZEBCOM.INC'
      PARAMETER (ILIM = 100)
      INTEGER NRUN,RUNNO,NRUNI,BADRUN_BEGIN(1:ILIM),BADRUN_END(1:ILIM)
      INTEGER RECOV,RECOP,IER,N,I,N_ZONES,K,LOC,LUDST,GZUDST,UDST_VRS
      LOGICAL BDR,FIRST
      DATA NRUNI/0/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      BDR = .FALSE.
C
      UDST_VRS = 0
      LUDST    = GZUDST()
      IF (LUDST.GT.0) UDST_VRS = IQ(LUDST+1)
C
      IF (UDST_VRS.EQ.1.OR.UDST_VRS.EQ.2) GOTO 999
C
C do not analyse DST's obtained with reco version 12.10,12.11
      CALL RECO_VERSION(RECOV,RECOP)
      IF(RECOV.EQ.12 .AND. RECOP.GE.9 .AND.RECOP.LT.13) THEN
        BDR = .TRUE.
        GO TO 999
      ENDIF
C
      IF (IQ(LHEAD+1) .GT. 1000)      THEN   !MC events
        BDR=.FALSE.
        GO TO 999
      END IF
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZLOC ('TRD_ANALYSIS_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP ('TRD_ANALYSIS_RCP',IER)
        CALL EZPICK ('TRD_ANALYSIS_RCP')
        CALL EZGETA ('ANODE_BADRUN',0,0,0,N,IER)
        N_ZONES = N / 2
        DO I = 1,N_ZONES
          K = (I-1)*2
          CALL EZGETA ('ANODE_BADRUN',K+1,K+1,1,BADRUN_BEGIN(I),IER)
          CALL EZGETA ('ANODE_BADRUN',K+2,K+2,1,BADRUN_END(I),IER)
        ENDDO
        CALL EZRSET
      ENDIF
      NRUN=RUNNO()
      IF(NRUN.EQ.NRUNI)GO TO 999
      NRUNI=NRUN
      BDR=.TRUE.
C      IF(NRUN.LT.54650)GO TO 999 ! TRD not working before Oct 15th 1992
C  between May and june 94 the potential HV was incorrect
C      IF(NRUN.GE.79175 .AND. NRUN.LE.79312)GO TO 999
C      IF(NRUN.GE.79405 .AND. NRUN.LE.80045)GO TO 999
C      IF(NRUN.EQ.86922)GO TO 999 ! HV set to 50%
C      IF(NRUN.GE.91206 .AND. NRUN.LE.91213)GO TO 999 ! Bad peds
      DO I = 1,N_ZONES
        IF (NRUN.GE.BADRUN_BEGIN(I).AND.NRUN.LE.BADRUN_END(I)) GO TO 999
      ENDDO
C
      BDR=.FALSE.
C
  999 TRD_BADRUN=BDR
      RETURN
      END
