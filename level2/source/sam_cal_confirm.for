      SUBROUTINE SAM_CAL_CONFIRM(ETA,PHI,PASS,ESUM,REGION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given ETA & PHI for SAMUS candidate track
C-
C-
C-   Inputs  : ETA & PHI FOR TRACK
C-   Outputs : PASS
C-   Controls: none
C-
C-   Created  10-AUG-1993   j.balderston
C-   Updated  17-OCT-1993   j.balderston
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IPHI,IETA,I,ACT_PHI,CHK,IER,REGION
C
      LOGICAL FIRST,PASS,FIRSTOK
      DATA FIRST/.TRUE./
C
      REAL ETA,PHI,CALOR_THRESH,ETA_RANGE(4),ESUM
      DATA ETA_RANGE/3.42,3.7,4.1,4.45/
C
      REAL DIMA
      COMMON/DIM/DIMA
C----------------------------------------------------------------------
      PASS=.FALSE.
      IF(FIRST) THEN
        CALOR_THRESH=1.5
! GET THRESH ENERGY = SAM_THRESH
        CALL EZPICK('SAMUS_UTIL_PARAM')
        CALL EZGET('CALOR_THRESH',CALOR_THRESH,IER)
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
C Derive ETA indice for candidate track
C
      IF(ABS(ETA).LT.3.2) THEN
        IETA=SIGN(INT(ABS(ETA)*10)+1,INT(ETA))
      ELSE
        CHK=0
        IETA=SIGN(37,INT(ETA))
        DO I=1,4
          IF(ABS(ETA).LT.ETA_RANGE(I).AND.CHK.EQ.0) THEN
            CHK=1
            IETA=SIGN(32+I,INT(ETA))
          ENDIF
        ENDDO
      ENDIF
C
C Derive PHI indice for candidate track
C
      IPHI=INT(PHI*64/TWOPI)+1
      IF(IPHI.GE.65) THEN
        ACT_PHI=IPHI-64
        IPHI=ACT_PHI
      ENDIF
      CALL MUON_CALOR_CONFIRM(IETA,IPHI,CALOR_THRESH,ESUM,PASS,REGION)
  999 RETURN
      END
