      FUNCTION NP_MULQ_TIGHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-NOV-1993   DOUGLAS M. NORMAN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
C
      LOGICAL FIRST
      LOGICAL NP_MULQ_TIGHT,NP_MULQ_TIGHT_EOJ
C
      INTEGER LPMUO,LPARH,MSTAT,NMUO,MUO_COUNT
      INTEGER GZPARH,IER,IOK,I
C
      REAL     MUON_PT,MUON_ETA,MU_PT_CUT,MU_ETA_CUT
      REAL RSUMM(20),RSUM(20)
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      NP_MULQ_TIGHT = .FALSE.
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('NP_MULQ_TIGHT_RCP',IER)
        IF (IER.EQ.0) THEN
          CALL EZPICK('NP_MULQ_TIGHT_RCP')
          CALL EZGET('MU_PT_CUT',MU_PT_CUT,ier)
          CALL EZGET('MU_ETA_CUT',MU_ETA_CUT,ier)
        ELSE
          CALL ERRMSG('NP_MULQ_TIGHT_RCP not found',
     2      'NP_MSP_MU_SELECT',' ','F')
        ENDIF
        CALL VZERO(RSUM,20)
      ENDIF
      RSUM(1) = RSUM(1) + 1
      LPARH=GZPARH()
      LPMUO=LQ(LPARH-IZPMUO)
      NMUO=0
      MUO_COUNT=0
      IF (LPMUO.LE.0) RETURN
 99   CONTINUE
      MUON_PT=Q(LPMUO+14)
      MUON_ETA=Q(LPMUO+16)
      NMUO = NMUO + 1
      CALL MULQ_MUON_SELECT_TIGHT(LPMUO,MSTAT,IOK)      
      IF (MUON_PT.GT.MU_PT_CUT .AND. MUON_ETA.LT.MU_ETA_CUT
     &  .AND. IOK.GT.0) MUO_COUNT=MUO_COUNT+1 
      LPMUO=LQ(LPMUO)
      IF (LPMUO.GT.0) GOTO 99
      IF (NMUO .GE. 2) RSUM(2) = RSUM(2) + 1
      IF (MUO_COUNT.GE.2) THEN
        NP_MULQ_TIGHT = .TRUE.
        RSUM(3) = RSUM(3) + 1
      ENDIF
  999 RETURN
C
C   
      ENTRY NP_MULQ_TIGHT_EOJ(RSUMM)
C
      DO I = 1,20
        RSUMM(I) = RSUM(I)
      ENDDO
C
      RETURN
      END
