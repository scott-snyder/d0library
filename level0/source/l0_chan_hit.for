      SUBROUTINE L0_CHAN_HIT(CHAN_HIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Marks an array for all Level 0 channels 
C-                         with a valid hit.
C-
C-   Inputs  : None
C-   Outputs : CHAN_HIT(ch#) = 1, if channel has valid hit
C-   Controls: None
C-
C-   Created  20-JUL-1992   Jeffrey Bantly   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER CBUNCH
      INTEGER ICHAN
      INTEGER RAW_TIME(80)
      INTEGER BUNCH_ID(80)
      INTEGER RAW_CHARGE(80)
      INTEGER CORRECT_TIME(80)
      INTEGER CHAN_HIT(80)
      INTEGER ERR
      INTEGER IBUNCH,NCHAN,NWORD
C
      LOGICAL FIRST,LV0,GOODFZ,GOODSZ,GOODSSZ
C     LOGICAL EZERROR
C
C     SAVE FIRST
C     DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C      IF (FIRST) THEN  
C        CALL EZPICK('LEVEL0_RCP')
C        IF ( EZERROR(ERR) ) THEN
C         CALL ERRMSG('LEVEL0-no-rcp','LV0HIS',
C    &                                 'LEVEL0_RCP not found.','W')
C       ELSE
C         CALL EZRSET
C       ENDIF
CC
C       FIRST = .FALSE.
C     END IF
C
      CALL L0_COR_BUNCH(CBUNCH)
C
      CALL GTL0AD(CBUNCH,IBUNCH,NCHAN,NWORD,RAW_TIME,BUNCH_ID,
     &                                        RAW_CHARGE,CORRECT_TIME)
      IF ( IBUNCH.NE.CBUNCH ) THEN
        CALL ERRMSG('LEVEL0-wrong-bunch-found','L0_CHAN_HIT',
     &    'Found wrong bunch returned','W')
      ENDIF
C
      CALL VZERO(CHAN_HIT,80)
C
        DO ICHAN=1,72
            IF (FLOAT(CORRECT_TIME(ICHAN)).GT. 0.0) THEN
              CHAN_HIT(ICHAN)=1
            ENDIF
        ENDDO
C
C-------------------------------------------------------------------------
  999 RETURN
      END
