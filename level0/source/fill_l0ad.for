      SUBROUTINE FILL_L0AD(IBUNCH,RAW_TIME,BUNCH_ID,RAW_CHARGE,
     &  CORRECT_TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills out L0AD Zebra banks
C-
C-   Inputs  : IBUNCH - bunch number
C-             RAW_TIME(ch#)
C-             BUNCH_ID(ch#)
C-             RAW_CHARGE(ch#)
C-             CORRECT_TIME(ch#)
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER RAW_TIME(6,80)
      INTEGER BUNCH_ID(6,80)
      INTEGER RAW_CHARGE(6,80)
      INTEGER CORRECT_TIME(6,80)
      INTEGER LKL0AD
      INTEGER IBUNCH, J
      INTEGER GZL0AD_BUNCH
      EXTERNAL GZL0AD_BUNCH
C
C----------------------------------------------------------------------
C
C ****  Fetch L0AD link
C
      LKL0AD=GZL0AD_BUNCH(IBUNCH)
      IF ( LKL0AD.LE.0 ) CALL BKL0AD(LKL0AD,IBUNCH)
C
C ****  Fill L0AD bank
C
      DO J = 1 ,80
        IQ(LKL0AD+3+J)=RAW_TIME(IBUNCH,J)          ! Get raw time
        IQ(LKL0AD+3+J+80)= BUNCH_ID(IBUNCH,J)      ! Get bunch number
        IQ(LKL0AD+3+J+160)=RAW_CHARGE(IBUNCH,J)    ! Get raw charge
        IQ(LKL0AD+3+J+240)=CORRECT_TIME(IBUNCH,J)  ! Get correct time
        IF ( IBUNCH.NE.BUNCH_ID(IBUNCH,J)+1 ) THEN
C          CALL ERRMSG('LEVEL0-bunch-id-mismatch2','FILL_L0AD',
C     &            'ADC Bunch_id not equal to descriptor bunch','W')
C          CALL LV0SET(2)
        ENDIF
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
