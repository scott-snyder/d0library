      SUBROUTINE GTL0AD(BUNCH,IBUNCH,NCHANNELS,NWORDS,RAW_TIME,BUNCH_ID,
     &  RAW_CHARGE,CORRECT_TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch contents of Zebra bank L0AD
C-
C-   Inputs  : BUNCH - interested bunch
C-   Outputs : IBUNCH - ith bunch
C-             NCHANNELS - number of channels
C-             NWORDS - number of words per channel
C-             RAW_TIME(CH#) - raw signal times
C-             BUNCH_ID(CH#) - associated bunch number
C-             RAW_CHARGE(CH#) - raw signal charge
C-             CORRECT_TIME(CH#) - corrected signal time
C-   Controls: None
C-
C-   Created  21-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER BUNCH, IBUNCH, NCHANNELS, NWORDS
      INTEGER RAW_TIME(80), BUNCH_ID(80)
      INTEGER RAW_CHARGE(80), CORRECT_TIME(80)
      INTEGER I
      INTEGER LKL0AD
      INTEGER GZL0AD_BUNCH, LZFIND
      EXTERNAL GZL0AD_BUNCH, LZFIND
C----------------------------------------------------------------------
C
C ****  Fetch bank link for desired bunch
C
      LKL0AD=GZL0AD_BUNCH(BUNCH)
C
C ****  Load arrays with values from L0AD bank
C
      IF ( LKL0AD.NE.0 ) THEN
        IBUNCH=IQ(LKL0AD+1)
        NCHANNELS=IQ(LKL0AD+2)
        NWORDS=IQ(LKL0AD+3)
        DO 10 I = 1 ,80
          RAW_TIME(I)=IQ(LKL0AD+3+I)
   10   CONTINUE
        DO 20 I = 1 ,80
          BUNCH_ID(I)=IQ(LKL0AD+3+80+I)
   20   CONTINUE
        DO 30 I = 1 ,80
          RAW_CHARGE(I)=IQ(LKL0AD+3+160+I)
   30   CONTINUE
        DO 40 I = 1 ,80
          CORRECT_TIME(I)=IQ(LKL0AD+3+240+I)
   40   CONTINUE
      ELSE
C
C ****  else zero everything
C
        IBUNCH=0
        NCHANNELS=0
        NWORDS=0
        CALL VZERO(RAW_TIME,80)
        CALL VZERO(BUNCH_ID,80)
        CALL VZERO(RAW_CHARGE,80)
        CALL VZERO(CORRECT_TIME,80)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
