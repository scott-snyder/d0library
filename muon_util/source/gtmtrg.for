      SUBROUTINE GTMTRG(ICRT,IOTC,IDNO,IFLG,ISTAT,NBNK,IBNK)
C====================================================================
C
C-   Purpose and Methods : Extract subset of MTRG data
C
C-   Inputs  :  ICRT   - Crate number (1=21,2=31,3=41,4=51,5=61)
C-              IOTC   - Slot number (0-15); -1 for mgr, -2 for CCT.
C- 
C-   Output  :  IDNO   - Module ID (or Version)
C-              IFLG   - OTC Quadrant (-1 = reply, -2 = spare 2)
C-              ISTAT  - Status word  (-1 = mgr stat, -2 = trig bits)
C-              NBNK   - Number in MOTR (-1 = spare 1, -2 = CCT 1)
C-              IBNK   - Location in MOTR (-1 = crate ID, -2 = CCT 2)
C-
C-   Created:   29-Jan-1994  Mike Fortner
C-
C=======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ICRT,IOTC,IDNO,IFLG,ISTAT,NBNK,IBNK
C
      INTEGER GZMTRG,LMTRG,IMTRG
      EXTERNAL GZMTRG
C
      IDNO = 0
      LMTRG = GZMTRG(0)
      IF (LMTRG .EQ. 0) RETURN                 ! No bank present
      IMTRG = LMTRG + 89*(ICRT-1)
C
C                Get otc mgr information
C
      IF (IOTC.EQ.-1) THEN
          IDNO = IQ(IMTRG+2)
          IFLG = IQ(IMTRG+3)
          ISTAT = IQ(IMTRG+4)
          NBNK = IQ(IMTRG+5)
          IBNK = IQ(IMTRG+1)
C
C                Get CCT latch information
C
      ELSE IF (IOTC.EQ.-2) THEN
          IDNO = IQ(IMTRG+2)
          IFLG = IQ(IMTRG+6)
          ISTAT = IQ(IMTRG+7)
          NBNK = IQ(IMTRG+8)
          IBNK = IQ(IMTRG+9)
C
C                Get specific OTC information
C
      ELSE IF (IOTC.GE.0.AND.IOTC.LT.16) THEN
          IMTRG = IMTRG + IOTC*5 + 9
          IDNO = IQ(IMTRG+1)
          IFLG = IQ(IMTRG+2)
          ISTAT = IQ(IMTRG+3)
          NBNK = IQ(IMTRG+4)
          IBNK = IQ(IMTRG+5)
C
C
      ENDIF
C
      RETURN
      END
