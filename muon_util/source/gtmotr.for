      SUBROUTINE GTMOTR(ITRG,ICEN1,ICEN2,ICEN3,IPT,ICCT,ISP1,ISP2,ISP3)
C=====================================================================
C-
C- Purpose and Methods: Gets information from bank MOTR for trigger ITRG
C-
C- Input:    ITRG  - Number of trigger in bank
C-
C- Output:   ICEN1 - Centroid 1
C-           ICEN2 - Centroid 2
C-           ICEN3 - Centroid 3
C-           IPT   - Pt table
C-           ICCT  - CCT match
C-           ISP1,2,3 - Spares
C-
C- Created:  29-JAN-1994  M. Fortner
C-
C======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ITRG,ICEN1,ICEN2,ICEN3,IPT,ICCT,ISP1,ISP2,ISP3
      INTEGER LMOTR,GZMOTR,IMOTR
      EXTERNAL GZMOTR
C
      ICEN1 = 0
      LMOTR = GZMOTR(0)
      IF (LMOTR.EQ.0) RETURN
      IMOTR = LMOTR + 8*(ITRG-1)
      ICEN1 = IQ(IMOTR+1)
      ICEN2 = IQ(IMOTR+2)
      ICEN3 = IQ(IMOTR+3)
      IPT   = IQ(IMOTR+4)
      ICCT  = IQ(IMOTR+5)
      ISP1  = IQ(IMOTR+6)
      ISP2  = IQ(IMOTR+7)
      ISP3  = IQ(IMOTR+8)
      RETURN
      END
