      FUNCTION GZZTRK(ITRACK)
C-------------------------------------------------------------------
C
C  Returns pointer to Zebra bank ZTRK for CDC track ITRACK
C   If ITRACK=0 return pointer to first bank in linear structure
C
C  Daria Zieminska 1989
C-   Updated   2-NOV-1989   Qizhong Li
C
C------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZZTRK
      INTEGER  GZZTRH,LZTRH,ITRACK,LZTRK,LZFIND
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'
C------------------------------------------------------------------
C
      GZZTRK=0
      LZTRH=GZZTRH()
      IF(LZTRH.EQ.0) RETURN
      LZTRK=LQ(LZTRH-IZZTRK)
      GZZTRK=LZTRK
      IF (LZTRK.NE.0.AND.ITRACK.NE.0)
     &  GZZTRK=LZFIND(IXCOM,LZTRK,ITRACK,-5)
      RETURN
      END
