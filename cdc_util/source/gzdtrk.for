      INTEGER FUNCTION GZDTRK(ITRACK)
C-------------------------------------------------------------------
C
C  Returns pointer to Zebra bank DTRK for CDC track ITRACK
C   If ITRACK=0 return pointer to first bank in linear structure
C
C  Daria Zieminska 1989                         
C-   Updated   2-NOV-1989   Qizhong Li   
C                                  
C------------------------------------------------------------------
      IMPLICIT NONE                           
      INTEGER  GZZTRH,LZTRH,LDTRH,ITRACK,LDTRK,LZFIND
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZDTRH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZDTRK.LINK/LIST'
C------------------------------------------------------------------
      GZDTRK = 0
      LZTRH = GZZTRH()
      IF (LZTRH .EQ. 0) RETURN
      LDTRH = LQ(LZTRH-IZDTRH)
      IF (LDTRH .EQ. 0) RETURN
      LDTRK = LQ(LDTRH-IZDTRK)
      GZDTRK = LDTRK
      IF (LDTRK.NE.0 .AND. ITRACK.NE.0) 
     &  GZDTRK = LZFIND(IXCOM,LDTRK,ITRACK,-5)
C
      RETURN
      END   
