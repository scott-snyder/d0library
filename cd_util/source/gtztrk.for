      SUBROUTINE GTZTRK(ITRACK,QTRAK)
C-----------------------------------------------------------------------
C
C  Returns contents of ZTRK (central detector track bank) 
C
C  Input:   ITRACK       track number
C
C  Output:  QTRAK(1:8)  
C
C  Daria Zieminska Feb. 1989
C  Modified Dec 1989: call GTZTRK_LINK
C-----------------------------------------------------------------------
      IMPLICIT NONE           
      REAL QTRAK(8) 
      INTEGER ITRACK,LZTRH,GZZTRH,LZTRK,LZFIND 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'                             
      LZTRH=GZZTRH()
      LZTRK=LQ(LZTRH-IZZTRK)
      IF (LZTRK.NE.0) LZTRK=LZFIND(IXCOM,LZTRK,ITRACK,-5)
      CALL GTZTRK_LINK(LZTRK,QTRAK)
 1000 RETURN
      END
