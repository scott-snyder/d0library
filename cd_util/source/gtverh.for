      SUBROUTINE GTVERH(ICONT)
C-----------------------------------------------------------------  
C          
C  Fetch contents of Zebra bank VERH (Head bank vertices)
C
C  Output: ICONT(1) = NVERT number of vertices 
C                     
C    Daria Zieminska Feb. 1989
C-   Updated  26-SEP-1991   Daria Zieminska  : check LVERH 
C
C-----------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER ICONT(10),IWORD,LVERH,GZVERH
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C
      LVERH=GZVERH()
      IF (LVERH.GT.0) THEN
        DO 100 IWORD=1,10
          ICONT(IWORD)=IQ(LVERH+IWORD)
  100   CONTINUE
      ELSE
        CALL VZERO(ICONT,10)
      END IF
 1000 RETURN
      END
