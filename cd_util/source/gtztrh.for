      SUBROUTINE GTZTRH(ICONT)
C-----------------------------------------------------------------  
C          
C  Fetch contents of Zebra bank ZTRH (Head bank for central detector
C  tracks )
C
C  Output: ICONT(1) = NTRACK   number of CD tracks 
C                     
C    Daria Zieminska Feb. 1989
C-   Updated   4-DEC-1990   Susan K. Blessing  Check link value,
C-              set default to -1
C
C-----------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER ICONT(10),IWORD,LZTRH,GZZTRH
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C-----------------------------------------------------------------
C
      LZTRH=GZZTRH(0)
C
      IF (LZTRH.EQ.0) THEN              
        CALL VFILL(ICONT,10,-1)
      ELSE
        DO 100 IWORD=1,10
          ICONT(IWORD)=IQ(LZTRH+IWORD)
  100   CONTINUE
      END IF
C
 1000 RETURN
      END
