      SUBROUTINE GTVTRH(ICONT)
C-----------------------------------------------------------------  
C          
C  Fetch contents of Zebra bank VTRH (Head bank for tracks in  
C  Vertex Drift Chamber ) 
C
C  Output: ICONT(1) = NTRACK   number of tracks in Vertex Chamber   
C                     
C    Daria Zieminska MAY 1988
C-   Updated   4-DEC-1990   Susan K. Blessing  Check link value,
C-              set default to -1
C
C-----------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER ICONT(10),IWORD,LVTRH,GZVTRH
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C
      LVTRH=GZVTRH(0)
C
      IF (LVTRH.EQ.0) THEN
        CALL VFILL(ICONT,10,-1)
      ELSE
        DO 100 IWORD=1,10
          ICONT(IWORD)=IQ(LVTRH+IWORD)
  100   CONTINUE
      END IF
C
 1000 RETURN
      END
