      SUBROUTINE GTDTRH(ICONT)
C-----------------------------------------------------------------  
C          
C  Fetch contents of Zebra bank DTRH (Head bank for tracks in  
C  Central Drift Chamber ) 
C
C  Daria Zieminska Feb. 1989
C-   Updated  27-JUL-1989   Qizhong Li-Demarteau  bank DTRH is modified
C-   Updated   4-DEC-1990   Susan K. Blessing  Check link value,
C-              set default to -1
C-   Updated  11-FEB-1993   Qizhong Li-Demarteau  increased words in DTRH 
C
C-----------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER ICONT(9),IWORD,LDTRH,GZDTRH
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C-----------------------------------------------------------------
C
      LDTRH=GZDTRH(0)
C
      IF (LDTRH.EQ.0) THEN
        CALL VFILL(ICONT,9,-1)
      ELSE
        DO 100 IWORD=1,9
          ICONT(IWORD)=IQ(LDTRH+IWORD)
  100   CONTINUE
      END IF
C
 1000 RETURN
      END
