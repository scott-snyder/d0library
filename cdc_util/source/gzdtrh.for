      INTEGER FUNCTION GZDTRH()
C-------------------------------------------------------------------
C
C  Returns pointer to Zebra bank DTRH ( header for CDC tracks)
C
C  Daria Zieminska Feb. 1989                         
C-   Updated  23-MAY-1990   Qizhong Li-Demarteau  added () 
C                                  
C------------------------------------------------------------------
      IMPLICIT NONE                           
      INTEGER LZTRH,GZZTRH,LDTRH 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZDTRH.LINK/LIST'
      GZDTRH=0
      LZTRH=GZZTRH()
      IF (LZTRH.NE.0) GZDTRH=LQ(LZTRH-IZDTRH)
      RETURN
      END   
