      SUBROUTINE GTSEGM(LAYER,ISEG,CONT)
C------------------------------------------------------------------------  
C
C  Fetch contents of segment ISEG in layer LAYER of 
C  Vertex Drift Chamber 
C
C  Inputs: LAYER,ISEG   
C                     
C  Output:  CONT(1:27) = segment paramereters           
C           
C                     
C  Daria Zieminska MAY 1987
C-   Updated   4-NOV-1991   Peter M. Grudberg  Fix PATH 
C-   Updated   6-NOV-1992   Peter M. Grudberg  New VSGn format 
C_   Updated  23-APR-1993   Al Clark update the dimension of CON in comments
C-   Updated  24-APR-1993   Liang-ping Chen use LOC=LZFIND(0,LSEGM,ISEG,-5)
C_                          since IQ(LSEGM+1) is no longer for bank number
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INTEGER ISEG,LSEGM,LOC,LZFIND,LAYER,LVTRH,GZVTRH
      REAL CONT(*)
C-------------------------------------------------------------------------
      LVTRH=GZVTRH()
      LSEGM=LQ(LVTRH-3-LAYER)
C                      
      LOC=LZFIND(0,LSEGM,ISEG,-5)
      CALL UCOPY(Q(LOC+1),CONT,31)
  999 RETURN
      END
