      SUBROUTINE GTVERT(IVERT,VERT)
C-----------------------------------------------------------------------
C
C  Returns contents of a vertex bank 
C
C  Input:   IVERT        vertex number
C
C  Output:  VERT(1:14) 
C
C  Daria Zieminska Feb. 1989
C-----------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER IVERT,LVERT,GZVERT,LOC 
      REAL VERT(14) 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C                      
      CALL VZERO(VERT,14)
      LOC=GZVERT(IVERT)
      IF (LOC.GT.0) CALL UCOPY(Q(LOC+1),VERT,14)
 1000 RETURN
      END
