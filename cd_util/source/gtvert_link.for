      SUBROUTINE GTVERT_LINK(LOC,VERT)
C-----------------------------------------------------------------------
C
C  Returns contents of a vertex bank 
C
C  Input:   LOC           Bank location
C
C  Output:  VERT(1:14) 
C
C  Daria Zieminska DEC. 1989
C-----------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER LOC 
      REAL VERT(14) 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C                      
      IF (LOC.EQ.0) THEN
        CALL VZERO(VERT,14)
        GO TO 1000
      END IF
      CALL UCOPY(Q(LOC+1),VERT,14)
 1000 RETURN
      END
