      SUBROUTINE GTZTRK_LINK(LOC,QTRAK)
C-----------------------------------------------------------------------
C
C  Returns contents of ZTRK (central detector track bank) 
C
C  Input:   LOC      track bank location
C
C  Output:  QTRAK(1:8)  
C
C  Daria Zieminska Dec. 1989
C-----------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER LOC
      REAL QTRAK(8) 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'                             
      IF (LOC.EQ.0) THEN
        CALL VZERO(QTRAK,8)
        GO TO 1000
      END IF
      CALL UCOPY(Q(LOC+1),QTRAK,8)
 1000 RETURN
      END
