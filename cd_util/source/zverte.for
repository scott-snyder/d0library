      SUBROUTINE ZVERTE(NVER,ZVER,DZVER)                 
C------------------------------------------------------------------
C 
C     Returns number primary vertices and z and dz  
C
C     Input from banks VERT
C     Output:
C               NVER           number of primary vertices
C               ZVER(1:NVER)   z coordinates
C               DZVER(1:NVER)  errors of z coordinates
C
C     Daria Zieminska  Feb. 1989
C     Modified Dec 1989 (original assumed only one vertex)
C------------------------------------------------------------------
      IMPLICIT NONE 
      INTEGER NVER,IVER,ICONT(10)
      REAL VERT(14),ZVER(*),DZVER(*)
      CALL GTVERH(ICONT)
      NVER=ICONT(2)
      DO 100 IVER=1,NVER
        CALL GTVERT(IVER,VERT) 
        ZVER(IVER)=VERT(5)
        DZVER(IVER)=VERT(8)
  100 CONTINUE
 1000 RETURN
      END       
