      SUBROUTINE VHISTO 
C----------------------------------------------------------------------
C
C  Fill histograms of track finding results for Vertex Chamber. 
C
C   23-JUN-1987   Daria Zieminska
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICONT(10),NTRACK,ITRACK 
      CALL GTVTRH(ICONT)
      NTRACK=ICONT(1)
      IF (NTRACK.LE.0) GO TO 1000
      DO 100 ITRACK=1,NTRACK
        CALL HSVTXT(ITRACK)
  100 CONTINUE
 1000 CONTINUE 
      RETURN
      END       
