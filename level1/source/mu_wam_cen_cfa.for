      SUBROUTINE MU_WAM_CEN_CFA(MCELL,I,MCENT)
C  Centroid positions for CFA-layer modules
C  Created 6-90  M. Fortner
C  zero out Centroid arrays at initialization     9-27-91, K. Bazizi
C
      IMPLICIT NONE
      INTEGER MCELL(26,4),MCENT(2),I,J
C<<
C.. LEFT SIDE / ODD CELL
      MCENT(1)=0
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).NE.0) MCENT(1)=1
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I,2).NE.0) MCENT(1)=1
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+2,2).NE.0) MCENT(1)=1
      IF(MCELL(I+1,3).NE.0.AND.MCELL(I+1,2).NE.0) MCENT(1)=1
      IF(MCELL(I+1,3).NE.0.AND.MCELL(I,1).NE.0) MCENT(1)=1
      IF(MCELL(I+1,3).NE.0.AND.MCELL(I+1,1).NE.0) MCENT(1)=1
      IF(MCELL(I+1,3).NE.0.AND.MCELL(I+2,1).NE.0) MCENT(1)=1
C<<
C.. RIGHT SIDE / EVEN CELL
      MCENT(2)=0
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+2,3).NE.0) MCENT(2)=1
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).NE.0.AND.MCELL(I+2,1).NE.0)
     &  MCENT(2)=1
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).EQ.0.AND.MCELL(I+1,2).NE.0)
     &  MCENT(2)=1
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).EQ.0.AND.MCELL(I,1).NE.0)
     &  MCENT(2)=1
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).EQ.0.AND.MCELL(I+1,1).NE.0)
     &  MCENT(2)=1
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).EQ.0.AND.MCELL(I+2,1).NE.0)
     &  MCENT(2)=1
C<<
      RETURN
      END
