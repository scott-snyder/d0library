      SUBROUTINE MU_WAM_CEN_B(MCELL,I,MCENT)
C  Centroid positions for CFB-layer module
C  Created 6-90  M. Fortner
C  zero out Centroid arrays at initialization          9-27-91
C
      IMPLICIT NONE
      INTEGER MCELL(26,4),MCENT(2),I
C
C                       LEFT SIDE / ODD CELL
      MCENT(1)=0
      IF(MCELL(I,1).NE.0.AND.MCELL(I+2,3).NE.0) MCENT(1)=1
      IF(MCELL(I+1,1).NE.0.AND.MCELL(I+1,2).NE.0) MCENT(1)=1
      IF(MCELL(I+1,1).NE.0.AND.MCELL(I+1,3).NE.0) MCENT(1)=1
      IF(MCELL(I+1,2).NE.0.AND.MCELL(I+1,3).NE.0) MCENT(1)=1
      IF(MCELL(I,1).NE.0.AND.MCELL(I+1,2).NE.0) MCENT(1)=1
      IF(MCELL(I+2,1).NE.0.AND.MCELL(I,3).NE.0) MCENT(1)=1
C
C                       RIGHT SIDE / EVEN CELL
      MCENT(2)=0
      IF(MCELL(I+2,1).NE.0.AND.MCELL(I+1,2).NE.0) MCENT(2)=1
      IF(MCELL(I+1,1).NE.0.AND.MCELL(I+2,3).NE.0) MCENT(2)=1
      IF(MCELL(I+2,1).NE.0.AND.MCELL(I+1,3).NE.0) MCENT(2)=1
      IF(MCELL(I+1,2).NE.0.AND.MCELL(I+2,3).NE.0) MCENT(2)=1
      IF(MCELL(I+2,2).NE.0.AND.MCELL(I+1,3).NE.0) MCENT(2)=1
      RETURN
      END
