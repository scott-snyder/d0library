C VAX/DEC CMS REPLACEMENT HISTORY, Element MU_WAM_CEN_EFA_L2.FOR
C *1    21-OCT-1993 08:54:16 FORTNER "add terms for scintillator"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MU_WAM_CEN_EFA_L2.FOR
      SUBROUTINE MU_WAM_CEN_EFA_L2(MCELL,I,MCELL_OUT,NCLUSTERS)
C  Centroid positions for EFA-layer modules (3/4 hits)
C  Created nov-3-92 G. Alves
C FOR USE IN L2 OVERLAP CODE; 6/93 DKF
C<<
      IMPLICIT NONE
      INTEGER MCELL(26,4),MCELL_OUT(26,4),I,J,NCLUSTERS
      LOGICAL FOUND
C
      J = NCLUSTERS + 1
      FOUND = .FALSE.
C
C                       LEFT SIDE / ODD CELL

      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).NE.0.AND.MCELL(I+1,2).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I+1,2).EQ.0) THEN
          MCELL_OUT(I+1,2)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+1,3)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,2).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I+1,2)
          MCELL_OUT(I+1,3)=MCELL_OUT(I+1,2)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,3).EQ.0.AND.
     &      MCELL_OUT(I+1,2).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+1,3) = J
          MCELL_OUT(I+1,2) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,3).NE.0.AND.MCELL(I+1,2).NE.0.AND.MCELL(I+1,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,3).NE.0.AND.MCELL_OUT(I+1,1).EQ.0) THEN
          MCELL_OUT(I+1,1)=MCELL_OUT(I+1,3)
          MCELL_OUT(I+1,2)=MCELL_OUT(I+1,3)
        END IF
        IF (MCELL_OUT(I+1,3).EQ.0.AND.MCELL_OUT(I+1,1).NE.0) THEN
          MCELL_OUT(I+1,3)=MCELL_OUT(I+1,1)
          MCELL_OUT(I+1,2)=MCELL_OUT(I+1,1)
        END IF
        IF (MCELL_OUT(I+1,3).EQ.0.AND.MCELL_OUT(I+1,2).EQ.0.AND.
     &      MCELL_OUT(I+1,1).EQ.0) THEN
          MCELL_OUT(I+1,3) = J
          MCELL_OUT(I+1,2) = J
          MCELL_OUT(I+1,1) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,2).NE.0.AND.MCELL(I+1,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I+1,1).EQ.0) THEN
          MCELL_OUT(I+1,1)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+1,2)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,1).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I+1,1)
          MCELL_OUT(I+1,2)=MCELL_OUT(I+1,1)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,2).EQ.0.AND.
     &      MCELL_OUT(I+1,1).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+1,2) = J
          MCELL_OUT(I+1,1) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).NE.0.AND.MCELL(I+1,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I+1,1).EQ.0) THEN
          MCELL_OUT(I+1,1)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+1,3)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,1).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I+1,1)
          MCELL_OUT(I+1,3)=MCELL_OUT(I+1,1)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,3).EQ.0.AND.
     &      MCELL_OUT(I+1,1).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+1,3) = J
          MCELL_OUT(I+1,1) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).NE.0.AND.MCELL(I+2,2).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I+2,2).EQ.0) THEN
          MCELL_OUT(I+2,2)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+1,3)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+2,2).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I+2,2)
          MCELL_OUT(I+1,3)=MCELL_OUT(I+2,2)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,3).EQ.0.AND.
     &      MCELL_OUT(I+2,2).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+1,3) = J
          MCELL_OUT(I+2,2) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I,2).NE.0.AND.MCELL(I,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I,1)=MCELL_OUT(I+1,4)
          MCELL_OUT(I,2)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I,1).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I,1)
          MCELL_OUT(I,2)=MCELL_OUT(I,1)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I,2).EQ.0.AND.
     &      MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I,2) = J
          MCELL_OUT(I,1) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,3).NE.0.AND.MCELL(I,2).NE.0.AND.MCELL(I,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,3).NE.0.AND.MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I,1)=MCELL_OUT(I+1,3)
          MCELL_OUT(I,2)=MCELL_OUT(I+1,3)
        END IF
        IF (MCELL_OUT(I+1,3).EQ.0.AND.MCELL_OUT(I,1).NE.0) THEN
          MCELL_OUT(I+1,3)=MCELL_OUT(I,1)
          MCELL_OUT(I,2)=MCELL_OUT(I,1)
        END IF
        IF (MCELL_OUT(I+1,3).EQ.0.AND.MCELL_OUT(I,2).EQ.0.AND.
     &      MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I+1,3) = J
          MCELL_OUT(I,2) = J
          MCELL_OUT(I,1) = J
          FOUND = .TRUE.
        END IF
      END IF
C
C                       RIGHT SIDE / EVEN CELL
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+2,2).NE.0.AND.MCELL(I+2,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I+2,1).EQ.0) THEN
          MCELL_OUT(I+2,1)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+2,2)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+2,1).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I+2,1)
          MCELL_OUT(I+2,2)=MCELL_OUT(I+2,1)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+2,2).EQ.0.AND.
     &      MCELL_OUT(I+2,1).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+2,2) = J
          MCELL_OUT(I+2,1) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+2,3).NE.0.AND.MCELL(I+2,2).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I+2,2).EQ.0) THEN
          MCELL_OUT(I+2,2)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+2,3)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+2,2).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I+2,2)
          MCELL_OUT(I+2,3)=MCELL_OUT(I+2,2)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+2,3).EQ.0.AND.
     &      MCELL_OUT(I+2,2).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+2,3) = J
          MCELL_OUT(I+2,2) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+2,3).NE.0.AND.MCELL(I+2,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I+2,1).EQ.0) THEN
          MCELL_OUT(I+2,1)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+2,3)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+2,1).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I+2,1)
          MCELL_OUT(I+2,3)=MCELL_OUT(I+2,1)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+2,3).EQ.0.AND.
     &      MCELL_OUT(I+2,1).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+2,3) = J
          MCELL_OUT(I+2,1) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).NE.0.AND.MCELL(I,2).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I,2).EQ.0) THEN
          MCELL_OUT(I,2)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+1,3)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I,2).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I,2)
          MCELL_OUT(I+1,3)=MCELL_OUT(I,2)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,3).EQ.0.AND.
     &      MCELL_OUT(I,2).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+1,3) = J
          MCELL_OUT(I,2) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,2).NE.0.AND.MCELL(I,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I,1)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+1,2)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I,1).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I,1)
          MCELL_OUT(I+1,2)=MCELL_OUT(I,1)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,2).EQ.0.AND.
     &      MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+1,2) = J
          MCELL_OUT(I,1) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,4).NE.0.AND.MCELL(I+1,3).NE.0.AND.MCELL(I,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,4).NE.0.AND.MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I,1)=MCELL_OUT(I+1,4)
          MCELL_OUT(I+1,3)=MCELL_OUT(I+1,4)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I,1).NE.0) THEN
          MCELL_OUT(I+1,4)=MCELL_OUT(I,1)
          MCELL_OUT(I+1,3)=MCELL_OUT(I,1)
        END IF
        IF (MCELL_OUT(I+1,4).EQ.0.AND.MCELL_OUT(I+1,3).EQ.0.AND.
     &      MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I+1,4) = J
          MCELL_OUT(I+1,3) = J
          MCELL_OUT(I,1) = J
          FOUND = .TRUE.
        END IF
      END IF
      IF(MCELL(I+1,3).NE.0.AND.MCELL(I+1,2).NE.0.AND.MCELL(I,1).NE.0)
     +  THEN
        IF (MCELL_OUT(I+1,3).NE.0.AND.MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I,1)=MCELL_OUT(I+1,3)
          MCELL_OUT(I+1,2)=MCELL_OUT(I+1,3)
        END IF
        IF (MCELL_OUT(I+1,3).EQ.0.AND.MCELL_OUT(I,1).NE.0) THEN
          MCELL_OUT(I+1,3)=MCELL_OUT(I,1)
          MCELL_OUT(I+1,2)=MCELL_OUT(I,1)
        END IF
        IF (MCELL_OUT(I+1,3).EQ.0.AND.MCELL_OUT(I+1,2).EQ.0.AND.
     &      MCELL_OUT(I,1).EQ.0) THEN
          MCELL_OUT(I+1,3) = J
          MCELL_OUT(I+1,2) = J
          MCELL_OUT(I,1) = J
          FOUND = .TRUE.
        END IF
      END IF
C
      IF (FOUND) NCLUSTERS = NCLUSTERS + 1
      RETURN
      END
