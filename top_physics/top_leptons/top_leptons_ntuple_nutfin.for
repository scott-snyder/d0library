      SUBROUTINE TOP_LEPTONS_NTUPLE_NUTFIN(NUT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to construct NUT vector of PNUT1,2,3 
C-                         quantities
C-
C-   Inputs  : None
C-   Outputs : vector NUT
C-   Controls: 
C-
C-   Created  16-SEP-1991   Jim Cochran
C-   modified  1-SEP-1992   to be used in TOP_LEPTONS package - jc
C-   Modified 17-Mar-1993   routine name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,J,LPNUT,GZPNUT
      REAL NUT(3,8),MET_VEC(3)
      REAL MISET_TEMP(4),MISET_THETA,MISET_PHI
C----------------------------------------------------------------------
C
C        missing ET
C
      DO I=1,3
        DO J=1,8
          NUT(I,J)= -9.                 !  diagnostic
        ENDDO
      ENDDO
C
      LPNUT=GZPNUT(1)     ! pick PNUT bank with no correction
C
      IF (LPNUT .EQ. 0) THEN            ! No PNUT(1) banks (-6)
        DO I=1,8
          NUT(1,I) = -6.                !  diagnostic
        ENDDO
C
      ELSEIF(LPNUT.GT.0) THEN
        CALL VADD(Q(LPNUT+3),MET_VEC(1),MISET_TEMP(1),3)
        MISET_TEMP(4)=SQRT( MISET_TEMP(1)**2+MISET_TEMP(2)**2+
     1    MISET_TEMP(3)**2 )
        CALL TOP_LEPTONS_UTIL_LAB_ANGLES(MISET_TEMP,MISET_THETA,
     1    MISET_PHI)
        DO I=1,4
          NUT(1,I)=MISET_TEMP(I)
        ENDDO
        NUT(1,5)=SQRT(MISET_TEMP(1)**2+MISET_TEMP(2)**2)
        NUT(1,6) = MISET_THETA
        NUT(1,7) = -6.
        NUT(1,8) = MISET_PHI
      ENDIF
C
      LPNUT=GZPNUT(2)     ! pick PNUT bank with ICD correction
C
      IF (LPNUT .EQ. 0) THEN            ! No PNUT(2) banks (-6)
        DO I=1,8
          NUT(2,I) = -6.                ! diagnostic
        ENDDO
C
      ELSEIF(LPNUT.GT.0) THEN
        CALL VADD(Q(LPNUT+3),MET_VEC(1),MISET_TEMP(1),3)
        MISET_TEMP(4)=SQRT( MISET_TEMP(1)**2+MISET_TEMP(2)**2+
     1    MISET_TEMP(3)**2 )
        CALL TOP_LEPTONS_UTIL_LAB_ANGLES(MISET_TEMP,MISET_THETA,
     1    MISET_PHI)
        DO I=1,4
          NUT(2,I)=MISET_TEMP(I)
        ENDDO
        NUT(2,5)=SQRT(MISET_TEMP(1)**2+MISET_TEMP(2)**2)
        NUT(2,6) = MISET_THETA
        NUT(2,7) = -6.
        NUT(2,8) = MISET_PHI
      ENDIF
C
      LPNUT=GZPNUT(3)     ! pick PNUT bank with ICD & MU correction
C
      IF (LPNUT .EQ. 0) THEN            ! No PNUT(3) banks (-6)
        DO I=1,8
          NUT(3,I) = -6.                ! diagnostic
        ENDDO
C
      ELSEIF(LPNUT.GT.0) THEN
        DO I=1,8
          NUT(3,I)=Q(LPNUT+I+2)
        ENDDO
      ENDIF
C
  999 RETURN
      END
