      SUBROUTINE TOP_LEPTONS_NTUPLE_MAX3_14(VECT,T)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : VECT
C-   Outputs : VECT
C-   Controls: T
C-
C-   Created   1-AUG-1991   Jim Cochran
C-   Modified 17-Mar-1993   Routine name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,T
      REAL VECT(0:3,14)
C----------------------------------------------------------------------
      IF (VECT(0,T) .GT. VECT(3,T) .AND. VECT(0,T) .LE. VECT(2,T)) THEN
        DO I=1,14
          VECT(3,I)=VECT(0,I)
        ENDDO
      ELSEIF (VECT(0,T).GT.VECT(2,T) .AND. VECT(0,T).LE.VECT(1,T)) THEN
        DO I=1,14
          VECT(3,I)=VECT(2,I)
          VECT(2,I)=VECT(0,I)
        ENDDO
      ELSEIF (VECT(0,T) .GT. VECT(1,T)) THEN
        DO I=1,14
          VECT(3,I)=VECT(2,I)
          VECT(2,I)=VECT(1,I)
          VECT(1,I)=VECT(0,I)
        ENDDO
      ENDIF
  999 RETURN
      END
