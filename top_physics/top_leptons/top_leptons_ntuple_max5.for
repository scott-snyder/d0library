      SUBROUTINE TOP_LEPTONS_NTUPLE_MAX5(VECT,T)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : VECT
C-   Outputs : VECT
C-   Controls: T    
C-
C-   Created  26-Sep-1993   Joey Thompson - I don't like this, but it's
C-                                          backwards compatible
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,T
      REAL VECT(0:5,8)
C----------------------------------------------------------------------
      IF (VECT(0,T) .GT. VECT(5,T) .AND. VECT(0,T) .LE. VECT(4,T)) THEN
        DO I=1,8
          VECT(5,I)=VECT(0,I)
        ENDDO
      ELSEIF (VECT(0,T).GT.VECT(4,T) .AND. VECT(0,T).LE.VECT(3,T)) THEN
        DO I=1,8
          VECT(5,I)=VECT(4,I)
          VECT(4,I)=VECT(0,I)
        ENDDO
      ELSEIF (VECT(0,T).GT.VECT(3,T) .AND. VECT(0,T).LE.VECT(2,T)) THEN
        DO I=1,8
          VECT(5,I)=VECT(4,I)
          VECT(4,I)=VECT(3,I)
          VECT(3,I)=VECT(0,I)
        ENDDO
      ELSEIF (VECT(0,T).GT.VECT(2,T) .AND. VECT(0,T).LE.VECT(1,T)) THEN
        DO I=1,8
          VECT(5,I)=VECT(4,I)
          VECT(4,I)=VECT(3,I)
          VECT(3,I)=VECT(2,I)
          VECT(2,I)=VECT(0,I)
        ENDDO
      ELSEIF (VECT(0,T).GT.VECT(1,T)) THEN
        DO I=1,8
          VECT(5,I)=VECT(4,I)
          VECT(4,I)=VECT(3,I)
          VECT(3,I)=VECT(2,I)
          VECT(2,I)=VECT(1,I)
          VECT(1,I)=VECT(0,I)
        ENDDO
      ENDIF
  999 RETURN
      END
