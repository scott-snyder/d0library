      SUBROUTINE MAKE_ON_SHELL(PIN,M,POUT,IT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PUT A 4 VECTOR P ON MASS SHELL OF MASS M
C-
C-   Inputs  : PIN(*)= INPUT 4 VECTOR TO BE CHANGED
C-             M = MASS
C-   Outputs : POUT(*) CHANGED 4 VECTOR
C-             IT= 1, CHANGE SO THAT PIN(4) = POUT(4)
C-             IT= 2, CHANGE SO THAT MOMENTUM IS CONSERVED
C-   Controls:
C-
C-   Created  14-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION    PIN(*),M,POUT(*)
      INTEGER IT
      DOUBLE PRECISION    PPIN,PPOUT
      INTEGER I
C----------------------------------------------------------------------
      PPIN = SQRT(PIN(1)**2+PIN(2)**2+PIN(3)**2)
      IF ( IT.EQ.1 ) THEN
C
C ****  ENERGY REMAINS SAME. MOMENTUM IS SCALED TO PUT ON SHELL
C
        POUT(4) = PIN(4)
        PPOUT = SQRT(POUT(4)**2-M*M)
        DO I = 1 , 3
          POUT(I) = PIN(I)*PPOUT/PPIN
        ENDDO
      ELSE
        DO I = 1 , 3
          POUT(I) = PIN(I)
        ENDDO
        POUT(4) = SQRT(PPIN**2+M*M)
      ENDIF
  999 RETURN
      END
