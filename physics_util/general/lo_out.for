      SUBROUTINE LO_OUT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To close n-tuple file
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-Oct-1993   Sandor Feher and Patrick Mooney
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INCLUDE 'D0$INC:LO_COM.INC'
      INTEGER  I
C
C----------------------------------------------------------------------
C
C   write variables and N-tuples out
C
      CALL PDFSTA
      CALL HCDIR('//PAWC/LO',' ')
      CALL HCDIR('//LO',' ')
      I = 0
      CALL HROUT(0,I,' ')     ! write out the last buffer
      CALL HREND('LO')
C
C
      RETURN
C
C----------------------------------------------------------------------
C
      END
