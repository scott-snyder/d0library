      SUBROUTINE LSQ_MATRIX_DELETE(MATRIX_NAME,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MARKS THE MATRIX AS BEING DELETED
C-   DROPS BANK
C-
C-   Inputs  : MATRIX_NAME
C-   Outputs : IER = 1 . MATRIX DOES NOT EXIST
C-   Controls: 
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      CHARACTER*(*) MATRIX_NAME
      INTEGER INDEX,IER
      INTEGER I
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      IER = 0
      IF ( .NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 1
C          CALL ERRMSG('LSQ','LSQ_MATRIX_DELETE',
C     &      ' Matrix does not exist ','W')
        RETURN
      ENDIF
C
      M_DELETE(INDEX) = 1
      CALL MZDROP(IDVSTP,LSTLNK(INDEX),' ')
      LSTLNK(INDEX) = 0
  999 RETURN
      END
