      SUBROUTINE LSQ_MATRIX_PRINT(PRUNIT,MATRIX_NAME,NCOLS,FRMT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRINT MATRIX_NAME
C-
C-   Inputs  : PRUNIT = FORTRAN UNIT NUMBER TO PRINT ON
C-                      MATRIX_NAME = NAME OF MATRIX TO BE PRINTED
C-                      NCOLS = NUMBER OF COLUMNS ACROSS PAGE TO BE PRINTED
C-                      FRMT = FORMAT FOR A SINGLE ELEMENT (DEFAULT = 'D10.3')
C-   Outputs : IER = 1 MATRIX DOES NOT EXIST
C-   Controls:
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      CHARACTER*(*) MATRIX_NAME
      INTEGER INDEX,IER
      INTEGER LINK,ROWS,COLS
      INTEGER I,PRUNIT
      LOGICAL LSQ_MATRIX_EXIST
      INTEGER NCOLS
      CHARACTER*(*) FRMT
      CHARACTER*80 FORM,MSG
C----------------------------------------------------------------------
      IER = 0
      IF ( .NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 1
          CALL ERRMSG('LSQ','LSQ_MATRIX_PRINT',
     &      ' Matrix does not exist ','W')
        RETURN
      ENDIF
      WRITE(PRUNIT,1)M_ROWS(INDEX),M_COLS(INDEX),MATRIX_NAME
    1 FORMAT(' PRINTING ',I8,' ROWS  AND ',I8,
     &  ' COLUMNS OF MATRIX ',A)
      LINK = LSTLNK(INDEX)
      ROWS = IC(LINK+1)
      COLS = IC(LINK+2)
      WRITE(PRUNIT,2)ROWS,COLS
    2 FORMAT(' NUMBER OF ROWS AND COLUMNS FROM MATRIX BANK = ',2I8)
C
      MSG = ' Dump of matrix '//MATRIX_NAME
C
      IF ( FRMT.EQ.' ' ) THEN
        FORM = '(D10.3)'
      ELSE
        FORM = FRMT
      ENDIF
C
      LINK = LINK + 3 !now points to first matrix element
C
      CALL MXPRND(PRUNIT,MSG,C(LINK),ROWS,
     &  COLS,ROWS,COLS,NCOLS,FORM)
C
  999 RETURN
      END
