      SUBROUTINE LSQ_GET_VAL(MATRIX_NAME,IROW,ICOL,INDEX,VAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET VALUE OF A MTRIX ELEMENT
C-
C-   Inputs  : MATRIX_NAME = Name of matrix
C-             IROW = Row of matrix
C-             ICOL = column of matrix
C-             INDEX = INDEX OF MATRIX. IF ZERO, WILL
C-             DETERMINE INDEX
C-
C-   Outputs : VAL = value of matrix element in Double precision
C-             IER = 1 Matrix does not exist
C-   Controls:
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-   Updated   7-APR-1995   Alan M. Jonckheere  Change call DGET -> DDGET
C-      to avoid conflict with new intrinsic function 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LSQ_MATRIX_EXIST
      INTEGER INDEX
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
C
      CHARACTER*(*) MATRIX_NAME
      INTEGER IROW,ICOL,IER
C
      DOUBLE PRECISION VAL
      INTEGER NROWS,NCOLS,LINK,IND
C----------------------------------------------------------------------
      IER = 0
      IF ( INDEX.EQ.0 ) THEN
        IF (.NOT. LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
          CALL ERRMSG('LSQ','LSQ_GET_VAL',
     &      ' Matrix does not exist ','W')
          IER = 1
          RETURN
        ENDIF
      ENDIF
C
      LINK = LSTLNK(INDEX)
      NROWS = IC(LINK+1)
      NCOLS = IC(LINK+2)
C
      IND = (ICOL-1)*NROWS + IROW     !Columns run fastest
      IND = 2*(IND-1) + 3   !DOUBLE PRECISION + 1ST TWO WORDS
      CALL DDGET(LINK+IND,VAL)
C
  999 RETURN
      END
