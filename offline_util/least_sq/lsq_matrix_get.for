      SUBROUTINE LSQ_MATRIX_GET(MATRIX_NAME,ARRAY,NROWS,NCOLS,
     &  PRECISION,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET THE CONTENTS OF AN ARRAY FROM THE MATRIX
C-
C-   Inputs  : MATRIX_NAME = Name of matrix. If it does not exist, 
C-                           it is created
C-             PRECESION   = 1. Array is single precision
C-                         = 2. Array is double precision
C-   Outputs : ARRAY       = Single or double precision array
C-                           of rows NROWS and NCOLS.
C-                           IER = 1 Illegal value of precision
C-                           IER = 2 Rows and cols of matrix specified 
C-                           does not match existing matrix.
C-                           IER =3 Specified matrix does not exist
C-   Controls: 
C-
C-   Created  24-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      CHARACTER*(*) MATRIX_NAME
      INTEGER PRECISION
      INTEGER INDEX,IER
      INTEGER LINK,NROWS,NCOLS
      LOGICAL LSQ_MATRIX_EXIST
      REAL    ARRAY(*)
      DOUBLE PRECISION VAL
C
      DOUBLE PRECISION DTEMP
      REAL TEMP(2)
      EQUIVALENCE (TEMP,DTEMP)
C
      INTEGER I,J
      INTEGER IND
C----------------------------------------------------------------------
      IER = 0
      IF ( PRECISION.NE.1 .AND. PRECISION .NE. 2 ) THEN
        CALL ERRMSG('LSQ','LSQ_MATRIX_GET',
     &    ' Illegal value of precision specified ','W')
        IER = 2
        RETURN
      ENDIF
C
      IF ( .NOT.LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER) ) THEN
        IER = 3
        RETURN
      ENDIF
C
      IF(NROWS.NE.M_ROWS(INDEX).OR.NCOLS.NE.M_COLS(INDEX))THEN
        IER = 2
        CALL ERRMSG('LSQ','LSQ_MATRIX_GET',
     &    ' Row or column mismatch with existing matrix','W')

        RETURN
      ENDIF
C
      DO I = 1 , NROWS
        DO J = 1 , NCOLS
            CALL LSQ_GET_VAL(MATRIX_NAME,I,J,INDEX,VAL,IER)
            IND = (J-1)*NROWS + I - 1
          IF ( PRECISION.EQ.1 ) THEN
             ARRAY(IND+1) = VAL
          ELSE
            IND = 2*IND
            DTEMP = VAL
            ARRAY(IND+1) = TEMP(1)
            ARRAY(IND+2) = TEMP(2)
          ENDIF
        ENDDO
      ENDDO
C
  999 RETURN
      END
