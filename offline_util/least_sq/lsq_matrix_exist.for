      FUNCTION LSQ_MATRIX_EXIST(MATRIX_NAME,INDEX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CHECK IF MATRIX EXISTS
C-
C-   Inputs  : MATRIX_NAME
C-   Outputs : LSQ_MATRIX_EXIST = .TRUE. IF IT EXISTS
C-             INDEX = INDEX INTO COMMON BLOCKS ZLSQ,LSQ_MATRIX
C-             IER = 0 IF AOK
C-   Controls:
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      CHARACTER*(*) MATRIX_NAME
      INTEGER INDEX,IER
      INTEGER I
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      LSQ_MATRIX_EXIST = .FALSE.
      IER = 0
      INDEX = 0
      DO I = 1 , NMAT
        IF ( MATRIX_NAME.EQ.M_NAME(I).AND.M_DELETE(I).NE.1 ) THEN
          INDEX = I
          LSQ_MATRIX_EXIST = .TRUE.
          RETURN
        ENDIF
      ENDDO
  999 RETURN
      END
