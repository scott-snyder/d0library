      LOGICAL FUNCTION LSQ_POLY_INIT(NORDER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-MAR-1992   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER INDEX,IER,NORDER
      LOGICAL LSQ_MATRIX_EXIST
C----------------------------------------------------------------------
      LSQ_POLY_INIT=.FALSE.
C
C ****  CHECK TO SEE IF THE WORK MATRICES ALREAD EXIST.  IF THEY DO, DELETE
C ****  THEM!!
C
      INDEX=0
      IF ( LSQ_MATRIX_EXIST('ALPHA',INDEX,IER) ) THEN
        CALL LSQ_MATRIX_DELETE('ALPHA',IER)
      ENDIF

      INDEX=0
      IF ( LSQ_MATRIX_EXIST('BETA',INDEX,IER) ) THEN
        CALL LSQ_MATRIX_DELETE('BETA',IER)
      ENDIF
C
C ****  book the work matrices ALPHA and BETA
C
      CALL LSQ_BKMATRIX('ALPHA',NORDER+1,NORDER+1,IER)
      IF( IER.NE.0 ) RETURN

      CALL LSQ_BKMATRIX('BETA',NORDER+1,1,IER)
      IF( IER.NE.0 ) RETURN

      LSQ_POLY_INIT=.TRUE.

  999 RETURN
      END
