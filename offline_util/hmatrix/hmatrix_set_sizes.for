      SUBROUTINE HMATRIX_SET_SIZES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : READ RCP AND SET SIZES FOR
C-                         Various matrices
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      LOGICAL EZERROR
      INTEGER IER
C----------------------------------------------------------------------
      CALL EZPICK('HMATRIX_RZ_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('HMATRIX','HMATRIX_SET_SIZES',
     &    'ERROR PICKING HMATRIX_RZ_RCP','W')
        GOTO 999
      ENDIF
C      CALL INTMSG(' SETTING BANK SIZES FROM RZ RCP FILE ')
      CALL EZ_GET_CHARS('VISIBLE_QUANTITIES',VIS_DIM,
     &  VISIBLE_QUANTITIES,IER)
      IF(IER.NE.0)THEN
        CALL ERRMSG('HMATRIX','HMATRIX_SET_SIZES',
     &    'ERROR READING FROM RCP','W')
        IER = 0
      ENDIF
      CALL EZ_GET_CHARS('INVISIBLE_QUANTITIES',INVIS_DIM,
     &  INVISIBLE_QUANTITIES,IER)
      IF(IER.NE.-2.AND.IER.NE.0)THEN           ! -2 MEANS NO INVIS_DIM QUANTS.
        CALL ERRMSG('HMATRIX','HMATRIX_SET_SIZES',
     &    'ERROR READING FROM RCP','W')
      ENDIF
      TOT_DIM = VIS_DIM + INVIS_DIM      ! TOTAL DIMENSIONS
      CALL EZRSET
  999 RETURN
      END
