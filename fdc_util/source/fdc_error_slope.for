      FUNCTION FDC_ERROR_SLOPE(SLOPE,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correction to hit position error based on slope 
C-     of segment.
C-
C-   Returned value  : FDC_ERROR_SLOPE, Amount of error in cm
C-   Inputs  : SLOPE, DY/DZ of segment
C-             unit, 0=theta, 1=phi
C-
C-   Created  13-SEP-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    FDC_ERROR_SLOPE
C----------------------------------------------------------------------
C Input:
      REAL    SLOPE
      INTEGER UNIT
C Local:
      INTEGER MAX_COEF
      PARAMETER( MAX_COEF = 4 )
C
      INTEGER IER,I
      INTEGER N_COEF_THETA
      INTEGER N_COEF_PHI
C
      REAL    ERR_COEF_THETA(0:MAX_COEF-1)
      REAL    ERR_COEF_PHI(0:MAX_COEF-1)
      REAL    ERROR 
C
      LOGICAL FIRST
C
      SAVE FIRST,N_COEF_THETA,N_COEF_PHI,ERR_COEF_THETA,ERR_COEF_PHI
C
      DATA FIRST /.TRUE./
      DATA    ERR_COEF_THETA /0.0272,3*0.0/     ! equiv 8. ns at 34 mic/ns
      DATA    ERR_COEF_PHI /0.0238,3*0.0/       ! equiv 7. ns at 34 mic/ns
C                                               ! for MC use 155. for both.
C                                               
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGETA('ERR_COEF_THETA',0,0,0,N_COEF_THETA,IER)
        CALL EZGETA('ERR_COEF_PHI',0,0,0,N_COEF_PHI,IER)
        CALL EZGET('ERR_COEF_THETA',ERR_COEF_THETA,IER)
        CALL EZGET('ERR_COEF_PHI',ERR_COEF_PHI,IER)
        CALL EZRSET
      ENDIF
C      
      IF ( UNIT.EQ.0 ) THEN
        ERROR = ERR_COEF_THETA(0) 
        DO  I =  1, N_COEF_THETA - 1
          ERROR = ERROR + ERR_COEF_THETA(I) * SLOPE**(2*I)
        ENDDO
      ELSE
        ERROR = ERR_COEF_PHI(0) 
        DO  I =  1, N_COEF_PHI - 1
          ERROR = ERROR + ERR_COEF_PHI(I) * SLOPE**(2*I)
        ENDDO
      ENDIF
C
      FDC_ERROR_SLOPE = ERROR 
C
  999 RETURN
      END
