      LOGICAL FUNCTION VTXTHR(TYPE,THETA,DELTHE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Evaluate parametrization of VTX theta resolution as 
C-               a function of THETA.  Compare this with DELTHE.
C-
C-   Returned value  : TRUE if ABS(DELTHE) less then tolerance, else FALSE
C-   Inputs  :         THETA -- theta value param. is to be evaluated at
C-                     DELTHE - Test value
C-   Outputs : 
C-   Controls:         TYPE = 1 --> THETA of track
C-                            2 --> THETA of track's COG
C-
C-   Created  26-MAY-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c I/O:
      INTEGER TYPE
      REAL    THETA,DELTHE
c Locals:
      LOGICAL FIRST
      REAL    THER(3,2),TOL
      INTEGER IER
c Data:
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('THETA_RESOL',THER(1,1),IER)
        CALL EZGET('THCEN_RESOL',THER(1,2),IER)
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
      TOL = AMAX1( THER(2,TYPE) , THER(1,TYPE)*SIN(THETA)**2 )
      VTXTHR = ABS(DELTHE) .LT. THER(3,TYPE)*TOL
  999 RETURN
      END
