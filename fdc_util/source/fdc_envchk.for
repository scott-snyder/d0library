      SUBROUTINE FDC_ENVCHK(VAL,NDEVT,NDEVP,PABS,T_DEGC,GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert contents of DBM banks to useable
C-      pressure and temperature
C-
C-   Inputs  : VAL(10)           Enviormental sensors
C-   Outputs : PABS,T_DEGC
C-             GOOD             Status
C-   Controls: 
C-
C-   Created   5-APR-1993   Robert E. Avery   Based on VTX_ENVCHK
C-   Updated  10-OCT-1994   Srini Rajagopalan New arguments - NDEVT,NDEVP. 
C-   Allow averaging over NDEVP,NDEVT devices instead of a fixed 1,6 resp.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL VAL(*),PABS,T_DEGC
      LOGICAL GOOD
      INTEGER I,TOT,ERR
      INTEGER NDEVT,NDEVP
      REAL SUM
      REAL T_DEFAULT
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('ENV_TEMP_DEFAULT',T_DEFAULT,ERR)
        IF (ERR.NE.0) T_DEFAULT = 22.0
        CALL EZRSET
      ENDIF
      GOOD = .FALSE.
C
C ****  First 6 devices are temperatures -- require at least 2 of 6
C ****    to be good else just use a default (T_DEFAULT)
C
      SUM = 0.
      TOT = 0
      DO I = 1,NDEVT
        IF (    (VAL(I) .GT.  0.0) 
     &    .AND. (VAL(I) .LT. 30.0) ) THEN
          TOT = TOT + 1
          SUM = SUM + VAL(I)
        ENDIF
      ENDDO
      IF (TOT .GE. 2) THEN 
        T_DEGC = SUM/TOT
      ELSE
        T_DEGC = T_DEFAULT
      ENDIF
C        
C ****  Next device is barometric pressure
C
      SUM = 0.
      TOT = 0
      PABS = 0.
      DO I = NDEVT+1,NDEVT+NDEVP
        IF (VAL(I).GT.0.0) THEN
          SUM = SUM + VAL(I)
          TOT = TOT + 1
        ENDIF
      ENDDO
      IF (TOT.GE.1) PABS = SUM/TOT
C
      IF (PABS .GT. 0) THEN
        GOOD = .TRUE.
      ENDIF
  999 RETURN
      END
