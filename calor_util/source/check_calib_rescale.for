      SUBROUTINE CHECK_CALIB_RESCALE(IETA,IPHI,ILYR,AWC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CHECK CALIB IN CFC BANK WITH DIRECT ARRAY
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-OCT-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,ILYR
      REAL    AWC
      LOGICAL first
      SAVE first
      DATA first / .true. /
      INTEGER IM,CAL_MODULE
      CHARACTER*10 MNAME
      REAL    DEL
      INTEGER NMODM
      PARAMETER( NMODM = 10 )
      REAL    CALIB(64,-37:37,NMODM)
      SAVE CALIB
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        READ(45)CALIB
        close(unit=45)
      ENDIF
      IM = CAL_MODULE(IETA,ILYR,MNAME)
      DEL = CALIB(IPHI,IETA,IM)-AWC
      IF ( ABS(DEL).GT.0.001 ) THEN
        CALL ERRMSG('ESCALE','CHECK_CALIB_RESCALE',
     &    ' MISMATCH IN CFC AND CALIB ','W')
      ENDIF
  999 RETURN
      END
