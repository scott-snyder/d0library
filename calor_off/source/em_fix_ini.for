      LOGICAL FUNCTION EM_FIX_INI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-JUL-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER IER
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      LOGICAL FLGCHK,FLGVAL
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        EM_FIX_INI=.FALSE.
C
C
C ****  Book ZLINKC area if NOT already declared as protected
C
        IF (.NOT. FLGCHK('ZLINKC_INI'))THEN
          CALL FLGBK('ZLINKC_INI',1)
        ENDIF
        IF (.NOT. FLGVAL('ZLINKC_INI'))THEN
          CALL MZLINK(IXCOM,'/ZLINKC/',CSTLNK,CRFLNK,CRFLNK(LNKMX))
          CALL FLGSET('ZLINKC_INI',.TRUE.)
        ENDIF
C
C       read in files
        CALL INRCP('CAPHEL_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        CALL INRCPE('CAPHEL_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &  CALL ERRMSG('EM_FIX','EM_FIX_INI',
     &  ' Default CAPHEL_RCP modified','W')
C
C DECLARE LINK AREA
C
        CALL EZPICK('CAPHEL_RCP')              ! select CAPHEL bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          EM_FIX_INI= .TRUE.
        ELSE
          CALL ERRMSG('EM_FIX','EM_FIX_INI',
     &      ' CAPHEL_RCP file does not have a CAPHEL bank.','W')
          EM_FIX_INI = .FALSE.
        ENDIF
C
        CALL INRCP('CLEANEM_RCP',IER) 
        IF (IER.NE.0) THEN
          CALL ERRMSG('EM_FIX','EM_FIX_INI',
     &      ' Default NO CLEANEM_RCP FILE ','W')
          GOTO 999            
        ENDIF
C
        CALL EZRSET
      ELSE
C
C ****  Not first entry into EM_FIX_INI - set EM_FIX_INI .TRUE.
C
        EM_FIX_INI=.TRUE.
      ENDIF
  999 RETURN
      END
