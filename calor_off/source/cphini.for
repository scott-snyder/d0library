      FUNCTION CPHINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine for Photons and Electrons
C-
C-   Returned value  : TRUE if all OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-APR-1989   Rajendran Raja
C-   Updated  24-AUG-1992   Rajendran Raja  removed reading in oldstyle 
C-   Hmatrix 
C-   Updated  16-NOV-1992   Meenakshi Narain  read in cleanem_rcp  
C-   Updated  27-FEB-1994   Meenakshi Narain  
C-                          make ZLINKC area if not already declared 
C-                          using ZLINKC_INI flag
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      LOGICAL CPHINI,OK
      CHARACTER*32 FILNAM
      INTEGER FILARR(8),IER
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      LOGICAL HMREAD,DO_HMATRIX
      LOGICAL FLGCHK,FLGVAL
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        CPHINI=.FALSE.
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
     &  CALL ERRMSG('CAPHEL','CPHINI',
     &  ' Default CAPHEL_RCP modified','W')
C
C DECLARE LINK AREA
C
        CALL EZPICK('CAPHEL_RCP')              ! select CAPHEL bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          CPHINI= .TRUE.
        ELSE
          CALL ERRMSG('CAPHEL','CPHINI',
     &      ' CAPHEL_RCP file does not have a CAPHEL bank.','W')
          CPHINI = .FALSE.
        ENDIF
C
        CALL INRCP('CLEANEM_RCP',IER) 
        IF (IER.NE.0) THEN
          CALL ERRMSG('CAPHEL','CPHINI',
     &      ' Default NO CLEANEM_RCP FILE ','W')
          GOTO 999            
        ENDIF
C
        CALL EZRSET
      ELSE
C
C ****  Not first entry into CPHINI - set CPHINI .TRUE.
C
        CPHINI=.TRUE.
      ENDIF
  999 RETURN
      END
