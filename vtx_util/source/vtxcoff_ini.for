      FUNCTION VTXCOFF_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C   Initialization routine for VTXCOFF, VTX offline calibration package
C                 Read VTXCOFF_RCP
C                 Modify RCP parameters in VTRAKS_RCP, and ZTRAKS_RCP
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-SEP-1992   Myungyun Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL VTXCOFF_INI
C
      LOGICAL FIRST,OK
      INTEGER IER
      CHARACTER*(*) RCPFIL,RCPEFIL
      PARAMETER( RCPFIL = 'VTXCOFF_RCP' )   ! Logical name of control file
      PARAMETER( RCPEFIL = 'VTXCOFF_EDIT_RCP' ) ! Logical name of control file
      LOGICAL T_ZERO
      LOGICAL VTZERO_INI
      DATA FIRST / .TRUE. /
C---------------------------------------------------------------------------
      VTXCOFF_INI = .TRUE.
C
      IF (.NOT. FIRST) GOTO 999
C
      CALL INRCP(RCPFIL,IER)
      CALL EZPICK( 'VTXCOFF_RCP' )
      CALL EZGET('T_ZERO',T_ZERO,IER)
      CALL EZRSET
      CALL INRCPE(RCPEFIL,IER)
 
      IF ( T_ZERO ) THEN
        CALL DHDIR('VTXCOFF_RCP','HBOOK_DIRECTORY_T',IER,' ')
        OK = VTZERO_INI()
        VTXCOFF_INI = OK
      END IF
C
C
      FIRST = .FALSE.
C
  999 RETURN
      END
