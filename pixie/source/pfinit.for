      FUNCTION PFINIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         FDCDIS
C-
C-   Read RCP file PX_FDCDIS.RCP into memory, and initialize FDC geometry.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  19-SEP-1990   PXBUILD V1.00
C-   Updated  28-NOV-1990   Harrison B. Prosper  
C-      Add call to EZ_SETUP_COMPACK 
C-   Updated  30-APR-1991   Jeffrey Bantly  handle STP file correctly 
C-   Updated  23-AUG-1991   Robert E. Avery  init. SHIFT and FCODER 
C-   Updated  21-NOV-1991   Robert E. Avery  Choose right Phi region for
C-                              test beam (phi1-phi2 is X>0) 
C-   Updated  25-JUN-1992   Robert E. Avery  Book flags. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PFINIT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER,RUNTYPE,PFFLAGS
      INTEGER LRCP
      INTEGER IMODE
C
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_FDCDIS_RCP' )
C
      LOGICAL OK, OKF, FIRST
C
C Functions:
      LOGICAL FTRINI
      LOGICAL FGEOM_INIT
      LOGICAL EZERROR
C
      SAVE OK, FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP (RCPFILE,IER)
C
C ****  Build COMPACK menus
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &      ('PIXIE','PFINIT','Problem accessing PX_FDCDIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
C
C ****  Book flags
C
        IER = PFFLAGS()
        OK = OK .AND. (IER .EQ. 0)
C
C ****  Fetch STP file name from FTRAKS.RCP
C
        CALL EZLOC('FTRAKS_RCP',LRCP)                        
        IF(LRCP.LE.0) THEN                                   
          OKF=FTRINI()
          IF(.NOT. OKF) THEN
            CALL ERRMSG
     &        ('PIXIE','PFINIT','FTRAKS_RCP not available','F')
            OK = .FALSE.
            GOTO 990
          ENDIF
        ENDIF
C
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('RUNTYPE',RUNTYPE,IER)
        CALL EZRSET
        CALL FCODER_INI(RUNTYPE,0)
C
C ****  Read geometry data
C
        OK = OK .AND. FGEOM_INIT()
C
C ****  Set PHI mode for TB NWA data
C
        IF ( RUNTYPE.EQ.2 ) THEN
           CALL EZPICK('PX_FDCDIS_RCP')
           CALL PUGETV( 'FDC PHI MODE', IMODE )
           IF ( IMODE.EQ.0 ) THEN
             CALL PUSETV( 'FDC PHI MODE', -1 )
           ENDIF
           CALL EZRSET
        ENDIF
      ENDIF
C
  990 CONTINUE
      PFINIT = OK
C----------------------------------------------------------------------------
  999 RETURN
      END
