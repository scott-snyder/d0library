      FUNCTION PVINIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         VTXDIS
C-
C-   Read RCP file PX_VTXDIS.RCP into memory.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-SEP-1990   Lupe Howell
C-   Updated  22-MAR-1991   Harrison B. Prosper  
C-      CALL VTISTP 
C-   Updated   5-JUL-1994   Danilo Puseljic create the VTX link area
C-                          by CALL VTPLNK
C-   Updated   8-NOV-1995   Nobuaki Oshima - No 'CALL VTISTP' anymore
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PVINIT
C----------------------------------------------------------------------
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_VTXDIS_RCP' )
C----------------------------------------------------------------------
      LOGICAL OK, FIRST
      DATA FIRST/.TRUE./
      SAVE OK, FIRST
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP (RCPFILE,IER)
C
C ****  Initialize menu
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &      ('PIXIE','PVINIT','Problem accessing PX_VTXDIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
      ENDIF
C
C ****  Read in Vertex geometry data
C
CNO      CALL VTISTP('VTX_STPFILE',IER)
CNO      IF ( IER .NE. 0 ) THEN
CNO        CALL ERRMSG
CNO     &    ('PIXIE','PVINIT','Problem accessing VTX_STPFILE','F')
CNO      ENDIF
CNO      OK = OK .AND. (IER.EQ.0)
      PVINIT = OK
C
      CALL VTPLNK                ! Create permanent link area for hit banks
C
  999 RETURN
      END
