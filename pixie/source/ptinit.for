      FUNCTION PTINIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         TRDDIS
C-
C-   Read RCP file PX_TRDDIS.RCP into memory.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  24-SEP-1990   PXBUILD V1.00
C-   Updated  22-MAR-1991   Harrison B. Prosper  
C-      CALL TRISTP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PTINIT
C----------------------------------------------------------------------
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_TRDDIS_RCP' )
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
     &      ('PIXIE','PTINIT','Problem accessing PX_TRDDIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
C set up number of wires per layer. Layer3-256 Run Ia, =512 Run Ib.
        CALL TRD_NWIRE_PER_LAYER
      ENDIF
C
C ****  Read in TRD geometry data
C
      CALL TRISTP('TRD_STPFILE',IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG
     &    ('PIXIE','PTINIT','Problem accessing TRD_STPFILE','F')
      ENDIF
      OK = OK .AND. (IER.EQ.0)
C
      PTINIT = OK
  999 RETURN
      END
