      LOGICAL FUNCTION MURECO_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Muon reconstruction package,
C-                         'MURECO'.
C-
C-   Returned value  : 
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: 
C-
C-   Created   08-OCT-1989   Shuichi Kunori
C-   Updated   08-MAY-1990   Shahariar Abachi    : Geant Geometry set up added
C-   Updated   01-DEC-1990   Shahariar Abachi    : GSAVE.DAT is read from STP
C-   DH 12/91 move constants to beginning of run
C-   Updated   12-APR-1994   M. Fortner : Add call to MUDROP_STA
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE,GEOFIL
      PARAMETER ( RCPFILE = 'MURECO_RCP' )
      PARAMETER ( GEOFIL  = 'GEANT_GEOMETRY' )
      INTEGER IER,IDX
      CHARACTER*64 FILN
C
      LOGICAL MURECO_DDF,OK,SAMRECO_INI
      INTEGER I,LEN
C
      MURECO_INI=.FALSE. 
      I=2
      CALL INZCOM(I)            !Initialize Zebra data banks
      CALL INZSTP               !Initialize Zebra STP banks
      CALL MUON_BOOK_FLAGS      !Book logical flags for application
C
C  read RCP file.
C ================
C
      CALL INRCP (RCPFILE,IER)
      IF(IER.NE.0) THEN
         CALL ERRMSG('Error return from S/R INRCP','MURECO_INI'
     +            ,'stop processing.','F')
         MURECO_INI = .FALSE.
         GO TO 999
      ENDIF
C
C  set RCP bank to 'MUON_RCP'.
C=============================
C
      CALL EZPICK('MURECO_RCP')
C
C - GET GEANT GEOMETRY  (SA)
C
      IDX = 1
      CALL EZGETS (GEOFIL,IDX,FILN,LEN,IER)
      IF (IER.NE.0) THEN
         GO TO 999
      ENDIF
      CALL MGET_GEANT(FILN)
C
C  Identify banks to drop in STA
C  =============================
C
      CALL MUDROP_STA(IER)
C
C  Reset RCP bank.
C  ===============
C
      CALL EZRSET
C
C  User initialization.
C  ====================
C
      CALL MUUSER_INIT
C
C  Every thing is OK.  Reset the flag.  
C =====================================
C
      MURECO_INI=MURECO_DDF()
C
      OK=SAMRECO_INI()
C
  999 RETURN
      END
