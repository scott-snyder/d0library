      LOGICAL FUNCTION MUGT_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Muon with Geant package,
C-
C-
C-   Returned value  : 
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: 
C-
C-
C-      CREATED FROM MURECO_INI  DEC-31-1990     SHAHRIAR ABACHI
C-
C-   Created   08-OCT-1989   Shuichi Kunori
C-   Updated   08-MAY-1990   Shahariar Abachi    : Geant Geometry set up added
C-   Updated   01-DEC-1990   Shahariar Abachi    : GSAVE.DAT is read from STP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE,GEOFIL
      PARAMETER ( RCPFILE = 'MURECO_RCP' )
      PARAMETER ( GEOFIL  = 'GEOMETRY_FILE' )
      INTEGER IER,IDX
CC      PARAMETER ( IDX = 1 )
      CHARACTER*64 FILNAM, FILN
C
      LOGICAL TRIG_SET,FLGVAL,OK,MURECO_DDF
      INTEGER I
      INTEGER           NMODUL          ! number of active modules
      INTEGER           IMODUL          ! for looping over NMODUL
      INTEGER           MODNMS(400)     ! list of active modules
      INTEGER           MUNMOD
      INTEGER           DUM,LENGTH,LEN
      CHARACTER*3       CMODUL          ! character containing IMODUL
      CHARACTER*16      CMDNMS          ! character containing MODNMS(NMODUL)
      LOGICAL           OK1,OK2,OK_OUT
      CHARACTER*32      PEDFIL_NAM
      CHARACTER*32      GEOFIL_NAM
      CHARACTER*32      TIMFIL_NAM
      CHARACTER*32      DTMFIL_NAM
      CHARACTER*32      GANFIL_NAM
      CHARACTER*64      PARFIL_NAM
      CHARACTER*64      MSGSTR
C
      CHARACTER*80 MSG
C
CC      MUGT_INI=.FALSE. 
      MUGT_INI=.TRUE.
C
      OK=.TRUE.
C
      I=2
      CALL INZCOM(I)            !Initialize Zebra data banks
      CALL INZSTP               !Initialize Zebra STP banks
C
      CALL MUON_BOOK_FLAGS      !Book logical flags for application
C
C  read RCP file.
C ================
C
      CALL INRCP (RCPFILE,IER)

      IF(IER.NE.0) THEN
         CALL ERRMSG('Error return from S/R INRCP','MUGT_INI'
     +            ,'stop processing.','F')
         MUGT_INI = .FALSE.
         GO TO 999
      ENDIF
C
C  set RCP bank to 'MUON_RCP'.
C=============================
C
      CALL EZPICK('MURECO_RCP')
C
C
C - GET GEANT GEOMETRY  (SA)
C
      IDX = 2
      CALL EZGETS (GEOFIL,IDX,FILN,LEN,IER)
      IF (IER.NE.0) THEN
         GO TO 999
      ENDIF
C
      CALL MGET_GEANT(FILN)
C
C  Reset RCP bank.
C  ===============
C
      CALL EZRSET
C
C
C  Every thing is OK.  Reset the flag.  
C =====================================
C
CC      MUGT_INI=MURECO_DDF()
C
C
  999 RETURN
      END
