      LOGICAL FUNCTION MUNGT_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize special Muon reconstruction package,
C-                         'MUNGT'.
C-
C-   Returned value  : 
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: 
C-
C-   CREATED FROM MURECO_INI   21-DEC-1990   Shahariar Abachi
C-                      NO GEANT NO TRACKING                    
C-
C-   Created   08-OCT-1989   Shuichi Kunori
C-   Updated   08-MAY-1990   Shahariar Abachi    : Geant Geometry set up added
C-   Updated   01-DEC-1990   Shahariar Abachi    : GSAVE.DAT is read from STP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE,GEOFIL
      PARAMETER ( RCPFILE = 'MURECO_RCP' )
      PARAMETER ( GEOFIL  = 'SMUO_FILE' )
      INTEGER IER,IDX
CC      PARAMETER ( IDX = 1 )
      CHARACTER*32 FILNAM, FILN
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
      MUNGT_INI=.FALSE. 
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
         CALL ERRMSG('Error return from S/R INRCP','MURECO_INI'
     +            ,'stop processing.','F')
         MUNGT_INI = .FALSE.
         GO TO 999
      ENDIF
C
C  set RCP bank to 'MUON_RCP'.
C=============================
C
      CALL EZPICK('MURECO_RCP')
C
C  Get constants, i.e. pedestals etc. 
C  ===================================
C
C           (this part is temporary deleted.)
C
      IDX = 1
      CALL EZGETS (GEOFIL,IDX,FILNAM,LENGTH,IER)
      IF (IER.NE.0) THEN
         GO TO 999
      ENDIF
C
      CALL MRZCON('ALL',FILNAM(1:LENGTH),0,OK2)    !  READ MUON GEOMETRY
      IF(OK2) THEN
        CALL INTMSG(' MU ALL STP init OK')
      ELSE
        CALL INTMSG(' MU ALL STP init fails')
      ENDIF
C
C - GET GEANT GEOMETRY  (SA)
C
CC      IDX = 2
CC      CALL EZGETS (GEOFIL,IDX,FILN,LEN,IER)
CC      IF (IER.NE.0) THEN
CC         GO TO 999
CC      ENDIF
C
CC      CALL MGET_GEANT(FILN)
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
      MUNGT_INI=MURECO_DDF()
C
C
  999 RETURN
      END
