      LOGICAL FUNCTION MUINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize MUON package.
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-OCT-1989   Shuichi Kunori
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      CHARACTER*(*) RCPFILE
      PARAMETER ( RCPFILE = 'MUON_RCP' )
      INTEGER IER
C
      LOGICAL TRIG_SET,FLGVAL,OK
      INTEGER I
      INTEGER           NMODUL          ! number of active modules
      INTEGER           IMODUL          ! for looping over NMODUL
      INTEGER           MODNMS(400)     ! list of active modules
      INTEGER           MUNMOD
      INTEGER           DUM
      CHARACTER*3       CMODUL          ! character containing IMODUL
      CHARACTER*16      CMDNMS          ! character containing MODNMS(NMODUL)
      LOGICAL           OK1,OK2,OK_OUT
      CHARACTER*32      PEDFIL
      CHARACTER*32      GEOFIL
      CHARACTER*32      TIMFIL
      CHARACTER*32      DTMFIL
      CHARACTER*32      GANFIL
      CHARACTER*64      PARFIL
      CHARACTER*64      MSGSTR
C
      CHARACTER*80 MSG
      DATA GEOFIL/'MU$GEOM'/
      MUINIT=.TRUE.
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
         MUINIT = .FALSE.
         GO TO 999
      ENDIF
C
C  set RCP bank to 'MUON_RCP'.
C=============================
C
      CALL EZPICK('MUON_RCP')
C
C  Get constants, i.e. pedestals etc. 
C  ===================================
C
C           (this part is temporary deleted.)
C
C
      CALL MRZCON('GEOM',GEOFIL,0,OK2)    !  READ MUON GEOMETRY
      IF(OK2) THEN
        CALL INTMSG(' MU GEOM init OK')
      ELSE
        CALL INTMSG(' MU GEOM init fails')
      ENDIF
C
C  Reset RCP bank.
C  ===============
C
      CALL EZRSET
C
  999 RETURN
      END
