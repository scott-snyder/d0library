      SUBROUTINE D0RZOPEN(LUN, FILNAM, CHOPT, LRECL, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open a direct access file (machine independent)
C-
C-   Inputs  :
C-
C-   LUN   = unit number
C-
C-   FILNAM= file name.  Can be plain file or FATMEN generic name.
C-
C-   CHOPT = character options
C-           'I' Input  (status OLD, + READONLY if allowed by local Fortran)
C-           'O' Output (VAX status NEW; UNIX status UNKNOWN)
C-           'M' Modify (status OLD, not READONLY).  May also be specified
C-               as 'IO'.
C-           'S' Shared (include SHARED keyword or equivalent if allowed
C-               by the local Fortran).
C-           'F' Formatted
C-           'U' Unformatted
C-           'N' No translate (suppress normal filename conversions on UNIX.
C-                             No effect on VAX).
C-           '/' FATMEN options follow
C-
C-   LRECL = Record length in bytes.  Specifying 0 gives the default length
C-           of 4096 bytes.
C-
C-   Outputs :
C-
C-   OK    = set to false if there is a problem opening file
C-           true otherwise
C-
C-   Created  25-Aug-1992   Herbert B. Greenlee
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:FATCOM.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INTEGER LUN, LRECL
      CHARACTER*(*) FILNAM, CHOPT
      LOGICAL OK
      CHARACTER*80 CLUN, CFMOPT
      CHARACTER*12 FORM, STATUS
      INTEGER I, L, IOPT, IER, OPENLEN, RECL, IOSTAT, ISLASH, LFMBNK
      CHARACTER*255 LOCAL_FILENAME
      CHARACTER*1 CH
      LOGICAL EXACT, READONLY, SHARED
      LOGICAL OPTI, OPTO, OPTM, OPTS, OPTF, OPTU, OPTN
      INTEGER LENOCC
C&IF VAXVMS
C&ELSE
C&      INTEGER CONTEXT
C&ENDIF
      CHARACTER*132 CHLINE
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C
      L=LEN(CHOPT)
      OK=.TRUE.
C
C  Default options and keywords
C
      STATUS = 'OLD'
      FORM= 'UNFORMATTED'
      READONLY = .TRUE.
      SHARED = .FALSE.
      EXACT = .FALSE.
C
C  Parse options
C
      OPTI = .FALSE.
      OPTO = .FALSE.
      OPTM = .FALSE.
      OPTS = .FALSE.
      OPTF = .FALSE.
      OPTU = .FALSE.
      OPTN = .FALSE.
      DO 1 I=1,L
        CALL UPCASE(CHOPT(I:I),CH)
        IF(CH.EQ.'I')OPTI = .TRUE.
        IF(CH.EQ.'O')OPTO = .TRUE.
        IF(CH.EQ.'M')OPTM = .TRUE.
        IF(CH.EQ.'S')OPTS = .TRUE.
        IF(CH.EQ.'F')OPTF = .TRUE.
        IF(CH.EQ.'U')OPTU = .TRUE.
        IF(CH.EQ.'N')OPTN = .TRUE.
    1 CONTINUE
C
C  Convert 'IO' to 'M'
C
      IF(OPTI.AND.OPTO)THEN
        OPTI = .FALSE.
        OPTO = .FALSE.
        OPTM = .TRUE.
      ENDIF
C
C ****  decide if this is "normal" or generic name
C ****  this is probably OK only for the VMS; for the UNIX one may need
C ****  invent something different if we have directories like //FNAL
C
      IF ( FILNAM(1:9).NE. D0TRNK ) THEN
C
C ****  normal name proceede as befeore
C
C
C- Look for 'I' option (open for input).
C
        IF(OPTI)THEN
          READONLY = .TRUE.
          STATUS = 'OLD'
        ENDIF
C
C- Look for 'O' option (open new file for output).
C
        IF(OPTO)THEN
          READONLY = .FALSE.
          STATUS = 'NEW'
        ENDIF
C
C- Look for 'M' option (open for modify).
C
        IF(OPTM)THEN
          READONLY = .FALSE.
          STATUS = 'OLD'
        ENDIF
C
C- Look for 'S' option (shared access).
C
        IF(OPTS)THEN
          SHARED = .TRUE.
        ENDIF
C
C- Look for 'U' option ('UNFORMATTED').
C
        IF(OPTU)THEN
          FORM = 'UNFORMATTED'
        ENDIF
C
C- Look for 'F' option ('FORMATTED').
C
        IF(OPTF)THEN
          FORM = 'FORMATTED'
        ENDIF
C
C- Look for 'N' option (exact filename).
C
        IF(OPTN)THEN
          EXACT = .TRUE.
        ENDIF
C
C- Here we convert to the local filename.  This section does nothing in VMS
C- or if the 'N' option has been specified in UNIX.  Conversion consists of
C- calling LIB$FIND_FILE or simply converting to lower case.
C
        LOCAL_FILENAME = FILNAM
C&IF VAXVMS
C&ELSE
C&        IF(.NOT.EXACT)THEN
C&          IF(STATUS .EQ. 'OLD')THEN
C&            CONTEXT = 0
C&            CALL LIB$FIND_FILE(FILNAM, LOCAL_FILENAME, CONTEXT)
C&            CALL LIB$FIND_FILE_END(CONTEXT)
C&            IF(LOCAL_FILENAME.EQ.' ')THEN
C&              CHLINE = 'LIB$FIND_FILE could not translate '//FILNAM
C&              CALL ERRMSG('D0RZOPEN','D0RZOPEN',
C&     &          CHLINE(1:LENOCC(CHLINE)),'I')
C&              OK=.FALSE.
C&              GO TO 999
C&            ENDIF
C&          ELSE
C&            CALL CUTOL(LOCAL_FILENAME)
C&          ENDIF
C&        ENDIF
C&ENDIF
C
C- Here we unconditionally convert STATUS = 'NEW' to STATUS = 'UNKNOWN' on 
C- non-VMS machines.
C
C&IF VAXVMS
C&ELSE
C&        IF(STATUS .EQ. 'NEW')STATUS = 'UNKNOWN'
C&ENDIF
C
C- Here we supply the default record length (if necessary) and convert to the 
C- correct units for the local Fortran's OPEN statement (bytes or words).  
C
        RECL = LRECL
        IF(RECL.EQ.0)RECL = 4096
C&IF VAXVMS, SIUNIX, ULTRIX
        OPENLEN = (RECL+3)/4
C&ELSE
C&        OPENLEN = RECL
C&ENDIF
C
C- OPEN statements follow.  There are four possible combinations of keywords
C- depending on whether READONLY or SHARED access has been requested.
C
        IF(READONLY)THEN
          IF(SHARED)THEN
C&IF VAXVMS, SIUNIX, ULTRIX,ALFOSF
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,READONLY,SHARED,
     &           FILE=LOCAL_FILENAME,ACCESS='DIRECT',RECL=OPENLEN,
     &           ERR=100,IOSTAT=IOSTAT)
C&ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ACCESS='DIRECT',RECL=OPENLEN,
C&     &        ERR=100,IOSTAT=IOSTAT)
C&ENDIF
          ELSE
C&IF VAXVMS, SIUNIX, ULTRIX,ALFOSF
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,READONLY,
     &           FILE=LOCAL_FILENAME,ACCESS='DIRECT',RECL=OPENLEN,
     &           ERR=100,IOSTAT=IOSTAT)
C&ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ACCESS='DIRECT',RECL=OPENLEN,
C&     &        ERR=100,IOSTAT=IOSTAT)
C&ENDIF
          ENDIF
        ELSE
          IF(SHARED)THEN
C&IF VAXVMS, SIUNIX, ULTRIX,ALFOSF
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,SHARED,
     &           FILE=LOCAL_FILENAME,ACCESS='DIRECT',RECL=OPENLEN,
     &           ERR=100,IOSTAT=IOSTAT)
C&ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ACCESS='DIRECT',RECL=OPENLEN,
C&     &        ERR=100,IOSTAT=IOSTAT)
C&ENDIF
          ELSE
C&IF VAXVMS, SIUNIX, ULTRIX,ALFOSF
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
     &           FILE=LOCAL_FILENAME,ACCESS='DIRECT',RECL=OPENLEN,
     &           ERR=100,IOSTAT=IOSTAT)
C&ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ACCESS='DIRECT',RECL=OPENLEN,
C&     &        ERR=100,IOSTAT=IOSTAT)
C&ENDIF
          ENDIF
        ENDIF
C
        GO TO 999
C
C           handle errors
C
  100   CONTINUE
        WRITE (CHLINE, 101)IOSTAT, FILNAM(1:LENOCC(FILNAM))
  101   FORMAT(' OPEN returned IOSTAT value ',I10,' for file ',A)
        CALL ERRMSG('D0RZOPEN','D0RZOPEN',
     &    CHLINE(1:LENOCC(CHLINE)),'I')
        OK=.FALSE.
        GO TO 999
      ELSE
C
C ****  this is a generic name
C
        IF( FIRST ) THEN
C
C ****  initialize FATMEN
C
          FIRST = .FALSE.
          CALL INFTMN
        ENDIF
C
C ****  find out if there are special FATMEN options (look for "/")
C
        ISLASH=INDEX(CHOPT,'/')
C
        IF (ISLASH.EQ.0) THEN
C
C ****  there are not; put the default read access
C
          CFMOPT='RV'
C
        ELSE
C
C ****  parse the FATMEN options
C
          CALL UPCASE(CHOPT(ISLASH+1:),CFMOPT)
C
        ENDIF
C
C ****  parse the unit number
C
        CLUN=' '
        IF ( LUN.GT.0 .AND. LUN.LT.10 ) THEN
          WRITE(CLUN,'(I1)') LUN
        ELSEIF ( LUN.GE.10 .AND. LUN.LT.100 ) THEN
          WRITE(CLUN,'(I2)') LUN
        ELSE
          CALL ERRMSG('D0RZOPEN','D0RZOPEN',
     &      'D0RZOPEN called with illegal value for LUN','F')
        ENDIF
C
C ****  call the famoust FMOPEN
C
        CALL MZDROP(IFMSTR,LFMINF,'L')
        LFMBNK=0
        CALL FMOPEN(FILNAM(1:LENOCC(FILNAM)),CLUN(1:LENOCC(CLUN)),
     &    LFMBNK,CFMOPT,IER)
        IF (IER.NE.0) OK=.FALSE.
C
C ****  protecting the FATMEN bank link
C
        CALL ZSHUNT(IFMSTR,LFMBNK,LFMINF,1,0)
C
      ENDIF
  999 RETURN
      END





