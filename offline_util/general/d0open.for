      SUBROUTINE D0OPEN(LUN,FILNAM,CHOPT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open a file (machine independent)
C-
C-   Inputs  :
C-   LUN   = unit number
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C-   FILNAM= file name
C-   CHOPT = character option
C-           'I' Input  (VAX status OLD, READONLY;  UNIX status OLD)
C-           'O' Output (VAX status NEW;      UNIX status UNKNOWN)
C-           'M' Modify (status OLD, not READONLY).  May also be specified
C-               as 'IO'.
C-           'A' Append (this option may not be available on all machines)
C-           'F' Formatted
C-           'U' Unformatted
C-           'D' Delete after close (only allowed combined with O,
C-                                   i.e. 'OD' or 'DO')
C-           'L' List (open with CARRIAGECONTROL='LIST' on VAX)
C-           'N' No translate (suppress normal filename conversions on UNIX.
C-                             No effect on VAX).
C-           'X' Exchange mode for ZEBRA files.  This option should not be
C-               used on any files except ZEBRA files.  Legal options with X
C-               are 'I', 'O', 'N', 'U'.  Others will be ignored. Append
C-               access on a ZEBRA file will cause an error retrun. 'U' option
C-               is implicit for exchange more and need not be specified.
C-           'S' Separated data sets.  This option causes 'S' to be included
C-               in the list of returned FZFILE options.  This only has an
C-               effect when opening 'X' format files for writing.  Note that
C-               separated data sets is the default for 'G' format files even
C-               if 'S' is missing from the list of D0OPEN options.
C-           'G' Exchange mode, separated data sets.  This is for use by
C-               ISAJET when producing input files for the UNIX farm. It should
C-               NOT be used for any other purpose. It implies the 'X' and 'U'
C-               options.
C-               NOTE: On IBM machines there should be no other ZEBRA
C-                     calls between D0OPEN and FZFILE.  This is due 
C-                     to passing of file pointer via IQUEST(1).
C-               
C-               On all machines the proper sequence for opening an exchange
C-               ZEBRA file is :
C-                    CALL D0OPEN(LUN,FILE,CHOPT,OK)
C-                    CALL XZRECL(ILEN,XCHOP)
C-                    IF (ILEN .GE. 0) CALL FZFILE(LUN,ILEN,XCHOP)
C-           'T' Trusted mode.  D0OPEN will assume you know what you are
C-               doing when you open the file and not check for proper
C-               exchange/native mode files.  This applies only to UNFORMATTED
C-               input files, i.e. 'IU' option.
C-           'Z' Open a compressed zebra file.
C-
C-           combinations allowed but some are illegal (i.e. 'UF')
C-           defaults 'I' and 'F' (i.e. ' ' equivalent to 'IF' or 'FI')
C-   Outputs :
C-   OK    = set to false if there is a problem opening file
C-           true otherwise
C-
C-   Serban D. Protopopescu Nov. 2,1988
C-   Updated   1-NOV-1991   Krzysztof L. Genser
C-         to include call to FATMEN FMOPEN in case of generic name
C-   Updated   9-DEC-1991   Krzysztof L. Genser   
C-         ABORT replaced by ERRMSG
C-         Call to INFTMN (FATMEN initialization) after first occurece of
C-         a generic name (I would be nicer to have it in the frame 
C-         initalization, but it is probably to late to do it now...)
C-         Explicte options for FMOPEN are placed behind a '/' in CHOPT
C-   Updated  11-Feb-1992   Herbert B. Greenlee
C-         Added various UNIX flavor stuff:
C-            a) Call lib$find_file before open for read.
C-            b) Use lower case filename and status = 'UNKNOWN' on 
C-               open for write.
C-            c) Added 'L' and 'N' options (see above).
C-   Updated  18-FEB-1992   Krzysztof L. Genser   
C-         use of MZDROP
C-         add SHARED in open with READONLY
C-   Updated  20-FEB-1992   M. Diesburg
C-         Added options to open ZEBRA exchange mode files. Also added 
C-         entry to retrieve record length.
C-   Updated  26-Feb-1992   Herbert B. Greenlee
C-         Remove SHARED from open statements ( doesn't work with tapes )
C-   Updated  10-Mar-1992   Herbert B. Greenlee
C-         Always use CC='NONE' with FORM='UNFORMATTED'
C-   Updated  12-Mar-1992   Krzysztof L. Genser
C-         Add option V for FMOPEN
C-         Add IOSTAT in OPEN
C-   Updated   7-Apr-1992   Herbert B. Greenlee
C-         Add M (modify) option (open old file with write access).
C-   Updated   1-May-1992   Herbert B. Greenlee
C-         Add R option (RZ file)
C-   UPdated  18-May-1992   Michael Diesburg
C-         Add check for exchange/native file type on input
C-   Updated  25-NOV-1992   Krzysztof L. Genser  
C-         Enable full tape staging for FATMEN
C-   Updated  3-Dec-1992    Herbert B. Greenlee
C-         Changed SGI to use C I/O instead of direct access fortran I/O.
C-         Added error messages for C I/O.
C-   Updated 30-Dec-1992    Herbert B. Greenlee
C-         Added D0CLOSE entry point
C-   Updated   8-JAN-1993   Krzysztof L. Genser  
C-         Added VAXVMS retry for network kind of problems while opening an
C-         existing file using FORTRAN I/O
C-   Updated  11-FEB-1993   Krzysztof L. Genser  remove directory not found,
C-         no such file   case from the I/O error FOR$IOS_OPEFAI retry list
C-   Updated 13-Jul-1993   Herbert Greenlee
C-         Added 'S' option.
C-   Updated  22-NOV-1993   Michael J. Wendling  updated for ULTRIX 
C-   Updated  21-Dec-1993   Herbert B. Greenlee
C-         Added CSPACK open method (CZOPEN, XZOPEN etc.)
C-   Updated  12-MAY-1994   Krzysztof L. Genser  
C-         Disable full tape staging in FATMEN
C-   Updated  26-NOV-1994   sss - Add zzip support.
C-   Updated   9-JAN-1995   sss - Fix compilation problems on ibm.
C-   Updated  19-JAN-1995   sss - Eliminate warning on ibms.
C-         Return `P' in xchopt for a zzip open.
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:FATCOM.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INTEGER LUN,L,IOPT,I,J,CLEN
      CHARACTER*80 CLUN
      CHARACTER*(*) FILNAM,CHOPT,XZCHPT
      CHARACTER*12 FORM, CC, STATUS, XCHOPT
      LOGICAL OK,AOK
      INTEGER ISLASH,LFMBNK,IER
      INTEGER XRECL,GRECL,XZRL,XZRLEN,LREC
      CHARACTER*80 CFMOPT
      CHARACTER*255 LOCAL_FILENAME
      CHARACTER*1 CH
      LOGICAL EXACT
      INTEGER XMODE
      LOGICAL OPTI, OPTO, OPTA, OPTF, OPTU, OPTD, OPTL, OPTN
      LOGICAL OPTX, OPTG, OPTM, OPTT, OPTS, OPTZ
      INTEGER IOSTAT, ZZIP_PACKLEVEL
      INTEGER CONTEXT
      INTEGER LENOCC
      INTEGER LCH
      PARAMETER( LCH = 132 )
      CHARACTER*(LCH) CHLINE
      INTEGER LNF
      CHARACTER*80 CTEMP
      LOGICAL FIRST
      INTEGER TRULEN
      SAVE FIRST,XZRL,XRECL,GRECL,XCHOPT
C-
C- The following variables are static data giving the status of all unit
C- numbers referenced so far.  These data are used by D0CLOSE.
C-
      INTEGER MAX_UNIT
      PARAMETER (MAX_UNIT = 99)
      INTEGER NUM_UNIT               ! Number of unit numbers referenced.
      INTEGER D_LUN(MAX_UNIT)        ! List of unit numbers referenced.
      INTEGER D_STATUS(MAX_UNIT)     ! Unit status: 0 - Not open
                                     !              1 - Opened by Fortran OPEN
                                     !              2 - Opened by FMOPEN
                                     !              3 - Opened by CFOPEN
                                     !              4 - Opened by XZOPEN
                                     !              5 - Opened by zzip.
                                     !              6 - Opened by zunzip.
      INTEGER D_DES(MAX_UNIT)        ! File descriptor returned by CFOPEN 
                                     !   (returned to user via IQUEST(1)).
                                     !   Zero otherwise.
      INTEGER D_MEDIUM(MAX_UNIT)     ! CFOPEN medium.
      CHARACTER*12 D_CHOPT(MAX_UNIT) ! Options used to open unit (if open).
      CHARACTER*4 D_CLUN(MAX_UNIT)   ! Character unit (FATMEN)
      SAVE NUM_UNIT, D_LUN, D_STATUS, D_DES, D_MEDIUM, D_CHOPT, D_CLUN
      INTEGER D_INDEX                ! Index into static data (temporary 
                                     !   variable).
      INTEGER K_NOTOPEN, K_OPEN, K_FMOPEN, K_CFOPEN, K_XZOPEN
      INTEGER K_ZZIPOPEN, K_ZUNZIPOPEN
      PARAMETER (K_NOTOPEN=0, K_OPEN=1, K_FMOPEN=2, K_CFOPEN=3,
     &  K_XZOPEN=4, K_ZZIPOPEN = 5, K_ZUNZIPOPEN = 6)
      DATA NUM_UNIT /0/
C-
C- End of D0CLOSE data.
C-
      DATA FIRST / .TRUE. /
      DATA XRECL,GRECL /8190,900/
C&IF VAXVMS
C
C ****  parameters used by the ERRSNS routine etc...
C
      INTEGER FNUM,RMS_STS,RMS_STV,IUNIT,CONDVAL,NFAIL,NRETRY
      REAL    RETTM
      PARAMETER( RETTM = 600., NRETRY = 10 )
      INCLUDE '($FORIOSDEF)/LIST'
      INCLUDE '($RMSDEF)/LIST'
      INCLUDE '($SSDEF)/LIST'
C&ENDIF

      INTEGER  ZZIP_OPEN, ZUNZIP_OPEN
      EXTERNAL ZZIP_OPEN, ZUNZIP_OPEN
      REAL     VALUEX
      EXTERNAL VALUEX
C----------------------------------------------------------------------
C
C&IF ULTRIX,ALFOSF
C&      INTEGER NFAIL
C&ENDIF
C
      L = INDEX(CHOPT, '/') - 1
      IF(L.LT.0)L = LEN(CHOPT)
      OK=.TRUE.
C
C- Default options and keywords
C
      STATUS = 'OLD'
      FORM='FORMATTED'
      CC='FORTRAN'
      EXACT = .FALSE.
      XZRL = 0
      XCHOPT = ' '
      IOSTAT = 0
      IER = 0
C-
C-  Find index into unit data list.  Increase list if necessary.  It is a
C-  fatal error if the maximum number of units is exceeded.
C-
      D_INDEX = 0
      DO I = 1,NUM_UNIT
        IF(D_LUN(I).EQ.LUN)THEN
          D_INDEX = I
          GO TO 10
        ENDIF
      ENDDO
 10   CONTINUE
      IF(D_INDEX.EQ.0)THEN
        IF(NUM_UNIT.GE.MAX_UNIT)CALL ERRMSG('D0OPEN', 'D0OPEN',
     &    'Maximum number of units exceeded', 'F')
        NUM_UNIT = NUM_UNIT + 1
        D_INDEX = NUM_UNIT
      ENDIF
      IF(D_INDEX.LT.1 .OR. D_INDEX.GT.MAX_UNIT)CALL ERRMSG('D0OPEN',
     &  'D0OPEN', 'D0OPEN error -- invalid index', 'F')
C-
C-  Initialize unit data.  Any previously stored data are ignored.  The 
C-  D_STATUS, D_DES, D_MEDIUM and D_CLUN words may be modified later.
C-
      D_LUN(D_INDEX) = LUN
      D_STATUS(D_INDEX) = K_NOTOPEN
      D_DES(D_INDEX) = 0
      D_MEDIUM(D_INDEX) = 0
      CALL UPCASE(CHOPT, D_CHOPT(D_INDEX))
      D_CLUN(D_INDEX) = ' '
C
C       find options
C
      OPTI = .FALSE.
      OPTO = .FALSE.
      OPTA = .FALSE.
      OPTD = .FALSE.
      OPTF = .FALSE.
      OPTU = .FALSE.
      OPTL = .FALSE.
      OPTN = .FALSE.
      OPTX = .FALSE.
      OPTG = .FALSE.
      OPTM = .FALSE.
      OPTT = .FALSE.
      OPTS = .FALSE.
      OPTZ = .FALSE.
      DO 1 I=1,L
        CALL UPCASE(CHOPT(I:I),CH)
        IF(CH.EQ.'I')OPTI = .TRUE.
        IF(CH.EQ.'O')OPTO = .TRUE.
        IF(CH.EQ.'A')OPTA = .TRUE.
        IF(CH.EQ.'D')OPTD = .TRUE.
        IF(CH.EQ.'F')OPTF = .TRUE.
        IF(CH.EQ.'U')OPTU = .TRUE.
        IF(CH.EQ.'L')OPTL = .TRUE.
        IF(CH.EQ.'N')OPTN = .TRUE.
        IF(CH.EQ.'X')OPTX = .TRUE.
        IF(CH.EQ.'G')OPTG = .TRUE.
        IF(CH.EQ.'M')OPTM = .TRUE.
        IF(CH.EQ.'T')OPTT = .TRUE.
        IF(CH.EQ.'S')OPTS = .TRUE.
        IF(CH.EQ.'Z')OPTZ = .TRUE.
    1 CONTINUE
C
C  Convert 'IO' to 'M'
C
      IF(OPTI.AND.OPTO)THEN
        OPTI = .FALSE.
        OPTO = .FALSE.
        OPTM = .TRUE.
      ENDIF
      IF(OPTI) XCHOPT='I'
      IF(OPTO) XCHOPT='O'
      IF(OPTM) XCHOPT='IO'
C
C ****  decide if this is "normal" or generic name
C ****  this is probably OK only for the VMS; for the UNIX one may need
C ****  invent something different if we have directories like //FNAL
C
      IF ( FILNAM(1:9).NE. D0TRNK ) THEN
C
C ****  normal name proceede as befeore
C
        IF(L.EQ.0.OR.CHOPT.EQ.' ') THEN
          IOPT=1
        ELSE
          IOPT=0
C
C- Look for 'U' option ('UNFORMATTED')
C
          IF(OPTU)THEN
            FORM='UNFORMATTED'
          ENDIF
C
C- Look for 'F' option ('FORMATTED')
C
          IF(OPTF)THEN
            IF(FORM.EQ.'UNFORMATTED') GOTO 200
          ENDIF
C
C- Look for 'I' option (open for input)
C
          IF(OPTI)THEN
            IOPT=1
          ENDIF
C
C- Look for 'O' option (open for output)
C
          IF(OPTO)THEN
            IOPT=2
            STATUS = 'NEW'
          ENDIF
C
C- Look for 'A' option (open for append)
C
          IF(OPTA)THEN
            IOPT=3
          ENDIF
C
C- Look for 'D' option (open for scratch)
C
          IF(OPTD)THEN
            IOPT=4
            STATUS = 'NEW'
          ENDIF
C
C- Look for 'M' option (open for modify)
C
          IF(OPTM)THEN
            IOPT=5
          ENDIF
C
C- Look for 'L' option (CARRIAGECONTROL='LIST')
C
          IF(OPTL)THEN
            CC = 'LIST'
          ENDIF
C
C- Look for 'N' option (exact filename)
C
          IF(OPTN)THEN
            EXACT = .TRUE.
          ENDIF
C
C- Look for 'X' option (ZEBRA exchange mode)
C
          IF(OPTX) THEN
            FORM = 'UNFORMATTED'
            XZRL = XRECL
            IF(OPTA) GO TO 200
          ENDIF
C
C- Look for 'G' option (ZEBRA exchange mode, separated data sets)
C
          IF(OPTG) THEN
            FORM = 'UNFORMATTED'
            XZRL = GRECL
            IF(OPTA) GO TO 200
          ENDIF
C
C- Look for 'S' option (separated data sets) -- Add 'S' to options list.
C
          IF(OPTS) THEN
            CTEMP= 'S'//XCHOPT
            XCHOPT = CTEMP
          ENDIF
        ENDIF
C-
C- Check for remote filename.
C-
        CALL TRNLNM(FILNAM, LOCAL_FILENAME, L)
        IF(INDEX(LOCAL_FILENAME(1:L), '::').NE.0)THEN
          CALL D0CSOPEN_HOOK(LUN, LOCAL_FILENAME(1:L), CHOPT, OK)
          IF(OK)THEN
            XZRL = XRECL
            IF(OPTI)XCHOPT = 'PCXI'
            IF(OPTO)XCHOPT = 'PCXO'
            IF(OPTS)THEN
              CTEMP = 'S'//XCHOPT
              XCHOPT = CTEMP
            ENDIF
            D_STATUS(D_INDEX) = K_XZOPEN
            GO TO 999
          ELSE
            OK = .TRUE.
          ENDIF
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
C&              CALL ERRMSG('D0OPEN','D0OPEN',
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
C- Unconditionally use CC='NONE' with FORM='UNFORMATTED'.
C
        IF(FORM.EQ.'UNFORMATTED')CC='NONE'
C
C- Now open file.  SIUNIX OPEN statements have the same keywords as VMS (but 
C- possibly different values).  Other flavors use only standard keywords.
C
 1001   CONTINUE                    
        IF(IOPT.LT.2) THEN

          IF((FORM.EQ.'UNFORMATTED').AND.(.NOT.OPTT)) THEN
            CALL XCHKER(LUN,LOCAL_FILENAME,XMODE,LREC,AOK)
            IF(AOK)THEN
              IF(XMODE .EQ. 1) THEN
                XZRL = LREC
                IF((.NOT.OPTX).AND.(.NOT.OPTG)) THEN
                  CLEN = LEN(FILNAM)
                  CTEMP = 'Exchange mode file '//FILNAM(1:CLEN)//
     &                    ' was specified as native mode.'
                  CALL ERRMSG('FILE MODE','D0OPEN',CTEMP,'I')
                  CTEMP = 'File will be opened in exchange mode'
                  CALL ERRMSG('FILE MODE','D0OPEN',CTEMP,'I')
                ELSE IF (OPTZ) THEN
                  CLEN = LEN(FILNAM)
                  CTEMP = 'Exchange mode file '//FILNAM(1:CLEN)//
     &                    ' was specified as compressed mode.'
                  CALL ERRMSG ('FILE MODE', 'D0OPEN', CTEMP, 'I')
                  CTEMP = 'File will be opened in exchange mode'
                  CALL ERRMSG('FILE MODE','D0OPEN',CTEMP,'I')
                  OPTZ = .FALSE.
                ENDIF
                IF(LREC.GE.XRECL) OPTX = .TRUE.
                IF(LREC.EQ.GRECL) OPTG = .TRUE.
              ELSE IF (XMODE .EQ. 0) THEN
                XZRL = 0
                IF(OPTX.OR.OPTG)  THEN
                  CLEN = LEN(FILNAM)
                  CTEMP = 'Native mode file '//FILNAM(1:CLEN)//
     &                    ' was specified as exchange mode.'
                  CALL ERRMSG('FILE MODE','D0OPEN',CTEMP,'I')
                  CTEMP = 'File will be opened in native mode'
                  CALL ERRMSG('FILE MODE','D0OPEN',CTEMP,'I')
                ELSE IF (OPTZ) THEN
                  CLEN = LEN(FILNAM)
                  CTEMP = 'Native mode file '//FILNAM(1:CLEN)//
     &                    ' was specified as compressed mode.'
                  CALL ERRMSG('FILE MODE','D0OPEN',CTEMP,'I')
                  CTEMP = 'File will be opened in native mode'
                  CALL ERRMSG('FILE MODE','D0OPEN',CTEMP,'I')
                  OPTZ = .FALSE.
                ENDIF
                OPTX = .FALSE.
                OPTG = .FALSE.
              ELSE IF (XMODE .EQ. 2) THEN
                IF (.NOT. OPTZ) THEN
                  CLEN = LEN(FILNAM)
                  IF (OPTX .OR. OPTG) THEN
                    CTEMP = 'Compressed file '//FILNAM(1:CLEN)//
     &                      ' was specified as exchange mode.'
                  ELSE
                    CTEMP = 'Compressed file '//FILNAM(1:CLEN)//
     &                      ' was specified as native mode.'
                  ENDIF
                  CALL ERRMSG('FILE MODE','D0OPEN',CTEMP,'I')
                  CTEMP = 'File will be opened in compressed mode'
                  CALL ERRMSG('FILE MODE','D0OPEN',CTEMP,'I')
                  OPTZ = .TRUE.
                ENDIF
              ELSE
                WRITE (CTEMP, '(I6)') XMODE
                CALL ERRMSG ('Funny xmode value', 'D0OPEN', CTEMP, 'F')
              ENDIF
            ENDIF
          ENDIF
C
C- Open existing file for read
C
          if (optz) then
            xzrl = -1
            xchopt = 'P'
            ok = .true.
            if (zunzip_open (lun, local_filename) .ge. 0) then
              d_status(d_index) = K_ZUNZIPOPEN
            else
              call zzip_get_last_err (ctemp)
              call errmsg ('zzip error', 'D0OPEN', ctemp, 'E')
              ok = .false.
            endif
            goto 999
          endif
C&IF VAXVMS, ULTRIX,ALFOSF
C
C ****  come here while retrying to open a file
C
          NFAIL=0
          IF(OPTX.OR.OPTG) THEN
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,READONLY,
     &           RECORDTYPE='FIXED',RECL=XZRL,
     &           FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
            D_STATUS(D_INDEX) = K_OPEN
            IF(OPTX)THEN
              CTEMP = 'X'//XCHOPT
              XCHOPT = CTEMP
            ENDIF
            IF(OPTG)THEN
              CTEMP= 'XS'//XCHOPT
              XCHOPT = CTEMP
            ENDIF
          ELSE
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,READONLY,
     &           FILE=LOCAL_FILENAME,ERR=100,
     &           IOSTAT=IOSTAT)
            D_STATUS(D_INDEX) = K_OPEN
          ENDIF
C&ENDIF
C&IF SIUNIX
C&          IF(OPTX.OR.OPTG) THEN
C&            CALL CFOPEN(IQUEST(1),0,XZRL,'r',1,LOCAL_FILENAME,IER)
C&            IF(OPTX)THEN
C&              IF(XZRL.GT.8190)THEN
C&                CTEMP = 'K'//XCHOPT
C&                XCHOPT = CTEMP
C&              ELSE
C&                CTEMP = 'L'//XCHOPT
C&                XCHOPT = CTEMP
C&              ENDIF
C&            ENDIF
C&            IF(OPTG)THEN
C&              CTEMP = 'LS'//XCHOPT
C&              XCHOPT = CTEMP
C&            ENDIF
C&            IF(IER.NE.0)THEN
C&              OK = .FALSE.
C&            ELSE
C&              D_STATUS(D_INDEX) = K_CFOPEN
C&              D_DES(D_INDEX) = IQUEST(1)
C&              D_MEDIUM(D_INDEX) = 0
C&            ENDIF
C&          ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,READONLY,
C&     &           FILE=LOCAL_FILENAME,ERR=100,
C&     &           IOSTAT=IOSTAT)
C&            D_STATUS(D_INDEX) = K_OPEN
C&          ENDIF
C&ENDIF
C&IF IBMAIX
C&          IF(OPTX.OR.OPTG) THEN
C&            CALL CFOPEN(IQUEST(1),0,XZRL,'r',1,LOCAL_FILENAME,IER)
C&            IF(OPTX)THEN
C&              IF(XZRL.GT.8190)THEN
C&                CTEMP = 'K'//XCHOPT
C&                XCHOPT = CTEMP
C&              ELSE
C&                CTEMP = 'L'//XCHOPT
C&                XCHOPT = CTEMP
C&              ENDIF
C&            ENDIF
C&            IF(OPTG)THEN
C&              CTEMP = 'LS'//XCHOPT
C&              XCHOPT = CTEMP
C&            ENDIF
C&            IF(IER.NE.0)THEN
C&              OK = .FALSE.
C&            ELSE
C&              D_STATUS(D_INDEX) = K_CFOPEN
C&              D_DES(D_INDEX) = IQUEST(1)
C&              D_MEDIUM(D_INDEX) = 0
C&            ENDIF
C&          ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ERR=100,
C&     &        IOSTAT=IOSTAT)
C&            D_STATUS(D_INDEX) = K_OPEN
C&          ENDIF
C&ENDIF
C&IF VAXVMS,SIUNIX,IBMAIX,ULTRIX,ALFOSF
C- This is a sleazey way of making an ELSEIF for D0FLAVOR.
C&ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ERR=100,
C&     &        IOSTAT=IOSTAT)
C&            D_STATUS(D_INDEX) = K_OPEN
C&ENDIF
        ELSEIF(IOPT.EQ.2) THEN
C
C- Open new file for write
C
          if (optz) then
            zzip_packlevel = 6
C&IF  VAXVMS
            call trnlnm ('ZZIP_PACKLEVEL', ctemp, l)
            if (ctemp .eq. 'ZZIP_PACKLEVEL') ctemp = ' '
C&ELSE
C&            ctemp = ' '
C&            call getenv ('ZZIP_PACKLEVEL', ctemp)
C&ENDIF
            if (ctemp .ne. ' ') then
              zzip_packlevel = valuex (ctemp, i, j, l)
              if (zzip_packlevel .lt. 1) zzip_packlevel = 1
              if (zzip_packlevel .gt. 9) zzip_packlevel = 9
              write (ctemp, '(''packlevel set to '', i1)')
     &             zzip_packlevel
              call errmsg ('ZZIP', 'D0OPEN', ctemp, 'S')
            endif
            xzrl = -1
            xchopt = 'P'
            ok = .true.
            if (zzip_open (lun, zzip_packlevel,
     &          xrecl, local_filename) .ge. 0) then
              d_status(d_index) = K_ZZIPOPEN
            else
              call zzip_get_last_err (ctemp)
              call errmsg ('zzip error', 'D0OPEN', ctemp, 'E')
              ok = .false.
            endif
            goto 999
          endif
C&IF VAXVMS, ULTRIX,ALFOSF
          IF(OPTX.OR.OPTG) THEN
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
     &           RECORDTYPE='FIXED',RECL=XZRL,
     &           FILE=LOCAL_FILENAME,CARRIAGECONTROL=CC,
     &           ERR=100,IOSTAT=IOSTAT)
            D_STATUS(D_INDEX) = K_OPEN
            IF(OPTX)THEN
              CTEMP = 'X'//XCHOPT
              XCHOPT = CTEMP
            ENDIF
            IF(OPTG)THEN
              CTEMP = 'XS'//XCHOPT
              XCHOPT = CTEMP
            ENDIF
          ELSE
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
     &        FILE=LOCAL_FILENAME,ERR=100,CARRIAGECONTROL=CC,
     &        IOSTAT=IOSTAT)
            D_STATUS(D_INDEX) = K_OPEN
          ENDIF
C&ENDIF
C&IF SIUNIX
C&          IF(OPTX.OR.OPTG) THEN
C&            CALL CFOPEN(IQUEST(1),0,XZRL,'w',1,LOCAL_FILENAME,IER)
C&            IF(OPTX)THEN
C&              IF(XZRL.GT.8190)THEN
C&                CTEMP = 'K'//XCHOPT
C&                XCHOPT = CTEMP
C&              ELSE
C&                CTEMP = 'L'//XCHOPT
C&                XCHOPT = CTEMP
C&              ENDIF
C&            ENDIF
C&            IF(OPTG)THEN
C&              CTEMP = 'LS'//XCHOPT
C&              XCHOPT = CTEMP
C&            ENDIF
C&            IF(IER.NE.0)THEN
C&              OK = .FALSE.
C&            ELSE
C&              D_STATUS(D_INDEX) = K_CFOPEN
C&              D_DES(D_INDEX) = IQUEST(1)
C&              D_MEDIUM(D_INDEX) = 0
C&            ENDIF
C&          ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &           FILE=LOCAL_FILENAME,ERR=100,CARRIAGECONTROL=CC,
C&     &           IOSTAT=IOSTAT)
C&            D_STATUS(D_INDEX) = K_OPEN
C&          ENDIF
C&ENDIF
C&IF IBMAIX
C&          IF(OPTX.OR.OPTG) THEN
C&            CALL CFOPEN(IQUEST(1),0,XZRL,'w',1,LOCAL_FILENAME,IER)
C&            IF(OPTX)THEN
C&              IF(XZRL.GT.8190)THEN
C&                CTEMP = 'K'//XCHOPT
C&                XCHOPT = CTEMP
C&              ELSE
C&                CTEMP = 'L'//XCHOPT
C&                XCHOPT = CTEMP
C&              ENDIF
C&            ENDIF
C&            IF(OPTG)THEN
C&              CTEMP = 'LS'//XCHOPT
C&              XCHOPT = CTEMP
C&            ENDIF
C&            IF(IER.NE.0)THEN
C&              OK = .FALSE.
C&            ELSE
C&              D_STATUS(D_INDEX) = K_CFOPEN
C&              D_DES(D_INDEX) = IQUEST(1)
C&              D_MEDIUM(D_INDEX) = 0
C&            ENDIF
C&          ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
C&            D_STATUS(D_INDEX) = K_OPEN
C&          ENDIF
C&ENDIF
C&IF VAXVMS,SIUNIX,IBMAIX,ULTRIX,ALFOSF
C- This is a sleazey way of making an ELSEIF for D0FLAVOR.
C&ELSE
C&          OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &      FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
C&          D_STATUS(D_INDEX) = K_OPEN
C&ENDIF
        ELSEIF(IOPT.EQ.3) THEN
C
C- Open existing file for append (not available on all UNIXes)
C
C&IF VAXVMS, SIUNIX, ULTRIX,ALFOSF
          OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,ACCESS='APPEND',
     &      FILE=LOCAL_FILENAME,ERR=100,CARRIAGECONTROL=CC,
     &      IOSTAT=IOSTAT)
          D_STATUS(D_INDEX) = K_OPEN
C&ENDIF
C&IF IBMAIX
C&          OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &      FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
C&          D_STATUS(D_INDEX) = K_OPEN
C&ENDIF
C&IF VAXVMS,SIUNIX,IBMAIX,ULTRIX,ALFOSF
C- This is a sleazey way of making an ELSEIF for D0FLAVOR.
C&ELSE
C&          CALL ERRMSG('D0OPEN','D0OPEN',
C&     &      'Append access not available for this flavor','F')
C&ENDIF
        ELSEIF(IOPT.EQ.4) THEN
C
C- Open scratch file
C
C&IF VAXVMS, SIUNIX, ULTRIX,ALFOSF
          OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,DISPOSE='DELETE',
     &      FILE=LOCAL_FILENAME,ERR=100,
     &      CARRIAGECONTROL=CC,IOSTAT=IOSTAT)
          D_STATUS(D_INDEX) = K_OPEN
C&ENDIF
C&IF IBMAIX
C&          OPEN(UNIT=LUN,FORM=FORM,STATUS='SCRATCH',ERR=100,
C&     &      IOSTAT=IOSTAT)
C&          D_STATUS(D_INDEX) = K_OPEN
C&ENDIF
C&IF VAXVMS,SIUNIX,IBMAIX,ULTRIX
C- This is a sleazey way of making an ELSEIF for D0FLAVOR.
C&ELSE
C&          OPEN(UNIT=LUN,FORM=FORM,STATUS='SCRATCH',ERR=100,
C&     &      IOSTAT=IOSTAT)
C&          D_STATUS(D_INDEX) = K_OPEN
C&ENDIF
        ELSEIF(IOPT.EQ.5) THEN
C
C- Open existing file for write
C
C&IF VAXVMS, ULTRIX,ALFOSF
          IF(OPTX.OR.OPTG) THEN
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
     &           RECORDTYPE='FIXED',RECL=XZRL,
     &           FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
            IF(OPTX)THEN
              CTEMP = 'X'//XCHOPT
              XCHOPT = CTEMP
            ENDIF
            IF(OPTG)THEN
              CTEMP = 'XS'//XCHOPT
              XCHOPT = CTEMP
            ENDIF
          ELSE
            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
     &           FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
          ENDIF
          D_STATUS(D_INDEX) = K_OPEN
C&ENDIF
C&IF SIUNIX
C&          IF(OPTX.OR.OPTG) THEN
C&            CALL CFOPEN(IQUEST(1),0,XZRL,'r+',1,LOCAL_FILENAME,IER)
C&            IF(OPTX)THEN
C&              IF(XZRL.GT.8190)THEN
C&                CTEMP = 'K'//XCHOPT
C&                XCHOPT = CTEMP
C&              ELSE
C&                CTEMP = 'L'//XCHOPT
C&                XCHOPT = CTEMP
C&              ENDIF
C&            ENDIF
C&            IF(OPTG)THEN
C&              CTEMP = 'LS'//XCHOPT
C&              XCHOPT = CTEMP
C&            ENDIF
C&            IF(IER.NE.0)THEN
C&              OK = .FALSE.
C&            ELSE
C&              D_STATUS(D_INDEX) = K_CFOPEN
C&              D_DES(D_INDEX) = IQUEST(1)
C&              D_MEDIUM(D_INDEX) = 0
C&            ENDIF
C&          ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &           FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
C&            D_STATUS(D_INDEX) = K_OPEN
C&          ENDIF
C&ENDIF
C&IF IBMAIX
C&          IF(OPTX.OR.OPTG) THEN
C&            CALL CFOPEN(IQUEST(1),0,XZRL,'r+',1,LOCAL_FILENAME,IER)
C&            IF(OPTX)THEN
C&              IF(XZRL.GT.8190)THEN
C&                CTEMP = 'K'//XCHOPT
C&                XCHOPT = CTEMP
C&              ELSE
C&                CTEMP = 'L'//XCHOPT
C&                XCHOPT = CTEMP
C&              ENDIF
C&            ENDIF
C&            IF(OPTG)THEN
C&              CTEMP = 'LS'//XCHOPT
C&              XCHOPT = CTEMP
C&            ENDIF
C&            IF(IER.NE.0)THEN
C&              OK = .FALSE.
C&            ELSE
C&              D_STATUS(D_INDEX) = K_CFOPEN
C&              D_DES(D_INDEX) = IQUEST(1)
C&              D_MEDIUM(D_INDEX) = 0
C&            ENDIF
C&          ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
C&            D_STATUS(D_INDEX) = K_OPEN
C&          ENDIF
C&ENDIF
C&IF VAXVMS,SIUNIX,IBMAIX,ULTRIX,ALFOSF
C- This is a sleazey way of making an ELSEIF for D0FLAVOR.
C&ELSE
C&            OPEN(UNIT=LUN,FORM=FORM,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
C&            D_STATUS(D_INDEX) = K_OPEN
C&ENDIF
        ENDIF
C
        IF(OK)GO TO 999
C
C           handle errors
C
  100   CONTINUE
        LNF = TRULEN(FILNAM)
        LNF = MIN(LNF,LCH-50)
C TO prevent overflow
        IF(IOSTAT.NE.0)THEN
C&IF VAXVMS
C
C ****  translate the errors, do retry
C
          IF ( IOSTAT.EQ.FOR$IOS_OPEFAI .AND. IOPT.EQ.1 ) THEN
            CALL ERRSNS(FNUM,RMS_STS,RMS_STV,IUNIT,CONDVAL)
            CALL LIB$SIGNAL (%VAL(RMS_STS),%VAL(RMS_STV))
            NFAIL=NFAIL+1
C
C ****  exclude 
C%RMS-E-DNF, directory not found
C-SYSTEM-W-NOSUCHFILE, no such file
C
            IF ( NFAIL.LE.NRETRY.AND.
     &        (RMS_STS.NE.RMS$_DNF.OR.
     &        RMS_STV.NE.SS$_NOSUCHFILE) ) THEN
              WRITE (CHLINE, 103) RETTM
  103         FORMAT(' OPEN shall retry in ',F10.0,' seconds ')
              CALL ERRMSG('D0OPEN','D0OPEN',
     &        CHLINE(1:LENOCC(CHLINE)),'I')
              OK = .TRUE.
              CALL LIB$WAIT(RETTM)
              GOTO 1001
            ENDIF
          ENDIF
C&ENDIF
          WRITE (CHLINE, 101)IOSTAT, FILNAM(1:LNF)
  101     FORMAT(' OPEN returned IOSTAT value ',I10,' for file ',A)
        ELSE
          WRITE (CHLINE, 102)IER, FILNAM(1:LNF)
  102     FORMAT(' CFOPEN returned IER value ',I10,' for file ',A)
        ENDIF
        CALL ERRMSG('D0OPEN','D0OPEN',
     &    CHLINE(1:LENOCC(CHLINE)),'I')
        OK=.FALSE.
        GO TO 999
  200   CONTINUE
        CALL ERRMSG('D0OPEN','D0OPEN',
     &    'D0OPEN called with illegal value for CHOPT' ,'F')
C  200   CALL ABORT('D0OPEN called with illegal value for CHOPT')
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
          IF( OPTO .OR. OPTA .OR. OPTD)THEN
            CALL ERRMSG('D0OPEN','D0OPEN',
     &        'D0OPEN called with illegal value for CHOPT for gen name',
     &        'F')
          ENDIF
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
C ****  stage dsts in the full tape mode only
C ****  disable full tape staging as of 12-MAY-1994
C
C        IF ( INDEX(CFMOPT,'H').EQ.0 .AND. 
C     &       INDEX(FILNAM,'DST').NE.0 ) THEN
C             CFMOPT=CFMOPT(1:LENOCC(CFMOPT))//'H'
C        ENDIF
C
C ****  if user requests staging of a full tape do it
C
        IF ( INDEX(FILNAM,'#').NE.0  ) THEN
C
C ****  disable full tape staging as of 12-MAY-1994
C
C           IF ( INDEX(CFMOPT,'H').EQ.0 ) THEN
C             CFMOPT=CFMOPT(1:LENOCC(CFMOPT))//'H'
C           ENDIF
C
C ****  strip # before calling fmopen
C
           FILNAM=FILNAM(1:INDEX(FILNAM,'#')-1)
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
          CALL ERRMSG('D0OPEN','D0OPEN',
     &      'D0OPEN called with illegal value for LUN','F')
        ENDIF
C
C ****  call the famoust FMOPEN
C
        CALL MZDROP(IFMSTR,LFMINF,'L')
        LFMBNK=0
        CALL FMOPEN(FILNAM(1:LENOCC(FILNAM)),CLUN(1:LENOCC(CLUN)),
     &    LFMBNK,CFMOPT,IER)
        IF (IER.NE.0)THEN
          OK=.FALSE.
        ELSE
          D_STATUS(D_INDEX) = K_FMOPEN
          D_CLUN(D_INDEX) = CLUN
        ENDIF
C
C ****  protecting the FATMEN bank link
C
        CALL ZSHUNT(IFMSTR,LFMBNK,LFMINF,1,0)
C
      ENDIF
      GO TO 999


      ENTRY XZRECL(XZRLEN,XZCHPT)
C-
C-  This entry point returns the record length in words of the last
C-  call to d0open.  If the last call did not specify 'X' or 'G' options
C-  then the returned record length is 0.
C-  It should be called immediately after D0OPEN.  
C-  The argument XZCHPT returns an appropriate character option string
C-  to be passed to FZFILE.
C-

      XZRLEN = XZRL
      XZCHPT = XCHOPT
      GO TO 999


      ENTRY D0CLOSE(LUN, CHOPT, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Close a file opened by D0OPEN (machine independent)
C-
C-   Inputs  : LUN   = unit number
C-             CHOPT = Character options:
C-                     'D' - Delete file (Only for Fortran CLOSE).
C-                     '/' - FATMEN options follow.
C-   Outputs : OK    = .TRUE. if operation succeeded
C-
C-   Created 30-Dec-1992    Herbert B. Greenlee
C-
C----------------------------------------------------------------------
      L = INDEX(CHOPT, '/') - 1
      IF(L.LT.0)L = LEN(CHOPT)
      OK=.TRUE.
C-
C-  Find index into unit data list.  If the number is not found, a warning
C-  is printed and no action is taken.
C-
      D_INDEX = 0
      DO I = 1,NUM_UNIT
        IF(D_LUN(I).EQ.LUN)THEN
          D_INDEX = I
          GO TO 20
        ENDIF
      ENDDO
 20   CONTINUE
      IF(D_INDEX.LT.1 .OR. D_INDEX.GT.MAX_UNIT)THEN
        CALL ERRMSG('D0CLOSE', 'D0CLOSE', 
     &    'UNIT number not opened by D0OPEN', 'W')
        OK = .FALSE.
        GO TO 999
      ENDIF
C-
C- Scan options.
C-
      OPTD = .FALSE.
      DO I=1,L
        CALL UPCASE(CHOPT(I:I),CH)
        IF(CH.EQ.'D')OPTD = .TRUE.
      ENDDO
C-
C- Close the file.
C-
      IF(D_STATUS(D_INDEX).EQ.K_NOTOPEN)THEN
        CALL ERRMSG('D0CLOSE', 'D0CLOSE', 'UNIT not open', 'W')
        OK = .FALSE.
C-
C- Fortran OPEN/CLOSE?
C-
      ELSEIF(D_STATUS(D_INDEX).EQ.K_OPEN)THEN
        IF(OPTD)THEN
          CLOSE(LUN, STATUS='DELETE', IOSTAT=IOSTAT)
        ELSE
          CLOSE(LUN, IOSTAT=IOSTAT)
        ENDIF
        IF(IOSTAT.NE.0)OK = .FALSE.
        IF(OK)D_STATUS(D_INDEX) = K_NOTOPEN
C-
C- XZOPEN?
C-
      ELSEIF(D_STATUS(D_INDEX).EQ.K_XZOPEN)THEN
        CALL D0CSCLOSE_HOOK(LUN, CHOPT, OK)
        IF(OK)D_STATUS(D_INDEX) = K_NOTOPEN
C-
C- FATMEN?
C-
      ELSEIF(D_STATUS(D_INDEX).EQ.K_FMOPEN)THEN
C
C ****  find out if there are special FATMEN options (look for "/")
C
        ISLASH=INDEX(CHOPT,'/')
        IF (ISLASH.EQ.0) THEN
          CFMOPT = ' '
        ELSE
          CALL UPCASE(CHOPT(ISLASH+1:),CFMOPT)
        ENDIF
        CALL FMCLOS(' ', D_CLUN(D_INDEX), 0, CFMOPT, IER)
        IF(IER.NE.0)OK=.FALSE.
        IF(OK)D_STATUS(D_INDEX) = K_NOTOPEN
C-
C- CFOPEN?
C-
C&IF VAXVMS
C&ELSE
C&      ELSEIF(D_STATUS(D_INDEX).EQ.K_CFOPEN)THEN
C&        CALL CFCLOS(D_DES(D_INDEX), D_MEDIUM(D_INDEX))
C&        D_STATUS(D_INDEX) = K_NOTOPEN
C&ENDIF
c
c *** zzip/zunzip?
c
      else if (d_status(d_index) .eq. K_ZZIPOPEN) then
        call zzip_close (lun)
        d_status (d_index) = K_NOTOPEN
      else if (d_status(d_index) .eq. K_ZUNZIPOPEN) then
        call zunzip_close (lun)
        d_status (d_index) = K_NOTOPEN
C-
C- Bad D_STATUS
C-
      ELSE
        CALL ERRMSG('D0CLOSE', 'D0CLOSE', 
     &    'D0CLOSE error -- bad status', 'W')
        OK = .FALSE.
      ENDIF
  999 RETURN
      END
