      SUBROUTINE D0OPEN_TEXT(LUN,FILNAM,CHOPT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open a file (machine independent).  This subroutine
C-                         is a streamlined version of d0open that only
C-                         works for text files.
C-
C-   Inputs  :
C-   LUN   = unit number
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C-   FILNAM= file name
C-   CHOPT = character option
C-           'I' Input  (VAX status OLD, READONLY;  UNIX status OLD)
C-           'O' Output (VAX status NEW;      UNIX status UNKNOWN)
C-           'L' List (open with CARRIAGECONTROL='LIST' on VAX)
C-           'N' No translate (suppress normal filename conversions on UNIX.
C-                             No effect on VAX).
C-   Outputs :
C-   OK    = set to false if there is a problem opening file
C-           true otherwise
C-
C-   Created  26-Jun-1992  Herbert B. Greenlee
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER LUN
      CHARACTER*(*) FILNAM, CHOPT
      LOGICAL OK
      CHARACTER*80 LOCAL_FILENAME
      CHARACTER*8 STATUS, CC
      CHARACTER*1 CH
      LOGICAL EXACT
      LOGICAL OPTI, OPTO, OPTL, OPTN
      INTEGER I, L, IOPT, IOSTAT
C&IF VAXVMS
C&ELSE
C&      INTEGER CONTEXT
C&ENDIF
C
C----------------------------------------------------------------------
C
      L=LEN(CHOPT)
      OK=.TRUE.
C
C- Default options and keywords
C
      STATUS = 'OLD'
      CC='FORTRAN'
      EXACT = .FALSE.
C
C       find options
C
      OPTI = .FALSE.
      OPTO = .FALSE.
      OPTL = .FALSE.
      OPTN = .FALSE.
      DO 1 I=1,L
         CH = CHOPT(I:I)
         CALL CLTOU(CH)
         IF(CH.EQ.'I')OPTI = .TRUE.
         IF(CH.EQ.'O')OPTO = .TRUE.
         IF(CH.EQ.'L')OPTL = .TRUE.
         IF(CH.EQ.'N')OPTN = .TRUE.
    1 CONTINUE
C
C ****  normal name proceede as befeore
C
      IF(L.EQ.0.OR.CHOPT.EQ.' ') THEN
         IOPT=1
      ELSE
         IOPT=0
C
C- Look for 'I' option (open for input)
C
         IF(OPTI)THEN
            IOPT=1
         ENDIF
C
C- Lookk for 'O' option (open for output)
C
         IF(OPTO)THEN
            IOPT=2
            STATUS = 'NEW'
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
      ENDIF
C
C- Here we convert to the local filename.  This section does nothing in VMS
C- or if the 'N' option has been specified in UNIX.  Conversion consists of
C- calling LIB$FIND_FILE or simply converting to lower case.
C
      LOCAL_FILENAME = FILNAM
C&IF VAXVMS
C&ELSE
C&      IF(.NOT.EXACT)THEN
C&        IF(STATUS .EQ. 'OLD')THEN
C&          CONTEXT = 0
C&          CALL LIB$FIND_FILE(FILNAM, LOCAL_FILENAME, CONTEXT)
C&          CALL LIB$FIND_FILE_END(CONTEXT)
C&          IF(LOCAL_FILENAME.EQ.' ')THEN
C&            OK=.FALSE.
C&            GO TO 999
C&          ENDIF
C&        ELSE
C&          CALL CUTOL(LOCAL_FILENAME)
C&        ENDIF
C&      ENDIF
C&ENDIF
C
C- Here we unconditionally convert STATUS = 'NEW' to STATUS = 'UNKNOWN' on
C- non-VMS machines.
C
C&IF VAXVMS
C&ELSE
C&      IF(STATUS .EQ. 'NEW')STATUS = 'UNKNOWN'
C&ENDIF
C
C- Now open file.
C
      IF(IOPT.LT.2) THEN
C
C- Open existing file for read
C
C&IF VAXVMS, SIUNIX, ULTRIX, ALFOSF
         OPEN(UNIT=LUN,STATUS=STATUS,READONLY,
     &        FILE=LOCAL_FILENAME,ERR=100,
     &        IOSTAT=IOSTAT)
C&ELSE
C&         OPEN(UNIT=LUN,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ERR=100,
C&     &        IOSTAT=IOSTAT)
C&ENDIF
      ELSEIF(IOPT.EQ.2) THEN
C
C- Open new file for write
C
C&IF VAXVMS, ULTRIX, SIUNIX, ALFOSF
       OPEN(UNIT=LUN,STATUS=STATUS,
     &        FILE=LOCAL_FILENAME,ERR=100,CARRIAGECONTROL=CC,
     &        IOSTAT=IOSTAT)
C&ELSE
C&         OPEN(UNIT=LUN,STATUS=STATUS,
C&     &        FILE=LOCAL_FILENAME,ERR=100,IOSTAT=IOSTAT)
C&ENDIF
      ELSE
         OK = .FALSE.
      ENDIF
      GO TO 999
 100  CONTINUE
      OK=.FALSE.
      GO TO 999
 999  RETURN
      END
