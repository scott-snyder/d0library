      SUBROUTINE PARSE_FILES(SPEC,IER,TAPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Uses VMS services to get all file names
C-                         matching an input file specification that
C-                         may contain wild cards, search strings, etc.
C-
C-   Inputs  : ENTRY PARSE  SPEC = input char variable with file spec
C-                          TAPE = true disables wild card search
C-   Outputs : ENTRY SEARCH FILE = output char variable with expanded
C-                                 file name or ' '
C-   Controls: IER               = 0 normal return from PARSE and SEARCH
C-                               = 1 error return from PARSE
C-                               = 2 no more file names return from SEARCH
C-   Notes   :  Entry PARSE must be called once with a character variable
C-              containing a valid VMS file specification possibly including
C-              wild cards and/or search strings.  Entry SEARCH is then
C-              called repeatedly until IER = 2, each call returning a
C-              complete file name string matching the original specification.
C-              The output string must be long enough to contain the longest
C-              possible file name string.
C-              The wild card search feature is disabled if TAPE is set
C-              to true as it does not work when reading files directly
C-              from tape.
C-
C-   Created  22-FEB-1990   Michael W. Peters
C-   Modified 23-JAN-1991   S. Protopopescu  (include disable flag)
C-   Updated  18-FEB-1992   Krzysztof L. Genser  
C-     STORE_NAME 255 CHAR
C-     LEN replaced by LENOCC
C-   Updated  17-MAR-1992   Serban D. Protopopescu   
C-            SPEC=FILE_NAME triggers use of ASCII file with list of files
C-                 ASCII file should be assigned to logical FILE_NAME
C-   Updated   3-APR-1992   Krzysztof L. Genser   
C-            corections to handle lower/upper case names correctly
C-   Updated  10-May-1992   Herbert Greenlee
C-               Modified to use lib$find_file instead of sys$parse/sys$search.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TAPE
      CHARACTER*(*) SPEC,FILE
      INTEGER IER
      CHARACTER*255 STORE_NAME, TMPFNAME 
      LOGICAL NEW,OFF
      INTEGER LENOCC
      INTEGER CONTEXT
      INTEGER LIB$FIND_FILE, LIB$FIND_FILE_END,OK
      LOGICAL FOUND
      SAVE OFF,STORE_NAME,NEW
      DATA CONTEXT/0/
C----------------------------------------------------------------------
C
      STORE_NAME=SPEC
      TMPFNAME = SPEC
      CALL CLTOU(TMPFNAME)
      NEW=.TRUE.
      IER=0
      OFF=TAPE
      IF(TMPFNAME(1:10).EQ.'FILE_NAMES') OFF=.TRUE.
      IF(CONTEXT.NE.0)THEN
        OK = LIB$FIND_FILE_END(CONTEXT)
        CONTEXT = 0
      ENDIF
      RETURN
C
      ENTRY SEARCH_FILES(FILE,IER)
      IF (OFF) THEN
        IF(NEW) THEN
          FILE = STORE_NAME
          TMPFNAME = STORE_NAME
          CALL CLTOU(TMPFNAME)
          IER=0
          NEW=.FALSE.
          IF(TMPFNAME(1:10).EQ.'FILE_NAMES') CALL FILE_NAME(FILE,IER)
        ELSE
          FILE=' '
          IER=2
          NEW=.TRUE.
          IF(TMPFNAME(1:10).EQ.'FILE_NAMES') CALL FILE_NAME(FILE,IER)
        ENDIF
      ELSE
        FOUND=LIB$FIND_FILE(STORE_NAME, FILE, CONTEXT)
        IF(FOUND) THEN
          IER=0
        ELSE
          FILE=' '
          IER=2
          OK = LIB$FIND_FILE_END(CONTEXT)
          CONTEXT = 0
        ENDIF
      ENDIF
      RETURN
      END
