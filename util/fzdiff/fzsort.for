      PROGRAM FZSORT
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Sort an FZ exchange mode D0 data file.
C-                        Use with the program fzscramble.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Usage:
C-
C-   fzsort $in_file
C-
C-   Fzsort writes to standard output a list of five numbers for each 
C-   event in the input file:
C-
C-   <run#> <event#> <random#> <record#> <offset>
C-
C-   Output may be written to a file or piped directly to fzscramble:
C-
C-   % fzsort $in_file | sort | fzscramble $in_file $out_file -
C-
C-   Created  20-Mar-1995   Adam L. Lyon
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$UTIL$FZDIFF:FZDIFF_ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'

      CHARACTER*256, in_file
      CHARACTER*4, chopt
      INTEGER in_unit, ilen, ios, ixwipe
      INTEGER run, event, runno, evonum
      EXTERNAL runno, evonum
      LOGICAL ok
      REAL r, rndm
C-
C- For command line scanning
C-
      CHARACTER*1024 line         ! Command line input buffer
      INTEGER*2 llen              ! Number of characters in command line
      CHARACTER*40 prompt         ! User prompt
      INTEGER force               ! Force prompt flag
      INTEGER i, j, n
      INTEGER trulen
      LOGICAL lib$get_foreign
      
C-----------------------------------------------------------------------
      
C*****Initialize zebra
      CALL MZEBRA(-3)            ! Init zebra
      CALL FZDIFF_INZCOM(2)      ! Setup /ZEBCOM/
      CALL INZSTP                ! Setup /ZEBSTP/
      
C*****Get input file name.
 10   CONTINUE
      FORCE = 0
      PROMPT = 'Enter input file: '
      OK = LIB$GET_FOREIGN(LINE, PROMPT(1:TRULEN(PROMPT)+1),
     &     LLEN, FORCE)
      IF(.NOT.OK)GO TO 10
      CALL WORD(LINE(1:LLEN), I, J, N)
      IF(N.EQ.0)GO TO 10
      IN_FILE = LINE(I:J)

C*****Open input file.
      IN_UNIT = 20
      CALL D0OPEN(in_unit, in_file, 'IX', ok)
      IF ( .NOT. ok ) THEN
        PRINT*, 'Cannot open in file'
        STOP
      ENDIF

C*****Make sure that input file is exchange mode and call FZFILE.
      CALL XZRECL(ILEN, CHOPT)
      IF(INDEX(CHOPT,'X').EQ.0 .AND. INDEX(CHOPT,'K').EQ.0 .AND.
     &   INDEX(CHOPT,'L').EQ.0)THEN
        PRINT*, 'File is not exchange mode'
        STOP
      ENDIF
      CALL FZFILE(in_unit, ilen, chopt)

C*****  Read until run/event is found
 410  CONTINUE

C*****Clear out previous event
      IXWIPE = IXMAIN
      CALL MZWIPE(IXWIPE)
      
C*****Get the event
      CALL FZIN(in_unit,ixmain,lhead,1,' ',0,0)
      
C*****Get the data type
      IOS = IQUEST(1)
      IF ( IOS .EQ. 0 ) THEN
        IOS=MOD(IQ(LHEAD+1),1000)
        IF(IOS.GT.4) IOS=0
      ENDIF
      
C*****Check the IOS result
      IF ( ios .LT. 0 ) THEN    ! Read error
        PRINT*, 'Read error on in file'
        STOP
      ENDIF
      
      IF ( ios .EQ. 0 ) THEN    ! Got an event
        run = RUNNO()
        event = EVONUM()

        R = RNDM()

        PRINT 200, run, event, r, iquest(5), iquest(6)
 200    FORMAT (1X,I6,1X,I6,4X,Z8,1X,I6,1X,I6)
        
      ENDIF

      IF ( ios .GE. 3 ) THEN ! end of file
        GOTO 450
      ENDIF
      
C*****Get the next event
      GOTO 410

 450  CONTINUE
      CALL FZENDI(in_unit, 'T')
      CALL D0CLOSE(in_unit, ' ', ok)
      STOP
      END
