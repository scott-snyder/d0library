      SUBROUTINE D0C_STANDARDS(LISTING,FILTERED_LISTING,OK,FOUND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Check if given code satisfies D0 standards by analyzing the
C-   compiler listing file.
C-
C-   Inputs  : LISTING         Name of .LIS file resulting
C-                                      from /LIST compiler switch.
C-
C-   Outputs : FILTERED_LISTING         Name of filtered listing.
C-             OK                       TRUE if code satisfies stand-
C-                                      dards, FALSE otherwise.
C-   Controls: None
C-
C-   Created  12-JUN-1989   Harrison B. Prosper
C-   Updated  26-FEB-1992   Harrison B. Prosper  
C-    Extend standards 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*) LISTING
      CHARACTER*(*) FILTERED_LISTING
      LOGICAL OK
      LOGICAL FOUND

      LOGICAL OK1,OK2,OK3,OK4

      INTEGER LUNINP
      PARAMETER( LUNINP = 20 )

      INTEGER LUNOUT
      PARAMETER( LUNOUT = 30 )

      INTEGER LUNSUM
      PARAMETER( LUNSUM = 40 )

      INTEGER MAXNUM
      PARAMETER( MAXNUM = 511 )

      INTEGER NUMBER,NVNAME,IMAP(MAXNUM)
      CHARACTER*23 DATETIME
      CHARACTER*32 NAME (MAXNUM),VNAME(MAXNUM)
      CHARACTER*132 RECORD,SUMMARY_FILE
      INTEGER I,II,JJ,KK,NN,LSUM
      LOGICAL SUMMARY
C----------------------------------------------------------------------
C
C ****  Open compiler listing file
C
      FOUND = .TRUE.
      OPEN (UNIT=LUNINP,FILE=LISTING,STATUS='OLD',ERR=950)
C
C ****  Open file for summary if /SUMMARY[=file-spec] switch is
C ****  defined.
C
      CALL DECODE_QUALIFIERS('SUMMARY',1,SUMMARY)
C
      IF ( SUMMARY ) THEN
C
C ****  Get name of summary file
C
        CALL GET_VALUES('SUMMARY',SUMMARY_FILE,LSUM,NN,1)
C
        I = INDEX(FILTERED_LISTING,'.')
        IF ( NN .LE. 0 ) THEN
          SUMMARY_FILE = FILTERED_LISTING(1:I)//'SUM'
          LSUM = I + 3
        ENDIF
C
        OPEN(UNIT=LUNSUM,FILE=SUMMARY_FILE(1:LSUM),STATUS='NEW',
     &        CARRIAGECONTROL='LIST')
C
        CALL LIB$DATE_TIME(DATETIME)
        WRITE(UNIT=LUNSUM,FMT='(''Compiled: '',A23,''    Program '',A)')
     &    DATETIME,FILTERED_LISTING(1:I-1)
        WRITE(UNIT=LUNSUM,FMT='('' '')')
      ENDIF
C
C ****  Open file for filtered compiler listing
C
      OPEN (UNIT=LUNOUT,FILE=FILTERED_LISTING,STATUS='NEW',
     &        CARRIAGECONTROL='LIST')
C
C
C ****  Step 0 : Search for %FORT-I-EXT-STMT,
C ****                      %FORT-I-EXT_COM,
C ****                      %FORT-I-EXT-NAME
C ****           and filter out references to these statements if and
C ****           only if the next line contains:
C ****                               'DO'
C ****                               'END'
C ****                               'IMPLICIT NONE'
C ****           or an identifier.
C
      CALL D0C_APPLY_FILTER
     &  (LUNINP,LUNOUT,LISTING,FILTERED_LISTING,OK)
C
C ****  Step 1 : Get COMMON BLOCKS
C
      CALL D0C_GET_NAMES        (LUNINP,'PROGRAM SECTIONS',NAME,NUMBER)
      CALL D0C_VALIDATE_NAMES   (LUNOUT,'COMMON BLOCK',NAME,NUMBER,OK1)
C
      IF ( SUMMARY ) THEN
        WRITE(UNIT=LUNSUM,FMT='(''Common Blocks'')')
        DO I =  1, NUMBER
          CALL WORD(NAME(I),II,JJ,KK)
          WRITE(UNIT=LUNSUM,FMT='(5X,A)') NAME(I)(II:JJ)
        ENDDO
        WRITE(UNIT=LUNSUM,FMT='('' '')')
      ENDIF
C
C ****  Step 2 : Get ENTRY points
C
      NN = NUMBER
      CALL D0C_GET_NAMES  (LUNINP,'ENTRY POINTS',NAME(NN+1),NUMBER)
      CALL D0C_VALIDATE_NAMES
     &  (LUNOUT,'ROUTINE',NAME(NN+1),NUMBER,OK2)
C
      IF ( SUMMARY ) THEN
        WRITE(UNIT=LUNSUM,FMT='(''Entry Points'')')
        DO I =  1, NUMBER
          CALL WORD(NAME(NN+I),II,JJ,KK)
          WRITE(UNIT=LUNSUM,FMT='(5X,A)') NAME(NN+I)(II:JJ)
        ENDDO
        WRITE(UNIT=LUNSUM,FMT='('' '')')
      ENDIF
C
      NUMBER = NUMBER + NN
C
C ****  Step 3 : Get VARIABLES and ARRAYS
C
      CALL D0C_GET_NAMES        (LUNINP,'VARIABLES',VNAME,NVNAME)
      CALL D0C_GET_NAMES        (LUNINP,'ARRAYS',VNAME(NVNAME+1),NN)
      NVNAME = NVNAME + NN
      CALL D0C_VALIDATE_NAMES   (LUNOUT,'VARIABLE',VNAME,NVNAME,OK3)
C
C ****  Step 4 : Get FUNCTIONS AND SUBROUTINES:
C
      NN = NUMBER
      CALL D0C_GET_NAMES        (LUNINP,'FUNCTIONS',NAME(NN+1),NUMBER)
      CALL D0C_VALIDATE_NAMES   
     &  (LUNOUT,'ROUTINE' ,NAME(NN+1),NUMBER,OK4)
C
      IF ( SUMMARY ) THEN
        WRITE(UNIT=LUNSUM,FMT='(''Functions and Subroutines'')')
        DO I =  1, NUMBER
          CALL WORD(NAME(NN+I),II,JJ,KK)
          WRITE(UNIT=LUNSUM,FMT='(5X,A)') NAME(NN+I)(II:JJ)
        ENDDO
        WRITE(UNIT=LUNSUM,FMT='('' '')')
      ENDIF
C
      NUMBER = NUMBER + NN
C
C ****  Step 5 :  Check for duplicate names between common blocks 
C ****            and entry points
C
      CALL SRTCHR(NAME,NUMBER,IMAP)
      DO I =  2, NUMBER
        IF ( NAME(I) .EQ. NAME(I-1) ) THEN
          RECORD = 
     & '%D0CHECK-W-SAMENAME, Common block name same as Entry point name'
          CALL SWORDS(RECORD,II,JJ,KK)
          WRITE(UNIT=LUNOUT,FMT='(A)') RECORD(1:JJ)
C
          CALL WORD(NAME(I),II,JJ,KK)
          RECORD = 
     &    '           ['//NAME(I)(II:JJ)//']'
          CALL SWORDS(RECORD,II,JJ,KK)
          WRITE(UNIT=LUNOUT,FMT='(A)') RECORD(1:JJ)
          WRITE(UNIT=LUNOUT,FMT='(A)') ' '
        ENDIF
      ENDDO
C
C ****  Step 6 : Close/Delete files
C
      CLOSE (UNIT=LUNINP,DISP='DELETE')

      OK = OK .AND. OK1 .AND. OK2 .AND. OK3 .AND. OK4

      IF ( OK ) THEN
        CLOSE (UNIT=LUNOUT,DISP='DELETE')
      ELSE
        CLOSE (UNIT=LUNOUT)
      ENDIF
C
      IF ( SUMMARY ) THEN
        CLOSE(UNIT=LUNSUM)
      ENDIF
      RETURN

  950 CONTINUE
      FOUND = .FALSE.

  999 RETURN
      END
