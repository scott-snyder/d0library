      SUBROUTINE D0HLDIR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : List directories to screen.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: 
C-
C-   Created  22-APR-1992   Harrison B. Prosper
C-   Modified 18-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) D0HLDIR_FILE
      PARAMETER( D0HLDIR_FILE = 'SYS$LOGIN:D0HLDIR' )
      CHARACTER*80 REMARK,RECORD,CURDIR,IMESS
      LOGICAL OK, ACTIVE
      DATA IMESS/' Complete DIRECTORY STRUCTURE:'/
C----------------------------------------------------------------------
C
C ****  Get current directory
C
      CALL HCDIR(CURDIR,'R') 
      CALL INTMSG(' ')
      REMARK = ' The Current Directory is '//CURDIR
      CALL INTMSG(REMARK)
      CALL INTMSG(' ')
      CALL INTMSG(IMESS)
C ****  Direct I/O to a file
C
      CALL D0OPEN(6,D0HLDIR_FILE,'OF',OK)
      IF ( .NOT. OK ) GOTO 1000
C
C ****  List IDs
C
C change to TOP directory
      CALL HCDIR('//PAWC',' ')
      CALL HLDIR(CURDIR,'TN')
      CLOSE(UNIT=6)
C
C ****  Read in file
C
      CALL D0OPEN(80,D0HLDIR_FILE,'IF',OK)
      IF ( .NOT. OK ) GOTO 1000
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        READ(UNIT=80,FMT='(A)',END=900) RECORD
        CALL INTMSG(RECORD)
      ENDDO
  900 CONTINUE
      CLOSE(UNIT=80)
C change back to old directory
      CALL HCDIR(CURDIR,' ') 
C
C ****  Delete the file
C
      CALL D0OPEN(80,D0HLDIR_FILE,'M',OK)
      CLOSE(UNIT=80,STATUS='DELETE')
  999 RETURN
C
C ****  ERRORS
C
 1000 CONTINUE
      REMARK = ' Unable to open file '//D0HLDIR_FILE
      CALL INTMSG(REMARK)
      RETURN
      END
