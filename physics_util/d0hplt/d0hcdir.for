      SUBROUTINE D0HCDIR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Change HBOOK directory. Uses LISBOX.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  21-JUN-1992   Harrison B. Prosper
C-   Updated  26-JUN-1992   Harrison B. Prosper   
C-   Modified 18-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ROW,COL,MAXDIR
      PARAMETER( ROW = 5)
      PARAMETER( COL = 2)
      PARAMETER( MAXDIR = 500 )
      CHARACTER*(*) D0HINDEX_FILE
      PARAMETER( D0HINDEX_FILE = 'SYS$LOGIN:D0HCDIR' )
C
      LOGICAL OK, ACTIVE
      INTEGER I, J, K, ID, N, II, NDIR
C
      CHARACTER*76 RECORD,REMARK
      CHARACTER*80 NEWDIR
C----------------------------------------------------------------------
C
C ****  Show current directory
C
      CALL HCDIR(NEWDIR,'R')
      REMARK = ' Current Directory: '//NEWDIR
      CALL STAMSG(REMARK,.FALSE.)
C
C ****  Get list of directories
C
C ****  Direct I/O to a file
C
      CALL D0OPEN(6,D0HINDEX_FILE,'OF',OK)
      IF ( .NOT. OK ) GOTO 1000
      CALL HLDIR('//PAWC','TN')
      CLOSE(UNIT=6)
C
C ****  Read in file
C
      CALL D0OPEN(80,D0HINDEX_FILE,'IF',OK)
      IF ( .NOT. OK ) GOTO 1000
C
C ****  Fill LISBOX
C
      CALL PFLABL(' ',' ',' ','BACK')
      CALL LISFIL(0,' ')
C
      NDIR = 0
      ACTIVE  = .TRUE.
      DO WHILE ( ACTIVE )
        READ(UNIT=80,FMT='(A)',END=900) RECORD
        CALL WORD(RECORD,I,J,K)
C
C ****  Ignore blank lines
C
        IF ( K .GT. 0 ) THEN
C
C ****  Get directory name
C
          I = INDEX(RECORD,'//')
          IF ( I .GT. 0 ) THEN
C
C ****  Directory found
C
            IF ( NDIR .LT. MAXDIR ) THEN
              NDIR = NDIR + 1
              RECORD = RECORD(I:)
              CALL LISFIL(1,RECORD)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
  900 CONTINUE
      CLOSE(UNIT=80)
C
C ****  Delete file
C
      CALL D0OPEN(80,D0HINDEX_FILE,'M',OK)
      CLOSE(UNIT=80,STATUS='DELETE')
C
C ****  Select directory
C
      CALL LISPOS(ROW,COL)
      CALL LISBOX('DIRECTORIES; Double Click PF1 to SELECT',' ',N,ID,1)
C
C ****  Set directory
C
      IF ( N .GT. 0 ) THEN
        CALL LISGET(RECORD)
        CALL WORD(RECORD,I,J,K)
        CALL HCDIR(RECORD(I:J),' ')
C Save directory name
        CALL D0HSDN(RECORD(I:J))
      ELSE
        CALL INTMSG(' No NEW directory was selected')
      ENDIF
C
C ****  Show current directory
C
      CALL HCDIR(NEWDIR,'R')
      REMARK = ' Current Directory: '//NEWDIR
      CALL STAMSG(REMARK,.FALSE.)
C
  999 RETURN
C
C ****  ERRORS
C
 1000 CONTINUE
      REMARK = ' Unable to access file '//D0HINDEX_FILE
      CALL INTMSG(REMARK)
      RETURN
      END
