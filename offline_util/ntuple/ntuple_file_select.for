      SUBROUTINE NTUPLE_FILE_SELECT (TOP_DIRECTORY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select the file mapped to the specified
C-   top directory. If TOP_DIRECTORY = ' ' then select the first file
C-   that was opened. If no file has been opened then simply return.
C-
C-   Inputs  : TOP_DIRECTORY  [C*]
C-   Outputs :
C-   Controls:
C-
C-   Created  26-JUN-1991   B.S.Acharya
C-   Updated   5-NOV-1991   Harrison B. Prosper
C-    Remove HRFILE; add alternate entry point to avoid argument
C-    change.
C-   Updated   2-DEC-1991   Harrison B. Prosper  
C-    Make compatible with DHDIR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOP_DIRECTORY
C----------------------------------------------------------------------
      LOGICAL FOUND,ALTERNATE
C
      INTEGER I,J,K,N
      INTEGER II,IDX,LTOP,STATUS
      INTEGER IOUT
C
      CHARACTER*32 TOPDIR,FIRST_TOP_DIRECTORY
      CHARACTER*80 REMARK
      CHARACTER*132 CURDIR,NEWDIR,CURRENT_DIR,PAWDIR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:NT_HOUSEKEEP.INC'
C----------------------------------------------------------------------
      SAVE CURRENT_DIR,FIRST_TOP_DIRECTORY,FIRST
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      ALTERNATE = .FALSE.
      GOTO 1
C
      ENTRY NTUPLE_FILE_SELECT1(TOP_DIRECTORY,IOUT)
      ALTERNATE = .TRUE.
    1 CONTINUE
C
      LTOP  = LEN(TOP_DIRECTORY)
      FOUND = .FALSE.
C
C ****  Note first top directory
C
      IF ( FIRST ) THEN
        IF ( NFILES .GT. 0 ) THEN
          FIRST = .FALSE.
          FIRST_TOP_DIRECTORY = TOPDIR_LIST(1)
        ELSE
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Pick default top directory
C
      IF ( TOP_DIRECTORY(1:1) .EQ. ' ') THEN
        TOPDIR = FIRST_TOP_DIRECTORY
      ELSE
        TOPDIR = TOP_DIRECTORY(1:LTOP)
      ENDIF
C
C--- Fish out the Directory name & Logical unit associated with the file
C
      CALL LOCSTR(TOPDIR,TOPDIR_LIST,NFILES,FOUND,II)
      IF ( .NOT. FOUND ) THEN
        REMARK = ' Could not find top directory '//TOPDIR
        CALL ERRMSG('NOT_FOUND','NTUPLE_FILE_SELECT',REMARK,'W')
        GOTO 999
      ENDIF
C
C ****  Get pathname in PAWC.
C
      CALL HCDIR(CURDIR,'R')          ! Get current directory
      CALL WORD(CURDIR,I,J,N)
      K = INDEX(CURDIR(3:J),'/')
      IF ( K .GT. 0 ) THEN
        K = K + 2
        NEWDIR = CURDIR(K:J)          ! Get path beneath current top directory
      ELSE
        NEWDIR = ' '
      ENDIF
      PAWDIR = '//PAWC'//NEWDIR       ! Get pathname in PAWC
C
C ****  Create or set the directory in the RZ file identified by the
C ****  top directory then replicate the directory in PAWC
C
      CALL DHDIR_DECLARE_FILE(TOPDIR)     ! Give name of RZ-file top directory
      CALL DHDIR (' ',PAWDIR,STATUS,' ')  ! Create/set directory in PAWC
      CALL DHDIR_DECLARE_FILE(' ')        ! Cancel declaration of file
C
C ****  Get pointer into UNIT buffer
C
      IDX = IDLIST(II)
C
  999 CONTINUE
      IF ( ALTERNATE ) THEN
        IF ( FOUND ) THEN
          IOUT = IDX
        ELSE
          IOUT = 0
        ENDIF
      ENDIF
      RETURN
C
C ****  Save Current Directory
C
      ENTRY NTUPLE_SAVE_DIRECTORY
      CALL HCDIR(CURRENT_DIR,'R')
      RETURN
C
C ****  Restore directory
C
      ENTRY NTUPLE_RESTORE_DIRECTORY
      CALL HCDIR(CURRENT_DIR,' ')
      RETURN
      END
