      SUBROUTINE PX_RESTORE_PARAMS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Restores the PXSCREEN or PXPARAM parameters
C-   to their original values
C-
C-   Inputs  : None
C-   Outputs :
C-   Controls:
C-
C-   Created  13-MAR-1991   LUPE HOWELL
C-   Updated   4-APR-1991   Lupe Howell  Restore for combined view array  
C-   Updated   7-JAN-1992   Lupe Howell  Modify call to PX_DISPLAY_ITEMS
C-   Updated   2-NOV-1992   Lupe Howell  Add DISPLAY_ITEMS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INTEGER MAXVAL
      PARAMETER( MAXVAL = 10000 )
C
      INTEGER NOPTION,COMMAND,IVAL(MAXVAL),LARR
      PARAMETER( NOPTION = 3 )
C
      CHARACTER*(*)  TEMP_RCP
      PARAMETER( TEMP_RCP='%PX_TEMP' )
C
      CHARACTER*(*) OLD_RCP
      PARAMETER( OLD_RCP = '%PX_OLD' )
C----------------------------------------------------------------------
      INTEGER IER,I,LENG,X
      INTEGER IDX,J,NSCREEN,TYP
C
      LOGICAL RESTORE,READ_RCP_FILE
C
      CHARACTER*64 RESTORE_KEY,STRING
      CHARACTER*40 ARRAY_NAME,FILE,COMBINE_ARRAY
      CHARACTER*32 RCPFILE
C----------------------------------------------------------------------
      CHARACTER*40 OPTIONS(NOPTION),REM_OPTIONS(NOPTION)
      DATA OPTIONS/'RESTORE PXPARAMS',
     &             'RESTORE PXSCREEN',
     &             'RESTORE COMBINE VIEWS'/
      DATA REM_OPTIONS/
     &  ' Restore Genereal Parameters to their original values',
     &  ' Restore Screen Parameters to their original values',
     &  ' Restore Combined View Parameters to their original values'/
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      SAVE RESTORE_KEY
C----------------------------------------------------------------------
      READ_RCP_FILE = .TRUE.
      COMMAND = 3
      DO WHILE ( COMMAND .NE. 0 )
        CALL DISPLAY_ITEMS
     &    (NOPTION,OPTIONS,REM_OPTIONS,'RESTORE',COMMAND)
        RESTORE = .FALSE.
        IF     ( COMMAND .EQ. 1) THEN
          ARRAY_NAME = 'PXPARAMS'
          RESTORE = .TRUE.
        ELSEIF ( COMMAND .EQ. 2 ) THEN
          ARRAY_NAME = 'PXSCREEN'
          RESTORE = .TRUE.
        ELSEIF ( COMMAND .EQ. 3 ) THEN
          CALL PXGET_COMBINED_VIEWS(COMBINE_ARRAY,IER)
          IF ( IER .NE. 0 ) 
     &      GOTO 999
          ARRAY_NAME = COMBINE_ARRAY
          RESTORE = .TRUE.
        ENDIF
C
C ****  Getting the original values of the array to restore
C
        IF ( RESTORE ) THEN
C
C ****  Renaming the active package to a temporary name
C
          IF ( READ_RCP_FILE ) THEN
            READ_RCP_FILE = .FALSE.
C
C ****  Setting up the key for reading the bank
C
            CALL EZTELL(RCPFILE,LENG)
            LARR = LEN(ARRAY_NAME)
            RESTORE_KEY = RCPFILE//ARRAY_NAME(1:LARR)
C
C ****  Rename current rcp file to temporary name
C
            CALL EZRNAM(RCPFILE(1:LENG),TEMP_RCP)
            CALL EZTELL(FILE,X)
C
C ****  Read original RCP file and rename it PXOLD_RCP
C
            CALL STAMSG(' Reading file '//RCPFILE(1:LENG),.FALSE.)
            CALL INRCP (RCPFILE,IER)! Read parameter file into an SRCP bank
            CALL EZRNAM(RCPFILE(1:LENG),OLD_RCP)
C
C ****  Now restore name of current RCP file
C
            CALL EZRNAM(TEMP_RCP,RCPFILE)
          ENDIF
C
C ****  Getting the old bank and restore the array requested
C
          CALL EZTELL(FILE,X)
          CALL EZPICK(OLD_RCP)
          CALL EZTELL(FILE,X)
          CALL EZGETA(ARRAY_NAME,1,0,1,IVAL,IER)
          CALL EZRSET
          CALL EZTELL(FILE,X)
C
C ****  Restoring the original parameter values
C
          CALL EZSETA(ARRAY_NAME,1,0,1,IVAL,IER)
        ENDIF
      ENDDO
C
C ****  Drop OLD rcp bank
C
      CALL EZDROP(OLD_RCP)
  999 RETURN
C
C ****  ENTRY - PX_RESTORE_KEY
C
      ENTRY PX_RESTORE_KEY(STRING)
      STRING = RESTORE_KEY
      RESTORE_KEY = ' '
      RETURN
      END
