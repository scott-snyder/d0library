      LOGICAL FUNCTION PBD_FILE_OK (FILE_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine inquires the status of the given file
C-                         and returns .TRUE. if the file exists and .FALSE. 
C-                         if not.
C-
C-   Inputs  : FILE_NAME - Input file name
C-   Outputs : Returns .TRUE. if the input file exists
C-                     .FALSE. if not
C-   Controls: 
C-
C-   Created  02-JUL-1991   Hyon Joo Kehayias
C-   Updated  17-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) FILE_NAME           ! INPUT FILE NAME
      LOGICAL RETFLAG                   ! FILE EXIST FLAG RETURNED
C&IF VAXVMS
C&ELSE
C&      INTEGER CONTEXT
C&      CHARACTER*80 LOCAL_FILE_NAME
C&ENDIF

C&IF VAXVMS
      INQUIRE ( FILE = FILE_NAME,EXIST=RETFLAG )
C&ELSE
C&      CONTEXT = 0
C&      CALL LIB$FIND_FILE(FILE_NAME, LOCAL_FILE_NAME, CONTEXT)
C&      CALL LIB$FIND_FILE_END(CONTEXT)
C&      INQUIRE ( FILE = LOCAL_FILE_NAME,EXIST=RETFLAG )
C&ENDIF
      PBD_FILE_OK = RETFLAG
      RETURN
      END

