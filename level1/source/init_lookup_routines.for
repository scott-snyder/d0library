      SUBROUTINE   INIT_LOOKUP_ROUTINES ( IO_UNIT, FILE_NAME, STATUS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the common block for subsequent direct
C-                         access or use of library routines.
C-
C-                         This subroutine opens the Lookup Management File,
C-                         initializes the common block from the file, and
C-                         then closes the file.
C-
C-   Inputs  :
C-      IO_UNIT            Is the logical unit number to use for input file.
C-
C-      FILE_NAME          Is the name of the Lookup System Management file 
C-                         to use as input in filling the common block.
C-
C-   Outputs :
C-      STATUS             Is the initialization status.
C-                         The normally returned success status is 1.
C-                         In case of file open or read failure, the FORTRAN
C-                         IO error code is returned (positive number).
C-                         Negative numbers are reserved for file syntax
C-                         interpretation error codes.
C-
C-   filled common block : LEVEL1_LOOKUP.
C-
C-   Controls: None
C-
C-   Comments :
C-                         A call to INIT_LOOKUP_ROUTINES must be made
C-                         before any routine can be called, or any varible
C-                         accessed.
C-
C-                         The file name is passed as argument to allow
C-                         selection of a CURRENT hardware description file or
C-                         any other EXPLORATORY configuration, or retrieval
C-                         from ARCHIVES.
C-
C-                         The routine will fill with zeros all variables
C-                         referencing trigger towers whose description does
C-                         not appear in the lookup tables. The existence test
C-                         is made on the downloaded DAC byte that by
C-                         definition is non-zero only for existing Trigger
C-                         Towers.
C-
C-   NOTE: The ZEBRA system must have been previously initialized with a call
C-     to MZEBRA. For example:
C-   
C-     CALL MZEBRA(0)
C-   
C-   
C-
C-   Defined 21-FEB-1990 MICHIGAN STATE UNIVERSTITY, TRIGGER CONTROL SOFTWARE
C-   Created   6-JUN-1990   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  10-AUG-1990   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C       Argument declarations
C
      INTEGER IO_UNIT
      CHARACTER*(*) FILE_NAME
      INTEGER STATUS
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
C      IF (FIRST .EQV. .TRUE.) THEN
C        CALL MZEBRA(0)
C        FIRST = .FALSE.
C      ENDIF
C
      CALL INZWRK()
C
      CALL INIT_LSO_BANK_NAMES()
C
      CALL INIT_COMMON_BLOCK_MINIMAL()
      CALL READ_LSO_MINIMAL(IO_UNIT, FILE_NAME, STATUS)
      IF (STATUS .EQ. 0) STATUS = 1
  999 CONTINUE
      RETURN
      END
