      SUBROUTINE CREATE_TSP_FILE (LUN,TSP_FILE,DATETIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a timestamp file with the specified
C-                         date and time.
C-
C-   Inputs  : LUN              Logical unit number for internal use.
C-             TSP_FILE         Name of Time-stamp file.
C-             DATETIME [C*23]  Date and time.
C-                              format: dd-mmm-yyyy hh:mm:ss.tt
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-JUL-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUN
      CHARACTER*(*) TSP_FILE
      CHARACTER*23  DATETIME

      INTEGER STATEMENT
      INTEGER STATUS
      INTEGER FDL$CREATE

      CHARACTER*80  FDL_FILE

      INTEGER      NFDL
      PARAMETER(   NFDL   = 19  )

      INTEGER      CREATION
      PARAMETER(   CREATION = 13 )

      INTEGER      REVISION
      PARAMETER(   REVISION = CREATION + 1 )

      CHARACTER*80 FDL(NFDL)
      DATA FDL /
     &'SYSTEM',
     &'  SOURCE                 VAX/VMS',
     &'FILE',
     &'  ALLOCATION             0',
     &'  BEST_TRY_CONTIGUOUS    no',
     &'  CLUSTER_SIZE           3',
     &'  CONTIGUOUS             no',
     &'  EXTENSION              0',
     &'  GLOBAL_BUFFER_COUNT    0',
     &'  ORGANIZATION           sequential',
     &'  PROTECTION     (system:RWE, owner:RWED, group:RE, world:RE)',
     &'DATE',
     &'  CREATION       "',
     &'  REVISION       "',
     &'RECORD',
     &'  BLOCK_SPAN             yes',
     &'  CARRIAGE_CONTROL       carriage_return',
     &'  FORMAT                 variable',
     &'  SIZE                   0'
     &/

      INTEGER I,J,L
C----------------------------------------------------------------------
C
C ****  Create FDL file 
C
      I = INDEX (FDL(CREATION),'"')
      FDL(CREATION) = FDL(CREATION)(1:I)//DATETIME//'"'
      FDL(REVISION) = FDL(REVISION)(1:I)//DATETIME//'"'

      L = LEN(TSP_FILE)
      I = INDEX(TSP_FILE(1:L),'.')
      IF ( I .GT. 0 ) THEN
        FDL_FILE = TSP_FILE(1:L)//'FDL'
      ELSE
        FDL_FILE = TSP_FILE(1:L)//'.FDL'
      ENDIF

      OPEN (UNIT=LUN,FILE=FDL_FILE,STATUS='NEW')
      WRITE(UNIT=LUN,FMT='(A)') FDL
      CLOSE(UNIT=LUN)
C
C ****  Create USERLIB.TSP using characteristics in FDL file
C
      STATUS = FDL$CREATE (FDL_FILE,
     &                     TSP_FILE,
     &                     ,,,,
     &                     STATEMENT,
     &                     ,,)
      IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C
C ****  Delete FDL file
C
      OPEN (UNIT=LUN,FILE=FDL_FILE,STATUS='OLD')
      CLOSE(UNIT=LUN,DISP='DELETE')

  999 RETURN
      END
