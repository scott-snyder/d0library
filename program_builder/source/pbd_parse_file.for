      SUBROUTINE PBD_PARSE_FILE ( FILE_SPEC, NODE_NAME, NODE_LEN,
     &  DEV_NAME, DEV_LEN,DIR_NAME,DIR_LEN,FILE_NAME,
     &  FILE_LEN,FILE_TYPE,TYPE_LEN, RET_FLAG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Parse the input file specification using VMS PARSE 
C-                         system service and returns node name, device name,
C-                         directory specification, file name and file type
C-                         with their corresponding string length.
C-
C-   Inputs  :  FILE_SPEC - Input file specification 
C-   Outputs :          
C-              NODE_NAME - Node name
C-              DEV_NAME  - Device name
C-              DIR_NAME  - Directory specification
C-              FILE_NAME - File name
C-              FILE_TYPE - File type
C-              NODE_LEN  - Node name length
C-              DEV_LEN   - Device name length
C-              DIR_LEN   - Direcotry spec string length
C-              FILE_LEN  - File name length
C-              TYPE_LEN  - File type string length
C-
C-   Controls:  RET_FLAG  - Error return status flag
C-              ( .FALSE. if no error, .TRUE. if an error )
C-
C-   Modules called by this routine:  SYS$PARSE
C-  
C-   Based on D0$OFFLINE_UTIL$ROOT:[GENERAL]PARSE_FILES.FOR
C-
C-   Created   4-SEP-1991   Hyon Joo Kehayias
C-   Updated  17-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      CHARACTER*(*) FILE_SPEC           ! INPUT FILE SPECIFICATION
      CHARACTER*(*) NODE_NAME           ! NODE NAME
      CHARACTER*(*) DEV_NAME            ! DEVICE NAME
      CHARACTER*(*) DIR_NAME            ! DIRECTORY SPECIFICATION
      CHARACTER*(*) FILE_NAME           ! FILE NAME
      CHARACTER*(*) FILE_TYPE           ! FILE TYPE
      INTEGER*2     NODE_LEN            ! NODE NAME LENGTH
      INTEGER*2     DEV_LEN             ! DEVICE NAME LENGTH
      INTEGER*2     DIR_LEN             ! DIRECOTRY SPEC STRING LENGTH
      INTEGER*2     FILE_LEN            ! FILE NAME LENGTH
      INTEGER*2     TYPE_LEN            ! FILE TYPE STRING LENGTH
      INTEGER*2     STARTPOS            ! STRING START POSITON
      INTEGER*2     LASTPOS             ! STRING ENDING POSITON
      LOGICAL       RET_FLAG            ! RETURN STATUS FLAG

C&IF VAXVMS
      INCLUDE '($SYSSRVNAM)'            ! SYSTEM SERVICE NAME DEFINITON
      INCLUDE '($RMSDEF)'               ! RMS DEFINITION
      INCLUDE '($FABDEF)'               ! FILE ACCESS BLOCK DEFINITON
      INCLUDE '($NAMDEF)'               ! FILE NAME BLOCK DEFINITION
      RECORD/FABDEF/FAB_BLK
      RECORD/NAMDEF/NAM_BLK
      INTEGER*4 STATUS                  ! PARSE RETURN STATUS

      CHARACTER*(NAM$C_MAXRSS) EXP_STR  ! EXPANDED STRING RETURNED BY PARSE
C&ELSE
C&      INTEGER N, TRULEN
C&      CHARACTER*80 CTEMP
C&ENDIF

C----------------------------------------------------------------------
C
C     Initialize the File Access Block (FAB )
C
C&IF VAXVMS
        FAB_BLK.FAB$B_BID=FAB$C_BID
        FAB_BLK.FAB$B_BLN=FAB$C_BLN
        FAB_BLK.FAB$L_NAM=%LOC(NAM_BLK)
        FAB_BLK.FAB$L_FNA=%LOC(FILE_SPEC)
        FAB_BLK.FAB$B_FNS=LEN(FILE_SPEC)
C
C     Initialize the Name Block( NAM )
C
        NAM_BLK.NAM$B_BID=NAM$C_BID
        NAM_BLK.NAM$B_BLN=NAM$C_BLN
        NAM_BLK.NAM$L_ESA=%LOC(EXP_STR)
        NAM_BLK.NAM$B_ESS=NAM$C_MAXRSS
        NAM_BLK.NAM$B_NOP = NAM$M_SYNCHK
C
        STATUS=SYS$PARSE(FAB_BLK)

        IF(STATUS .EQ. RMS$_NORMAL) THEN
          RET_FLAG = .FALSE.
C
C         Extract node name and length
C
          NODE_LEN = NAM_BLK.NAM$B_NODE
          NODE_NAME = EXP_STR (1:NODE_LEN)
C
C         Extract device name and length
C
          DEV_LEN = NAM_BLK.NAM$B_DEV
          STARTPOS = NODE_LEN + 1
          LASTPOS = NODE_LEN + DEV_LEN
          DEV_NAME = EXP_STR( STARTPOS:LASTPOS)
C
C         Extract directory name and length
C
          DIR_LEN = NAM_BLK.NAM$B_DIR
          STARTPOS = NODE_LEN + DEV_LEN + 1
          LASTPOS =  NODE_LEN + DEV_LEN + DIR_LEN
          DIR_NAME = EXP_STR( STARTPOS:LASTPOS)
C
C         Extract file name and length
C
          FILE_LEN = NAM_BLK.NAM$B_NAME
          STARTPOS = NODE_LEN + DEV_LEN + DIR_LEN + 1
          LASTPOS =  NODE_LEN + DEV_LEN + DIR_LEN + FILE_LEN
          FILE_NAME = EXP_STR( STARTPOS:LASTPOS)
C
C         Extract directory name and length
C
          TYPE_LEN = NAM_BLK.NAM$B_TYPE
          STARTPOS = NODE_LEN + DEV_LEN + DIR_LEN + FILE_LEN + 1
          LASTPOS =  NODE_LEN + DEV_LEN + DIR_LEN + FILE_LEN + TYPE_LEN
          FILE_TYPE = EXP_STR( STARTPOS:LASTPOS)
C
C       PARSE error
C
        ELSE
          RET_FLAG = .TRUE.
        ENDIF
C&ELSE
C&      CTEMP = FILE_SPEC
C&      N = INDEX(CTEMP, '::')
C&      IF(N.EQ.0)THEN
C&        NODE_LEN = 0
C&      ELSE
C&        NODE_LEN = N + 1
C&      ENDIF
C&      NODE_NAME = CTEMP(1:NODE_LEN)
C&      CTEMP = CTEMP(NODE_LEN+1:)
C&      N = INDEX(CTEMP, ':')
C&      IF(N.EQ.0)THEN
C&        DEV_LEN = 0
C&      ELSE
C&        DEV_LEN = N
C&      ENDIF
C&      DEV_NAME = CTEMP(1:DEV_LEN)
C&      CTEMP = CTEMP(DEV_LEN+1:)
C&      N = INDEX(CTEMP, ']')
C&      IF(N.EQ.0)THEN
C&        DIR_LEN = 0
C&      ELSE
C&        DIR_LEN = N
C&      ENDIF
C&      DIR_NAME = CTEMP(1:DIR_LEN)
C&      CTEMP = CTEMP(DIR_LEN+1:)
C&      N = INDEX(CTEMP, '.')
C&      IF(N.EQ.0)THEN
C&        FILE_LEN = TRULEN(CTEMP)
C&      ELSE
C&        FILE_LEN = N - 1
C&      ENDIF
C&      FILE_NAME = CTEMP(1:FILE_LEN)
C&      CTEMP = CTEMP(FILE_LEN+1:)
C&      TYPE_LEN = TRULEN(CTEMP)
C&      FILE_TYPE = CTEMP
C&ENDIF
      RETURN
      END
