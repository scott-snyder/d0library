      SUBROUTINE PBD_INIT_COM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initializes variables in common block
C-                         PBDCOM.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-SEP-1991   Hyon Joo Kehayias
C-   Updated  19-FEB-1992   Hyon Joo Kehayias
C-   Updated  24-May-1992   Herbert Greenlee
C-     Changed from BLOCK DATA to SUBROUTINE
C-   Updated  18-DEC-1992   Hyon Joo Kehayias
C-     ( Initialized prodid, version and pass to '0' instead of a blank )
C-   Updated  28-MAY-1993   Hyon Joo Kehayias
C-     ( Added initialization of LOC_PACK_NAME, LOC_PACK_LEN and TOT_INP_PACK)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'

      INTEGER*2 I,J
      INTEGER*2 MAX_PACK_HOOK
      INTEGER*2 MAX_PACK_OBJ
      INTEGER*2 MAX_PACK_RCP

      PARAMETER ( MAX_PACK_HOOK = MAX_PACK * MAX_HOOK )
      PARAMETER ( MAX_PACK_OBJ = MAX_PACK * MAX_OBJ )
      PARAMETER ( MAX_PACK_RCP = MAX_PACK * MAX_RCP )
C
C     Initialize character variables to blanks, integer variables to 0
C     and logicals to .FALSE.
C
      ZEBCOM_SIZE = ' '
      ZEBSTP_SIZE = ' '
      ZEBWRK_SIZE = ' '
      PAWC_SIZE   = ' '
      GCBANK_SIZE = ' '
      FRAME_NAME  = ' '
      COMBINED_NAME    = ' '
      COMB_PACK_NAME =  ' '
      COMB_PACK_LEN  =  0

      DO I=1,MAX_PACK
        PACKAGE_NAME(I) =  ' '
        NUMOBJ(I) =  0
        PACK_NAME_LEN(I) =  0
        LOC_PACK_LEN(I) =  0
        LOC_PACK_NAME(I) =  ' '
      ENDDO

      LIBRARY_NAME = ' '
      LOG = .FALSE.
      COMPILE = .FALSE.
      HISTORY = .FALSE.
      PRODID = '0'
      VERSION = '0'
      PASS = '0'
      DO I=1,MAX_HOOK
        VALID_HOOK(I) =  ' '
        HOOK_NAME(I)  =  ' '
        SUB_NAME(I) =  ' '
        ACTION(I) =  0
        HOOK_NAME_LEN(I) =  0
        SUB_NAME_LEN(I) =  0
      END DO
      DO J=1,MAX_HOOK
        DO I=1,MAX_PACK
          INT_NAME(I,J) =  ' '
          INT_NAME_LEN(I,J) =  0
        END DO
      END DO
      DO J=1,MAX_OBJ
        DO I=1,MAX_PACK
          OBJ_FILE(I,J) =  ' '
          OBJ_FILE_LEN(I,J) =  0
        END DO
      END DO
      DO I=1,MAX_RCP
        RCP_FILE(I) =  ' '
        RCP_FILE_LEN(I) =  0
        RCP_OPT(I) =  ' '
        RCP_OPT_LEN(I) =  0
      END DO
      TOTAL_HOOKS =  0
      NUMHOOK =  0
      NUMPACK =  0
      TOT_INP_PACK =  0
      NUMRCP =  0

      FRAME_LEN =  0
      COMBINED_LEN =  0
      LIBRARY_LEN =  0
      ZEBCOM_LEN  =  0
      ZEBSTP_LEN  =  0
      ZEBWRK_LEN  =  0
      PAWC_LEN    =  0
      GCBANK_LEN  =  0
      PRODID_LEN  =  1
      VERSION_LEN =  1
      PASS_LEN    =  1
      FOR_FILE_LEN  =  0
      DO I=1,NUMQUAL
        QUALFLAG(I) =  .FALSE.
      END DO
      ERROR_FLAG =  .FALSE.

      FOR_FILE_NAME = ' '
      DO I=1,4
        COMMON_LINE(I) = ' '
      END DO
      DATE_TIME =  ' '
C
C     Initialize the FORTRAN code related data
C
      COMMENT_STR =  'C-    '
      COMMENT_LINE =  'C-    '
      RET_LINE =  '      RETURN'
      END_LINE =  '      END'
      START_LINE =  '      '
      CONT_LINE =  '     &'

      END
