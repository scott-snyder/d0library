      SUBROUTINE PBD_BLD_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates FORTRAN code for the PBD initialization 
C-                         subroutine PBDINI.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: Common error flag ERROR_FLAG
C-            ( .TRUE. if fatal error, .FALSE. if no error ) 
C-
C-   Modules called by this routine:  PBD_HST_BANK, PBD_INIT_CONST,PBD_MSG
C-                                    DATE, TIME
C-  
C-   Based on the PASCAL procedure Build_Initialization_Routine of the old
C-   Program Builder.
C-
C-   Created  16-JUL-1991   Hyon Joo Kehayias
C-   Updated  17-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-   Updated  02-AUG-1993   Hyon Joo Kehayias
C-      ( PBD_HST_BANK call is made after ZEBRA common init. calls )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
C
C     Local Data Area
C
      INTEGER*4 STATUS                  ! I/O RETURN STATUS WORD
      INTEGER*2 I                       ! INDEX VARIABLE
      CHARACTER*80 FILE_NAME            ! FILE NAME
      CHARACTER*80 MESSAGE(2)           ! DISPLAY MESSAGES
      CHARACTER*80 MSG_BUF              ! MESSAGE BUFFER
      CHARACTER*80 ERROR_BUF            ! ERROR MESSAGE BUFFER
      CHARACTER*10 COM_NAME             ! ZEBRA COMMON BLOCK NAME
      CHARACTER*9  DATE_STR             ! ASCII DATE 
      CHARACTER*8  TIME_STR             ! ASCII TIME
      CHARACTER*2  NUMSTR               ! ASCII NUMBER
      LOGICAL      OPENED               ! OPEN STATUS

      DATA MESSAGE /       
     & '-- Generating FORTRAN Source File ',
     & '-- Building Initialization Routine for '/
      DATA ERROR_BUF /
     & '-E-File open error for '/

C
C     Determine the FORTRAN source file name
C
      FOR_FILE_NAME = COMBINED_NAME(1:COMBINED_LEN)//'_'//
     &                 FRAME_NAME(1:FRAME_LEN)
      FOR_FILE_LEN = COMBINED_LEN+1+FRAME_LEN

      FILE_NAME = FOR_FILE_NAME(1:FOR_FILE_LEN)//'.FOR'

      MSG_BUF = MESSAGE(1)(1:34)//FILE_NAME
      CALL PBD_MSG ( MSG_BUF )
C
C     Open the FORTRAN source file
C
      CALL D0OPEN(8, FILE_NAME, 'OFL', OPENED)
 10   IF ( .NOT.OPENED ) THEN    ! Open error
        MSG_BUF = ERROR_BUF(1:23)//FILE_NAME
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF

      MSG_BUF = MESSAGE(2)(1:39)//FRAME_NAME(1:FRAME_LEN)
      CALL PBD_MSG ( MSG_BUF )
C
C     Get current date and time
C
      CALL DATE ( DATE_STR )
      CALL TIME ( TIME_STR )
      DATE_TIME = COMMENT_STR//DATE_STR//' '//TIME_STR
C
C     First build the main program to Book the Flags for the
C        package selectors
C
      OUTLINE = START_LINE// 'SUBROUTINE PBDINI'
100   FORMAT ( A )
      WRITE (8,100) OUTLINE
      WRITE (8,100) COMMENT_LINE 
      WRITE (8,100) COMMENT_LINE
      OUTLINE = COMMENT_STR//'Purposes and Methods:'
      WRITE (8,100) OUTLINE
      OUTLINE = COMMENT_STR//'--------------------' 
      WRITE (8,100) OUTLINE
      OUTLINE = COMMENT_STR//'This routine must be '//
     &           'called by the FrameWork to initialize'
      WRITE (8,100) OUTLINE
      OUTLINE = COMMENT_STR//'the run time '//
     &           'switches associated with the packages'
      WRITE (8,100) OUTLINE
      OUTLINE = COMMENT_STR//'Created by the PROGRAM BUILDER' 
      WRITE (8,100) OUTLINE
      WRITE (8,100) DATE_TIME
      WRITE (8,100) COMMENT_LINE
      WRITE (8,100) COMMENT_LINE 
      WRITE (8,100) COMMENT_LINE 

      OUTLINE = START_LINE//'LOGICAL WRNGOK'
      WRITE (8,100) OUTLINE
      OUTLINE = START_LINE//
     &           'INTEGER LOGUNT,MAXLOG,MAXWRN'
      WRITE (8,100) OUTLINE
      OUTLINE = START_LINE//'CHARACTER*32 STRGLG'
      WRITE (8,100) OUTLINE
C
C     Convert # of packages in ASCII character string
C
      WRITE (NUMSTR,200) NUMPACK
200   FORMAT (I2)
C
C     Add in the code for the common block which will hold the
C     PBD calling flags
C

      OUTLINE = START_LINE//
     &   'LOGICAL PBD_FLAG_VALUE('//NUMSTR//')'
      WRITE (8,100) OUTLINE
      COMMON_LINE(1) = OUTLINE
      OUTLINE = START_LINE//
     &   'CHARACTER*32 PBD_FLAG_NAME(' // NUMSTR// ')' 
      WRITE (8,100) OUTLINE
      COMMON_LINE(2) = OUTLINE
      OUTLINE = START_LINE//
     &   'INTEGER PBD_FLAG_MAX'
      WRITE (8,100) OUTLINE
      COMMON_LINE(3) = OUTLINE

      WRITE (8,100) COMMENT_LINE
      OUTLINE = START_LINE//'COMMON /PBD_COMMON/'//
     &          ' PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX' 
      WRITE (8,100) OUTLINE
      COMMON_LINE(4) = OUTLINE
      WRITE (8,100) COMMENT_LINE

      OUTLINE = START_LINE//'LOGUNT = 0' 
      WRITE (8,100) OUTLINE
      OUTLINE = START_LINE//'WRNGOK = .TRUE.' 
      WRITE (8,100) OUTLINE
      OUTLINE = START_LINE//
     &         'CALL ERRINI(LOGUNT,WRNGOK)'
      WRITE (8,100) OUTLINE
      OUTLINE = START_LINE//'MAXLOG = 1'
      WRITE (8,100) OUTLINE
      OUTLINE = START_LINE//'MAXWRN = 1'
      WRITE (8,100) OUTLINE
      OUTLINE = START_LINE//'STRGLG = '' '''
      WRITE (8,100) OUTLINE
      OUTLINE = START_LINE//
     &         'CALL ERRMAX(STRGLG,MAXLOG,MAXWRN)'
      WRITE (8,100) OUTLINE
C
C     Add code to initialize the variables stored in the PBD_common block
C
      WRITE (8,100) COMMENT_LINE
      OUTLINE = START_LINE//
     &  'PBD_FLAG_MAX = '// NUMSTR
      WRITE (8,100) OUTLINE
      WRITE (8,100) COMMENT_LINE
C
C     Generate code to initialize the package flag name with the package name
C
      DO I = 1, NUMPACK
C
C       Convert the package number in ASCII character string
C
        WRITE (NUMSTR,200) I
        OUTLINE = START_LINE//
     &              'PBD_FLAG_VALUE('// NUMSTR//') = .TRUE.'
        WRITE (8,100) OUTLINE
        OUTLINE = START_LINE//
     &              'PBD_FLAG_NAME('// NUMSTR// ') = '// ''''//
     &              PACKAGE_NAME(I)(1:PACK_NAME_LEN(I) )//
     &              ''''
        WRITE (8,100) OUTLINE

      END DO 

      WRITE (8,100) RET_LINE
      WRITE (8,100) END_LINE
C
C     If any ZEBRA common block size changed, generate a new ZEBRA common
C     initialization routine.
C
      IF ( ZEBCOM_LEN .NE. 0 ) THEN
        COM_NAME = 'ZEBCOM'
        CALL PBD_INIT_CONST( COM_NAME ) 
        WRITE (8,100) COMMENT_LINE
        WRITE (8,100) COMMENT_LINE
      END  IF

      IF ( ZEBSTP_LEN .NE. 0 ) THEN
        COM_NAME = 'ZEBSTP'
        CALL PBD_INIT_CONST( COM_NAME ) 
        WRITE (8,100) COMMENT_LINE
        WRITE (8,100) COMMENT_LINE
      END  IF

      IF ( ZEBWRK_LEN .NE. 0 ) THEN
        COM_NAME = 'ZEBWRK'
        CALL PBD_INIT_CONST( COM_NAME ) 
        WRITE (8,100) COMMENT_LINE
        WRITE (8,100) COMMENT_LINE
      END  IF

      IF ( PAWC_LEN .NE. 0 ) THEN
        COM_NAME = 'PAWC'
        CALL PBD_INIT_CONST( COM_NAME ) 
        WRITE (8,100) COMMENT_LINE
        WRITE (8,100) COMMENT_LINE
      END  IF

      IF ( GCBANK_LEN .NE. 0 ) THEN
        COM_NAME = 'GCBANK'
        CALL PBD_INIT_CONST( COM_NAME ) 
        WRITE (8,100) COMMENT_LINE
        WRITE (8,100) COMMENT_LINE
      END  IF
C
C     If HISTORY_BANK qualifier provided, generate HISTORY_BANK routine
C
      IF ( HISTORY ) THEN
        CALL PBD_HST_BANK
      END IF

      RETURN
      END
