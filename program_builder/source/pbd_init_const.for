      SUBROUTINE PBD_INIT_CONST ( COM_NAME )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates FORTRAN code for Zebra common
C-                         initialization routines with modifed parameters.
C-                         It reads the existing source file associated with
C-                         the given common, modifies the common block size
C-                         parameter and writes the new code in the PBD
C-                         FORTRAN source file.
C-
C-   Inputs  : COM_NAME - Zebra Common Name
C-   Outputs :
C-   Controls: Common error flag ERROR_FLAG
C-            ( .TRUE. if fatal error, .FALSE. if no error )
C-
C-   Modules called by this routine:  PBD_MSG
C-
C-   Created  18-JUL-1991   Hyon Joo Kehayias
C-   Updated  17-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
C
C     Local Data Area
C
      INTEGER*4 STATUS                  ! I/O RETURN STATUS WORD
      INTEGER*4 NUMLINE                 ! # OF INPUT LINES
      INTEGER*2 I,POS,COM_LEN           ! INDEX VARIABLE
      INTEGER*2 INCLUDE_LEN, PARAM_LEN
      CHARACTER*80 MESSAGE              ! DISPLAY MESSAGE
      CHARACTER*80 MSG_BUF              ! MESSAGE BUFFER
      CHARACTER*80 ERROR_BUF(2)         ! ERROR MESSAGE BUFFER
      CHARACTER*80 INIT_FILE            ! FORTRAN SOURCE FILE NAME
      CHARACTER*80 INCLUDE_FILE         ! INCLUDE SOURCE FILE NAME
      CHARACTER*80 INCLUDE_LINE         ! INCLUDE LINE
      CHARACTER*80 PARAM_LINE           ! PARAMETER LINE
      CHARACTER*80 INPLINE              ! INPUT LINE
      CHARACTER*10 COM_NAME             ! ZEBRA COMMON BLOCK NAME
      CHARACTER*10 COM_SIZE             ! ASCII ZEBRA COMMON BLOCK SIZE
      CHARACTER*2  NUMSTR               ! ASCII NUMBER
      LOGICAL OPENED                    ! OPEN STATUS

      DATA MESSAGE /
     &  '-- Reading file '/
      DATA ERROR_BUF /
     &  '-E-File open error for ',
     &  '-E-Error detected while reading '/
C
C     Determine which FORTRAN and INCLUDE files to read by the COMMON name.
C
      IF ( COM_NAME .EQ. 'ZEBCOM' ) THEN
        INIT_FILE = 'D0$ZEBRA_UTIL$SOURCE:INZCOM.FOR'
        INCLUDE_FILE = 'D0$INC:ZEBCOM.INC'
        INCLUDE_LINE = 'INCLUDE ''D0$INC:ZEBCOM.INC'''
        PARAM_LINE = 'PARAMETER (NNQ='
        INCLUDE_LEN = 27
        PARAM_LEN = 15
        COM_SIZE = ZEBCOM_SIZE
        COM_LEN = ZEBCOM_LEN

      ELSE IF ( COM_NAME .EQ. 'ZEBSTP' ) THEN
        INIT_FILE = 'D0$ZEBRA_UTIL$SOURCE:INZSTP.FOR'
        INCLUDE_FILE = 'D0$INC:ZEBSTP.INC'
        INCLUDE_LINE = 'INCLUDE ''D0$INC:ZEBSTP.INC'''
        PARAM_LINE = 'PARAMETER (NNC='
        INCLUDE_LEN = 27
        PARAM_LEN = 15
        COM_SIZE = ZEBSTP_SIZE
        COM_LEN = ZEBSTP_LEN

      ELSE IF ( COM_NAME .EQ. 'ZEBWRK' ) THEN
        INIT_FILE = 'D0$ZEBRA_UTIL$SOURCE:INZWRK.FOR'
        INCLUDE_FILE = 'D0$INC:ZEBWRK.INC'
        INCLUDE_LINE = 'INCLUDE ''D0$INC:ZEBWRK.INC'''
        PARAM_LINE = 'PARAMETER (NNW='
        INCLUDE_LEN = 27
        PARAM_LEN = 15
        COM_SIZE = ZEBWRK_SIZE
        COM_LEN = ZEBWRK_LEN


      ELSE IF ( COM_NAME .EQ. 'PAWC' ) THEN
        INIT_FILE = 'D0$ZEBRA_UTIL$SOURCE:INPAWC.FOR'
        INCLUDE_FILE = 'D0$INC:PAWC.INC'
        INCLUDE_LINE = 'INCLUDE ''D0$INC:PAWC.INC'''
        PARAM_LINE = 'PARAMETER (NPAWC='
        INCLUDE_LEN = 25
        PARAM_LEN = 17
        COM_SIZE = PAWC_SIZE
        COM_LEN = PAWC_LEN


      ELSE IF ( COM_NAME .EQ. 'GCBANK' ) THEN
        INIT_FILE = 'D0$ZEBRA_UTIL$SOURCE:INZGCB.FOR'
        INCLUDE_FILE = 'D0$INC:GCBANK.INC'
        INCLUDE_LINE = 'INCLUDE ''D0$INC:GCBANK.INC'''
        PARAM_LINE = 'PARAMETER( NGCBNK ='
        INCLUDE_LEN = 27
        PARAM_LEN = 20
        COM_SIZE = GCBANK_SIZE
        COM_LEN = GCBANK_LEN

      END IF
C
C     Open the initialization FORTRAN source file and read
C
      CALL D0OPEN(5, INIT_FILE, 'IF', OPENED)
   10 IF ( .NOT.OPENED ) THEN    ! Open error
        MSG_BUF = ERROR_BUF(1)(1:23)//INIT_FILE
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF

      MSG_BUF = MESSAGE(1:16)//INIT_FILE
      CALL PBD_MSG(MSG_BUF)
      DO i = 1, 500
        READ (5, 100, END = 20, ERR = 20, IOSTAT = STATUS )
     &       INPBUF(I)
      ENDDO
  100 FORMAT (A)
   20 CONTINUE
      NUMLINE = I - 1
      CLOSE (5)
C
C     Check I/O return status.  EOF if STATUS = -1.
C     Error condition If STATUS > 0.  If any error, send error message
C
      IF ( STATUS .GT. 0 ) THEN
        MSG_BUF = ERROR_BUF(2)(1:32)//INIT_FILE
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF

      WRITE (8,100) COMMENT_LINE
      WRITE (8,100) COMMENT_LINE
      OUTLINE =  COMMENT_STR//
     &   'The following routine has been generated '//
     &   'by the Program Builder'
      WRITE (8,100) OUTLINE
      OUTLINE = COMMENT_STR//
     &   'because the size of the common blocks has been modified'
      WRITE (8,100) OUTLINE
      OUTLINE = COMMENT_STR// 'by the user.'
      WRITE (8,100) OUTLINE
      WRITE (8,100) COMMENT_LINE
      OUTLINE = COMMENT_STR//
     &   'Created by the PROGRAM BUILDER'
      WRITE (8,100) OUTLINE
      WRITE (8,100) DATE_TIME
      WRITE (8,100) COMMENT_LINE
      WRITE (8,100) COMMENT_LINE

      DO I = 1, NUMLINE
C
C       Find the INCLUDE line from the FORTRAN source.  If it exists,
C       make it as a comment line.
C
        POS = INDEX ( INPBUF(I),INCLUDE_LINE(1:INCLUDE_LEN) )
        IF ( POS .NE. 0 ) THEN
          WRITE (8,100) COMMENT_LINE
          INPBUF(I)(1:1) = 'C'
          WRITE (8,100) INPBUF(I)
          WRITE (8,100) COMMENT_LINE
          WRITE (8,100) COMMENT_LINE
C
C         Open the initialization FORTRAN source file and read
C
          CALL D0OPEN(5, INCLUDE_FILE, 'IF', OPENED)
   30     IF ( .NOT.OPENED ) THEN    ! Open error
            MSG_BUF = ERROR_BUF(1)(1:23)//INCLUDE_FILE
            ERROR_FLAG = .TRUE.
            CALL PBD_MSG ( MSG_BUF )
            RETURN
          END IF

          MSG_BUF = MESSAGE(1:16)//INCLUDE_FILE
          CALL PBD_MSG(MSG_BUF)

          DO WHILE ( .TRUE. )
            READ (5, 100, END = 50, ERR = 40, IOSTAT = STATUS )
     &           INPLINE
C
C           Check I/O return status.  EOF if STATUS = -1.
C           Error condition If STATUS > 0.  If any error, send error message
C
   40       IF ( STATUS .GT. 0 ) THEN
              MSG_BUF = ERROR_BUF(2)(1:32)//INCLUDE_FILE
              ERROR_FLAG = .TRUE.
              CALL PBD_MSG ( MSG_BUF )
              CLOSE (5)
              RETURN
            END IF
C
C           Check if this line includes PARAMETER statement for the common
C
            POS = INDEX ( INPLINE, PARAM_LINE(1:PARAM_LEN) )
            IF ( POS .GT. 0 ) THEN
C
C             Make the original parameter line as comment line
C
              INPLINE(1:1) = 'C'
              WRITE(8,100) INPLINE
C
C             Replace the old parameter value with the new one
C
              WRITE (8,100) COMMENT_LINE
              OUTLINE = COMMENT_STR//
     &           'The following is the new parameter value'
              WRITE (8,100) OUTLINE
              WRITE (8,100) COMMENT_LINE
              OUTLINE = START_LINE//PARAM_LINE(1:PARAM_LEN)//
     &                    COM_SIZE(1:COM_LEN)//' )'
              WRITE (8,100) OUTLINE
              WRITE (8,100) COMMENT_LINE

            ELSE
              WRITE(8,100) INPLINE
            END IF

          END DO

   50     CONTINUE
          CLOSE ( 5 )
          WRITE (8,100) COMMENT_LINE
          WRITE (8,100) COMMENT_LINE

        ELSE

          WRITE (8,100) INPBUF(I)
        END IF
      END DO

  999 RETURN
      END
