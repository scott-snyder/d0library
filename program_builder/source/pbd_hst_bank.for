      SUBROUTINE PBD_HST_BANK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine generates FORTRAN code for History Bank
C-                         and Produciton routines based on the existing
C-                         sources - 'D0$ZEBRA_UTIL$SOURCE:HSTRFL.FOR' and
C-                         'D0$OFFLINE_UTIL$GENERAL:PRODUC.FOR'.
C-                         It fills the History Bank routine with -
C-                         production id, version, pass, production
C-                         site, creation time, program name.  For the
C-                         Production routine it sets the production flag
C-                         to .TRUE.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: Common error flag ERROR_FLAG
C-            ( .TRUE. if fatal error, .FALSE. if no error )
C-
C-   Modules called by this routine:  PBD_MSG, PBD_REM_BLANK, OFFTIM,
C_                                    TRNLNM
C-
C-   Based on the PASCAL procedure Write_History_Bank of the old
C-   Program Builder.
C-
C-
C-   Created  09-SEP-1991   Hyon Joo Kehayias
C-   Updated  17-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-   Updated  18-MAY-1992   Hyon Joo Kehayias
C-       ( To avoid the VAX FORTRAN compiler optimization problem )
C-   Updated  24-May-1992   Herbert Greenlee
C-       Additional changes for UNIX.
C-   Updated  16-Dec-1992   Hyon Joo Kehayias
C-       ( Added a check for HSTRFL qualifier to skip code for PRODUC routine )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
C
C     Local Data Area
C
      INTEGER*4 STATUS                  ! I/O RETURN STATUS WORD
      INTEGER*4 D0TIME                  ! D0 STANDARD TIME
      INTEGER*4 NUMLINE                 ! # OF INPUT LINES
      INTEGER*2 NODE_LEN                ! NODE NAME STRING LENGTH
      INTEGER*2 LENGTH                  ! STRING LENGTH
      INTEGER*2 TIME_LEN                ! STRING CUR_TIME LENGTH
      INTEGER*2 I,J                     ! INDEX VARIABLE
      CHARACTER*80 MESSAGE              ! DISPLAY MESSAGE
      CHARACTER*80 MSG_BUF              ! MESSAGE BUFFER
      CHARACTER*80 ERROR_BUF(2)         ! ERROR MESSAGE BUFFER
      CHARACTER*80 FILE_NAME            ! FILE NAME TO OPEN
      CHARACTER*80 INPLINE              ! INPUT LINE
      CHARACTER*20 NODE_NAME            ! NODE NAME
      CHARACTER*20 CUR_TIME             ! CURRENT D0 STANDARD TIME IN ASCII

      INTEGER*4 OFFTIM                  ! EXTERNAL FUNCTION OFFTIM
      INTEGER*4 TRNLNM                  ! EXTERNAL FUNCTION TRNLNM
      LOGICAL OPENED                    ! OPEN STATUS

      DATA MESSAGE /
     &  '-- Reading file '/
      DATA ERROR_BUF /
     &  '-E-File open error for ',
     &  '-E-Error detected while reading '/
C
C     Get the node name
C
      STATUS = TRNLNM ('SYS$NODE',NODE_NAME,NODE_LEN)
C
C     Remove '::' from the node name
C
      LENGTH = INDEX ( NODE_NAME,'::')
      IF ( LENGTH .GT. 0 ) THEN
        NODE_NAME = NODE_NAME(1:LENGTH-1)
        NODE_LEN = NODE_LEN - 2
      END IF
C
C     Get D0 standard time
C
      D0TIME = OFFTIM()
C
C     Convert D0 standard time in Integer to ASCII format
C
      WRITE ( CUR_TIME,200) D0TIME
  200 FORMAT ( I20 )
      CALL PBD_REM_BLANK ( CUR_TIME, TIME_LEN)
C
C     Open History Bank FORTRAN source file and read
C
      FILE_NAME = 'D0$ZEBRA_UTIL$SOURCE:HSTRFL.FOR'
      CALL D0OPEN(5, FILE_NAME, 'IF', OPENED)

   10 IF ( .NOT. OPENED ) THEN     ! Open error
        MSG_BUF = ERROR_BUF(1)(1:23)//FILE_NAME
        CALL PBD_MSG ( MSG_BUF )
        MSG_BUF = '-W-Function HISTORY not included'
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF

      MSG_BUF = MESSAGE(1:16)//FILE_NAME
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
        MSG_BUF = ERROR_BUF(2)(1:32)//FILE_NAME
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF

      DO I = 1, NUMLINE

        OUTLINE = INPBUF(I)
        INPLINE = INPBUF(I)
        CALL PBD_REM_BLANK(INPLINE,LENGTH)

        IF (INDEX(INPLINE,'PARAMETER(PRODID=') .NE.0 ) THEN
          OUTLINE = '      PARAMETER( PRODID = '//PRODID(1:PRODID_LEN)
     &              //' )'
        END IF

        IF (INDEX(INPLINE,'PARAMETER(VERSION=') .NE. 0 ) THEN
          OUTLINE = '      PARAMETER( VERSION = '//
     &              VERSION(1:VERSION_LEN) // ' )'
        END IF

        IF (INDEX(INPLINE,'PARAMETER(PASS=') .NE. 0 ) THEN
          OUTLINE = '      PARAMETER( PASS = '//
     &              PASS(1:PASS_LEN)//' )'
        END IF

        IF (INDEX(INPLINE,'PARAMETER(CREATION_TIME=') .NE. 0 ) THEN
          OUTLINE = '      PARAMETER( CREATION_TIME = '//
     &              CUR_TIME(1:TIME_LEN) //' )'
        END IF

        IF (INDEX(INPLINE,'PARAMETER(CREATION_SITE=') .NE. 0) THEN
          OUTLINE = '      PARAMETER( CREATION_SITE = '''//
     &      NODE_NAME(1:NODE_LEN)//''' )'
        END IF

        IF (INDEX(INPLINE,'PARAMETER(PROGRAM_NAME=').NE.0) THEN
          OUTLINE = '      PARAMETER( PROGRAM_NAME = '''//
     &             FOR_FILE_NAME(1:FOR_FILE_LEN)//''' )'
        END IF

        WRITE (8, 100) OUTLINE

      END DO
C
C     Check if HSTRFL qualifier present.  If so skip code generation for
C     PRODUC routine.
C
      IF ( QUALFLAG(19) ) RETURN
C
C     Open Production FORTRAN source file and read
C
      FILE_NAME = 'D0$OFFLINE_UTIL$GENERAL:PRODUC.FOR'
      CALL D0OPEN(5, FILE_NAME, 'IF', OPENED)

   30 IF ( .NOT. OPENED ) THEN    ! Open error
        MSG_BUF = ERROR_BUF(1)(1:23)//FILE_NAME
        CALL PBD_MSG ( MSG_BUF )
        MSG_BUF = '-W-Function PRODUCTION not included'
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF

      MSG_BUF = MESSAGE(1:16)//FILE_NAME
      CALL PBD_MSG(MSG_BUF)
C
C     Read PRODUC function.  Do not use implied DO loop for read -
C     VAX FORTRAN compiler will not correctly assign # of input lines.
C
      NUMLINE = 0

      DO J = 1, 500
        READ (5, 100, END = 40, ERR = 40, IOSTAT = STATUS )
     &         INPBUF(J)
        NUMLINE = NUMLINE + 1
      END DO

   40 CONTINUE
      CLOSE (5)
C
C     Check I/O return status.  EOF if STATUS = -1.
C     Error condition If STATUS > 0.  If any error, send error message
C
      IF ( STATUS .GT. 0 ) THEN
        MSG_BUF = ERROR_BUF(2)(1:32)//FILE_NAME
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF

      WRITE (8,100) COMMENT_LINE
      WRITE (8,100) COMMENT_LINE
      WRITE (8,100) COMMENT_LINE


      DO I = 1, NUMLINE

        OUTLINE = INPBUF(I)
        INPLINE = INPBUF(I)
        CALL PBD_REM_BLANK(INPLINE,LENGTH)

        IF (INDEX(INPLINE,'PRODUC=.FALSE.').NE.0) THEN
          OUTLINE = '      PRODUC=.TRUE.'
        END IF

        WRITE(8,100) OUTLINE

      END DO

  999 RETURN
      END
