      SUBROUTINE PBD_READ_FILE ( FILE_TYPE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine reads the master hook PBD file
C-                         D0$PBD:MASTER_HOOK.PBD, the framework PBD file
C-                         ( 'frame_name'_FRAME.PBD ) and the combined package
C-                         PBD file ( 'combined_name'_COMBINED.PBD ) and
C-                         fills the PBD common tables with data from these
C-                         files.
C-
C-   Inputs  : FILE_TYPE - File type ( 1 = master hook file
C-                                     2 = framework file
C-                                     3 = combined package file )
C-
C-   Outputs :
C-   Controls: Common error flag ERROR_FLAG
C-            ( .TRUE. if fatal error, .FALSE. if no error )
C-
C-   Modules called by this routine:  PBD_FILE_OK, PBD_GET_COM, PBD_IS_VALID,
C-                                    PBD_MSG, PBD_NEXT_WORD, PBD_UP_CASE
C-
C-   Created  18-JUN-1991   Hyon Joo Kehayias
C-   Updated  17-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-   Updated  28-MAY-1992   Hyon Joo Kehayias
C-       ( Made to read only one combined package )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'

      INTEGER*4 STATUS                  ! I/O RETURN STATUS
      INTEGER*4 COM_SIZE                ! ZEBRA BANK SIZE
      INTEGER*2  STRTPOS,POS            ! STARTING POSITION OF TOKEN
      INTEGER*2  LENGTH                 ! TOKEN LENGTH
      INTEGER*2  NUMLINE                ! # OF INPUT LINES
      INTEGER*2  FILE_LEN               ! FILE NAME LENGTH
      INTEGER*2  FILE_TYPE              ! FILE TYPE TO READ
      INTEGER*2  OFFSET                 ! OFFSET TO VALID_HOOK TABLE
      INTEGER*2  I,J                    ! INDEX VARIABLES
      CHARACTER*32 TOKEN                ! TOKEN
      CHARACTER*80 FILE_NAME            ! FILE NAME
      CHARACTER*80 USER_FILE_NAME       ! USER FILE NAME
      CHARACTER*80 LIB_FILE_NAME        ! FULL LIBRARY FILE NAME
      CHARACTER*80 PBD_FILE_NAME        ! FULL PBD FILE NAME
      CHARACTER*80 ERROR_BUF(12)        ! ERROR MESSAGE BUFFER
      CHARACTER*80 MESSAGE (3)          ! MESSAGE BUFFER
      CHARACTER*80 ERROR_MSG            ! ERROR MESSAGE BUFFER
      CHARACTER*10 COM_NAME             ! ZEBRA BANK NAME
      CHARACTER*1 DOT
      LOGICAL USER_FILE                 ! USER FILE INDICATOR
      LOGICAL LIB_FILE                  ! LIBRARY FILE INDICATOR
      LOGICAL PBD_FILE                  ! PBD FILE INDICATOR
      LOGICAL FOUND,END_PACK            ! LOOP CONTROL VARIABLES
      LOGICAL RETFLAG                   ! ERROR RETURN FLAG
C
      LOGICAL    PBD_FILE_OK            ! LOGICAL FUNCTION FOR FILE SEARCH
      LOGICAL OPENED                    ! OPEN STATUS
C
      DATA DOT /'.'/
      DATA ERROR_BUF /
     &     '-E-Missing hook name in master hook name file ',
     &     '-E-Invalid action field for ',
     &     '-E-Invalid hook name ',
     &     '-E-Missing hook name in framework file',
     &     ' frame file does not exist',
     &     ' combined package file does not exist',
     &     '-E-File open error for ',
     &     '-E-Error detected while reading ',
     &     '-E-.PACKAGES command missing',
     &     '-E-.END PACKAGES command missing',
     &     '-E-Combined package name not matching with input name',
     &     '-W-Duplicate package name '/
C
      DATA MESSAGE /
     &     '-- Reading master hook name file ',
     &     '-- Reading framework PBD file ',
     &     '-- Reading combined package PBD file '/

      ERROR_FLAG = .FALSE.

      IF ( FILE_TYPE .EQ. MASTER_HOOK ) THEN
C
C       Open the master hook name file and read hook names
C
        FILE_NAME = 'D0$PBD:MASTER_HOOK.PBD'
        CALL D0OPEN(5, FILE_NAME, 'IF', OPENED)
C
C       Check open return status
C
    5   CONTINUE
        IF ( .NOT.OPENED ) THEN       ! Error, send message
          ERROR_MSG = ERROR_BUF(7)(1:23)//FILE_NAME(1:22)
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( ERROR_MSG )
          RETURN
        END IF
        ERROR_MSG = MESSAGE(1)(1:33)//FILE_NAME
        CALL PBD_MSG ( ERROR_MSG )
        DO i = 1, 500
          READ (5, 100, END = 10, ERR = 10, IOSTAT = STATUS )
     &       INPBUF(I)
        ENDDO
  100   FORMAT (A80)
   10   CONTINUE
        NUMLINE = I - 1
        CLOSE (5)
C
C       Check I/O return status.  If any error, send error message
C
        IF ( STATUS .GT. 0 ) THEN         ! An error conditon
          ERROR_MSG = ERROR_BUF(8)(1:32)//FILE_NAME
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( ERROR_MSG )
          RETURN
        END IF
C
C       Generate the hook name table
C
        TOTAL_HOOKS = 0
        DO I = 1, NUMLINE
C
C         Find the hook name and store in table VALID_HOOK
C
          IF ( INPBUF(I)(1:1) .NE. '!' ) THEN   ! hook name expected

            CALL PBD_NEXT_WORD(INPBUF(I),TOKEN,POS,LENGTH )
            IF ( LENGTH .GT. 0 ) THEN
              CALL PBD_UP_CASE (TOKEN,TOKEN)
              TOTAL_HOOKS = TOTAL_HOOKS + 1
              VALID_HOOK ( TOTAL_HOOKS ) = TOKEN(1:LENGTH)
            ELSE
C
C             No hook name provided, send error message
C
              ERROR_FLAG = .TRUE.
              CALL PBD_MSG ( ERROR_BUF (1) )
              CALL PBD_MSG ( INPBUF(I) )
            END IF
          END IF
        END DO
C
      ELSE IF ( FILE_TYPE .EQ. FRAME ) THEN
C
C       Open the framework file and read hook names and actions to take
C
        FILE_NAME = 'D0$PBD:'//FRAME_NAME (1:FRAME_LEN)//'_FRAME.PBD'
        CALL D0OPEN(5, FILE_NAME, 'IF', OPENED)
C
C       Check open return status
C
   15   CONTINUE
        IF ( .NOT.OPENED ) THEN       ! Error, send message
          FILE_LEN = FRAME_LEN + 17
          ERROR_MSG = '-E-'//FILE_NAME(1:FILE_LEN)//ERROR_BUF(5)
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( ERROR_MSG )
          RETURN
        END IF

        ERROR_MSG = MESSAGE(2)(1:30)//FILE_NAME
        CALL PBD_MSG ( ERROR_MSG )

        READ (5, 100, END = 20, ERR = 20, IOSTAT = STATUS )
     &       ( INPBUF(I),I=1,500)
   20   CONTINUE
        CLOSE (5)
C
C       Check I/O return status.  If any error, send error message
C
        IF ( STATUS .GT. 0 ) THEN         ! An error conditon
          ERROR_MSG = ERROR_BUF(8)(1:32)//FILE_NAME
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( ERROR_MSG )
          RETURN
        END IF

        NUMLINE = I - 1
C
C       Generate the hook name table and action table
C
        NUMHOOK = 0
        DO I = 1, NUMLINE
C
C        Find the hook name and validate
C
          IF ( INPBUF(I)(1:1) .EQ. DOT ) THEN   ! hook name expected
C
C        Hook name line, convert the entire line to upper case
C
            CALL PBD_UP_CASE (INPBUF(I), INPBUF(I))
            CALL PBD_NEXT_WORD (INPBUF(I)(2: ),TOKEN,STRTPOS,LENGTH )
            IF ( LENGTH .GT. 0 ) THEN
C
C           Check the hook name is valid
C
              CALL PBD_IS_VALID ( TOKEN,VALID_HOOK,TOTAL_HOOKS,OFFSET)
              IF ( OFFSET .GT. 0 ) THEN
                NUMHOOK = NUMHOOK + 1
                HOOK_NAME ( NUMHOOK ) = TOKEN
                HOOK_NAME_LEN( NUMHOOK ) = LENGTH
                POS = STRTPOS + LENGTH + 1
                CALL PBD_NEXT_WORD ( INPBUF (I)(POS: ), TOKEN, STRTPOS,
     &             LENGTH  )
C
C             Check if valid action specification
C
                IF ( TOKEN .EQ. 'ABORT' ) THEN
                  ACTION (NUMHOOK ) = 2
                ELSE IF ( TOKEN .EQ. 'IGNORE' ) THEN
                  ACTION (NUMHOOK ) = 0

                ELSE IF ( TOKEN .EQ. 'SKIP' ) THEN
                  ACTION (NUMHOOK ) = 1

                ELSE IF ( TOKEN .EQ. 'SKIP_IF_TRUE' ) THEN
                  ACTION (NUMHOOK ) = 3

                ELSE                      ! Error in action field

                  ERROR_MSG = ERROR_BUF(2)(1:28)//HOOK_NAME(NUMHOOK)
                  ERROR_FLAG = .TRUE.
                  CALL PBD_MSG( ERROR_MSG)
                END IF
C
C             Get alias hook name if exists
C
                POS = POS + STRTPOS + LENGTH - 1
                CALL PBD_NEXT_WORD ( INPBUF (I)(POS: ), TOKEN,
     &             POS, LENGTH )
                IF ( LENGTH .GT. 0 ) THEN
                  SUB_NAME(NUMHOOK) = TOKEN
                  SUB_NAME_LEN(NUMHOOK) = LENGTH
                END IF

              ELSE
C
C             Invalid hook name, send error message
C
                ERROR_MSG = ERROR_BUF(3)(1:21)//TOKEN
                ERROR_FLAG = .TRUE.
                CALL PBD_MSG ( ERROR_MSG )
C
              END IF

            ELSE
C
C           No hook name, send error message
C
              ERROR_FLAG = .TRUE.
              CALL PBD_MSG ( ERROR_BUF(4) )

            END IF

          END IF

        END DO
C
C     Check the combined package name provided. If not, no need to read.
C
      ELSE IF ( FILE_TYPE .EQ. COMBINED ) THEN
C
C       Determine combined package file name
C
        USER_FILE_NAME = COMB_PACK_NAME(1:COMB_PACK_LEN)//
     &              '_COMBINED.PBD'
C
C       Check if the file exists in user directory
C
        USER_FILE = PBD_FILE_OK ( USER_FILE_NAME)
C
C       Check if the file exists in the library directory
C
        LIB_FILE = .FALSE.
        IF ( QUALFLAG(LIBRARY_QUAL) ) THEN
          LIB_FILE_NAME = LIBRARY_NAME (1: LIBRARY_LEN)
     &                    //USER_FILE_NAME
          LIB_FILE = PBD_FILE_OK ( LIB_FILE_NAME )
        END IF

C
C       Check if the file exists in the PBD library
C
        PBD_FILE_NAME = 'D0$PBD:'//USER_FILE_NAME
        PBD_FILE = PBD_FILE_OK ( PBD_FILE_NAME )
C
C       Open the combined package file and read
C
        IF ( LIB_FILE ) THEN
          FILE_NAME = LIB_FILE_NAME
        ELSE IF ( USER_FILE ) THEN
          FILE_NAME = USER_FILE_NAME
        ELSE IF ( PBD_FILE ) THEN
          FILE_NAME = PBD_FILE_NAME
        ELSE
C
C         Combined PBD file does not exist, send error message
C
          FILE_LEN = COMB_PACK_LEN + 13
          ERROR_MSG = '-E-'//USER_FILE_NAME(1:FILE_LEN)//ERROR_BUF(6)
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( ERROR_MSG )
          RETURN
        END IF

        CALL D0OPEN(5, FILE_NAME, 'IF', OPENED)
C
C       Check open return status
C
   25   CONTINUE
        IF ( .NOT.OPENED ) THEN       ! Error, send error message
          ERROR_MSG = ERROR_BUF(7)(1:23)//FILE_NAME
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( ERROR_MSG )
          RETURN
        END IF

        ERROR_MSG = MESSAGE(3)(1:37)//FILE_NAME
        CALL PBD_MSG ( ERROR_MSG )

        READ (5, 100, END = 30, ERR = 30, IOSTAT = STATUS )
     &       ( INPBUF(I),I=1,500)
   30   CONTINUE
        CLOSE (5)
        IF ( STATUS .GT. 0 ) THEN       ! Error, send error message
          ERROR_MSG = ERROR_BUF(8)(1:32)//FILE_NAME
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( ERROR_MSG )
          RETURN
        END IF
C
C       Calculate total number of input lines
C
        NUMLINE = I - 1
C
C       Check the combined package name in this file
C
        I = 0
        FOUND = .FALSE.
        DO WHILE ( I .LE. NUMLINE .AND. .NOT. FOUND )
          I = I + 1
          IF ( INPBUF (I) ( 1:1 ) .NE. '!' ) THEN
            CALL PBD_NEXT_WORD (INPBUF(I),TOKEN,POS,LENGTH )
            IF ( LENGTH .GT. 0 ) THEN
              CALL PBD_UP_CASE (TOKEN, TOKEN)
              IF ( TOKEN .EQ. '.NAME' ) FOUND =.TRUE.
            END IF

          END IF
        END DO
C
        IF ( FOUND ) THEN

          POS = POS + 5
          CALL PBD_NEXT_WORD ( INPBUF (I)(POS:), TOKEN, POS, LENGTH )
          IF ( LENGTH .NE. 0 .AND. TOKEN .EQ. COMB_PACK_NAME ) THEN
C
C           Combined package name matches, get package names.
C           First skip lines until '.PACKAGES' found.
C
            END_PACK = .FALSE.
            DO WHILE ( I .LE. NUMLINE .AND. .NOT. END_PACK )
              I = I + 1
              IF ( INPBUF (I) (1:1) .NE. '!' ) THEN
                CALL PBD_NEXT_WORD (INPBUF(I),TOKEN,POS,LENGTH )
                IF ( LENGTH .GT. 0 ) THEN
                  CALL PBD_UP_CASE (TOKEN, TOKEN)
                  IF ( TOKEN .EQ. '.PACKAGES' ) END_PACK=.TRUE.
                END IF
              END IF
            END DO
C
C           If no .PACKAGES command found, send error message
C
            IF ( .NOT. END_PACK ) THEN
              ERROR_FLAG = .TRUE.
              CALL PBD_MSG ( ERROR_BUF(9) )
            END IF

            END_PACK = .FALSE.

            DO WHILE ( I .LE. NUMLINE .AND. .NOT. END_PACK )
C
C             Get the package names and store them in the common
C             data  table.
C
              I = I + 1
              IF ( INPBUF (I) (1:1) .NE. '!' ) THEN
                CALL PBD_NEXT_WORD (INPBUF(I),TOKEN,POS,LENGTH )
                IF ( LENGTH .GT. 0 ) THEN
                  CALL PBD_UP_CASE (TOKEN, TOKEN)
                  IF ( TOKEN .NE. '.END' ) THEN
C
C                   Check if this package already exists.
C
                    IF ( NUMPACK .GT. 0 ) THEN
                      CALL PBD_IS_VALID (TOKEN,PACKAGE_NAME,NUMPACK,
     &                  OFFSET)
                    ELSE                ! No entry in package name table
                      OFFSET = 0
                    END IF

                    IF ( OFFSET .EQ. 0 ) THEN
                      NUMPACK = NUMPACK + 1
                      PACKAGE_NAME(NUMPACK) = TOKEN
                      PACK_NAME_LEN(NUMPACK) = LENGTH
                    ELSE
C
C                     This package already exists, sent a warning message
C
                      ERROR_MSG = ERROR_BUF(12)(1:26)//
     &                            TOKEN(1:LENGTH)//' - Ignored'
                      CALL PBD_MSG ( ERROR_MSG )
                    END IF

                  ELSE
                    END_PACK = .TRUE.
                  END IF

                END IF

              END IF
            END DO
C
C           If no .END PACKAGES command found, send error message
C
            IF ( .NOT. END_PACK ) THEN
              ERROR_FLAG = .TRUE.
              CALL PBD_MSG ( ERROR_BUF(10) )
            END IF
C
C           Get zebra bank size if provided
C
            DO WHILE ( I .LE. NUMLINE )
C
              I = I + 1
              IF ( INPBUF (I) (1:1) .NE. '!' ) THEN
                CALL PBD_NEXT_WORD (INPBUF(I),TOKEN,POS,LENGTH )
                IF ( LENGTH .GT. 0 ) THEN
                  CALL PBD_UP_CASE (TOKEN, TOKEN)
                  IF ( TOKEN .EQ. '.ZEBCOM' ) THEN
C
                    POS = POS + 7
                    CALL PBD_NEXT_WORD (INPBUF(I)(POS:),TOKEN,POS,
     &                                  LENGTH )
C
C                   If ZEBCOM size not defined by command qualifier, then
C                   convert the ASCII number to integer format and store
C
                    IF ( ZEBCOM_LEN .EQ. 0 ) THEN
                      COM_NAME = 'ZEBCOM'
                      CALL PBD_GET_COM( TOKEN,LENGTH, COM_NAME(1:6),
     &                  ZEBCOM_SIZE, ZEBCOM_LEN, RETFLAG )
                      IF ( RETFLAG ) ERROR_FLAG = .TRUE.
                    END IF

                  ELSE IF ( TOKEN .EQ. '.ZEBSTP' ) THEN
C
                    POS = POS + 7
                    CALL PBD_NEXT_WORD (INPBUF(I)(POS:),TOKEN,POS,
     &                                  LENGTH )
C
C                   If ZEBSTP size not defined by command qualifier, then
C                   convert the ASCII number to integer format and store
C
                    IF ( ZEBSTP_LEN .EQ. 0 ) THEN
                      COM_NAME = 'ZEBSTP'
                      CALL PBD_GET_COM( TOKEN,LENGTH, COM_NAME(1:6),
     &                  ZEBSTP_SIZE, ZEBSTP_LEN, RETFLAG )
                      IF ( RETFLAG ) ERROR_FLAG = .TRUE.
                    END IF

                  ELSE IF ( TOKEN .EQ. '.ZEBWRK' ) THEN
C
                    POS = POS + 7
                    CALL PBD_NEXT_WORD (INPBUF(I)(POS:),TOKEN,POS,
     &                                  LENGTH )
C
C                   If ZEBWRK size not defined by command qualifier, then
C                   convert the ASCII number to integer format and store
C
                    IF ( ZEBWRK_LEN .EQ. 0 ) THEN
                      COM_NAME = 'ZEBWRK'
                      CALL PBD_GET_COM( TOKEN,LENGTH, COM_NAME(1:6),
     &                  ZEBWRK_SIZE, ZEBWRK_LEN, RETFLAG )
                      IF ( RETFLAG ) ERROR_FLAG = .TRUE.
                    END IF

                  ELSE IF ( TOKEN .EQ. '.PAWC' ) THEN
C
                    POS = POS + 5
                    CALL PBD_NEXT_WORD (INPBUF(I)(POS:),TOKEN,POS,
     &                                  LENGTH )
C
C                   If PAWC size not defined by command qualifier, then
C                   convert the ASCII number to integer format and store
C
                    IF ( PAWC_LEN .EQ. 0 ) THEN
                      COM_NAME = 'PAWC'
                      CALL PBD_GET_COM( TOKEN,LENGTH, COM_NAME(1:4),
     &                  PAWC_SIZE, PAWC_LEN, RETFLAG )
                      IF ( RETFLAG ) ERROR_FLAG = .TRUE.
                    END IF

                  ELSE IF ( TOKEN .EQ. '.GCBANK' ) THEN
C
                    POS = POS + 7
                    CALL PBD_NEXT_WORD (INPBUF(I)(POS:),TOKEN,POS,
     &                                  LENGTH )
C
C                   If GCBANK size not defined by command qualifier, then
C                   Convert the ASCII number to integer format and store
C
                    IF ( GCBANK_LEN .EQ. 0 ) THEN

                      COM_NAME = 'GCBANK'
                      CALL PBD_GET_COM( TOKEN,LENGTH, COM_NAME(1:6),
     &                  GCBANK_SIZE, GCBANK_LEN, RETFLAG )
                      IF ( RETFLAG ) ERROR_FLAG = .TRUE.
                    END IF

                  END IF        ! End of zebra bank processing

                END IF          ! End of token length checking

              END IF            ! End of comment line checking

            END DO              ! End of entire input buffer processing

          ELSE
C
C           Name in the combined PBD file is not matching with the input name
C
            ERROR_FLAG = .TRUE.
            CALL PBD_MSG ( ERROR_BUF(11) )

          END IF                ! End of Combined name checking

        END IF                  ! Combined name key word checking

      END IF                    ! End of file type checking

      RETURN
      END
