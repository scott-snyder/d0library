      SUBROUTINE PBD_READ_PACK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine reads individual PBD package file,
C-                         validates the data and saves them in the common
C-                         data area for later processing.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: Common error flag ERROR_FLAG
C-            ( .TRUE. if fatal error, .FALSE. if no error )
C-
C-   Modules called by this routine:  PBD_CHK_DUP, PBD_FILE_OK, PBD_IS_VALID,
C-                                    PBD_MSG, PBD_NEXT_WORD, PBD_REM_BLANK,
C-                                    PBD_UP_CASE
C-
C-   Created  09-JUL-1991   Hyon Joo Kehayias
C-   Updated  17-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-   Updated  26-FEB-1991 for RCP option processing.  H. Kehayias
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'

      INTEGER*4 STATUS                  ! I/O RETURN STATUS
      INTEGER*2  STRTPOS,POS            ! STARTING POSITION OF TOKEN
      INTEGER*2  LENGTH                 ! TOKEN LENGTH
      INTEGER*2  NUMLINE                ! # OF INPUT LINES
      INTEGER*2  PACK_NUM               ! PACKEAGE # BEING PROCESSED
      INTEGER*2  PACKINDX               ! PACKEAGE # FOR DUPLICATE INT NAME
      INTEGER*2  INTINDX                ! HOOK # FOR DUPLICATE INT NAME
      INTEGER*2  FILE_LEN               ! LENGTH OF FILE NAME STRING
      INTEGER*2  LOC_RCP_LEN            ! LENGTH OF LOCAL RCP STRING
      INTEGER*2  LOC_OPT_LEN            ! LENGTH OF LOCAL RCP OPTION STRING
      INTEGER*2  OPT_POS                ! RCP OPTION WORD POSITION
      INTEGER*2  OFFSET                 ! OFFSET TO FRAMEWORK HOOK NAME TABLE
      INTEGER*2  I,J                    ! INDEX VARIABLES

      CHARACTER*32 TOKEN                ! TOKEN
      CHARACTER*80 FILE_NAME            ! FILE NAME
      CHARACTER*80 USER_FILE_NAME       ! USER FILE NAME
      CHARACTER*80 LIB_FILE_NAME        ! FULL LIBRARY FILE NAME
      CHARACTER*80 PBD_FILE_NAME        ! FULL PBD FILE NAME
      CHARACTER*80 ERROR_BUF(9)         ! ERROR MESSAGES
      CHARACTER*80 MESSAGE              ! MESSAGE BUFFER
      CHARACTER*80 MSG_BUF              ! ERROR MESSAGE BUFFER
      CHARACTER*80 LONGWORD             ! VERY LONG WORD
      CHARACTER*80 LOC_RCP              ! LOCAL RCP FILE NAME
      CHARACTER*32 LOC_OPT              ! LOCAL RCP OPTION
      CHARACTER*32 OPT_WORD             ! RCP OPTION WORD
      CHARACTER*1  COMMA                ! COMMA

      LOGICAL USER_FILE                 ! USER FILE INDICATOR
      LOGICAL LIB_FILE                  ! LIBRARY FILE INDICATOR
      LOGICAL PBD_FILE                  ! PBD FILE INDICATOR
      LOGICAL PACK_OK                   ! PACKAGE FRAME NAME VALID FLAG
      LOGICAL FOUND,END_FRAME           ! LOOP CONTROL VARIABLES
C
      LOGICAL    PBD_FILE_OK            ! LOGICAL FUNCTION FOR FILE SEARCH
      LOGICAL OPENED                    ! OPEN STATUS
C
      DATA COMMA /','/
      DATA ERROR_BUF /
     &     '-E-Open file error for ',
     &     ' package file does not exist ',
     &     '-E-Framework name not found in the package file ',
     &     '-E-Error while reading package file ',
     &     '-E-Missing .FRAMES command',
     &     '-E-Missing .END FRAMES command',
     &     '-E-Invalid hook name ',
     &     '-E-Duplicate Interface ',
     &     '-E-Conflict RCP option '/
C
      DATA MESSAGE /
     &     '-- Reading package file '/

      ERROR_FLAG = .FALSE.
C
      IF ( NUMPACK .EQ. 0 ) RETURN  ! No package names entered, return

      DO PACK_NUM = 1, NUMPACK          ! For each package
C
C       Build the package file name to read
C
        USER_FILE_NAME = PACKAGE_NAME(PACK_NUM)
     &       (1:PACK_NAME_LEN(PACK_NUM))//'.PBD'
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
C       Determine which package file to read
C
        IF ( LIB_FILE ) THEN
          FILE_NAME = LIB_FILE_NAME
        ELSE IF ( USER_FILE ) THEN
          FILE_NAME = USER_FILE_NAME
        ELSE IF ( PBD_FILE ) THEN
          FILE_NAME = PBD_FILE_NAME
        ELSE
C
C         Package PBD file does not exist, send error message
C
          FILE_LEN = PACK_NAME_LEN(PACK_NUM) + 4
          MSG_BUF = '-E-'//USER_FILE_NAME(1:FILE_LEN)//ERROR_BUF(2)
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( MSG_BUF )
          RETURN
        END IF
C
C       Open the package file and read
C
        CALL D0OPEN(5, FILE_NAME, 'IF', OPENED)
   10   IF ( .NOT.OPENED ) THEN    ! Open error
          MSG_BUF = ERROR_BUF(1)(1:23)//FILE_NAME
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( MSG_BUF )
          RETURN
        END IF

        MSG_BUF = MESSAGE(1:24)//FILE_NAME
        CALL PBD_MSG(MSG_BUF)
        DO i = 1, 500
          READ (5, 100, END = 20, ERR = 20, IOSTAT = STATUS )
     &       INPBUF(I)
        ENDDO
  100   FORMAT (A)
   20   CONTINUE
        NUMLINE = I - 1
        CLOSE (5)
C
C       Check I/O return status.  EOF if STATUS = -1.
C       Error condition If STATUS > 0.  If any error, send error message
C
        IF ( STATUS .GT. 0 ) THEN
          MSG_BUF = ERROR_BUF(4)(1:37)//FILE_NAME
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( MSG_BUF )
          RETURN
        END IF
C
C       First skip lines until '.FRAMES' found
C
        FOUND = .FALSE.
        I = 0
        DO WHILE ( I .LE. NUMLINE .AND. .NOT. FOUND )
          I = I + 1
          IF ( INPBUF (I) ( 1:1 ) .NE. '!' ) THEN
            CALL PBD_NEXT_WORD ( INPBUF (I), TOKEN, POS, LENGTH )
            IF ( LENGTH .GT. 0 ) THEN
              CALL PBD_UP_CASE ( TOKEN, TOKEN)
              IF (TOKEN .EQ. '.FRAMES' ) THEN
                FOUND = .TRUE.
              END IF
            END IF
          END IF
        END DO
C
C       If no .FRAMES command found, send error message
C
        IF ( .NOT. FOUND ) THEN
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( ERROR_BUF (5) )
        END IF
C
C       Check the frame name in this file
C
        END_FRAME = .FALSE.
        PACK_OK = .FALSE.
        DO WHILE ( I .LT. NUMLINE .AND. .NOT. END_FRAME )
          I = I + 1
          IF ( INPBUF (I) ( 1:1 ) .NE. '!' ) THEN
            CALL PBD_NEXT_WORD ( INPBUF (I), TOKEN, POS, LENGTH )
            IF ( LENGTH .GT. 0 ) THEN
              CALL PBD_UP_CASE ( TOKEN, TOKEN )
              IF (TOKEN .EQ. FRAME_NAME ) THEN
                PACK_OK = .TRUE.
              ELSE IF ( TOKEN .EQ. '.END' ) THEN
                END_FRAME = .TRUE.
              END IF
            END IF
          END IF
        END DO
C
C       If no .END FRAMES command found, send error message
C
        IF ( .NOT. END_FRAME ) THEN
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( ERROR_BUF (6) )
        END IF
C
        IF ( PACK_OK ) THEN
C
C         Frame name in the package file matches, process hooks
C
          DO WHILE ( I .LT. NUMLINE )

            I = I + 1
            IF ( INPBUF (I) (1:1) .NE. '!' ) THEN
              CALL PBD_NEXT_WORD (INPBUF(I),TOKEN,POS,LENGTH )
              IF ( LENGTH .GT. 0 ) THEN
                CALL PBD_UP_CASE (TOKEN, TOKEN)
                IF ( TOKEN .EQ. '.HOOKS' ) THEN
C
C                 Hook name follows.  Get hook and interface routine name
C
                  J = I + 1
                  DO WHILE ( J .LE. NUMLINE .AND.
     &                INDEX ( INPBUF(J),'.END' ) .EQ. 0 )
                    IF ( INPBUF (J) (1:1) .NE. '!' ) THEN
                      CALL PBD_NEXT_WORD (INPBUF(J),TOKEN,POS,LENGTH )
                      IF ( LENGTH .GT. 0 ) THEN

                        CALL PBD_UP_CASE (TOKEN, TOKEN)
C
C                       Check if valid hook name
C
                        CALL PBD_IS_VALID ( TOKEN, HOOK_NAME,
     &                     NUMHOOK, OFFSET )
                        IF ( OFFSET .GT. 0 ) THEN  ! Valid hook name
C
C                         Get interface routine name if exists
C
                          CALL PBD_NEXT_WORD (INPBUF(J)(POS+LENGTH:),
     &                      TOKEN,POS,LENGTH )
                          IF ( LENGTH .GT. 0 ) THEN
                            CALL PBD_UP_CASE (TOKEN, TOKEN)
C
C                           Check if duplicate interface name
C
                            CALL PBD_CHK_DUP (TOKEN(1:LENGTH),
     &                            PACK_NUM, PACKINDX, INTINDX )
                            IF ( PACKINDX .EQ. 0 )THEN
                              INT_NAME(PACK_NUM,OFFSET)= TOKEN
                              INT_NAME_LEN(PACK_NUM, OFFSET) = LENGTH
                            ELSE
C
C                             The interface used in another package, send a
C                             warning message
C
                              IF ( INTINDX .EQ. OFFSET ) THEN
                                MSG_BUF = '-W-'//ERROR_BUF(8)(4:23)//
     &                                  TOKEN(1:LENGTH)//
     &                                  ' found in the package '//
     &                                  PACKAGE_NAME(PACKINDX)(1:
     &                                  PACK_NAME_LEN(PACKINDX))//
     &                                  ' -- Ignored'
                                CALL PBD_MSG ( MSG_BUF )

                              ELSE
C
C                               Same interface name used for different hook,
C                               Send error message
C
                                MSG_BUF = ERROR_BUF(8)(1:23)//
     &                                  TOKEN(1:LENGTH)//
     &                                  ' in the package '//
     &                                  PACKAGE_NAME(PACKINDX)(1:
     &                                  PACK_NAME_LEN(PACKINDX))//
     &                                  ' hook '//HOOK_NAME(OFFSET)
     &                                  (1:HOOK_NAME_LEN(OFFSET) )
                                ERROR_FLAG = .TRUE.
                                CALL PBD_MSG ( MSG_BUF )
                              END IF

                            END IF
                          END IF

                        ELSE
C
C                         Hook name not found in framework hook name table,
C                         Check against the master hook name table
C
                          CALL PBD_IS_VALID ( TOKEN, VALID_HOOK,
     &                      TOTAL_HOOKS, OFFSET )
                          IF ( OFFSET .EQ. 0 ) THEN

C                           Invalid hook name, send error message
C
                            MSG_BUF = ERROR_BUF(7)(1:21)//TOKEN
                            ERROR_FLAG = .TRUE.
                            CALL PBD_MSG ( MSG_BUF )
                          END IF
                        END IF

                      END IF

                    END IF
C
                    J = J + 1

                  END DO
C
C                 Update the buffer pointer
C
                  I = J
C
C                 Hook name list compeleted
C
                ELSE IF ( TOKEN .EQ. '.INPUT_BANKS' ) THEN
C
C                 Skip lines until '.END' found.  No processing for
C                 input banks.
C
                  DO WHILE ( I .LE. NUMLINE .AND.
     &                 INDEX ( INPBUF(I),'.END' ) .EQ. 0 )
                    I = I + 1
                  END DO
C
C                 Input banks lines completed
C
                ELSE IF ( TOKEN .EQ. '.OUTPUT_BANKS' ) THEN
C
C                 Skip lines until '.END' found.  No processing for
C                 output banks.
C
                  DO WHILE ( I .LE. NUMLINE .AND.
     &                 INDEX ( INPBUF(I),'.END' ) .EQ. 0 )
                    I = I + 1
                  END DO
C
                ELSE IF ( TOKEN .EQ. '.OBJECTS' ) THEN
C
C                 Save object file names.
C
                  I = I + 1
                  NUMOBJ (PACK_NUM) = 0     ! Initialize # of object files
                  DO WHILE ( I .LE. NUMLINE .AND.
     &                 INDEX ( INPBUF(I),'.END' ) .EQ. 0 )
                    IF ( INPBUF (I) (1:1) .NE. '!' ) THEN
                      CALL PBD_NEXT_WORD (INPBUF(I),LONGWORD,POS,
     &                     LENGTH )
                      IF ( LENGTH .GT. 0 ) THEN
                        CALL PBD_UP_CASE (LONGWORD,LONGWORD)
                        NUMOBJ (PACK_NUM) = NUMOBJ(PACK_NUM) + 1
                        OBJ_FILE(PACK_NUM,NUMOBJ(PACK_NUM)) = LONGWORD
                        OBJ_FILE_LEN(PACK_NUM,NUMOBJ(PACK_NUM))=LENGTH
                      END IF
                    END IF
                    I = I + 1
                  END DO
C
                ELSE IF ( TOKEN .EQ. '.RCP' ) THEN
C
C                 Save RCP file lines.
C
                  I = I + 1

                  DO WHILE ( I .LE. NUMLINE .AND.
     &                 INDEX ( INPBUF(I),'.END' ) .EQ. 0 )
                    IF ( INPBUF (I) (1:1) .NE. '!' ) THEN
                      CALL PBD_NEXT_WORD (INPBUF(I),LONGWORD,POS,
     &                     LENGTH )
                      IF ( LENGTH .GT. 0 ) THEN
                        CALL PBD_UP_CASE (LONGWORD,LONGWORD)
                        LOC_RCP = LONGWORD
                        LOC_RCP_LEN = LENGTH
C
C                       Get RCP option word if exists
C
                        LONGWORD = INPBUF(I)(LENGTH+1:)
                        CALL PBD_REM_BLANK(LONGWORD,LENGTH)
                        CALL PBD_NEXT_WORD (LONGWORD(1:LENGTH),
     &                     TOKEN, POS, LENGTH )
                        IF ( LENGTH .GT. 0 ) THEN
                          CALL PBD_UP_CASE (TOKEN,TOKEN)
                        END IF

                        LOC_OPT = TOKEN
                        LOC_OPT_LEN = LENGTH
C
C                       Check if duplicate RCP file name
C
                        IF ( NUMRCP .GT. 0 ) THEN
                          CALL PBD_IS_VALID ( LOC_RCP, RCP_FILE,
     &                                        NUMRCP, OFFSET)
                        ELSE
                          OFFSET = 0
                        END IF

C
C                       Save RCP file name if not duplicate
C
                        IF ( OFFSET .EQ. 0 ) THEN
                          NUMRCP = NUMRCP + 1
                          RCP_FILE (NUMRCP) = LOC_RCP
                          RCP_FILE_LEN(NUMRCP)=LOC_RCP_LEN
                          RCP_OPT(NUMRCP) = LOC_OPT
                          RCP_OPT_LEN(NUMRCP) = LOC_OPT_LEN
                        ELSE
C
C                        Duplicate, check the RCP options. If new option,
C                        add to the RCP option table
C
                          IF ( LOC_OPT_LEN .GT. 0 ) THEN
C
C                         If no option defined, just save the current option
C
                            IF ( RCP_OPT_LEN(OFFSET) .EQ. 0 ) THEN
                              RCP_OPT(OFFSET) = LOC_OPT
                              RCP_OPT_LEN(OFFSET) = LOC_OPT_LEN
                            ELSE

                              J = 1
                              DO WHILE ( J .LE. LOC_OPT_LEN)
                                OPT_POS = INDEX( LOC_OPT(J:LOC_OPT_LEN),
     &                                       COMMA)
                                IF (OPT_POS .GT. 0 ) THEN
                                  OPT_WORD = LOC_OPT(J:J+OPT_POS-2)
                                  OPT_POS =  OPT_POS - 1
                                ELSE
                                  OPT_WORD = LOC_OPT(J:LOC_OPT_LEN)
                                  OPT_POS = LOC_OPT_LEN - J + 1
                                END IF

                                IF ( OPT_POS .GT. 0 .AND.
     &                            INDEX ( RCP_OPT(OFFSET),
     &                            OPT_WORD(1:OPT_POS)) .EQ. 0 ) THEN
                                  RCP_OPT(OFFSET) = RCP_OPT(OFFSET)
     &                              (1:RCP_OPT_LEN(OFFSET) )
     &                              //COMMA//OPT_WORD
                                  RCP_OPT_LEN(OFFSET) =
     &                              RCP_OPT_LEN(OFFSET) + 1 + OPT_POS
                                END IF
                                J = J + OPT_POS + 1
                              END DO
                            ENDIF
                          END IF

                        END IF

                      END IF
                    END IF
                    I = I + 1
                  END DO
                END IF

              END IF                    ! End of token length checking
            END IF                      ! End of comment line checking

          END DO                        ! End of input buffer processing

        ELSE
C
C         The frame name in the package file does not match with the
C         current frame name, send error message and return to caller
C

          ERROR_FLAG = .TRUE.
          MSG_BUF = ERROR_BUF(3)(1:48)//USER_FILE_NAME
          CALL PBD_MSG ( MSG_BUF )
        END IF                          ! End of valid package processing
      END DO                            ! End of all package processing
      RETURN
      END
