      SUBROUTINE PBD_BLD_LINK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates PBD link command file and option files 
C-                         to be used with the link file.  The link commands
C-                         are based on the framework link file.  The OPT files
C-                         are generated from the object library names  provided
C-                         by the package files.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: Common error flag ERROR_FLAG
C-            ( .TRUE. if fatal error, .FALSE. if no error ) 
C-
C-   Modules called by this routine:  PBD_MSG, PBD_NEXT_WORD, PBD_PARSE_FILE
C-                                    PBD_REM_BLANK
C-  
C-   Based on the PASCAL procedure Build_Link_File of the old
C-   Program Builder.
C-
C-   Created  22-AUG-1991   Hyon Joo Kehayias
C-   Updated  17-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-   Updated  04-SEP-1992   H. Kehayias 
C-   ( To remove duplicate OLB's in sequence from OPT file ) 
C-                         
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'

      INTEGER*4    STATUS               ! ERROR RETURN STATUS
      INTEGER*2    FILE_NAME_LEN        ! FILE NAME LENGTH
      INTEGER*2    LENGTH               ! TOKEN LENGTH
      INTEGER*2    NUMLINE              ! # OF LINES TO PROCESS
      INTEGER*2    POS                  ! TOKEN POSITION
      INTEGER*2    NODE_LEN             ! NODE NAME LENGTH
      INTEGER*2    DEV_LEN              ! DEVICE NAME LENGTH
      INTEGER*2    DIR_LEN              ! DIRECOTRY SPEC STRING LENGTH
      INTEGER*2    TYPE_LEN             ! FILE TYPE STRING LENGTH
      INTEGER*2    I, J                 ! INDEX VARIABLES
      CHARACTER*80 ERROR_BUF(2)         ! ERROR MESSAGES
      CHARACTER*80 MSG_BUF              ! MESSAGE BUFFER
      CHARACTER*80 FILE_NAME            ! FILE NAME TO OPEN
      CHARACTER*80 TOKEN                ! TOKEN
      CHARACTER*80 DEF_DIR              ! CURRENT DEFAULT DIRECTORY SPEC
      CHARACTER*20 DIR_SPEC             ! DIRECTORY SPEC
      CHARACTER*80 OPTHEADER (2)        ! OPT FILE HEADER MESSAGE
      CHARACTER*22 MESSAGE(2)           ! MESSAGES TO SEND
      CHARACTER*19 SRC_OBJ_SYMB         ! OBJECT FILE SYMBOL
      CHARACTER*14 USER_LIB_SYMB        ! USER LIBRARY FILE SYMBOL
      CHARACTER*132 OUTBUF(200)         ! OUTPUT BUFFER
      CHARACTER*132 OUTBUF1(200)        ! SECOND OUTPUT BUFFER
      CHARACTER*132 LINE                ! LINE BUFFER
      
      CHARACTER*80 FILE_SPEC            ! FILE SPECIFICATION TO PARSE
      CHARACTER*80 NODE_NAME            ! NODE NAME
      CHARACTER*80 DEV_NAME             ! DEVICE NAME
      CHARACTER*80 DIR_NAME             ! DIRECTORY SPECIFICATION
      CHARACTER*20 FILE_TYPE            ! FILE TYPE
      LOGICAL      RET_FLAG             ! PARSE RETURN STATUS FLAG
      LOGICAL      EOF                  ! END OF FILE FLAG
      LOGICAL      OPENED               ! OPEN STATUS

      DATA MESSAGE /'-- Building Link File ',
     &              '-- Building OPT Files '/
      DATA SRC_OBJ_SYMB /'pgbd_sources_object' /
      DATA USER_LIB_SYMB /'user_libraries'/
      DATA ERROR_BUF /
     &     '-E-File Open error for ',
     &     '-E-Error detected while reading '/
      DATA OPTHEADER /
     &'!******************************************************!',
     &'!     Generated by the Program Builder                 !'/

      MSG_BUF = MESSAGE(1)//FOR_FILE_NAME(1:FOR_FILE_LEN)//'.LNK'
      CALL PBD_MSG ( MSG_BUF )
C
C     Determine frame LNK file name
C
      FILE_NAME = 'D0$'//FRAME_NAME(1:FRAME_LEN)//':'//
     &             FRAME_NAME(1:FRAME_LEN)//'.LNK'
C
C     Open 'frame_name'.LNK file
C
      CALL D0OPEN(5, FILE_NAME, 'IF', OPENED)
 10   IF ( .NOT.OPENED ) THEN    ! Open error
        MSG_BUF = ERROR_BUF(1)(1:23)//FILE_NAME
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF
C
C     Open PBD LNK file
C
      FILE_NAME = FOR_FILE_NAME(1:FOR_FILE_LEN)//'.LNK'

      CALL D0OPEN(8, FILE_NAME, 'OFL', OPENED)
 20   IF ( .NOT.OPENED ) THEN    ! Open error
        MSG_BUF = ERROR_BUF(1)(1:23)//FILE_NAME
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF
C
C     Write a header for .LNK file being generated
C
      WRITE (8,100) OPTHEADER(1)
      OUTLINE = '!     '//'File Name : '//FILE_NAME 
      OUTLINE(56:56) = '!'
      WRITE (8,100) OUTLINE
      WRITE (8,100) OPTHEADER(2)
      OUTLINE = '! '//DATE_TIME(3: )
      OUTLINE(56:56) = '!'
      WRITE (8,100) OUTLINE
      WRITE (8,100) OPTHEADER(1)

      OUTLINE = '$ TASK = "'//
     & COMBINED_NAME(1:COMBINED_LEN) //  '"'
      WRITE (8,100) OUTLINE
100   FORMAT ( A )
C 
C     Get the default directory name
C
      FILE_SPEC = '[]'
      CALL PBD_PARSE_FILE ( FILE_SPEC, NODE_NAME, NODE_LEN,
     &  DEV_NAME, DEV_LEN,DIR_NAME,DIR_LEN,FILE_NAME,
     &  FILE_NAME_LEN,FILE_TYPE,TYPE_LEN, RET_FLAG )

      DEF_DIR = DEV_NAME(1:DEV_LEN)//DIR_NAME(1:DIR_LEN) 
     
      OUTLINE = '$ DefDir = "'// DEF_DIR (1:DEV_LEN+DIR_LEN) // '"'
      WRITE (8,100) OUTLINE
C
C     Add the object file genrated by PBD
C

      OUTLINE = '$ '// SRC_OBJ_SYMB //
     &          ' = DefDir + "''DEB'''//
     &          FOR_FILE_NAME(1:FOR_FILE_LEN) //
     &          '.OBJ,"'
      WRITE (8,100) OUTLINE
      OUTLINE = '$ SHO SYMB '//SRC_OBJ_SYMB
      WRITE (8,100) OUTLINE

      OUTLINE = '$ '// USER_LIB_SYMB// ' = -'
      WRITE (8,100) OUTLINE

      NUMLINE = 0
      DO I = 1, NUMPACK                 ! For each package

        DO J = 1, NUMOBJ(I)             ! For each objec file

          IF ( INDEX ( OBJ_FILE(I,J), '.OLB') .GT. 0 .OR.
     &         INDEX ( OBJ_FILE(I,J), '/L' ) .GT. 0 ) THEN
C
C           Extract directory specification and file name
C
            POS = INDEX(OBJ_FILE(I,J),'/')
            IF ( POS .GT. 0 ) THEN
C
C             Remove the qualifier from file spec
C
              FILE_SPEC = OBJ_FILE(I,J)(1:POS-1)
            ELSE
              FILE_SPEC = OBJ_FILE(I,J)
            END IF

            CALL PBD_PARSE_FILE ( FILE_SPEC, NODE_NAME, NODE_LEN,
     &        DEV_NAME, DEV_LEN,DIR_NAME,DIR_LEN,FILE_NAME,
     &        FILE_NAME_LEN,FILE_TYPE,TYPE_LEN, RET_FLAG )
C
C           Check PARSE return status
C
            IF ( .NOT. RET_FLAG ) THEN

              NUMLINE = NUMLINE + 1
              OUTBUF ( NUMLINE ) =  OBJ_FILE(I,J)
C
C             Get directory spec length from the input file spec
C
              IF ( TYPE_LEN .EQ. 1 ) TYPE_LEN = 0
              IF ( POS .GT. 0 ) THEN
                LENGTH = OBJ_FILE_LEN(I,J) - ( FILE_NAME_LEN + 
     &                 TYPE_LEN )- ( OBJ_FILE_LEN(I,J) - POS + 1 )
              ELSE
                LENGTH = OBJ_FILE_LEN(I,J) - ( FILE_NAME_LEN +
     &                 TYPE_LEN )
              END IF
 
              LINE = FILE_SPEC(1:LENGTH)//'DEB_' //
     &             OBJ_FILE(I,J)(LENGTH+1:OBJ_FILE_LEN(I,J))
              OUTBUF1 ( NUMLINE ) =  LINE
            END IF

          ELSE        
C
C           Check if this is an .OPT file 
C
            POS = INDEX ( OBJ_FILE(I,J), '.OPT' )
            IF ( POS .GT. 0 ) THEN
              FILE_NAME = OBJ_FILE(I,J)(1:POS+3)
   
            ELSE
C
C             Check if the qualifier '/OPT' exists
C
              POS = INDEX ( OBJ_FILE(I,J), '/OPT' )
              IF ( POS .GT. 0 ) THEN
                FILE_NAME = OBJ_FILE(I,J)(1:POS-1)//'.OPT'
              END IF
            END IF

            IF ( POS .GT. 0 ) THEN
C
C             Open the .OPT file and read
C
              CALL D0OPEN(7, FILE_NAME, 'IF', OPENED)
 30           IF ( .NOT.OPENED ) THEN    ! Open error
                MSG_BUF = ERROR_BUF(1)(1:23)//FILE_NAME
                ERROR_FLAG = .TRUE.
                CALL PBD_MSG ( MSG_BUF )
                RETURN
              END IF

              EOF = .FALSE.
              DO WHILE ( .NOT. EOF )

                READ  ( 7,100, END = 40, ERR = 40, 
     &                IOSTAT = STATUS ) LINE
 40             IF ( STATUS .EQ. 0 ) THEN 
C
C                Check if this is a comment line
C
                 IF ( LINE(1:1) .NE. '!') THEN
                  CALL PBD_NEXT_WORD ( LINE,TOKEN,POS,LENGTH)
C
C                 Extract directory specification and file name
C
                  POS = INDEX(TOKEN, '/')
                  IF ( POS .GT. 0 ) THEN
C
C                   Remove the qualifier from file spec
C
                    FILE_SPEC = TOKEN (1:POS-1)
                  ELSE
                    FILE_SPEC = TOKEN
                  END IF

                  CALL PBD_PARSE_FILE ( FILE_SPEC, NODE_NAME, NODE_LEN,
     &              DEV_NAME, DEV_LEN,DIR_NAME,DIR_LEN,FILE_NAME,
     &              FILE_NAME_LEN,FILE_TYPE,TYPE_LEN, RET_FLAG )
C
C                 Check PARSE return status
C
                  IF ( .NOT. RET_FLAG ) THEN

                    NUMLINE = NUMLINE + 1
                    OUTBUF ( NUMLINE ) =  LINE
C
C                   Get directory spec length from the input file spec
C 
                    IF ( TYPE_LEN .EQ. 1 ) TYPE_LEN = 0
                    IF ( POS .GT. 0 ) THEN
                      LENGTH = LENGTH - ( FILE_NAME_LEN + 
     &                         TYPE_LEN )- ( LENGTH - POS + 1 )
                    ELSE
                      LENGTH = LENGTH - ( FILE_NAME_LEN +
     &                         TYPE_LEN )
                    END IF
 
                    LINE = FILE_SPEC(1:LENGTH)//'DEB_' //
     &                        LINE(LENGTH+1:)
                    OUTBUF1 ( NUMLINE ) =  LINE
                  END IF

                 ELSE
C
C                 Comment line, just copy it to the output buffers
C
                  NUMLINE = NUMLINE + 1
                  OUTBUF ( NUMLINE ) =  LINE
                  OUTBUF1 ( NUMLINE ) =  LINE

                 END IF 

                ELSE IF ( STATUS .LT. 0 ) THEN
                  EOF = .TRUE.

                ELSE
C
C                 Read error, send error message
C
                  MSG_BUF = ERROR_BUF(2)(1:32)//FILE_NAME
                  ERROR_FLAG = .TRUE.
                  CALL PBD_MSG ( MSG_BUF )
                  EOF = .TRUE.

                END IF

              END DO       

              CLOSE  ( 7 )

            ELSE                        ! Neither OLB nor OPT file

              FILE_SPEC = OBJ_FILE(I,J)

              CALL PBD_PARSE_FILE ( FILE_SPEC, NODE_NAME, NODE_LEN,
     &        DEV_NAME, DEV_LEN,DIR_NAME,DIR_LEN,FILE_NAME,
     &        FILE_NAME_LEN,FILE_TYPE,TYPE_LEN, RET_FLAG )
C
C             Check PARSE return status
C
              IF ( .NOT. RET_FLAG ) THEN

                NUMLINE = NUMLINE + 1
                OUTBUF ( NUMLINE ) =  OBJ_FILE(I,J)
C
C               Get directory spec length from the input file spec
C 
                IF ( TYPE_LEN .EQ. 1 ) TYPE_LEN = 0
                LENGTH = OBJ_FILE_LEN(I,J) - ( FILE_NAME_LEN +
     &                 TYPE_LEN )
 
                LINE = FILE_SPEC(1:LENGTH)//'DEB_' //
     &             OBJ_FILE(I,J)(LENGTH+1:OBJ_FILE_LEN(I,J))
                OUTBUF1 ( NUMLINE ) =  LINE
              END IF
 
            END IF

          END IF

        END DO
      END DO
C
C     Send informational messages
C
      MSG_BUF = MESSAGE(2)//FOR_FILE_NAME(1:FOR_FILE_LEN)//'.OPT'//
     &          ' and DEB_'//FOR_FILE_NAME(1:FOR_FILE_LEN)//'.OPT'
      CALL PBD_MSG ( MSG_BUF )
C
C     Open the PBD OPTIONS files 
C
      FILE_NAME = FOR_FILE_NAME(1:FOR_FILE_LEN)//'.OPT'
      CALL D0OPEN(7, FILE_NAME, 'OFL', OPENED)
 50   IF ( .NOT.OPENED ) THEN    ! Open error
        MSG_BUF = ERROR_BUF(1)(1:23)//FILE_NAME
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF
C
C     Write a header for .OPT file being generated
C
      WRITE (7,100) OPTHEADER(1)
      OUTLINE = '!     '//'File Name : '//FILE_NAME 
      OUTLINE(56:56) = '!'
      WRITE (7,100) OUTLINE
C
C     Generate Debug OPT file
C
      FILE_NAME = 'DEB_'//FOR_FILE_NAME(1:FOR_FILE_LEN)//'.OPT'
      CALL D0OPEN(9, FILE_NAME, 'OFL', OPENED)
 60   IF ( .NOT.OPENED ) THEN    ! Open error
        MSG_BUF = ERROR_BUF(1)(1:23)//FILE_NAME
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF
C
C     Write a header for DEBUG .OPT file being generated
C
      WRITE (9,100) OPTHEADER(1)
      OUTLINE = '!     '//'File Name : '//FILE_NAME 
      OUTLINE(56:56) = '!'
      WRITE (9,100) OUTLINE

      WRITE (7,100) OPTHEADER(2)
      WRITE (9,100) OPTHEADER(2)

      OUTLINE = '! '//DATE_TIME(3: )
      OUTLINE(56:56) = '!'
      WRITE (7,100) OUTLINE
      WRITE (9,100) OUTLINE
      WRITE (7,100) OPTHEADER(1)
      WRITE (9,100) OPTHEADER(1)

      DO I = 1, NUMLINE
        CALL PBD_REM_BLANK(OUTBUF(I),LENGTH)
C
C       Check if the OLB definition is the same as the previous line
C
        IF ( I .EQ. 1 .OR. INDEX ( OUTBUF(I),'/INCLUDE' ) .GT. 0) THEN
          WRITE (7,100) OUTBUF(I)(1:LENGTH)
          CALL PBD_REM_BLANK(OUTBUF1(I),LENGTH)
          WRITE (9,100) OUTBUF1(I)(1:LENGTH)
        ELSE
          POS = INDEX ( OUTBUF(I), '/' )
          IF ( POS .GT. 0 ) THEN 
            POS = POS - 1
            FILE_SPEC = OUTBUF(I)(1:POS)
          ELSE
            FILE_SPEC = OUTBUF(I)(1:LENGTH)
            POS = LENGTH
          END IF
          IF ( INDEX ( OUTBUF(I-1), FILE_SPEC(1:POS) ) .EQ. 0 ) THEN
            WRITE (7,100) OUTBUF(I)(1:LENGTH)
            CALL PBD_REM_BLANK(OUTBUF1(I),LENGTH)
            WRITE (9,100) OUTBUF1(I)(1:LENGTH)
          END IF
        END IF
         
      END DO

      CLOSE (7)
      CLOSE (9)

      OUTLINE ='DefDir + "''DEB'''//FOR_FILE_NAME(1:FOR_FILE_LEN)
     &          //'.OPT/OPT,'//'"'
      WRITE ( 8, 100 ) OUTLINE
C
C     Read a line from 'frame_name'.LNK file and write it to 
C     the output link file
C
      EOF = .FALSE.
      DO WHILE ( .NOT. EOF )
      
        READ  ( 5,100, END = 200, ERR = 200, IOSTAT = STATUS ) OUTLINE
200     IF ( STATUS .EQ. 0 ) THEN 
          WRITE (8,100) OUTLINE
        ELSE IF ( STATUS .LT. 0 ) THEN
          EOF = .TRUE.
        ELSE
C
C         Read error, send error message
C
          MSG_BUF = ERROR_BUF(2)(1:32)//
     &              'D0$PBD:'//FRAME_NAME(1:FRAME_LEN)//'.LNK'
          ERROR_FLAG = .TRUE.
          CALL PBD_MSG ( MSG_BUF )
          EOF = .TRUE.

        END IF

      END  DO

      CLOSE ( 8 )
      CLOSE ( 5 )

      RETURN
      END