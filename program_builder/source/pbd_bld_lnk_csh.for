      SUBROUTINE PBD_BLD_LNK_CSH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates the UNIX PBD Link Script file  
C-                         The link commands are based on the framework link 
C-                         file and .OPT files. 
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
C-   Created  10-FEB-1994   Carmem Silva
C-      Adapted from PBD_BLD_LINK.FOR
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
      INTEGER*2    POS, POS1 ,POS2      ! CHARACTER POSITION
      INTEGER*2    NODE_LEN             ! NODE NAME LENGTH
      INTEGER*2    DEV_LEN              ! DEVICE NAME LENGTH
      INTEGER*2    DIR_LEN              ! DIRECOTRY SPEC STRING LENGTH
      INTEGER*2    TYPE_LEN             ! FILE TYPE STRING LENGTH
      INTEGER*2    I, J, K ,INTC        ! INDEX VARIABLES
      INTEGER*2    OBJ_COUNT ,AUX       ! INDEX VARIABLES
      INTEGER*2    LIB_INDEX(200)       ! LIBRARY INDEX ARRAY
      INTEGER*2    OBJ_LEN(200)         ! OBJ NAME LENGTH
      CHARACTER*80 ERROR_BUF(2)         ! ERROR MESSAGES
      CHARACTER*80 MSG_BUF              ! MESSAGE BUFFER
      CHARACTER*80 FILE_NAME            ! FILE NAME TO OPEN
      CHARACTER*80 TOKEN                ! TOKEN
      CHARACTER*80 OPTHEADER (2)        ! OPT FILE HEADER MESSAGE
      CHARACTER*22 MESSAGE(2)           ! MESSAGES TO SEND
      CHARACTER*9  USER_LIB_SYMB        ! USER LIBRARY FILE SYMBOL
      CHARACTER*132 OUTBUF(200)         ! OUTPUT BUFFER
      CHARACTER*132 OUTBUF1(200)        ! SECOND OUTPUT BUFFER
      CHARACTER*132 OBJ_NAME(200)       ! NAME OF OBJ ROUTINES
      CHARACTER*132 LOWER_NAME          ! LOWER CASE FILE NAME
      CHARACTER*132 LINE                ! LINE BUFFER
      
      CHARACTER*80 FILE_SPEC            ! FILE SPECIFICATION TO PARSE
      CHARACTER*80 NODE_NAME            ! NODE NAME
      CHARACTER*80 DEV_NAME             ! DEVICE NAME
      CHARACTER*80 DIR_NAME             ! DIRECTORY SPECIFICATION
      CHARACTER*20 FILE_TYPE            ! FILE TYPE
      LOGICAL      RET_FLAG             ! PARSE RETURN STATUS FLAG
      LOGICAL      EOF                  ! END OF FILE FLAG
      LOGICAL      OPENED               ! OPEN STATUS
      LOGICAL      NO_WRITE             
      INTEGER      LENOCC

      DATA MESSAGE /'-- Building Link File ',
     &              '-- Building OPT Files '/
      DATA USER_LIB_SYMB /'user_libs'/
      DATA ERROR_BUF /
     &     '-E-File Open error for ',
     &     '-E-Error detected while reading '/
      DATA OPTHEADER /
     &'#******************************************************#',
     &'#     Generated by the Program Builder                 #'/

c      MSG_BUF = MESSAGE(1)//FOR_FILE_NAME(1:FOR_FILE_LEN)//'.LNK'
c      CALL PBD_MSG ( MSG_BUF )
C
C     Determine frame LNK file name
C
      FILE_NAME = 'D0$'//FRAME_NAME(1:FRAME_LEN)//':'//
     &             FRAME_NAME(1:FRAME_LEN)//'_LNK.CSH'

       DO POS = 1,FRAME_LEN
        IF ( LGE(FRAME_NAME(POS:POS),'A') .AND. 
     &        LLE(FRAME_NAME(POS:POS),'Z') ) THEN
           INTC = IBSET(ICHAR(FRAME_NAME(POS:POS)),5)
           FRAME_NAME(POS:POS) = CHAR(INTC)
        ENDIF
       ENDDO
     
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
      FILE_NAME = FOR_FILE_NAME(1:FOR_FILE_LEN)//'_LNK.CSH'

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
      WRITE (8,100)'#! /bin/csh -f'
      WRITE (8,100)'source `uff $d0unix/d0local.cshrc`'

      WRITE (8,100) OPTHEADER(1)(1:LENOCC(OPTHEADER(1)))
      OUTLINE = '#     '//'File Name : '//FILE_NAME 
      OUTLINE(56:56) = '#'
      WRITE (8,100) OUTLINE(1:LENOCC(OUTLINE))
      WRITE (8,100) OPTHEADER(2)(1:LENOCC(OPTHEADER(2)))
      OUTLINE = '# '//DATE_TIME(3: )
      OUTLINE(56:56) = '#'
      WRITE (8,100) OUTLINE(1:LENOCC(OUTLINE))
      WRITE (8,100) OPTHEADER(1)(1:LENOCC(OPTHEADER(1)))

      WRITE (8,100)'if( `echo $1 | tr D d | cut -c1` == d )then'
      WRITE (8,100)'  set pfx = deb_'
      WRITE (8,100)'  set debug = 1'
      WRITE (8,100)'  set debopt = -g'
      WRITE (8,100)'else'
      WRITE (8,100)'  set pfx = '''''
      WRITE (8,100)'  set debug = 0'
      WRITE (8,100)'  set debopt = '''''
      WRITE (8,100)'endif'
      WRITE (8,100)'set force_objects = ( $argv[2-] )'

100   FORMAT ( A )
C
C     Generate command to compile hooks file and define hooks link object.
C
      LOWER_NAME = FOR_FILE_NAME
      CALL CUTOL(LOWER_NAME)
      WRITE (8,100)' '
      WRITE (8,100)
     &  'set hooks_file = '//LOWER_NAME(1:FOR_FILE_LEN)//'.for'
      WRITE (8,100)'fort $debopt $hooks_file'
      WRITE (8,100)
     &  'set objects = ( `archive_name $hooks_file` )'
C
C     Get list of libraries from PBD files.  Read .OPT files.
C
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
 
              LINE = FILE_SPEC(1:LENGTH)//'''${pfx}'''//
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
 
                    LINE = FILE_SPEC(1:LENGTH)//'''${pfx}'''//
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
 
                LINE = FILE_SPEC(1:LENGTH)//'''${pfx}'''//
     &             OBJ_FILE(I,J)(LENGTH+1:OBJ_FILE_LEN(I,J))
                OUTBUF1 ( NUMLINE ) =  LINE
              END IF
 
            END IF

          END IF

        END DO
      END DO
C
C     Generate list of user libraries for selective search.
C
      WRITE (8,100)' '
      OUTLINE = 'set user_libs = (\'
      WRITE (8,100) OUTLINE(1:LENOCC(OUTLINE))
C
C Loop over libraries.  Generate selective search list and remember list
C of include objects.
C
      K = 1
      OBJ_COUNT = 0
      DO I = 1, NUMLINE
        CALL PBD_REM_BLANK(OUTBUF(I),LENGTH)
C
C       Check if the OLB definition is the same as the previous line
C
        POS = INDEX ( OUTBUF(I), '/' )
        IF ( POS .GT. 0 ) THEN 
          POS = POS - 1
          FILE_SPEC = OUTBUF(I)(1:POS)
        ELSE
          FILE_SPEC = OUTBUF(I)(1:LENGTH)
          POS = LENGTH
        END IF
        NO_WRITE = .FALSE.
        DO J = 1, I-1
          IF ( INDEX( OUTBUF(J)(1:POS),FILE_SPEC(1:POS)).NE.0 ) THEN
            NO_WRITE = .TRUE.
          ENDIF
        ENDDO
C-
C- See if we want to include this library in the selective search list.
C-
        IF (.NOT.NO_WRITE) THEN
          IF ( INDEX (OUTBUF(I),'/L'). GT. 0 ) THEN
            POS = INDEX (OUTBUF1(I),'/')
            IF ( POS.GT.0 ) THEN
              IF (OUTBUF1(I)(POS-4:POS-1).EQ.'.OLB') THEN
                OUTLINE = ' `vff '''//OUTBUF1(I)(1:POS-5)//'.A'' `\'
              ELSE IF (OUTBUF1(I)(POS-2:POS-1).EQ.'.A') THEN
                OUTLINE = ' `vff '''//OUTBUF1(I)(1:POS-1)//''' `\'
              ELSE
                OUTLINE = ' `vff '''//OUTBUF1(I)(1:POS-1)//'.A'' `\'
              ENDIF 
              WRITE(8,100)OUTLINE(1:LENOCC(OUTLINE))
            ENDIF
          ENDIF
        ENDIF
C     
C     Check for the include objects
C
        IF ( INDEX ( OUTBUF(I),'/INC' ) .GT. 0) THEN
          POS = INDEX ( OUTBUF(I) ,'=')
          IF ( POS .GT. 0 ) THEN
            POS1 = INDEX (OUTBUF(I), '(')
            IF ( POS1 .GT. 0 ) THEN
              POS2 = INDEX (OUTBUF(I),',')
              IF ( POS2 .GT. 0 ) THEN
 222            OBJ_COUNT = OBJ_COUNT + 1
                LIB_INDEX(OBJ_COUNT) = I
                OBJ_NAME(OBJ_COUNT) = OUTBUF(I)(POS1+1:POS2-1)
                OBJ_LEN(OBJ_COUNT) = POS2 - POS1 -1
                POS1 = POS2 
                POS2 = INDEX ( OUTBUF(I)(POS1+1:132), ',')
                IF ( POS2 .GT. 0 ) THEN
                  POS2 = POS1 + POS2
                  GOTO 222
                ENDIF
                POS2 = INDEX ( OUTBUF(I),')' )
                IF ( POS2 .GT. 0 ) THEN
                  OBJ_COUNT = OBJ_COUNT + 1
                  LIB_INDEX(OBJ_COUNT) = I
                  OBJ_NAME(OBJ_COUNT) = OUTBUF(I)(POS1+1:POS2-1)
                  OBJ_LEN(OBJ_COUNT) = POS2 - POS1 -1
                ENDIF
              ELSE 
                POS2 = INDEX ( OUTBUF(I),')' )
                OBJ_COUNT = OBJ_COUNT + 1
                LIB_INDEX(OBJ_COUNT) = I
                OBJ_NAME(OBJ_COUNT) = OUTBUF(I)(POS1+1:POS2-1)
                OBJ_LEN(OBJ_COUNT) = POS2 - POS1 - 1
              ENDIF
            ELSE
              POS2 = INDEX ( OUTBUF(I) , ',')
              IF ( POS2 .GT. 0 ) THEN
 333            OBJ_COUNT = OBJ_COUNT + 1
                LIB_INDEX(OBJ_COUNT) = I
                OBJ_NAME(OBJ_COUNT) = OUTBUF(I)(POS+1:POS2-1)
                OBJ_LEN(OBJ_COUNT) = POS2 - POS -1
                POS = POS2
                POS2 = INDEX ( OUTBUF(I)(POS+1:132), ',')
                IF ( POS2 .GT. 0 ) THEN
                  POS2 = POS + POS2
                  GOTO 333
                ENDIF
                POS2 = INDEX ( OUTBUF(I)(POS+1:132),'/' )
                IF ( POS2 .GT. 0 ) THEN
                  POS2 = POS2 + POS
                  OBJ_COUNT = OBJ_COUNT + 1
                  LIB_INDEX(OBJ_COUNT) = I
                  OBJ_NAME(OBJ_COUNT) = OUTBUF(I)(POS+1:POS2-1)
                  OBJ_LEN(OBJ_COUNT) = POS2 - POS1 - 1
                ELSE 
                  DO AUX = 132,1,-1
                    IF ( OUTBUF(I)(AUX:AUX).NE.' ') GOTO 555
                  ENDDO
 555              CONTINUE
                  POS2 = AUX
                  OBJ_COUNT = OBJ_COUNT + 1
                  LIB_INDEX(OBJ_COUNT) = I
                  OBJ_NAME(OBJ_COUNT) = OUTBUF(I)(POS+1:POS2)
                  OBJ_LEN(OBJ_COUNT) = POS2 - POS  
                ENDIF
              ELSE               
                POS2 = INDEX (OUTBUF(I)(POS+1:132),'/')
                IF ( POS2 .GT. 0 ) THEN
                  POS2 = POS + POS2
                  OBJ_COUNT = OBJ_COUNT + 1
                  LIB_INDEX(OBJ_COUNT) = I
                  OBJ_NAME(OBJ_COUNT) = OUTBUF(I)(POS+1:POS2-1)
                  OBJ_LEN(OBJ_COUNT) = POS2 - POS -1
                ELSE
                  OBJ_COUNT = OBJ_COUNT + 1
                  LIB_INDEX(OBJ_COUNT) = I
                  DO AUX = 132,1,-1
                    IF ( OUTBUF(I)(AUX:AUX).NE.' ') GOTO 444
                  ENDDO
 444              CONTINUE
                  POS2 = AUX
                  OBJ_NAME(OBJ_COUNT) = OUTBUF(I)(POS+1:POS2)
                  OBJ_LEN(OBJ_COUNT) = POS2 - POS
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF             
      ENDDO

      WRITE(8,100)') '
C-
C- Here we output the list of force include objects
C-
      WRITE (8,100)' '
      IF ( OBJ_COUNT. GT. 0) THEN
        OUTLINE = 'set objects = ( $objects \'
        WRITE(8,100)OUTLINE(1:LENOCC(OUTLINE))
        DO AUX = 1, OBJ_COUNT
          CALL CUTOL(OBJ_NAME(AUX))
          NO_WRITE = .FALSE.
          DO J = 1, AUX-1
            IF(OBJ_NAME(J  )(1:OBJ_LEN(J  )).EQ.
     &         OBJ_NAME(AUX)(1:OBJ_LEN(AUX)))NO_WRITE = .TRUE.
          ENDDO
          IF(.NOT.NO_WRITE)THEN
            I = LIB_INDEX(AUX)
            POS = INDEX ( OUTBUF1(I), '/')
            IF (OUTBUF1(I)(POS-4:POS-1).EQ.'.OLB') THEN
              OUTLINE = ' `d0module '//OBJ_NAME(AUX)(1:OBJ_LEN(AUX))//
     &          ' '''//OUTBUF1(I)(1:POS-5)//'.A'' `\'
              WRITE(8,100) OUTLINE(1:LENOCC(OUTLINE))
            ELSE IF (OUTBUF1(I)(POS-2:POS-1).EQ.'.A') THEN
              OUTLINE = ' `d0module '//OBJ_NAME(AUX)(1:OBJ_LEN(AUX))
     &           //' '''  //OUTBUF1(I)(1:POS-1)//'''` \'
              WRITE(8,100) OUTLINE(1:LENOCC(OUTLINE))
            ELSE
              OUTLINE = ' `d0module '//OBJ_NAME(AUX)(1:OBJ_LEN(AUX))//
     &         ' '''//OUTBUF1(I)(1:POS-1)//'.A'' `\'
              WRITE(8,100) OUTLINE(1:LENOCC(OUTLINE))
            ENDIF
          ENDIF
        ENDDO
        WRITE(8,100)')'
      ENDIF
C-
C- Generate name of executable.
C-
      WRITE(8,100)' '
      WRITE(8,100)
     &  'set exe  = ${pfx}'//LOWER_NAME(1:FOR_FILE_LEN)//'.x '
C
C     Read a line from 'frame_name'.LNK file and write it to 
C     the output link file

      EOF = .FALSE.
      DO WHILE ( .NOT. EOF )
      
        READ  ( 5,100, END = 200, ERR = 200, IOSTAT = STATUS ) OUTLINE
200     IF ( STATUS .EQ. 0 ) THEN
          DO I = 80,1,-1
            IF ( OUTLINE(I:I) .NE. ' ') THEN
              LENGTH = I
              GOTO 111
            ENDIF
          ENDDO  
111       WRITE (8,100) OUTLINE(1:LENOCC(OUTLINE))
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

       IF ( OBJ_COUNT .GE. 1) THEN
           WRITE(8,100)' '
           WRITE(8,100)'rm $objects'
       ENDIF
      

      CLOSE ( 8 )
      CLOSE ( 5 )

      RETURN
      END
