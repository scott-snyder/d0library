      SUBROUTINE PBD_INTERFACE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates FORTRAN code for the user hook interface
C-                         routines.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls:
C-
C-   Modules called by this routine:  PBD_MSG
C-  
C-   Based on the PASCAL procedure Build_Interface_Routines of the old
C-   Program Builder.
C-
 
C-
C-   Created  24-JUL-1991   Hyon Joo Kehayias
C-   Updated  18-MAY-1992   Hyon Joo Kehayias 
C-     ( for UNIX compatibility, added () to interface routine declaration )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
C
C     LOCAL DATA AREA
C
      INTEGER*2 I,J,K,PACK_NUM
      INTEGER*2 ROUT_NAME_LEN           ! PBD ROUTINE NAME LENGTH
      CHARACTER*80 MESSAGE              ! MESSAGE BUFFER
      CHARACTER*32 ROUT_NAME            ! PBD ROUTINE NAME TO GENERATE
      CHARACTER*2 NUMSTR                ! ASCII PACKAGE NUMBER
      LOGICAL DUPLICATE
C 
      DATA MESSAGE /'-- Building Interface Subroutines --'/

      CALL PBD_MSG ( MESSAGE )

      DO I = 1, NUMHOOK         ! For each hook in the framework
C
C       Determine routine name
C
        IF ( SUB_NAME_LEN(I) .GT. 0 ) THEN
          ROUT_NAME = SUB_NAME(I)
          ROUT_NAME_LEN = SUB_NAME_LEN(I)
        ELSE
          ROUT_NAME = HOOK_NAME(I)
          ROUT_NAME_LEN = HOOK_NAME_LEN(I)
        END IF

        OUTLINE = START_LINE //
     &            'LOGICAL FUNCTION '//
     &             ROUT_NAME(1:ROUT_NAME_LEN)//'()'

        WRITE (8,100) OUTLINE
100     FORMAT ( A )
        WRITE (8,100) COMMENT_LINE
        WRITE (8,100) COMMENT_LINE
        OUTLINE = COMMENT_STR//
     &           'Purposes and Methods:' 
        WRITE (8,100) OUTLINE
        OUTLINE = COMMENT_STR//
     &           '--------------------' 
        WRITE (8,100) OUTLINE
        WRITE (8,100) COMMENT_LINE
        OUTLINE = COMMENT_STR//
     &            'Created by the PROGRAM BUILDER Release' 
        WRITE (8,100) OUTLINE
        WRITE (8,100) DATE_TIME
        WRITE (8,100) COMMENT_LINE
        WRITE (8,100) COMMENT_LINE

        OUTLINE = START_LINE //
     &           'CHARACTER*32 MESSID,CALLER' 
        WRITE (8,100) OUTLINE
        OUTLINE = START_LINE //
     &                'CHARACTER*80 MESSAG' 
        WRITE (8,100) OUTLINE
 
        DO PACK_NUM = 1, NUMPACK

          IF ( INT_NAME_LEN (PACK_NUM, I ) .GT. 0 ) THEN

            OUTLINE = START_LINE //'LOGICAL '//
     &         INT_NAME(PACK_NUM,I)(1:INT_NAME_LEN(PACK_NUM,I))
            WRITE (8,100) OUTLINE

            OUTLINE = START_LINE //'EXTERNAL '//
     &         INT_NAME(PACK_NUM,I)(1:INT_NAME_LEN(PACK_NUM,I))
            WRITE (8,100) OUTLINE
          END IF

        END DO

        WRITE (8,100) COMMON_LINE
        WRITE (8,100) COMMENT_LINE
        OUTLINE = START_LINE //
     &                ROUT_NAME(1:ROUT_NAME_LEN) // ' = .TRUE.' 
        WRITE (8,100) OUTLINE
        WRITE (8,100) COMMENT_LINE

        DO PACK_NUM = 1, NUMPACK
C
C         Convert package number to ASCII
C
          WRITE ( NUMSTR,200 ) PACK_NUM
200       FORMAT (I2)
          IF ( INT_NAME_LEN(PACK_NUM,I ) .GT. 0 ) THEN
            WRITE ( 8,100 ) COMMENT_LINE
            OUTLINE = START_LINE //
     &             'IF ( (PBD_FLAG_VALUE('//NUMSTR//'))' 
            WRITE ( 8,100 ) OUTLINE
            OUTLINE = CONT_LINE//'    ) THEN' 
            WRITE ( 8,100 ) OUTLINE

            IF (ACTION (I) .EQ. 3 ) THEN  !  action = skip_if_true
              OUTLINE = START_LINE //
     &           '   IF ('//
     &           INT_NAME(PACK_NUM,I)(1:INT_NAME_LEN(PACK_NUM,I))//
     &           '()) RETURN' 
              WRITE ( 8,100 ) OUTLINE
            ELSE
              OUTLINE = START_LINE //
     &               '   IF (.NOT. '//
     &           INT_NAME(PACK_NUM,I)(1:INT_NAME_LEN(PACK_NUM,I))//
     &           '()) THEN' 
              WRITE ( 8,100 ) OUTLINE         

              OUTLINE = START_LINE //
     &                         '      '//
     &            ROUT_NAME(1:ROUT_NAME_LEN) // ' = .FALSE.' 

              WRITE ( 8,100 ) OUTLINE         
              IF ( ACTION (I) .EQ. 0 ) THEN     ! action =  ignore 

                OUTLINE = START_LINE //
     &                   '      CALLER = '''//
     &           INT_NAME(PACK_NUM,I)(1:INT_NAME_LEN(PACK_NUM,I))//
     &           '''' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = START_LINE //
     &             '      MESSAG = ''This error is ignored''' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = START_LINE //
     &          '      CALL ERRMSG(MESSID,CALLER,MESSAG,''I'')' 
                WRITE ( 8,100 ) OUTLINE

              ELSE IF ( ACTION(I) .EQ. 1 ) THEN ! action = skip

                OUTLINE = START_LINE //
     &                  '      MESSID = ''!'//
     &           INT_NAME(PACK_NUM,I)(1:INT_NAME_LEN(PACK_NUM,I))//
     &                  ' is false.''' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = START_LINE //
     &                    '      CALLER = '''//
     &           INT_NAME(PACK_NUM,I)(1:INT_NAME_LEN(PACK_NUM,I))//
     &           '''' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = START_LINE //
     &        '      MESSAG = ''Further processing is skipped''' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = START_LINE //
     &       '      CALL ERRMSG(MESSID,CALLER,MESSAG,''W'')' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = START_LINE //
     &                    '      RETURN' 
                WRITE ( 8,100 ) OUTLINE

              ELSE IF ( ACTION (I) .EQ. 2 ) THEN   ! action = abort 

                OUTLINE = START_LINE //
     &           '      MESSID = ''!'//
     &           INT_NAME(PACK_NUM,I)(1:INT_NAME_LEN(PACK_NUM,I))//
     &            ' is false.''' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = START_LINE //
     &             '      CALLER = '''//
     &           INT_NAME(PACK_NUM,I)(1:INT_NAME_LEN(PACK_NUM,I))//
     &           '''' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = START_LINE //
     &           '      MESSAG = ''Program execution is aborted''' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = START_LINE //
     &           '      CALL ERRMSG(MESSID,CALLER,MESSAG,''F'')' 
                WRITE ( 8,100 ) OUTLINE
                WRITE ( 8,100) COMMENT_LINE
                OUTLINE = COMMENT_STR//
     &           '      ERRMSG will trigger error messages printing'
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = COMMENT_STR//
     &           '      and will abort the execution.' 
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = COMMENT_STR//
     &           '      The following RETURN statement is only there'
                WRITE ( 8,100 ) OUTLINE
                OUTLINE = COMMENT_STR//
     &           '      to satisfy the FORTRAN compiler.' 
                WRITE ( 8,100 ) OUTLINE
                WRITE ( 8,100) COMMENT_LINE
                OUTLINE = START_LINE //
     &                   '      RETURN' 
                WRITE ( 8,100 ) OUTLINE
             END IF
             OUTLINE = START_LINE //
     &               '   END IF' 
             WRITE ( 8,100 ) OUTLINE

           END IF

           OUTLINE = START_LINE //'END IF' 
           WRITE ( 8,100 ) OUTLINE

         ELSE                           ! No interface routine name provided

           WRITE (8,100) COMMENT_LINE
           OUTLINE = COMMENT_STR//
     &       'No routine was provided for the package: '//
     &        PACKAGE_NAME(PACK_NUM)(1:PACK_NAME_LEN(PACK_NUM))
           WRITE ( 8,100 ) OUTLINE
           WRITE (8,100) COMMENT_LINE

         END IF

       END DO                           ! End of each package processing

       WRITE (8,100) RET_LINE 
       WRITE (8,100) END_LINE 
       WRITE (8,100) COMMENT_LINE
       WRITE (8,100) COMMENT_LINE
       WRITE (8,100) COMMENT_LINE

      END DO            ! End of each framework hook name processing
C
C     Close the FORTRAN source file
C
      CLOSE (8)

      RETURN
      END
