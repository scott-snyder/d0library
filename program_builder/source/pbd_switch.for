      SUBROUTINE PBD_SWITCH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates FORTRAN code for the PBD package switch 
C-                         function STSWCH. 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Modules called by this routine:  PBD_MSG
C-  
C-   Based on the PASCAL procedure Build_Switch_Functions of the old
C-   Program Builder.
C-
C-
C-   Created  19-JUL-1991   Hyon Joo Kehayias
C-   Modified 22-APR-1993   Hyon Joo Kehayias
C-   Modified 01-NOV-1993   Hyon Joo Kehayias
C-    ( Generate dummy STSWCH if qualifier SWITCH is not set )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
C
C     LOCAL DATA AREA
C
      INTEGER*2 I
      CHARACTER*80 PRG_MSG(6)           ! PROGRAM MESSAGES
      CHARACTER*80 MESSAGE              ! MESSAGE BUFFER
      CHARACTER*2 NUMSTR                ! ASCII PACKAGE NUMBER
      LOGICAL FIRST_LOOP
C
      DATA MESSAGE /'-- Building Package Switch Functions --'/
      DATA PRG_MSG /
     & 'C-    Purposes and Methods:',
     & 'C-    --------------------', 
     & 'C-    This routine allows the Program Builder user to set',
     & 'C-    and reset(ON/OFF) the run time switches associated',
     & 'C-    with the packages.',
     & 'C-    Created by the PROGRAM BUILDER'/ 

      CALL PBD_MSG ( MESSAGE )
C
C      { First build the main program to turn on/off the value of the
C        package selectors }
C
      OUTLINE = START_LINE//
     &          'LOGICAL FUNCTION STSWCH()' 
      WRITE (8,100) OUTLINE
100   FORMAT ( A )
      WRITE (8,100) COMMENT_LINE
      WRITE (8,100) COMMENT_LINE 
      WRITE (8,100) PRG_MSG
      WRITE (8,100) DATE_TIME
      WRITE (8,100) COMMENT_LINE 
      WRITE (8,100) COMMENT_LINE 
C
C     Check if SWITCH qualifier set.  If set, then generate code for
C     a full function.  If not, generate a dummy function.
C
      IF ( QUALFLAG (20) ) THEN

        OUTLINE = START_LINE//'LOGICAL PRODUC'
        WRITE (8,100) OUTLINE
C
C       Generate code for logicals for each package name
C
        DO I = 1, NUMPACK

          OUTLINE = START_LINE//'LOGICAL L'//
     &        PACKAGE_NAME(I)(1:PACK_NAME_LEN(I) )
          WRITE (8,100) OUTLINE

        END DO
C
C       Convert # of packages in ASCII character string
C
        WRITE (NUMSTR,200) NUMPACK
200     FORMAT (I2)

        OUTLINE = START_LINE//'INTEGER NUMPAR'
        WRITE (8,100) OUTLINE
        OUTLINE = START_LINE//'CHARACTER*30 LABELS('//
     &            NUMSTR//')'
        WRITE (8,100) OUTLINE
        OUTLINE = START_LINE//'CHARACTER TYPARR('//
     &            NUMSTR//')' 
        WRITE (8,100) OUTLINE
        OUTLINE = START_LINE//'INTEGER LIMITS(2,'//
     &            NUMSTR//')'            
        WRITE (8,100) OUTLINE

        WRITE (8,100) (COMMON_LINE(I),I=1,3)

        OUTLINE = START_LINE//'EXTERNAL PRODUC'
        WRITE (8,100) OUTLINE

        WRITE (8,100) COMMON_LINE(4)

        WRITE (8,100) COMMENT_LINE
 
        OUTLINE = START_LINE//'DATA NUMPAR /'//
     &            NUMSTR // '/' 
        WRITE (8,100) OUTLINE
        OUTLINE = START_LINE//'DATA LABELS /' 
        WRITE (8,100) OUTLINE

        FIRST_LOOP = .TRUE.

        DO I = 1, NUMPACK

          IF ( FIRST_LOOP ) THEN
            FIRST_LOOP = .FALSE. 
            OUTLINE = CONT_LINE//'          '''//
     &          PACKAGE_NAME(I)(1:PACK_NAME_LEN(I) )//
     &               ' Selected'''
            WRITE (8,100) OUTLINE
          ELSE
            OUTLINE = CONT_LINE//'         ,'''//
     &          PACKAGE_NAME(I)(1:PACK_NAME_LEN(I) )//
     &               ' Selected'''
            WRITE (8,100) OUTLINE
          END IF

        END DO
 
        OUTLINE = CONT_LINE// '       /'
        WRITE (8,100) OUTLINE

        OUTLINE = START_LINE//'DATA TYPARR /' 
        WRITE (8,100) OUTLINE

        FIRST_LOOP = .TRUE.

        DO I = 1, NUMPACK

          IF ( FIRST_LOOP ) THEN
            FIRST_LOOP = .FALSE. 
            OUTLINE = CONT_LINE// '           ''L'''
            WRITE (8,100) OUTLINE

          ELSE
            OUTLINE = CONT_LINE//'          ,''L''' 
            WRITE (8,100) OUTLINE
          END IF

        END DO
 
        OUTLINE = CONT_LINE//'       /' 
        WRITE (8,100) OUTLINE

        WRITE (8,100) COMMENT_LINE
        OUTLINE = START_LINE//
     &          'IF (.NOT. PRODUC()) THEN'
        WRITE (8,100) OUTLINE

        DO I = 1, NUMPACK
C
C         Convert package # in ASCII character string
C
          WRITE (NUMSTR,200) I
          OUTLINE = START_LINE// '   L'//
     &          PACKAGE_NAME(I)(1:PACK_NAME_LEN(I) )//
     &         ' = PBD_FLAG_VALUE('//NUMSTR//')'
          WRITE (8,100) OUTLINE
        END DO

        OUTLINE = START_LINE//
     &         '   CALL GETDIS(NUMPAR,LABELS,TYPARR,LIMITS'
        WRITE (8,100) OUTLINE

        DO I = 1, NUMPACK
          OUTLINE = CONT_LINE//
     &              '              ,L'//
     &              PACKAGE_NAME(I)(1:PACK_NAME_LEN(I) )
          WRITE (8,100) OUTLINE

        END DO

        OUTLINE = CONT_LINE//
     &       '                                          )'
        WRITE (8,100) OUTLINE

        WRITE (8,100) COMMENT_LINE

        DO I = 1, NUMPACK
C
C         Convert package # in ASCII character string
C
          WRITE (NUMSTR,200) I

          OUTLINE = START_LINE//
     &             '   PBD_FLAG_VALUE('//NUMSTR//') = L'//
     &              PACKAGE_NAME(I)(1:PACK_NAME_LEN(I) )
          WRITE (8,100) OUTLINE

        END DO 

        OUTLINE = START_LINE //'END IF'
        WRITE (8,100) OUTLINE

      ELSE
        WRITE (8,100) 'C-    A dummy routine becuase no SWITCH' 
        WRITE (8,100) 'C-    input qualifier specified' 
      END IF        ! End of SWITCH qualifier checking

      WRITE (8,100) COMMENT_LINE
      OUTLINE = START_LINE //'STSWCH = .TRUE. '
      WRITE (8,100) OUTLINE

      WRITE (8,100) RET_LINE
      WRITE (8,100) END_LINE

      WRITE (8,100) COMMENT_LINE
      WRITE (8,100) COMMENT_LINE

  999 RETURN
      END
