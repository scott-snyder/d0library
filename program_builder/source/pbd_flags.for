      SUBROUTINE PBD_FLAGS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates FORTRAN code for the PBD flag routines -
C-                         PBD_SET_FLAG, PBD_GET_FLAG, PBD_DUMP_FLAGS,
C-                         PBD_GET_FLAG_NAME and PBD_UPCASE.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Modules called by this routine:  PBD_MSG
C-  
C-   Based on the PASCAL procedure Build_Flag_Routine of the old
C-   Program Builder.
C-

C-
C-   Created  19-JUL-1991   Hyon Joo Kehayias
C-   Modified 28-OCT-1993   Hyon Joo Kehayias
C-    ( To generate code for a new subroutine PBD_GET_FLAG_NAME )
C-   Modified 07-DEC-1993   Hyon Joo Kehayias
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
C
C     LOCAL DATA AREA
C
      CHARACTER*80 MESSAGE              ! MESSAGE BUFFER
C
      DATA MESSAGE /'-- Building Set/Reset Flag Routines --'/
      CALL PBD_MSG ( MESSAGE )

C     The following set of code is for the PBD_set_flag routine

      WRITE ( 8, 100 ) COMMENT_LINE 
100   FORMAT ( A )
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE //
     &  'LOGICAL FUNCTION PBD_SET_FLAG(FLAG_NAME,VALUE)'
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE =  START_LINE //
     &   'IMPLICIT NONE'
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE// 
     &   'CHARACTER*(*) FLAG_NAME' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE// 
     &   'CHARACTER*80 TMP_FLAG_NAME' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE// 
     &   'INTEGER I' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE// 
     &   'LOGICAL DONE,VALUE' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMON_LINE
      WRITE ( 8, 100 ) COMMENT_LINE 

      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE//
     &   'I = 1' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'DONE = .FALSE.' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'CALL PBD_UPCASE(FLAG_NAME,TMP_FLAG_NAME)' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'DO WHILE ((I.LE.PBD_FLAG_MAX).AND.(.NOT.DONE))' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  IF (TMP_FLAG_NAME(1:LEN(FLAG_NAME)).EQ.' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = CONT_LINE//
     &   '      PBD_FLAG_NAME(I)(1:LEN(FLAG_NAME))) THEN' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '    PBD_FLAG_VALUE(i) = VALUE' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '    DONE = .TRUE.' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  END IF' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  I = I + 1' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'END DO' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'PBD_SET_FLAG = DONE' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) RET_LINE 
      WRITE ( 8, 100 ) END_LINE 
C
C     The following set of code is for the PBD_get_flag routine
C
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE// 
     &   'LOGICAL FUNCTION PBD_GET_FLAG(FLAG_NAME,VALUE)' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE// 
     &   'IMPLICIT NONE' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE// 
     &   'CHARACTER*(*) FLAG_NAME' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE// 
     &   'CHARACTER*80 TMP_FLAG_NAME' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE// 
     &   'INTEGER I' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE// 
     &  'LOGICAL DONE,VALUE' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMON_LINE
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE// 'I = 1' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE// 'DONE = .FALSE.' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &    'CALL PBD_UPCASE(FLAG_NAME,TMP_FLAG_NAME)' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'DO WHILE ((I.LE.PBD_FLAG_MAX).AND.(.NOT.DONE))' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  IF (TMP_FLAG_NAME(1:LEN(FLAG_NAME)).EQ.' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE =  CONT_LINE//
     &   '      PBD_FLAG_NAME(I)(1:LEN(FLAG_NAME))) THEN' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '    VALUE = PBD_FLAG_VALUE(I)' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '    DONE = .TRUE.' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  END IF' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  I = I + 1' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'END DO' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'PBD_GET_FLAG = DONE' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) RET_LINE 
      WRITE ( 8, 100 ) END_LINE 

      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
C
C     The following code will build the PBD_DUMP_FLAGS routine
C
      OUTLINE = START_LINE//
     &   'SUBROUTINE PBD_DUMP_FLAGS' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE//
     &   'IMPLICIT NONE' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE//
     &   'INTEGER I' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMON_LINE
      OUTLINE = START_LINE//
     &   'DO I = 1, PBD_FLAG_MAX' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  WRITE (6,''('''' '''',A,'''' : '''',L1)'') '//
     &   'PBD_FLAG_NAME(I)(1:32),' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE =  CONT_LINE//
     &   'PBD_FLAG_VALUE(I)' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'END DO' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) RET_LINE 
      WRITE ( 8, 100 ) END_LINE 

      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 

C     The following code will build the PBD_UPCASE routine }

      OUTLINE = START_LINE//
     &   'SUBROUTINE PBD_UPCASE(IN_STRING,OUT_STRING)' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'IMPLICIT NONE' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'CHARACTER*(*) IN_STRING,OUT_STRING' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'INTEGER I,OFFSET' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE//
     &   'OFFSET = ICHAR(''A'') - ICHAR(''a'')' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'DO I = 1, LEN(IN_STRING)' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &  '  IF ((IN_STRING(I:I).GE.''a'').AND.'//
     &  '(IN_STRING(I:I).LE.''z'')) THEN' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE// 
     &   '    OUT_STRING(I:I) = CHAR(ICHAR(IN_STRING(I:I)) + OFFSET)' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  ELSE' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '    OUT_STRING(I:I) = IN_STRING(I:I)' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  END IF' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'END DO' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) RET_LINE 
      WRITE ( 8, 100 ) END_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
C
C     The following code will build the PBD_GET_FLAG_NAME routine
C
      OUTLINE = START_LINE//
     &   'SUBROUTINE PBD_GET_FLAG_NAME(FLAG_ID,FLAG_NAME,FLAG_VALUE)' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE//
     &   'IMPLICIT NONE' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE//
     &   'INTEGER FLAG_ID' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'CHARACTER*(*) FLAG_NAME' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'LOGICAL FLAG_VALUE' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMON_LINE
      WRITE ( 8, 100 ) COMMENT_LINE 
      OUTLINE = START_LINE//
     &   'IF ( FLAG_ID .LE. PBD_FLAG_MAX ) THEN' 
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  FLAG_NAME = PBD_FLAG_NAME(FLAG_ID)'
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  FLAG_VALUE = PBD_FLAG_VALUE(FLAG_ID)'
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'ELSE'
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  FLAG_NAME = '' '' '
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  FLAG_ID = 0'
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   '  FLAG_VALUE = .FALSE.'
      WRITE ( 8, 100 ) OUTLINE 
      OUTLINE = START_LINE//
     &   'END IF' 
      WRITE ( 8, 100 ) OUTLINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) RET_LINE 
      WRITE ( 8, 100 ) END_LINE 

      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 
      WRITE ( 8, 100 ) COMMENT_LINE 

      RETURN
      END
