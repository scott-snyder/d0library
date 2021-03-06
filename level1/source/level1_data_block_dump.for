      SUBROUTINE LEVEL1_DATA_BLOCK_DUMP (LUN,MODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Debugging tool : dumps the Level 1 Trigger
C-                         Data Block. If "Short" option is requested
C-                         only the lines containing at least one non
C-                         value are printed. It is not transportable.
C-                                                      -------------
C-
C-   Inputs  : LUN : Logical Unit Number to be used for print out.
C-             MODE | 'D' :  decimal     full  printing
C-                  | 'O' :  octal       full  printing
C-                  | 'H' :  hexadecimal full  printing
C-                  | 'DS' : decimal     short printing
C-                  | 'OS' : octal       short printing
C-                  | 'HS' : hexadecimal short printing
C-
C-   Outputs : None.
C-   Controls: None.
C-
C-   Created  21-MAR-1990   Sylvain Tisserant (MSU)
C-   Revised  18-JUN-1990
C-   Updated  14-AUG-1990   James T. Linnemann   
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_TRIGGER_DATA_BLOCK.INC'
C
      INTEGER       LUN
      CHARACTER*(*) MODE
C
C&IF VAXVMS
      INTEGER      LINE_MAX
      PARAMETER   (LINE_MAX = (DATA_BLOCK_LENGTH+15)/16)
      INTEGER*2    DATA_BLOCK(DATA_BLOCK_LENGTH)
      EQUIVALENCE (DATA_BLOCK, LVL1_DATA_BLOCK)
C&ENDIF
C
      INTEGER   LINE, I, I1, I2
      CHARACTER FORMAT*27
      LOGICAL   FULL, FIRST
C
C----------------------------------------------------------------------
C
C
      IF(MODE(1:1).EQ.'D') THEN
        WRITE (LUN,9000) '(decimal)'
        FORMAT = '(Z5.4,I6,2X,8I7,2X,8I7)'
      ELSE IF(MODE(1:1).EQ.'O') THEN
        WRITE (LUN,9000) '(octal)'
        FORMAT = '(Z5.4,I6,2X,8O7.6,2X,8O7.6)'
      ELSE
        WRITE (LUN,9000) '(hexadecimal)'
        FORMAT = '(Z5.4,I6,2X,8Z5.4,2X,8Z5.4)'
      ENDIF
      IF((LEN(MODE).EQ.2).AND.(MODE(2:2).EQ.'S')) THEN
        FULL = .FALSE.
      ELSE
        FULL = .TRUE.
      ENDIF
C
      I2 = 0
      FIRST = .FALSE.
C&IF VAXVMS
      DO 20 LINE = 1, LINE_MAX
        I1 = I2 + 1
        I2 = MIN0 (I1+15, DATA_BLOCK_LENGTH)
        IF(FULL) GOTO 10
        DO I = I1, I2
          IF(DATA_BLOCK(I).NE.0) GOTO 10
        ENDDO
        IF(FIRST) THEN
          WRITE (LUN,*)
          FIRST = .FALSE.
        ENDIF
        GOTO 20
   10   WRITE (LUN,FORMAT) 2*(I1-1),I1,(DATA_BLOCK(I), I = I1, I2)
        FIRST = .TRUE.
   20 CONTINUE
C&ELSE
      WRITE(LUN,20)
   20 FORMAT(//' TRIGGER DATA BLOCK DUMP NOT WRITTEN FOR THIS MACHINE')
C&ENDIF
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
 9000 FORMAT (//,' Level 1 Trigger Data Block Dump : ',A13,/,
     +           ' =================================',/)
C
      END
