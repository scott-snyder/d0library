      INTEGER*4 FUNCTION HANDLER(SIGARGS, MECHARGS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : error handler for floating reserved operand faults
C-                         a floating reserved operand fault occurs if a
C-                         floating point number is -0.0 
C-
C-   Created  16-AUG-1993   Ulrich Heintz
C-
C----------------------------------------------------------------------
      INTEGER*4 SIGARGS(*), MECHARGS(5)
      HANDLER = LIB$FIXUP_FLT(SIGARGS, MECHARGS, 1.0E0)
      IF(HANDLER.EQ.SS$_CONTINUE)
     &  CALL ERRMSG('reserved operand','HANDLER','replaced by 1','W')
      RETURN
      END
