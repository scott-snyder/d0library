      SUBROUTINE EZGET_VALUE_TYPE_i (PARAM,VAL,TYPE,NVAL,IER)
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM
      INTEGER       VAL(*)
      INTEGER       TYPE(*)
      INTEGER       NVAL
      INTEGER       IER
C
      call ezget_value_type (param, val, type, nval, ier)
  999 RETURN
      END
