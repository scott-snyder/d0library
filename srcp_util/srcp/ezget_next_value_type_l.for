      SUBROUTINE EZGET_NEXT_VALUE_TYPE_l(PARAM,VAL,CVAL,TYPE,LVAL,IER,
     &                                   PTR)
      
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM
      logical       VAL
      CHARACTER*(*) CVAL
      INTEGER       TYPE
      INTEGER       LVAL
      INTEGER       IER
      INTEGER       PTR

      call ezget_next_value_type (param, val, cval, type, lval, ier,
     &     ptr)
      return
      end
