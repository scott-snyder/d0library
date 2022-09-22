      SUBROUTINE EZFILL_i (IDENTF,REMARK,NUMBER,TYPE,TOTAL)
      IMPLICIT NONE
C
      INTEGER      TOTAL
      CHARACTER*(*) REMARK
      CHARACTER*(*) IDENTF,TYPE(*)
      INTEGER       NUMBER(*)

      call ezfill (identf, remark, number, type, total)
      return
      end
