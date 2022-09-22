      SUBROUTINE EZFILL_l (IDENTF,REMARK,NUMBER,TYPE,TOTAL)
      IMPLICIT NONE
C
      INTEGER      TOTAL
      CHARACTER*(*) REMARK
      CHARACTER*(*) IDENTF,TYPE(*)
      logical       NUMBER(*)

      call ezfill (identf, remark, number, type, total)
      return
      end
