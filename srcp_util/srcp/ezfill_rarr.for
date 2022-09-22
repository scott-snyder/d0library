      SUBROUTINE EZFILL_rarr (IDENTF,REMARK,NUMBER,TYPE,TOTAL)
      IMPLICIT NONE
C
      INTEGER      TOTAL
      CHARACTER*(*) REMARK
      CHARACTER*(*) IDENTF,TYPE(*)
      real          NUMBER(*)

      call ezfill (identf, remark, number, type, total)
      return
      end
