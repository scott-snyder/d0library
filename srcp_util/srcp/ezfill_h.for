      SUBROUTINE EZFILL_h (IDENTF,REMARK,s,TYPE,TOTAL)
      IMPLICIT NONE
C
      INTEGER      TOTAL
      CHARACTER*(*) REMARK
      CHARACTER*(*) IDENTF,TYPE(*)
      character*(*) s

      call ezfill (identf, remark, s, type, total)
      return
      end
