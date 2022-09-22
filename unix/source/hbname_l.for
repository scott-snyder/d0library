      SUBROUTINE HBNAME_l(ID, BLKNAM, ADDRES, FORM)
      implicit none
      INTEGER       ID
      logical ADDRES(1)
      CHARACTER*(*) BLKNAM, FORM
      call hbname (id, blknam, addres, form)
      return
      end
