      SUBROUTINE HBNAME_i(ID, BLKNAM, ADDRES, FORM)
      implicit none
      INTEGER       ID, ADDRES(1)
      CHARACTER*(*) BLKNAM, FORM
      call hbname (id, blknam, addres, form)
      return
      end
