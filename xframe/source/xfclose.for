      SUBROUTINE XFCLOSE(dummy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : if type=0, file is data, 1=stp, 2=output
C-
C-   Inputs  : dummy
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      character*80 msg
      integer dummy,ierr
c
c
      if (file_type.eq.0) then
c
c       close filename for data
c
cccc        call mzend
        if (fzrz_opened) then
          call fzendi(fzrz_lun,'TQ')
          close(fzrz_lun)
          call rlunit(d0xuserunit,fzrz_lun,ierr)
          if (ierr.ne.0) then
            call fwarning(%ref('XFCLOSE Error releasing LUN'))
          endif
        endif
c
      else if (file_type.eq.1) then
c
c       no need to close filename for stp
c
cccc        call mzend
c
      else if (file_type.eq.2) then
c
c       close filename for output
c
        close(out_lun)
        out_lun = 6
        IQPRNT = 6
        IQPR2 = 6
        IQLOG = 6
c
      endif
c
      return
      end
