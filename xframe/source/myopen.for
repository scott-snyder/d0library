      SUBROUTINE MYOPEN(filnam,xopt,lun,ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : opens files for me
C-
C-   Inputs  : the usual d0open-type inputs
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
c
      implicit none
c
      integer lun
      logical ok
      character*(*) filnam
      character*1 xopt
c
      CALL EVOPIN(FILNAM,XOPT,LUN,OK)  ! OPEN INPUT FILE
      if (ok) then
      else
        call xerrmsg('D0OPEN CANNOT FIGURE OUT HOW TO OPEN THIS!')
      endif
c
      return
      end
