      SUBROUTINE READIT(lun,ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : my own version which calls D0 utilities
C-   and prints out stuff
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer lun,ios
      logical ok
      character*80 msg
c
      CALL EVTRD(lun,IOS)  ! read events from file
      ok = .true.
      nrun = iq(lhead+6)
      nevo = iq(lhead+9)
      nevi1 = iq(lhead+7)
      nevi2 = iq(lhead+8)
      grun = iq(lhead+12)
      dbhead(pstore) = lhead
      IF(IOS.EQ.0) THEN
        write(msg,'(
     &    '' EVENT: Run# '',I8,'' Global# '',I8,
     &    '' Out # '',I8,'' In#2/In#1 '',2I8)')
     &    nrun,grun,nevo,nevi2,nevi1
        call xerrmsg(' *** Event record found...')
        call xerrmsg(msg)
C
C           Begin run record
      ELSE IF(IOS.EQ.1) THEN
        call xerrmsg(' *** Begin run record found...')
C
C           End run record
      ELSE IF(IOS.EQ.2) THEN ! ENDRUN
        call xerrmsg(' *** End run record found...')
C
C         end-of-file
      ELSE IF(IOS.GT.2) THEN
        call xerrmsg(' *** End-of-file reached...closing...')
        close(lun)
        ok = .false.
C
      ENDIF
c
      return
      end
