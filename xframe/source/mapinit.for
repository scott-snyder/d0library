      SUBROUTINE MAPINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : generalized init for map and d0x
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
c     init d0map stuff
c
      dropkeep = 1
      nbdrop = 0
      pnuttype = 2
      jcone = .5
      iouttype = 0
      accoff = .false.
      acclin = .false.
      halt = .false.
      fzrz_mode = 0       ! default exchange mode
      file_type = 0       ! default fzrz file
      fzrz_spec = .false.
      stp_spec = .false.
      out_spec = .false.
      out_lun = 6
      zstate = 1   !enable .zeb display
      tstate = 0   !0=partial, 1=full
      dstate = 1   !enable data display (n/a now)
c
c     initialize zebra
c
      CALL MZEBRA(0)
c
c     reset for003 to point to the terminal
c
      IQPRNT = 6
      IQPR2 = 6
      IQLOG = 6
C
C     now finish the initialization
C
      CALL INZCOM(0)              ! initialize ZEBCOM
      CALL INZSTP                 ! initialize ZEBSTP
      CALL INZLNK                 ! initialize ZLINKA
c
      pstore = 0
      dbname(0) = 'ZEBCOM  '
      dbname(1) = 'ZEBSTP  '
      dbname(2) = 'GEANT   '
      dbname(3) = 'ZEBWRK  '
c
      return
      end
