      SUBROUTINE ERRDB(ROUTINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Writes message when an error occurs in a DBL3 routine
C-
C-   Inputs  : ROUTINE - subroutine name that called ERRDB
C-   Outputs : none
C-   Controls: none
C-
C-   Created   9-FEB-1990   Srini Rajagopalan
C-   Updated  28-MAY-1991   Jan Guida  Also write message to screen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:quest.inc'

      character*(*) routine
      character*81 msg
      integer iqread,iqprnt,iqpr2,iqlog,iqpnch,iqttin,iqtype

      common /zunit/ iqread,iqprnt,iqpr2,iqlog,iqpnch,iqttin,iqtype
C----------------------------------------------------------------------
      write(iqlog,*) ' ERROR in DBL3 routine ', routine ,
     #  '. IQUEST(1)=' , iquest(1)
      write(msg,*) ' ERROR in DBL3 routine. IQUEST(1)=' , iquest(1)
      call intmsg(msg)
      msg = ' '//routine
      call intmsg(msg)

  999 RETURN
      END
