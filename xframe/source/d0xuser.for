      LOGICAL FUNCTION D0XUSER(iarg)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : default dummy function for D0X.  this function
C-                         is called once per event by the framework, and
C-                         can be called explicitly through the "Control"
C-                         menu (see iarg below)
C-
C-   Returned value  : .TRUE./.FALSE. only if used as a filter for
C-                     event stripping (look at the OUTPUT menu)
C-   Inputs  : iarg    same philosophy as "iflag" in MINUIT:  called with
C-                     value iarg=0 for each event but that's it.  if you
C-                     want your function to do something different for
C-                     non-event reads (e.g. job finish, etc.) then test
C-                     on iarg and call D0XUSER explicitly with the right
C-                     argument from the "Control" menu
C-                     
C-   Outputs : 
C-   Controls: 
C-
C-   Created  19-AUG-1993   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      integer iarg
c
C----------------------------------------------------------------------
c
c     preset to .true.
c
      d0xuser = .true.

      if (iarg.eq.0) then
c
c       this is the "event" flag
c
      else
c
c       whatever....
c
      endif
  999 RETURN
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function d0xuser_init(iarg)
c
c     called for initialization
c
      implicit none
c
      integer iarg
c
      d0xuser_init = .true.
      if (iarg.eq.0) return
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function d0xuser_finish(iarg)
c
c     called for finishing everything
c
      implicit none
c
      integer iarg
c
      d0xuser_finish = .true.
      if (iarg.eq.0) return
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function d0xuser_talk(iarg)
c
c     called for interactive "talking"
c
      implicit none
c
      integer iarg
c
      d0xuser_talk = .true.
      if (iarg.eq.0) return
c
      return
      end
