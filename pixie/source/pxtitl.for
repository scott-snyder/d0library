      SUBROUTINE PXTITL(TITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRint title in a lego plot and calaculate the dir
C-
C-   Inputs  :  TITLE - Title of the lego plot.
C-
C-   Outputs :  Title of lego plot.
C-
C-   Created  28-JUL-1988   LUPE ROSAS
C-   Updated  10-JAN-1990   LUPE HOWELL Implementing Color Table
C-   Modified 18-JAN-1990   Nobuaki Oshima
C-      Change a segment type from retained to temporary
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C-   Argument Declaration:
C-   ---------------------
      CHARACTER*(*) TITLE
C----------------------------------------------------------------------
C-   Local Declaration:
C-   ------------------
      INTEGER SEGNUM
      REAL X,Y,DIS
      REAL XSIZ,YSIZ
C.N.O      REAL JBUFF(85)
C----------------------------------------------------------------------
C.N.O      CALL JVSAVE(JBUFF)
C-
      CALL JUPVEC(0.,1.,0.)
      CALL JNORML(0.,0.,-1.)
      CALL JWINDO(-25.,25.,-25.,25.)
      CALL JOPEN
C  CENTERING TITLE
      XSIZ=1.5
      YSIZ=1.5
      X=  0.
      Y=-20.
      CALL J3MOVE(X,Y,0.)
      CALL JSIZE(XSIZ,YSIZ)
      CALL JJUST(2,2)
      CALL PXCOLR ('FOR')
      CALL JFONT(5)
      CALL J1STRG(TITLE)
      CALL JCLOSE
C-
C.N.O.      CALL JVLOAD(JBUFF)
  999 RETURN
      END
