      SUBROUTINE PURSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Resets the DI3000 default viewpoint, and the
C-                      viewport/window parameters
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: 
C-
C-   Created  18-OCT-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
C----------------------------------------------------------------------
C
C ****  Set default 3-D viewpoint, upvector, normal, to have a correct screen.
C
C.N.O.CALL JDD3D( .FALSE. )
      CALL JRIGHT( .TRUE. )
      CALL JVUPNT( 0., 0., 0. )
      CALL JUPVEC( 0., 1., 0. )
      CALL JNORML( 0., 0.,-1. )
      CALL JVPORT( -XDVMAG, XDVMAG, -YDVMAG, YDVMAG )
      CALL JWINDO( -1., 1., -1., 1. )
  999 RETURN
      END
