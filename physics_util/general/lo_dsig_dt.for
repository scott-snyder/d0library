      SUBROUTINE LO_DSIG_DT(s_h,t_h,u_h,a_s,DS_DT_1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate all subprocess differential
C-                          cross sections to leading order
C-
C-   Inputs  : s_h, t_h, and u_h - subprocess Mandelstam Variables
C-             a_s               - alpha_s 
C-   Outputs : DS_DT_1(1:12)     - the twelve subprocess differential
C-                                  cross sections, dsigma/dt
C-   Controls: None
C-
C-   Created  18-Oct-1993   Sandor Feher and Patrick Mooney
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INTEGER         I
      REAL*8          s_h,sh2,t_h,th2,u_h,uh2
      REAL*8          pi,DS_DT_1(12)
      REAL*8          a_s,as2
      DATA            pi/3.1415927/
C
C----------------------------------------------------------------------
C
C   Initialization
C
      sh2 = s_h**2
      th2 = t_h**2
      uh2 = u_h**2
C
      as2 = a_s**2
C
      DO I = 1,12
        DS_DT_1(I) = 0.
      ENDDO
C
C----------------------------------------------------------------------
C
C   Quark + Quark' --> Quark + Quark'
C
      DS_DT_1( 1) = (as2*pi/sh2) * (
     &            +          ( 4./ 9.)*(sh2+uh2)/th2 )
C
C----------------------------------------------------------------------
C
C   Quark + Quark --> Quark + Quark
C
      DS_DT_1( 2) = (as2*pi/sh2) * (
     &            +        ( ( 4./ 9.)*(sh2+uh2)/th2 )
     &            +        ( ( 4./ 9.)*(sh2+th2)/uh2 )
     &            -        ( ( 8./27.)*sh2/(t_h*u_h) ) )
C
C----------------------------------------------------------------------
C
C   Quark + Quark_bar --> Quark' + Quark_bar
C
      DS_DT_1( 3) = (as2*pi/sh2) * (
     &            +          ( 4./ 9.)*(th2+uh2)/sh2 )
C
C----------------------------------------------------------------------
C
C   Quark + Quark_bar --> Quark + Quark_bar
C
      DS_DT_1( 4) = (as2*pi/sh2) * (
     &            +        ( ( 4./ 9.)*(sh2+uh2)/th2 )
     &            +        ( ( 4./ 9.)*(uh2+th2)/sh2 )
     &            -        ( ( 8./27.)*uh2/(s_h*t_h) ) )
C
C----------------------------------------------------------------------
C
C   Gluon + Quark --> Gluon + Quark
C
      DS_DT_1( 5) = (as2*pi/sh2) * (
     &            -        ( ( 4./ 9.)*s_h/u_h )
     &            -        ( ( 4./ 9.)*u_h/s_h )
     &            +        ( ( 1./ 1.)*(sh2+uh2)/th2 ) )
C
C----------------------------------------------------------------------
C
C   Quark + Quark_bar --> Gluon + Gluon
C
      DS_DT_1( 6) = (as2*pi/sh2) * (
     &            +        ( (32./27.)*t_h/u_h )
     &            +        ( (32./27.)*u_h/t_h )
     &            -        ( ( 8./ 3.)*(th2+uh2)/sh2 ) )
C
C----------------------------------------------------------------------
C
C   Gluon + Gluon --> Quark + Quark_bar
C
      DS_DT_1( 7) = (as2*pi/sh2) * (
     &            +        ( ( 1./ 6.)*t_h/u_h )
     &            +        ( ( 1./ 6.)*u_h/t_h )
     &            -        ( ( 3./ 8.)*(th2+uh2)/sh2 ) )
C
C----------------------------------------------------------------------
C
C   Gluon + Gluon --> Gluon + Gluon
C
      DS_DT_1( 8) = (as2*pi/sh2) * (
     &            +        ( ( 9./ 2.)*3. )
     &            -        ( ( 9./ 2.)*t_h*u_h/sh2 )
     &            -        ( ( 9./ 2.)*s_h*u_h/th2 )
     &            -        ( ( 9./ 2.)*s_h*t_h/uh2 ) )
C
C----------------------------------------------------------------------
  999 RETURN
      END
