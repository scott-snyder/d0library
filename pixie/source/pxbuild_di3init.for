C
C======================================================================
      SUBROUTINE PXBUILD_DI3INIT
C======================================================================
C
C  Description: Initialze DI-3000 system
C  ============
C
C
C  Author:
C  =======
C  LUPE ROSAS
C
C======================================================================

      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:GRAPHF77.INC'
C----------------------------------------------------------------------
C
C  Local Declarations:
C  ===================
C
      REAL VX,VY             ! Locator echo point
      REAL UMAX,VMAX         ! Window Coordenates
      REAL    RATIO,FRSCRE
C----------------------------------------------------------------------
      LOGICAL IRIGHT
      DATA IRIGHT/.TRUE./
      LOGICAL TRUTH
C----------------------------------------------------------------------
C
C ****  Initialize DI-3000
C
C
      CALL JBEGIN
      CALL JDINIT(1)
      CALL JDEVON(1)
C
C  Set the Debug value:
C  ====================
C
C  Set-up Default Values for DI-3000:
C  ==================================

      CALL JDJUST(2,2)
      CALL JDSIZE(2.,4.)
      CALL JDCOLR(COLMAG)
      CALL JDDETE(32767)
      CALL JDPINT(0)
      CALL JFRAME(1)
C
C  Set-up right-handed coordinate system:
C  ======================================
C
      CALL JRIGHT( .TRUE. )
      CALL JVUPNT( 0., 0., 0. )
      CALL JUPVEC( 0., 1., 0. )
      CALL JNORML( 0., 0.,-1. )
C
C  Set-up a default window
C  =======================
C      UMAX=120.* .700
C      VMAX=120.* .700
      UMAX = 1.
      VMAX = 1.
      CALL JWINDO(-UMAX,UMAX,-VMAX,VMAX)
C
C ****  Setting up DI3000 configuration
C
      CALL JASPEK(1,RATIO)
C
C ****  Map the device window taking account of the aspect ratio
C
      FRSCRE = 1.
      XCVPRT = 0.
      YCVPRT = 0.
      XMVPRT = 1.
      YMVPRT = 1.
      XDVMAG = FRSCRE
      YDVMAG = FRSCRE
      IF ( RATIO .LT. 1. ) THEN
        YDVMAG = FRSCRE * RATIO
        YMVPRT = 1.     * RATIO
      ELSEIF ( RATIO .GT. 1. ) THEN
        XDVMAG = FRSCRE / RATIO
        XMVPRT = 1.     / RATIO
      ENDIF
      CALL JDEVWN( 1, -XMVPRT, XMVPRT, -YMVPRT, YMVPRT)
      CALL JTTYPE( 2 )
      CALL JRIGHT(.TRUE.)
      CALL JWCLIP( .TRUE. )
C
      RETURN
      END
