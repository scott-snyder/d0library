      SUBROUTINE PUVPRT( X1, X2, Y1, Y2 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open a viewport, within the current DISPLAY
C-               viewport, this means that x1,x2, ... are relative to the
C-               current viewport.
C-
C-   Inputs  : X1, X2, Y1, Y2 [F] are the limit of the viewport, in the range
C-                                [-1., 1.], in relative coordinates.
C-   Outputs :
C-   Controls:
C-
C-   Created   1-AUG-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
      REAL    X1, X2, Y1, Y2, XCENT, XSCAL, YCENT, YSCAL
      REAL    XV1, XV2, YV1, YV2
C----------------------------------------------------------------------
      XV1 = MAX( X1, -1. )
      XV2 = MIN( X2,  1. )
      YV1 = MAX( Y1, -1. )
      YV2 = MIN( Y2,  1. )
      IF ( XV1 .EQ. XV2 ) THEN
        XV1 = MAX( X1-0.01, -1. )
        XV2 = MIN( X2+0.01,  1. )
      ENDIF
      IF ( YV1 .EQ. YV2 ) THEN
        YV1 = MAX( Y1-0.01, -1. )
        YV2 = MIN( Y2+0.01,  1. )
      ENDIF
      XCENT = .5 * ( XVPRT1 + XVPRT2 )
      YCENT = .5 * ( YVPRT1 + YVPRT2 )
      XSCAL = .5 * ( XVPRT2 - XVPRT1 )
      YSCAL = .5 * ( YVPRT2 - YVPRT1 )
      CALL JVPORT( XCENT + XV1 * XSCAL, XCENT + XV2 * XSCAL,
     &             YCENT + YV1 * YSCAL, YCENT + YV2 * YSCAL )
  999 RETURN
      END
