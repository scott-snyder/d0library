      SUBROUTINE MUGTWIR(NMOD,NPL,NCMAX,XH,NWIR,XWIR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a point in module finds the associated
C-                         wire number
C-
C-   Inputs  : NMOD     Module number
C-             NPL      plane number
C-             NCMAX    Number of cells in plane
C-             XH(3)    point coordinate
C-
C-   Outputs : NWIR     Wire number (starts from 1)
C-             XWIR     Center of Wire coordinates
C-   Controls:
C-
C-   Created   4-FEB-1993   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMOD,NPL,NCMAX,NWIR
      REAL XH(3),XWIR(3)
      INTEGER I,NSPAR,NBUF,IBUF,NC,J
      CHARACTER*4 HSHAPE
      REAL SPAR(3),XPAR(3),ROTM(3,3),VOFF
      REAL DIST,XMINDIST
C
      XMINDIST = 99999.0
      NWIR = 0
C
      DO I=1,NCMAX
        NC = I - 1
        CALL MUCELL(NMOD,NPL,NC,HSHAPE,NSPAR,SPAR,XPAR,ROTM,
     &                 VOFF,NBUF,IBUF)
        DIST = (XH(1)-XPAR(1))**2 + (XH(2)-XPAR(2))**2 +
     &             (XH(3)-XPAR(3))**2
        DIST = SQRT(DIST)
        IF(DIST .LT. XMINDIST) THEN
          NWIR = I
          DO J=1,3
            XWIR(J) = XPAR(J)
          ENDDO
          XMINDIST = DIST
        ENDIF
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
