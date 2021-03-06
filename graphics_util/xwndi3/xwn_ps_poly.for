      SUBROUTINE PS_POLY(N,P)
C  DRAW UNFILLED OR FILLED POLYGONS
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      REAL P(3,*)
c      type *,' PS_POLY - N',N
      IF(IFILL.EQ.1) THEN        ! CHECK FOR POLF
        IF(IPCOLO.GT.0) THEN
          IF(ICLR.EQ.0) THEN     ! BLACK AND WHITE, DO GREY SCALE
            CALL J_GREY(RVEC(IPCOLO),GVEC(IPCOLO),BVEC(IPCOLO),GREY)
            WRITE(IDRUNI,10) GREY
   10       FORMAT(' ',F5.3,' g')
          ELSE
            CALL PS_COLOR(IPCOLO)
          ENDIF
        ENDIF
      ENDIF
      CALL PS_MOVE(P(1,1),P(2,1),P(3,1))
      DO I=2,N
        CALL PS_DRAW(P(1,I),P(2,I),P(3,I))
      ENDDO
      CALL PS_DRAW(P(1,1),P(2,1),P(3,1))
      CALL PS_COLOR(ICOLOR)
      END
