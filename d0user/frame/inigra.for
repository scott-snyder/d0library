      SUBROUTINE INIGRA
C---------------------------------------------------------------------
C-                                                                   -
C-      Supposed to initialize all graphics, at present only         -
C-      sets size of blank common for histograms                     -
C-                                                                   -
C-                       SDP Oct,1986                                -
C-                                                                   -
C---------------------------------------------------------------------
C
      COMMON // HMEMOR(5000)
      CALL HTITLE('D0USER$')
      CALL HLIMIT(5000)
      RETURN
      END
