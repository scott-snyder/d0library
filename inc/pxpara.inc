C----------------------------------------------------------------------
C-
C-   Created   8-JUL-1988   Olivier Callot
C-   Added    19-OCT-1990   Nobuaki Oshima
C-
C----------------------------------------------------------------------
C
C ** These are the center and magnification of the current viewport, to map a
C ** -1,1 viewport to the Device viewport as modified by PXENTR
C
      REAL    XDVMAG, YDVMAG                   
      REAL    XCVPRT, YCVPRT, XMVPRT, YMVPRT
C
C--- These are the current viewport and window limits
C
      REAL    XVPRT1, XVPRT2, YVPRT1, YVPRT2
      REAL    XWIND1, XWIND2, YWIND1, YWIND2
      INTEGER MAXSEG    
      INTEGER PICSEG    
      INTEGER ICSCRN    
      INTEGER ICVPRT    
      INTEGER IRTSEG(10)
      LOGICAL DI3RDY    
      LOGICAL LCONNR    
C------------------------------------------------------------------------
      COMMON /PXPARA/ XDVMAG, YDVMAG,
     &                XCVPRT, YCVPRT, XMVPRT, YMVPRT,
     &                XVPRT1, XVPRT2, YVPRT1, YVPRT2,
     &                XWIND1, XWIND2, YWIND1, YWIND2,
     &                MAXSEG, PICSEG, ICSCRN, ICVPRT,
     &                IRTSEG, DI3RDY, LCONNR
