      SUBROUTINE PCTEXT_PST(ILINE,TEXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw trigger name at left side in multi-
C-                         viewports. Need this due to the funny PST
C-                         driver.
C-
C-   Inputs  : TEXT [C*] - Your messages to be shown
C-
C-   Created  03-NOV-1993   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL    XMIN, XMAX, YMIN, YMAX
      REAL    XSIZ, YSIZ, XBASE, YBASE
      REAL    VWSAVE(85)
      INTEGER ILINE
      CHARACTER*(*) TEXT
      LOGICAL FLGVAL
C----------------------------------------------------------------------
C-
C---
      CALL JVSAVE(VWSAVE)
C---
      CALL JUPVEC(0.,1.,0.)
      CALL JNORML(0.,0.,-1.)
      CALL J4RGET(1, XMIN, XMAX, YMIN, YMAX)
      XSIZ = (XMAX-XMIN) * .0143
      YSIZ = XSIZ*1.7
      IF ( FLGVAL('HARDCOPY') ) THEN
        XSIZ = XSIZ*.7
        YSIZ = YSIZ*.7
      ENDIF
      XBASE = XMIN + (XMAX-XMIN)*0.004
      YBASE = YMIN + (YMAX-YMIN)*0.969
      YBASE = YBASE - ((YSIZ*1.1) * (ILINE-1))
C-
C--- DRAW TEXT
C-
      CALL JOPEN
      CALL PXCOLR('FOR')
      CALL JJUST(1,2)
      CALL JSIZE(XSIZ,YSIZ)
CNO      CALL JFONT(1)
      CALL JFONT(5)
      CALL J3MOVE(XBASE, YBASE, 0.)
      CALL J1STRG(TEXT)
C...      CALL JHSTRG(TEXT)
      CALL JCLOSE
C-
      CALL JVLOAD(VWSAVE)
  999 RETURN
      END
