      SUBROUTINE PUSUMM( NITEM, SUMITEM )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a summary table at bottom left corner
C-
C-   Inputs  : NITEM [I] : Number of summary items.
C-             SUMITEM(1:*) [C*] : title of the summary items.
C-   Controls:
C-
C-   Created   8-FEB-1991   Nobuaki Oshima( Modified PUMENU.FOR )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NITEM
      CHARACTER*(*) SUMITEM(1:*)
      REAL    SXWMIN, SXWMAX, SYWMIN, SYWMAX
      REAL    SXVMIN, SXVMAX, SYVMIN, SYVMAX
      REAL    XVMIN,  XVMAX
      REAL    YWMIN,  YWMAX,  YVMIN,  YVMAX
      REAL    XCENTR, YCENTR
      REAL    X0, Y0, X1, Y1, VX, VY, XTENT, XPOS, YPOS, CHAR
      REAL    YCHAR,DIVSCR
      REAL    VWSAVE(85)
      INTEGER NBCHAR, I
      INTEGER ACTUAL, MAXCHR, INPFCT
      CHARACTER*1 STRING
      CHARACTER*3 DRIVER
      DATA MAXCHR/1/
C----------------------------------------------------------------------
C-
      CALL JVSAVE(VWSAVE)
      CALL PURSET
C-
      CALL J4RGET(1,SXWMIN,SXWMAX,SYWMIN,SYWMAX)
      CALL J4RGET(2,SXVMIN,SXVMAX,SYVMIN,SYVMAX)
C-
   10 NBCHAR =LEN( SUMITEM(1) ) + 1
      XVMIN = SXVMIN
      DIVSCR = 3.
      IF( NBCHAR .GT. 30 )
     &  DIVSCR = 2.
      CHAR  = ( (SXVMAX-SXVMIN)/DIVSCR ) / NBCHAR
      CHAR  = .9*CHAR
      XVMAX = SXVMIN + NBCHAR * CHAR
C-
C--- If it didn't fit in Y, then limit the number of items. It not enough
C- ( i.e. not the default one ) then reduce the size...
      YVMIN = SYVMIN
      YCHAR = (SYVMAX - SYVMIN)/26.
      YVMAX = YVMIN  + YCHAR * NITEM
      IF( YVMAX .GT. SYVMAX )  THEN
        NITEM = ( SYVMAX - SYVMIN ) / YCHAR
        YVMAX  = YVMIN + YCHAR * NITEM
      ENDIF
      CALL JVPORT(XVMIN, XVMAX, YVMIN, YVMAX)
      XTENT =   NBCHAR * CHAR
      YWMIN =   .45
      YWMAX =   YWMIN + NITEM
C-
C--- Define a window in character size unit ( easy position / pick )
C-
      CALL JWINDO( 0., XTENT, YWMIN, YWMAX )
      XPOS = YWMIN * NBCHAR
      YPOS = 2.*YWMIN
C-
C---  Draw summary
C-
        CALL JOPEN
        CALL PXCOLR('FOR')
      CALL JJUST( 1, 2 )
      CALL JSIZE( CHAR, .8 )
C-
      X0 = 0.
      X1 = XTENT
      DO I = 1, NITEM
C-
C--- Ignore empty menu items, don't draw the menu box.
C-
        IF( SUMITEM(I) .NE. ' ' ) THEN
          Y0 = YPOS - YWMIN
          Y1 = YPOS + YWMIN
          CALL JRECT( X0, Y0, X1, Y1 )
          CALL PUSTRG( .01, YPOS, SUMITEM(I) )
        ENDIF
        YPOS = YPOS + 1.
       ENDDO
        CALL JCLOSE
C-
      CALL JVLOAD(VWSAVE)
  999 RETURN
      END
