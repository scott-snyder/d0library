      SUBROUTINE MENUOP(NUMOPT,MENOPT,ISEL)
C-   Purpose and Methods : Make a simple and direct call to pop up
C-       menu of NUMOPT options listed in MENOPT.  The selection is
C-       returned in ISEL
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
      CHARACTER*(*) MENOPT(*)
      ISEL=MAKEMENU(NUMOPT,MENOPT,' SGI COMPACK')
      RETURN
      END
