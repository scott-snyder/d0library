      SUBROUTINE MENSET(MNUNAM)
C-   Purpose and Methods : Read menu.set files and convert them to menus.
      CHARACTER*(*) MNUNAM
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
      if(isdbug.gt.0) type *,' MENSET called for name,: ',MNUNAM
      RETURN
      END
