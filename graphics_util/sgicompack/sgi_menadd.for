      SUBROUTINE MENADD(MENNAM,NOTITL,ITMNAM,ACTION,HLPTXT)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put information for a new item into commonblock
C-
C-   Inputs : MENNAM [C*]: Name of menulevel to use
C-            NOTITL [L ]: Logical flag for title or no title
C-            ITMNAM [C*]: Name of item for menu display
C-            ACTION [C*]: Action verb for the command
C-            HELPTXT[C*]: Information for item.
C-
C-   Outputs: None
C-
C-   Controls: MAXLIN for level is increased
C-
C-   Created XX-XXX-XXX Mike Shupe
C-   Updated 08-FEB-1993 Lupe Howell Clean up add help
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) MENNAM,ITMNAM,ACTION,HLPTXT
      LOGICAL NOTITL
C
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
C
      INTEGER I,IMENU,NITEM
      INTEGER*4 STRSTO
C------------------------------------------------------------------------
C
C *** Search for the menu level
C
      IMENU=0
      DO 5000 I=1,NMENU
        IF(FNAME(I).NE.MENNAM) GO TO 5000
        IMENU=I
        GOTO 5001
 5000 CONTINUE
C
C *** If menu level not found display error message
C
      TYPE 555,MENNAM
  555 FORMAT(' MENADD--NO MATCH WITH MENU NAME:',A40)
      GO TO 999
C
C *** We recognize the menu name. Add the item
C *** the item count includes the exit items: exit and back
C
 5001 NITEM=NITEMS(IMENU)
      IF(NITEM.EQ.NITMAX) GO TO 6000
C
C *** Increase number of items counter
C
      NITEM=NITEM+1
C
C *** Add menu level to the common block
C
      MAXLIN(IMENU) = NITEM
C
C *** Add the item name to the common block
C
      ITEMNM(NITEM,IMENU)=ITMNAM
C
C *** Add the ation to the common block
C
      ITEMDS(NITEM,IMENU)=ACTION
C
C *** Add number item to common block
C
      NITEMS(IMENU)=NITEM
C
C *** Add Help information to common block
C
      HELP_COOKIES(NITEM,IMENU) = STRSTO(HLPTXT)
 6000 CONTINUE
  999 RETURN
      END
