C----------------------------------------------------------------
C-                  *****MAINPACK.INC*****
C-
C-  MENUS GOTTEN FROM MENNEW AND MENADD CALLS (LOGICAL NAME = FNAME)
C-
C-  Updated 02-DEC-1992 Lupe Howell Defined all variables
C-  Updated 09-FEB-1993 Lupe Howell Add a couple more variable for help
C-
C-------------------------------------------------------------------

C
C  *** MAXLIN(NITMAX)            : Maximum number of menu items per level
C  *** MENULEV                   : Current Level
C  *** HELP_COOKIES(NITMAX,NMENX): Help of items per menu
C  *** NMENU                     : Total number of menus
C  *** NITEMS(NMENMX)            : Total of items per menu
C  *** ITEMNM(NITMAX,NMENMX)     : Items Names per menu
C  *** ITEMDS(NITMAX,NMENX)      : Action names of items per menu
C  *** FNAME(NMENMX)             : Names of menus
C
      INTEGER NITMAX,NMENMX
      PARAMETER (NITMAX=40,NMENMX=40)
C
C *** Non Character common block
C
      INTEGER   MAXLIN(NITMAX),MENULEV
      INTEGER*4 HELP_COOKIES(NITMAX,NMENMX)
      INTEGER*2 NMENU,NITEMS(NMENMX)
C
      COMMON/GMENUS/MAXLIN,MENULEV,HELP_COOKIES,NMENU,NITEMS
C
C *** Character common block for the menu levels
C
      CHARACTER*40 ITEMNM(NITMAX,NMENMX),ITEMDS(NITMAX,NMENMX)
      CHARACTER*40 FNAME(NMENMX)
C
      COMMON/CGMENUS/ITEMNM,ITEMDS,FNAME
C
C ***  FLAGS
C
      COMMON/FLGS/ASTFLG,CANFLG
      LOGICAL ASTFLG,CANFLG
C
C *** ROUTING COMMON FOR INTERRUPT MENUS
C
      COMMON/ROUTE/IRNOW,JMENU,NDSPAT,DSNAME(NMENMX)
      INTEGER IRNOW,JMENU,NDSPAT
      CHARACTER*40 DSNAME
C
C *** PF BUTTONS
C
      COMMON/PFCOMN/PFSTR(4),IPFNUM
      CHARACTER*40 PFSTR
      INTEGER IPFNUM
C
C *** DEBUGGING
C
      COMMON/SDBCOM/ISDBUG
      INTEGER ISDBUG
