C **** COMMON BLOCK LINATT
C
C    This common block is used for line attributes.
C      HUECOL(COLOR, SELECT) real  Specifies Hue for either current or 
C                                  default table.
C                                  where:
C                                     COLOR  --> is the color index
C                                     SELECT --> is the color table
C                                                either 1 for current 
C                                                or 2 for default table
C      SATCOL(COLOR, SELECT) real  Specifies saturation for either 
C                                  current or default table.
C                                  where:
C                                     COLOR  --> is the color index
C                                     SELECT --> is the color table
C                                                either 1 for current 
C                                                or 2 for default table
C      MINTEN      real       Minimum allowable intensity.
C      CURCOL      integer    Current selected color.
C      DEFCOL      integer    Default current color.
C      CINTEN      integer    Current intensity (values 0 to 32767)
C      DINTEN      integer    Default intensity (values 0 to 32767)
C      CLSTYL      integer    Current line style (0 to 32767)
C      DLSTYL      integer    Default line style (0 to 32767)
C      CMARKR      integer    Current marker selected.
C      DMARKR      integer    Default marker.
C
      REAL MINTEN
      REAL HUECOL(40,2), SATCOL(40,2)
      INTEGER CURCOL, DEFCOL, DINTEN, CINTEN, DLSTYL, CLSTYL, CMARKR
      INTEGER DMARKR

      COMMON /LINATT/ CURCOL, DEFCOL, HUECOL, SATCOL, DINTEN, CINTEN
      COMMON /LINATT/ DLSTYL, CLSTYL, CMARKR, DMARKR, MINTEN
