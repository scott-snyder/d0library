C----------------------------------------------------------------------
C-
C-   Created   4-MAY-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER MX_FILE
      PARAMETER( MX_FILE = 256 )   
      COMMON /FILE_WT/ FILE_NAMES(MX_FILE),EVENTS(MX_FILE),
     &  WEIGHTS(MX_FILE),IFILE
      CHARACTER*255 FILE_NAMES
      REAL    WEIGHTS
      INTEGER EVENTS
      INTEGER IFILE
