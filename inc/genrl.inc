C **** COMMON BLOCK GENRL
C    This common block is the level 0 common block that contains the
C    global and most fundamental variables used for error checking and
C    determining status of the emulator.
C
C    The variable that are used are as follows
C
C        BEGIN    logical        designates that JBEGIN has been called.
C        EMVERS   real           Value is the current emulator version.
C        DSPDEV   integer        Value is a dummy for device number.
C
      LOGICAL BEGIN
      REAL    EMVERS
      INTEGER DSPDEV

      COMMON /GENRL/ BEGIN, EMVERS, DSPDEV
