C Link and Tree parameters...
      INTEGER MXNEXT
      PARAMETER (MXNEXT=7)
      INTEGER MAXLNK
      PARAMETER (MAXLNK=300)
      INTEGER MAXCHN
      PARAMETER (MAXCHN=20)
C For the routine DSVCHN we insert a maximum number of chains of 100
C     to prevent overflow in the Zebra bank...
      INTEGER NMXCHN
      PARAMETER (NMXCHN=100)
