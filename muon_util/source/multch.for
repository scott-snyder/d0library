      SUBROUTINE MULTCH(IADD,IODDL,IEVENL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack pad latch information
C-
C-   Inputs  : IADD   - MUD1 address word
C-   Outputs : IODDL  - = 1 if odd pad latch was set
C-             IEVENL - = 1 if even pad latch was set
C-   Controls: 
C-
C-   Created  8-MAY-1990   D.HEDIN
C-   DH 3/92 get rid of octal format (yuch)
C---------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IADD,IODDL,IEVENL
      INTEGER I,B30,B31,II
C      DATA B30,B31/'10000000000'O,'20000000000'O/
C----------------------------------------------------------------------
      IODDL=0
      IEVENL=0
      I=ISHFT(IADD,-31)
      IF(I.NE.0) IODDL=1
      II=ISHFT(IADD,-30)
      I=IAND(II,1)
      IF(I.NE.0) IEVENL=1
      RETURN
      END
