      SUBROUTINE BKSTTH(LSUP,NDAT,LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank 'STTH'.
C-
C-   Inputs  :
C-      LSUP    I   address of support bank.
C-
C-   Outputs :
C-      LADDR   I   address of bank, STTH.
C-
C-   Controls:
C-
C-   Created  22-MAR-1991   O.Eroshin
C-   Updated  24-JUN-1991   Daria Zieminska: eliminate one argument;
C-                          remove the code filling banks
C-   Updated  30-MAR-1992   V.Sirotenko add MZFORM
C-            6/94  MF  Add NDAT argument
C-   Updated  20-Dec-1994   I.Mandrichenko: new format of STTH
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
      INCLUDE 'D0$LINKS:IZSTTH.LINK'
C  -- variable in arguments...
      INTEGER LSUP,NDAT,LADDR
C  -- local variables...
      INTEGER ND,MMBK(5),IDUM
      LOGICAL FIRST
C  -- initialize data...
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        IDUM     = 0                      ! for dummy argument.
        CALL UCTOH('STTH',MMBK(1),4,4)    ! IDH (hollerith bank name)
        MMBK(2)  = 0                      ! NL (total number of links)
        MMBK(3)  = 0                      ! NS (number of struct. links)
        ND       = 1
        MMBK(4)  = ND                     ! ND (number of data words)
        CALL MZFORM('STTH','/ 1I 7F',MMBK(5))
        FIRST    = .FALSE.
      ENDIF
C
      LADDR       = 0
C
C  -- Book bank...
C
      IF(NDAT.EQ.0) THEN
        MMBK(4)=ND
      ELSE
        MMBK(4)=NDAT
      ENDIF
      IF(LSUP.NE.0.AND.NDAT.NE.0)
     +  CALL MZLIFT(IXMAIN,LADDR,LSUP,-IZSTTH,MMBK,0)
C
  999 RETURN
      END
