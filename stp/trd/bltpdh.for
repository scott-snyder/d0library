      SUBROUTINE BLTPDH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Builds the ZEBRA bank structure hooked
C-                        to TPDH
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-OCT-1990   JFG Simplified version of TSTPBK
C-                          Do not book the whole structure.
C-   Updated  11-MAY-1992   Alain PLUQUET add MZFORM
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPD,LTRPD
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$LINKS:IZTPDH.LINK'
      INCLUDE 'D0$LINKS:IZTPCH.LINK'
      INTEGER IOTRPD,IOTPDH
C Book header banks
      CALL MZFORM ('TPDH','-I',IOTPDH)
      CALL MZBOOK(IDVSTP,LTPDH,LSTRD,-IZTPDH,'TPDH',7,7,2,IOTPDH,0)
C Book pedestal banks
      CALL MZFORM ('TRPD','-F',IOTRPD)
      DO 10 IPD = 1, 6
        CALL MZBOOK(IDVSTP,LTRPD,LTPDH,-IPD,'TRPD',0,0,256,IOTRPD,0)
   10 CONTINUE
      END
