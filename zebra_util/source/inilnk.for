      SUBROUTINE INILNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Initialize the general Zebra link area ZLINKA
C-
C-   Created  10-DEC-1987   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C----------------------------------------------------------------------
      CALL MZLINK(IXMAIN,'ZLINKA',LSLINK,LRLINK,LRLINK(NRLINK))
  999 RETURN
      END
