      SUBROUTINE GTMUHT(NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD,
     &                  NMUHP,NMUOF,NMSCT,NVERS,LPMUOF)
C====================================================================
C
C  Description:  Gets all the information from the MUHT Zebra bank.
C  ============
C
C  Argument Declarations:
C  ========================
C  Output:
C          NWRAW - I - Number of raw WAMUS hits in bank MUD1
C          NWPRO - I - Number of processed WAMUS hits in MUOH
C          NWMOD - I - Number of WAMUS modules hit
C          NSRAW - I - Number of raw WAMUS hits in bank MUD1
C          NSPRO - I - Number of processed WAMUS hits in MUOH
C          NSMOD - I - Number of WAMUS modules hit
C          NMUHP - I - Number of hit pointers in MUHP
C          NMUOF - I - Number of hit modules in MUOF
C          NMSCT - I - Number of scintillator hits in MSCT
C          NVERS - I - Version number of MUD1
C    LPMUOF(460) - I - Pointer into MUOF bank for module id
C                      1-460. If 0, no hit in that module.
C  
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - June 29,1987
C
C *1     7-FEB-1988 17:34:59 TAMI "GET MUON HIT INFORMATION FROM MUHT"
C *1    27-OCT-1988 22:48:44 HEDIN "FROM ZEBRA_UTIL"
C       26-AUG-1993 M. Fortner      Use new format
C       15-DEC-1993 S. Hagopian -  Use either Run Ia or Ib format
C
C=======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NMUHT
      INTEGER NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD
      INTEGER NMUHP,NMUOF,NMSCT,NVERS,LPMUOF(460)
      INTEGER GZMUHT,LMUHT,I
C
      NVERS = 0
      LMUHT = GZMUHT(0)
      IF (LMUHT.EQ.0) GOTO 999
      NWRAW = IQ(LMUHT+1)
      NWPRO = IQ(LMUHT+2)
      NWMOD = IQ(LMUHT+3)
      NSRAW = IQ(LMUHT+4)
      NSPRO = IQ(LMUHT+5)
      NSMOD = IQ(LMUHT+6)
      NMUHP = IQ(LMUHT+7)
      NMUOF = IQ(LMUHT+8)
      NMSCT = IQ(LMUHT+9)
      NVERS = IQ(LMUHT+10)
      NMUHT = IQ(LMUHT-1)
      IF (NMUHT.LE.10) GOTO 999
      DO 34 I = 1,NMUHT-10
         LPMUOF(I) = IQ(LMUHT+10+I)
   34 CONTINUE
  999 RETURN
      END
