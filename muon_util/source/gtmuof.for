C VAX/DEC CMS REPLACEMENT HISTORY, Element GTMUOF.FOR
C *2    21-OCT-1993 08:48:18 FORTNER "add terms for scintillator"
C *1    15-SEP-1993 17:24:20 DARIEN "New MF code for 1B MUD1"
C VAX/DEC CMS REPLACEMENT HISTORY, Element GTMUOF.FOR
      SUBROUTINE GTMUOF(IMOD,NMOD,NRAW,IMUHP,NPROC,IMUOH,
     &                  NSCNT,IMSCT,NHPLR,NHPLP,IMUD1)
C=====================================================================
C
C  Description:  Gets all information from bank MUOF for module IMOD
C  ============
C
C  Argument Descriptions:
C  ======================
C  IMOD - Integer - Input - Module loop variable for which one would like hit
C                           information
C  NMOD - Integer - Output - Module number
C  NRAW -   I            NUMBER OF RAW HITS IN MODULE
C  IMUHP -  I            LOCATION IN MUHP OF FIRST DATA WORD FOR MODULE
C  NPROC -  I            NUMBER OF PROCESSED HITS IN MODULE
C  IMUOH -  I            LOCATION IN MUOH OF FIRST DATA WORD FOR MODULE
C  NSCNT -  I            NUMBER OF SCINTILLATOR HITS IN MODULE
C  IMSCT -  I            LOCATION IN MSCT OF FIRST DATA WORD FOR MODULE
C  NHPLR -  I            BIT MAP OF PLANES WITH RAW HITS
C  NHPLP -  I            BIT MAP OF PLANES WITH PROCESSED HITS
C  IMUD1 -  I            LOCATION IN MUD1 OF HEADER INFORMATION
C
C *1     7-FEB-1988 17:35:38 TAMI "GET MUON HIT INFO FROM MUOF BANK FOR MODULE IMOD"
C *1    27-OCT-1988 22:48:46 HEDIN "FROM ZEBRA_UTIL"
C *2    15-JUL-1989 22:04:08 HEDIN "NEW FORMAT"
C       26-AUG-1993  M. Fortner    Run 1B Format
C======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IMOD,NMOD,NRAW,IMUHP,IMUD1,NPROC,IMUOH
      INTEGER NSCNT,IMSCT,NHPLR,NHPLP
      INTEGER LMUOF,GZMUOF,IMUOF
C
      LMUOF = GZMUOF(0)
      IMUOF = LMUOF + 10*(IMOD-1)
      NMOD = IQ(IMUOF+1)
      NRAW = IQ(IMUOF+2)
      IMUHP= IQ(IMUOF+3)
      NPROC= IQ(IMUOF+4)
      IMUOH= IQ(IMUOF+5)
      NSCNT= IQ(IMUOF+6)
      IMSCT= IQ(IMUOF+7)
      NHPLR= IQ(IMUOF+8)
      NHPLP= IQ(IMUOF+9)
      IMUD1= IQ(IMUOF+10)
      RETURN
      END
