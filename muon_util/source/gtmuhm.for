      SUBROUTINE GTMUHM(IMOD,NMOD,IMUD1,ITRNC,NMUHP,IMUHP,
     &                  JPLN,NMUOH,IMUOH,NMSCT,IMSCT,ISP1,ISP2,
     &                  ITRIG,ICENFL,JCCT,JFINE,JLAT)
C=====================================================================
C
C  Purpose & Methods : Gets all information from bank MUHM for module IMOD
C
C  Author : M. Fortner    15-JAN-1994
C
C  Argument Descriptions:
C  ======================
C  IMOD - Integer - Input - Module loop variable for which one would like hit
C                           information
C  NMOD - Integer - Output - Module number (0 if no bank)
C  IMUD1 -  I            Location in MUD1 of header information
C  ITRNC -  I            Truncation flag from MUD1
C  NMUHP-   I            Number of raw MUHP hits in module
C  IMUHP -  I            Number in MUHP of first hit
C  JPLN  -  I            Bit map of planes with processed hits
C  NMUOH -  I            Number of processed MUOH hits in module
C  IMUOH -  I            Number in MUOH of first hit
C  NMSCT -  I            Number of scintillator MSCT hits in module
C  IMSCT -  I            Number in MSCT of first hit
C  ISP1  -  I            Spare
C  ISP2  -  I            Spare
C  ITRIG -  I            Trigger flag = 1:CCT, 2:OTC Low, 3: OTC High
C  ICENFL-  I            Centroid flag = 1:use MUHP, 2:use MUOH
C  JCCT  -  I            Bit map of coarse centroids (0-15)
C  JFINE(4) -  I         Bit maps of fine centroids (0-31,32-63,64-95,96-127)
C  JLAT(3)  -  I         Bit maps of latches (0-31,32-63,64-95)
C
C======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IMOD,NMOD,IMUD1,ITRNC,NMUHP,IMUHP,JPLN,NMUOH,IMUOH
      INTEGER NMSCT,IMSCT,ISP1,ISP2,ITRIG,ICENFL,JCCT,JFINE(4),JLAT(3)
      INTEGER LMUHM,GZMUHM
      EXTERNAL GZMUHM
C
      NMOD = 0
      LMUHM = GZMUHM(IMOD)
      IF (LMUHM.EQ.0) RETURN
      NMOD   = IQ(LMUHM+1)
      IMUD1  = IQ(LMUHM+2)
      ITRNC  = IQ(LMUHM+3)
      NMUHP  = IQ(LMUHM+4)
      IMUHP  = IQ(LMUHM+5)
      JPLN   = IQ(LMUHM+6)
      NMUOH  = IQ(LMUHM+7)
      IMUOH  = IQ(LMUHM+8)
      NMSCT  = IQ(LMUHM+9)
      IMSCT  = IQ(LMUHM+10)
      ISP1   = IQ(LMUHM+11)
      ISP2   = IQ(LMUHM+12)
      ITRIG  = IQ(LMUHM+13)
      ICENFL = IQ(LMUHM+14)
      JCCT   = IQ(LMUHM+15)
      JFINE(1)= IQ(LMUHM+16)
      JFINE(2)= IQ(LMUHM+17)
      JFINE(3)= IQ(LMUHM+18)
      JFINE(4)= IQ(LMUHM+19)
      JLAT(1)= IQ(LMUHM+20)
      JLAT(2)= IQ(LMUHM+21)
      JLAT(3)= IQ(LMUHM+22)
      RETURN
      END
