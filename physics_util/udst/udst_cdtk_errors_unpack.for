      SUBROUTINE UDST_CDTK_ERRORS_UNPACK(PKWORD,UNPK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : takes a packed word from s/r UDST_CDTK_ERRORS_PACK
c                          and returns unpacked CDC track information in an array. 
C-                         Packing algorithm is hardcoded to ensure that it 
c                          duplicates s/r UDST_CDTK_ERRORS_PACK
c                          
C-
C-   Inputs   : PKWORD: packed integer word
C-   Outputs  : UNPK(4): unpacked real array
c      word 1 : chi squared probability in XY
c      word 2 : chi squared probability in RZ
c      word 3 : error on XY impact parameter of track w.r.t. beam axis
c      word 4 : error on Z position of track at beam axis
C-
C-   Created  3-oct-1995   Ashutosh V. Kotwal
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER*4 PKWORD
      INTEGER JBYT,ITMP
      REAL    UNPK(4),CHISIN
      EXTERNAL CHISIN,JBYT

C----------------------------------------------------------------------
C
c extract from  32 bit integer

C XY chi2 probability
          ITMP = JBYT(PKWORD, 1,8)          
          UNPK(1) = FLOAT(ITMP)/255.0

c RZ chi2 probability
          ITMP =  JBYT(PKWORD, 9,8)          
          UNPK(2) = FLOAT(ITMP)/255.0

c XY impact parameter error
          ITMP = JBYT(PKWORD,17,8)          
c 0.5 is the scale factor used in packing and unpacking XY impact
c parameter error
          UNPK(3) = 0.5*CHISIN(FLOAT(ITMP)/255.0,1)

c error on Z at beam
          ITMP = JBYT(PKWORD,25,8)          
c 3.0 is scale factor used in packing and unpacking Z error
          UNPK(4) = 3.0*CHISIN(FLOAT(ITMP)/255.0,1)

          RETURN
          END
