      SUBROUTINE VHIT_UNPACK(VHIT_WORDS,LAYER,SECTOR,WIRE,JHIT,IENDS,
     &              ONTRK,ISIDE,IZTRK,DIST_PLUS,DIST_MINUS,ZPOS,AREA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack compressed hit information
C-
C-   Inputs  : VHIT_WORDS: array of 3 words containing compressed 
C-                         hit information
C-   Outputs : 
C-             LAYER, SECTOR, WIRE: layer, sector and wire # for this hit
C-             JHIT:  the hit number for this wire 
C-             IENDS: [1:2:3] means hit found on end [0:1:both].
C-             ONTRK: 1: if the hit is on a track VTRK; 0: not
C-             ISIDE: the hit is on which side of the wire 
C-                    (the ISIDE is meaningless if ONTRK=0)
C-             IZTRK: ZTRK id if the hit is on a ZTRK
C-             DIST_PLUS  : Drift distance +phi solution (cm)
C-             DIST_MINUS : Drift distance -phi solution (cm) (negative number)
C-             ZPOS:  Z position in (cm)
C-             AREA:  pulse area for this hit (FADC counts, for the moment)
C-                                            (will convert to MIP's later)
C-
C-   Created  16-JUL-1992  Tom Trippe (Patterned after 4-MAR-1992 DHIT_UNPACK
C-                                     by Qizhong Li-Demarteau)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER VHIT_WORDS(3)
      INTEGER LAYER, SECTOR, WIRE, JHIT, IENDS, ONTRK, ISIDE, IZTRK
      INTEGER I4WORD, STATUS
      INTEGER*2 I2WORD(2)
      EQUIVALENCE (I2WORD(1), I4WORD)
      REAL    DIST_PLUS, DIST_MINUS, ZPOS, AREA
C----------------------------------------------------------------------
C
C   unpack the first word
C
      STATUS = VHIT_WORDS(1)
      LAYER = IBITS(STATUS,16,3)
      SECTOR = IBITS(STATUS,11,5)
      WIRE = IBITS(STATUS,8,3)
      JHIT = IBITS(STATUS,1,7)
      IENDS = IBITS(STATUS,19,2)
      ONTRK = IBITS(STATUS,23,1)
      ISIDE = IBITS(STATUS,0,1)
      IZTRK = IBITS(STATUS,24,8)
C
C   unpack the second word and convert to cm
C
      I4WORD = VHIT_WORDS(2)
      DIST_PLUS  = FLOAT(I2WORD(WORD1))*0.0005 
      DIST_MINUS = FLOAT(I2WORD(WORD2))*0.0005 
C
C   unpack the third word into ZPOS (in cm) and AREA (in FADC counts)
C
      I4WORD = VHIT_WORDS(3)                      
      ZPOS = FLOAT(I2WORD(WORD1))*0.01 
      AREA = FLOAT(I2WORD(WORD2)) 
C                   AREA will be converted to MIP's in the future
C
  999 RETURN
      END
