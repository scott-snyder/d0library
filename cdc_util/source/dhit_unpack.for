      SUBROUTINE DHIT_UNPACK(DHIT_WORDS,LAYER,SECTOR,WIRE,JHIT,ONTRK,
     &                       ISIDE,IZTRK,TIME,ZPOS,AREA,words_per_hit)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack compressed hit information
C-
C-   Inputs  : DHIT_WORDS: array of 3 words containing compressed 
C-                         hit information
C-             words_per_hit : number of words/hit in DHIT
C-   Outputs : 
C-             LAYER, SECTOR, WIRE: layer, sector and wire # for this hit
C-             JHIT:  the hit number for this wire 
C-             ONTRK: 1: if the hit is on a track DTRK; 0: not
C-             ISIDE: the hit is on which side of the wire 
C-                    (the ISIDE is meaningless if ONTRK=0)
C-             IZTRK: ZTRK id if the hit is on a ZTRK
C-             TIME:  drift time in ns (T0 is subtracted already)
C-             ZPOS:  Z position in cm (if the hit is on an inner wire, 
C-                    ZPOS = 999.9)
C-             AREA:  pulse area for this hit (if the hit is on a outer wire,
C-                    the AREA=0.0)
C-
C-   Created   1-NOV-1991   Qizhong Li-Demarteau
C-   Updated   4-MAR-1992   Qizhong Li-Demarteau  use BYTE_ORDER.PARAMS 
C-   Updated  12-APR-1994   C. Klopfenstein use new (or old) version of DHIT    
C-   Updated  27-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER DHIT_WORDS(3), words_per_hit
      INTEGER LAYER, SECTOR, WIRE, JHIT, ISIDE, ONTRK, IZTRK
      INTEGER I4WORD, STATUS
      INTEGER*2 I2WORD(2)
      EQUIVALENCE (I2WORD(1), I4WORD)
      REAL    TIME, ZPOS, AREA, tmpfloat
C----------------------------------------------------------------------
C
C   unpack the first word
C
      STATUS = DHIT_WORDS(1)
      LAYER = IBITS(STATUS,16,2)
      SECTOR = IBITS(STATUS,11,5)
      WIRE = IBITS(STATUS,8,3)
      JHIT = IBITS(STATUS,1,7)
      ISIDE = IBITS(STATUS,0,1)
      ONTRK = IBITS(STATUS,22,1)
      IZTRK = IBITS(STATUS,23,9)
C
C   unpack the second word
C
      ZPOS = 999.9
      AREA = 0.0
      I4WORD = DHIT_WORDS(2)
      tmpfloat = I2WORD(WORD1)
      TIME = tmpfloat/10.                              ! in ns
      IF (WIRE .EQ. 0 .OR. WIRE .EQ. 6) THEN
        tmpfloat = I2WORD(WORD2)
        ZPOS = tmpfloat/100.                           ! in mm
C  new version of DHIT has 3rd word, with pulse area for
C  outer sense wire hits
        if (words_per_hit .ge. 3) then
          I4WORD = DHIT_WORDS(3)
          area = I2WORD(WORD1)
        endif
      ELSE
        AREA = I2WORD(WORD2)
      ENDIF
C
  999 RETURN
      END
