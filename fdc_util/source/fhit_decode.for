C VAX/DEC CMS REPLACEMENT HISTORY, Element FHIT_DECODE.FOR
C *2     9-NOV-1993 17:56:40 AVERY "updates in FDC for v12 RECO"
C *1     4-NOV-1993 10:57:28 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FHIT_DECODE.FOR
      SUBROUTINE FHIT_DECODE(FHIT_WORDS,
     &  HALF,UNIT,QUAD,SECTOR,WIRE,
     &  DL,LR,ON_SEG,TRK_FDCT,TRK_ZTRK,
     &  DRIFTD, DRIFTD_MIRROR, Z_POS, IONIZATION )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode all of the information encoded in the
C-              FDC compressed hit bank for a single hit.
C-
C-   Inputs  :  FHIT_WORDS(2)   The two words that make up an FDC compressed
C-                              hit in FHIT bank.
C-   Outputs :
C-      Logical position:
C-              HALF,UNIT,QUAD,SECTOR,WIRE
C-      Status of hit:
C-              DL              TRUE if delay line information good
C-              LR              0 = right, 1= left
C-              ON_SEG          TRUE if found on segment
C-              TRK_FDCT        FDC track number (0 if not on track)
C-              TRK_ZTRK        ZTRAK track number (0 if not on track)
C-      Data from hit:
C-              DRIFTD          Drift distance (cm)
C-              DRIFTD_MIRROR   Drift distance in mirror direction (cm)
C-              Z_POS           Z-position for theta SW0 (cm)
C-              IONIZATION      Ionization for other hits (MIP)
C-
C-   Created   7-OCT-1991   Robert E. Avery
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  17-FEB-1992   Robert E. Avery  Use BYTE_ORDER.PARAMS for
C-     UNIX compatibility.
C-   Updated  21-OCT-1993   Robert E. Avery  Allow for new
C-                              new verstion of FHIT.
C-   Updated   8-NOV-1993   Robert E. Avery  Make the  limit for making 
C-     mirror hits in sectors 0-2 a parameter in D0$PARAMS:FDPARA.PARAMS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
C  Input:
      INTEGER FHIT_WORDS(2)
C  Output:
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      LOGICAL DL
      INTEGER LR
      LOGICAL ON_SEG
      INTEGER TRK_FDCT,TRK_ZTRK
      REAL    DRIFTD, DRIFTD_MIRROR
      REAL    Z_POS
      REAL    IONIZATION
C  Local:
      INTEGER LFHIT,GZFHIT
      INTEGER STATUS_FHIT
      INTEGER LOGCHA, UBIT
      INTEGER MASK11
      PARAMETER( MASK11 =  4095 )        ! 2**12 - 1
      REAL    STAGGER
      LOGICAL FIRST
      LOGICAL VERSION1
C
      INTEGER*2 I2DATA(2)
      INTEGER I4DATA
      EQUIVALENCE (I2DATA,I4DATA)
C  Functions:
      REAL FSTAGR
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        LFHIT = GZFHIT()
        IF ( LFHIT.GT.0 ) THEN
          VERSION1 = IQ(LFHIT+1).EQ.1
        ELSE
          VERSION1 = .TRUE.
        ENDIF
      ENDIF
C
      STATUS_FHIT = FHIT_WORDS(1)
      I4DATA      = FHIT_WORDS(2)
C
C Decode STATUS
C
      LOGCHA = IAND( MASK11 , STATUS_FHIT )
      CALL MVBITS(STATUS_FHIT,12,1,LR,0)
      DL = BTEST(STATUS_FHIT,13)
      ON_SEG = BTEST(STATUS_FHIT,14)
      CALL MVBITS(STATUS_FHIT,15,9,TRK_FDCT,0)
      CALL MVBITS(STATUS_FHIT,24,8,TRK_ZTRK,0)
C
C Decode data
C
      CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE,UBIT,1)
      STAGGER = FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
C
      IF ( VERSION1 ) THEN 
        IF ( (UNIT.EQ.0).AND.(WIRE.EQ.8) ) THEN
          Z_POS = FLOAT(I2DATA(WORD2))/500.
          IONIZATION = 0.
        ELSE
          Z_POS = 0.
          IONIZATION = FLOAT(I2DATA(WORD2))/500.
        ENDIF
      ELSE
        IF ( (UNIT.EQ.0).AND.(WIRE.EQ.0) ) THEN
          Z_POS = FLOAT(I2DATA(WORD2))/100.
          IONIZATION = 0.
        ELSE
          Z_POS = 0.
          IONIZATION = FLOAT(I2DATA(WORD2))/100.
        ENDIF
      ENDIF
      I2DATA(WORD2)= 0
C
      IF ( I2DATA(WORD1) .NE. -1 ) THEN
        DRIFTD = FLOAT(I4DATA)/10000.
        DRIFTD_MIRROR =  DRIFTD * (-1)**(LR+1)  + STAGGER
        DRIFTD        =  DRIFTD * (-1)**(LR)    + STAGGER
        IF ( (UNIT.EQ.0) .AND. (SECTOR.LE.2) ) THEN
          IF( ABS(DRIFTD_MIRROR).GT.XCUT_HSEC ) THEN
            DRIFTD_MIRROR = 0.0
          ENDIF
        ENDIF
      ELSE
        DRIFTD = 9999.
        DRIFTD_MIRROR = 9999.
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
