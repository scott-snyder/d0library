      SUBROUTINE TB90L2_CADTFL 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank CADT -
C-                         the calorimeter CAD bank address look-up table
C-                         for the TB91 (TB90L2) data addressing.
C-
C-   Inputs  : none
C-
C-   Outputs : ISTAT - error code - 0 if OK
C-   Controls: none
C-
C-   Created  22-APR-1991   Chip Stewart
C-   Updated  20-APR-1992   Chip Stewart  - Turned off eta=-12,phi=21,lyr=2  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:TB90L2.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INCLUDE 'D0$LINKS:IZCADT.LINK'
      INTEGER ISEQ,ISTAT,LUN,LSTPC
      INTEGER PAKADR,PHYADR
      INTEGER JCRATE,MAX_ADDR
      INTEGER INDEX,IETA,IPHI,ILYR,IER
      INTEGER LCADT,INIT_ADR
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C----------------------------------------------------------------------
      ISTAT=-1
C
C ****  INITIALIZE ZEBRA (TB90L2_MAKE_TABEL ISN'T DOING IT)
C
      CALL MZEBRA(0)
      CALL INZSTP
C
C ****  Create SCAL bank
C
      CALL BKSCAL ('STPC',LSCAL)
C
C ****  Create CGEH bank
C
      CALL BKCGEH ('STPC',LCGEH)

      IF ( LCGEH .LE. 0 ) THEN
        CALL ERRMSG('CGEH NOT BOOKED','TB90L2_CADTFL',
     &    ' BAD STP/STPC CHAIN','W')
        GOTO 999
      ENDIF

C
C ****  LOOP OVER CAD ADDRESSES
C
      CALL BKCADT (LCADT )
      IF (LCADT.LT.0) THEN
        CALL ERRMSG('CADT NOT BOOKED','TB90L2_CADTFL',
     &    ' BAD STP/GCEH CHAIN','W')
        GOTO 999
      END IF
      IC(LCADT+2) = 7
      IC(LCADT+1) = 2**30+2**17+1   !NWA LOAD 2 DATA - SFTVSN 1
      BYTES(BYTE4)= 0
      BYTES(BYTE3)= 0
      BYTES(BYTE2)= -1
      BYTES(BYTE1)  = 0
      INIT_ADR=PAKADR
      CALL CADPAK(NADCC-1,NBLSC-1,NEFC-1, NDEPTC-1,0,0,MAX_ADDR)
      MAX_ADDR = IBITS(MAX_ADDR,2,13) 
      DO INDEX = 0, MAX_ADDR
        IC(LCADT + INDEX + 3) = INIT_ADR
      END DO
      DO 10 ISEQ = 1, NENTRIES
        INDEX = ELEC_SORT_ADCA(ISEQ) / 4
        IF (INDEX.GT.MAX_ADDR) 
     &    CALL ERRMSG('BAD INDEX IN ELEC_SORT_ADCA',
     &    'TB90L2_CADTFL','CHECK TB90L2_ADC_SORT.DAT','F')
        IETA  = ELEC_SORT_ETA(ISEQ)
        IPHI  = ELEC_SORT_PHI(ISEQ)
        ILYR  = ELEC_SORT_LAYER(ISEQ)
        IF (INDEX.EQ.5345) THEN
C         I SEQADC   ADCA CRD BLS  TW  LY MOD  ETA   PHI LAYER
C        14   4010  21380  10   3   2   1 'EM' -12    31     2
          CALL ERRMSG('EDIT OUT BAD CHANNEL','TB90L2_CADTFL',
     &      'ETA=-12,PHI=31,LYR=2 MAPPED TO LAYER -2','I')
          ILYR = -2
        END IF
        BYTES(BYTE4)=IETA
        BYTES(BYTE3)=IPHI
        BYTES(BYTE2)=ILYR
        BYTES(BYTE1)  = 0
        PHYADR=PAKADR
C
C ****  WRITE PACKED ADDRESS INTO LOOK-UP TABLE
C
        IF(IC(LCADT+INDEX+3).EQ.INIT_ADR)IC(LCADT+INDEX+3)=PHYADR
C
  10  CONTINUE
C
C ****  WRITE OUT ZEBRA CADT LOOK-UP TABLE 
C
      CALL GTUNIT(77,LUN,IER)
      CALL ZZOPEN (LUN,'TB90L2_CAD_STPFILE',IER,'OUTPUT')
      LSTPC = LC(LSTPH-IZSTPC)
      LSCAL = LC(LSTPC-IZSCAL)
      LCGEH = LC(LSCAL-IZCGEH)
      LCADT = LC(LCGEH-IZCADT)
      CALL FZOUT  (LUN,IDVSTP,LCADT,1,'L',1,0,0)
      CALL ZZCLOS (LUN,IER,'OUTPUT')
      CALL RLUNIT(77,LUN,IER)
  999 RETURN
      END
