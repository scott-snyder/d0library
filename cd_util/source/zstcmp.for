      SUBROUTINE ZSTCMP(LZTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set ZTRK id number in CD compressed hits banks
C-
C-   Inputs  : LZTRK: ZTRK bank address
C-   Outputs : bank VHIT, DHIT or FHIT are updated for this ZTRK
C-
C-   Created  29-AUG-1991   Qizhong Li-Demarteau
C-   Updated  18-OCT-1991   Robert E. Avery  implement FHIT update. 
C-   Updated  31-OCT-1991   Peter Grudberg   implement for VHIT
C-   Updated   1-MAY-1993   Ed Oltman  ACCOMADATE CHANGE TO VTTH BANK 
C-   Updated  19-OCT-1993   Robert E. Avery  Change in FHIT bank
C-                                              (still back compatable). 
C-   Updated  20-FEB-1994   Liang-Ping Chen  delete reference to VHIT 
C-                            
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVTTH.LINK'
C
      INTEGER LZTRK, IZTRK
      INTEGER LDTRK, PLDTTH, LDHIT, GZDHIT, IPD
      INTEGER LFDCT, PLFDTH, LFHIT, GZFHIT
      INTEGER IER, I, WRFLAG, LAYER, SECTOR, OLDLAY, OLDSEC
      INTEGER POINT, NHIT, IHIT, LABEL, JHIT, NHIT1
      INTEGER ONZTRK, BITPTN, MXZTRK
      PARAMETER (MXZTRK = 255)
      PARAMETER (ONZTRK = 2**23)
C
      INTEGER FDCT_BITS,FIRST_HIT
      INTEGER FIRST_DL,N_DL
      INTEGER ADDRESS,HALF,UNIT,QUAD,WIRE,UBIT
      INTEGER OLDADDRESS
      INTEGER TWO_TO_24
      PARAMETER (TWO_TO_24 = 2**24)
      INTEGER TWO_TO_15
      PARAMETER (TWO_TO_15 = 2**15)
      INTEGER MASKFDCSECTOR
      PARAMETER( MASKFDCSECTOR =  4080 )  ! Bits 4-11 of LOGADDR
      INTEGER MASKFDCT
      PARAMETER( MASKFDCT =  16744448 ) ! Bits 15-23 of FHIT STATUS
      INTEGER MASKVTXSECTOR
      PARAMETER ( MASKVTXSECTOR = 4080 ) ! Bits 4-11 of hit address in VTTH
C
      LOGICAL BUILD_DHIT, BUILD_FHIT
      LOGICAL VTXON, CDCON, FDCON
      LOGICAL FIRST, EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (LZTRK .LE. 0) GOTO 999
      IZTRK = IQ(LZTRK - 5)
      IF (IZTRK .GE. MXZTRK) IZTRK = MXZTRK
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTRAKS',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('CDCON',CDCON,IER)
        CALL EZGET('FDCON',FDCON,IER)
        CALL EZGET('VTXON',VTXON,IER)
        CALL EZRSET
        IF ( CDCON ) THEN
          CALL EZPICK('DTRAKS_RCP')
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('ZTRAKS','ZSTCMP',
     &       'Unable to find bank DTRAKS_RCP','W')
            GOTO 999
          ENDIF
          CALL EZGET('BUILD_DHIT',BUILD_DHIT,IER)
          CALL EZRSET
        ELSE
          BUILD_DHIT = .FALSE.
        ENDIF
        IF ( FDCON ) THEN
          CALL EZPICK('FTRAKS_RCP')
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('ZTRAKS','ZSTCMP',
     &       'Unable to find bank FTRAKS_RCP','W')
            GOTO 999
          ENDIF
          CALL EZGET('BUILD_FHIT',BUILD_FHIT,IER)
          CALL EZRSET
        ELSE
          BUILD_FHIT = .FALSE.
        ENDIF
      ENDIF
C
C  Update DHIT bank, if a CDC track is contributed to this ZTRK
C
      BITPTN = IZTRK * ONZTRK
      LDTRK = LQ(LZTRK - 7)
      IF (LDTRK .LE. 0) GOTO 200
      PLDTTH = LQ(LDTRK - 1)
      IF (PLDTTH .LE. 0) GOTO 200
      IF (.NOT. BUILD_DHIT) GOTO 200
      LDHIT = GZDHIT()
      IF (LDHIT .LE. 0) GOTO 200
      OLDLAY = -1
      OLDSEC = -1
      DO 100 I = 0,27
        WRFLAG = IBITS(IQ(LDTRK+3),I,1)  
        IF (WRFLAG.NE.0) THEN
          LABEL = IQ(PLDTTH+1)           
          PLDTTH = PLDTTH+2
          LAYER = IBITS(LABEL, 16, 2)
          SECTOR = IBITS(LABEL, 11, 5)
          IF (LAYER .NE. OLDLAY .OR. SECTOR .NE. OLDSEC) THEN
            CALL DHITPT(LAYER,SECTOR,POINT,NHIT)
            IPD = LDHIT + POINT
            OLDLAY = LAYER
            OLDSEC = SECTOR
            NHIT1 = NHIT
            JHIT = 0
          ELSE
            NHIT1 = NHIT - JHIT
          ENDIF
          DO 101 IHIT = 1, NHIT1
            JHIT = JHIT + 1
            IF (LABEL .EQ. IBITS(IQ(IPD+1),0,18)) THEN
              IQ(IPD+1) = IOR(IQ(IPD+1),BITPTN)     
              IPD = IPD + IQ(LDHIT+3)
              GOTO 100
            ENDIF
            IPD = IPD + IQ(LDHIT+3)
  101     CONTINUE
        ENDIF
  100 CONTINUE
C
C  Update FHIT bank, if a FDC track has contributed to this ZTRK
C
  200 CONTINUE
      BITPTN = ISHFT(BITPTN,1)
      LFDCT = LQ(LZTRK - 8)
      IF (LFDCT .LE. 0) GOTO 300
      PLFDTH = LQ(LFDCT - 1)
      IF (PLFDTH .LE. 0) GOTO 300
      IF (.NOT. BUILD_FHIT) GOTO 300
      LFHIT = GZFHIT()
      IF (LFHIT .LE. 0) GOTO 300
      FDCT_BITS = IQ(LFDCT - 5) * TWO_TO_15
      IF ( FDCT_BITS  .GE. TWO_TO_24 ) GOTO 300
      OLDADDRESS = -1
      DO I = 0, IQ(LFDCT+2)-1
        ADDRESS = IAND(MASKFDCSECTOR, IQ(PLFDTH+3*I+1)/2 )
C
C  Find all hits on the FDC track for each new sector
C
        IF ( ADDRESS.NE.OLDADDRESS  ) THEN
          OLDADDRESS = ADDRESS 
          CALL FCODER(ADDRESS,HALF,UNIT,QUAD,SECTOR,WIRE,UBIT,1)
          CALL FHITPT(HALF,UNIT,QUAD,SECTOR,
     &      FIRST_HIT,NHIT,FIRST_DL,N_DL)
          IPD = LFHIT + 3 + (FIRST_HIT-1)*IQ(LFHIT+3)
          DO IHIT =  1, NHIT
            IF ( IAND(MASKFDCT,IQ(IPD+1) ) .EQ. FDCT_BITS ) THEN
              IQ(IPD+1) = IOR(IQ(IPD+1),BITPTN)     
            ENDIF
            IPD = IPD + IQ(LFHIT+3)
          ENDDO
          IPD = LFHIT + 3 + (FIRST_DL-1)*IQ(LFHIT+3)
          DO IHIT =  1, N_DL
            IF ( IAND(MASKFDCT,IQ(IPD+1) ) .EQ. FDCT_BITS ) THEN
              IQ(IPD+1) = IOR(IQ(IPD+1),BITPTN)     
            ENDIF
            IPD = IPD + IQ(LFHIT+3)
          ENDDO
        ENDIF
      ENDDO
  300 CONTINUE
  999 RETURN
      END
