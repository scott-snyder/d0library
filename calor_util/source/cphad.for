      SUBROUTINE CPHAD(IETAC,IPHIC,ILYRC,CRATE,ADC,BLS,ROTOW,DEPTH,
     &  ICOND)
C------------------------------------------------------------------
C
C   Purpose and Methods : Converts an address in the PHysics system
C                         to the hardware ADC address.
C                         Reference for the conversion is D0 Note 774. 
C                         (Inverse transformation done by CADPH.FOR) 
C
C   Inputs  : IETAC     offline eta index        [-37,-1],[1,37]
C             IPHIC     offline phi index        [1,64]
C             ILYRC     offline radial index     [1,17]
C
C   Outputs : CRATE     ADC crate number         [NCRATE*10+BANK] 
C                            where NCRATE=0,5 and BANK=7,8
C                            old version [96,101],[112,117] (before 3/90)
C             ADC       ADC card number in crate [0,11]
C             BLS       BLS number in ADC        [0,7]
C             ROTOW     readout tower in BLS     [0,3]
C             DEPTH     depth in readout tower   [0,11]
C             ICOND     return code: 0 = OK
C                                    1 = invalid input
C
C    Created  12-DEC-1988   A.P. White
C    Updated  20-FEB-1989   K. Wyatt Merritt 
C    Updated  12-JUN-1989   K. Wyatt Merritt   revised MG/ICD cabling
C                      as per J. Kourlas' memo to Linnemann, 5/24/89
C    Updated  28-FEB-1990   Chip Stewart   - new CRATE number scheme
C    Updated  23-APR-1992   K. Wyatt Merritt  Add validity check on IPHIC 
C-   Updated  21-NOV-1992   Joan Guida  for missing main-ring channels
C-   Updated  29-OCT-1993   Joan Guida  fix missing main-ring channels 
C
C------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER CRATE,ADC,BLS,ROTOW,DEPTH,IETAC,IPHIC,ILYRC,ICOND
      INTEGER DATCAB
      INTEGER IESGN,IE,SUBPHI,SECTOR,ICHAN,IEE
      INTEGER NCHCRT,NCHADC,NCHBLS,NCHTOW,NEW_CRATE
      LOGICAL FIRST
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
C
C-----------------------------------------------------------------------
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        NCHCRT = NADCC*NBLSC*NEFC*NDEPTC        ! 4608
        NCHADC = NBLSC*NEFC*NDEPTC              !  384 
        NCHBLS = NEFC*NDEPTC                    !   48
        NCHTOW = NDEPTC                         !   12 
      END IF

C
      ICOND = 0                  !Initialize return code to OK setting
C
      IF (IPHIC.LT.1 .OR. IPHIC.GT.64) THEN
        ICOND = 1
        GO TO 999
      ENDIF
      IE = IABS(IETAC)
      IESGN = SIGN(1,IETAC)
      IF (IESGN.EQ.1) THEN
        DATCAB = 8
        SUBPHI = MOD(IPHIC,2)
        SECTOR = (80-IPHIC-SUBPHI)/2
        IF (SECTOR .GT. 31) SECTOR = SECTOR - 32
      ELSE
        DATCAB = 7
        SUBPHI = MOD(IPHIC+1,2)
        SECTOR = (IPHIC - SUBPHI - 17)/2
        IF (SECTOR .LT. 0) SECTOR = SECTOR + 32
      ENDIF                        
C
      IF (ILYRC.EQ.8) THEN              ! CC massless gaps
        IF (IE.LT.8 .OR. IE.GT.12) THEN
          ICOND = 1
          GO TO 999
        ENDIF
        ICHAN = SECTOR*576 + 540
        CRATE = ICHAN/NCHCRT
        ICHAN = ICHAN - NCHCRT*CRATE
        ADC = ICHAN/NCHADC
        ICHAN = ICHAN - NCHADC*ADC
        BLS = ICHAN/NCHBLS
        CRATE = CRATE + 2
        IF (MOD(IE,2) .EQ. 1) THEN
          ROTOW = 3
          DEPTH = 2*(IE - 9 + SUBPHI) 
        ELSE
          ROTOW = 1
          DEPTH = 2*(IE - 8 + SUBPHI)
        ENDIF
      ELSE IF (ILYRC .EQ. 10) THEN      ! EC massless gaps
        IF (IE.LT.8 .OR. IE.GT.13) THEN
          ICOND = 1
          GO TO 999
        ENDIF
        ICHAN = SECTOR*576 + 552
        CRATE = ICHAN/NCHCRT
        ICHAN = ICHAN - NCHCRT*CRATE
        ADC = ICHAN/NCHADC
        ICHAN = ICHAN - NCHADC*ADC
        BLS = ICHAN/NCHBLS
        CRATE = CRATE + 2
        IF (MOD(IE,2) .EQ. 1) THEN
          ROTOW = 3
          DEPTH = 2*(IE - 9 + SUBPHI) + 1
          IF (IE .EQ.13) DEPTH = 8 + SUBPHI
        ELSE
          ROTOW = 1
          DEPTH = 2*(IE - 8 + SUBPHI) + 1
        ENDIF
      ELSE IF (ILYRC .EQ. 9) THEN       ! ICDs
        IF (IE.LT.9 .OR. IE.GT.14) THEN
          ICOND = 1
          GO TO 999
        ENDIF
        ROTOW = 2
        ICHAN = SECTOR*576 + 564
        CRATE = ICHAN/NCHCRT
        ICHAN = ICHAN - NCHCRT*CRATE
        ADC = ICHAN/NCHADC
        ICHAN = ICHAN - NCHADC*ADC
        BLS = ICHAN/NCHBLS
        CRATE = CRATE + 2
        DEPTH = 2*(IE - 9) + SUBPHI
      ELSE                              ! Standard Liquid Argon channels
        CALL CLYRDP(IETAC,ILYRC,DEPTH)
        IF (DEPTH .EQ. -1) THEN
          ICOND = 1
          GO TO 999
        ENDIF
        IF (IE.GT.32 .AND. MOD(IPHIC,2).EQ.0) THEN
          ICOND = 1
          GO TO 999
        ENDIF
        IF (IE .LE. 12) THEN
C
C          Central Calorimeter
C
          ICHAN = SECTOR*288 + (IE-1)*24 + SUBPHI*12 + DEPTH
C
C           MISSING EC MAIN RING CHANNELS
          IF (IPHIC.EQ.18) THEN
            IF ((IE.EQ.8  .AND. ILYRC.EQ.15) .OR.
     &          (IE.EQ.9  .AND. ILYRC.EQ.16) .OR.
     &          (IE.EQ.10 .AND. ILYRC.EQ.16) .OR.
     &          (IE.EQ.11 .AND. ILYRC.EQ.17)) THEN
              ICOND=1
              GO TO 999
            ENDIF
          ENDIF
C
        ELSE IF (IE .LE. 37) THEN
C
C         End Calorimeters
C
          IEE = IE - 12
          IF (IEE .LE. 20) THEN
            ICHAN = SECTOR*576 + (IEE-1)*24 + SUBPHI*12 + DEPTH + 9216
          ELSE
            ICHAN = SECTOR*576 + 480 + (IEE-21)*12 + DEPTH + 9216
          ENDIF
        ELSE
          ICOND = 1
          GO TO 999
        ENDIF
        CRATE = ICHAN/NCHCRT
        ICHAN = ICHAN - NCHCRT*CRATE
        ADC = ICHAN/NCHADC
        ICHAN = ICHAN - NCHADC*ADC
        BLS = ICHAN/NCHBLS
        ICHAN = ICHAN - NCHBLS*BLS
        ROTOW = ICHAN/NCHTOW
      ENDIF
      CRATE = CRATE*10  + DATCAB
C
  999 RETURN
      END
