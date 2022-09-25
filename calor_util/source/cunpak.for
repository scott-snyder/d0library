      SUBROUTINE CUNPAK(CRATE,IDATA,PHYADR,ENERGY,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Unpack address and energy from raw data, 
C-     convert ADC pulse height to GeV. 
C-     Note conversion in EC/CC overlap region is arbitrary right now.
C-     Parameters are in file CAHITS.RCP but subroutine will run
C-     without file if necessary.
C-
C-   Inputs  : 
C-     CRATE = crate number
C-     IDATA = packed raw data
C-   Outputs : 
C-     PHYADR= packed physics address : equivalenced to BYTES(4)
C-             BYTES(4)  = eta index
C-             BYTES(3)  = phi index
C-             BYTES(2)  = layer index
C-             BYTES(1)  = flags, see CAEP.ZEB
C-     ENERGY= energy in GeV (if CONGEV is true, otherwise in ADC units)
C-     ICOND = error condition, 0 ok
C-               1 cannot unpack data, 2 cannot find valid address
C-
C-     ENTRY CUTENR(CTF,CNGV,DSPR)
C-        Change defaults for flags and for energy cutof
C-     Inputs:
C-        CTF = value of ECUTOF (energy cutof, must be in GeV if
C-                CNGV is true, in ADC counts otherwise)
C-        CNGV= logical, true if want to convert energy to GeV
C-        DSPR= logical, true if zero suppression must be done
C-
C-   Created  11-JAN-1989   Serban D. Protopopescu
C-   Updated   8-MAR-1990   Chip Stewart  - use CAD_GAIN function  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER IDATA,PHYADR,CRATE,ICOND
      INTEGER IETA,IPHI,LYR,SCALE,NEGLIM,PAKADR
      INTEGER PULSHT,IABSE,LYRS,JBIT,K,IOR,IER
      REAL ENERGY,CONVRT(NETAL,NLYRL),SAMPLF(8),ADCGEV
      REAL    CTF,ECUTOF,EM_GAIN,CAD_GAIN
      LOGICAL CONGEV,DOSUPR,CNGV,DSPR,RESET,NO_EM_GAIN
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
      LOGICAL FIRST,FLGVAL
      SAVE FIRST,CONVRT,ECUTOF,CONGEV,DOSUPR
      DATA FIRST,RESET/.TRUE.,.FALSE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
C                 get constants out of CAL_STPFILE
        CALL EZPICK('CAHITS_RCP')
        CALL EZERR(IER)
        IF ( IER.EQ.0 .AND. .NOT.RESET) THEN
          CALL EZGET('ENERGY_CUTOFF',ECUTOF,IER)
          CALL EZGET_l('DO_ADC_TO_GEV',CONGEV,IER)
          CALL EZGET_l('ZERO_SUPRESS',DOSUPR,IER)
        ELSE
C            Defaults if no file is used
          CALL INTMSG( 
     &      ' CAHITS.RCP file was not read, CUNPAK using own defaults')
          ECUTOF=0.1
          CONGEV=.TRUE.
          DOSUPR=.TRUE.
        ENDIF
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
C       unpack pulse height & miscellaneous
C
      PAKADR=0
      CALL CDTUPK(CRATE,IDATA,IETA,IPHI,LYR,PULSHT,SCALE,
     &  NEGLIM,ICOND)
      IF(ICOND.GT.0)  GOTO 999   ! data word no good
      ENERGY=FLOAT(PULSHT)
C
      IF(CONGEV) THEN         ! convert to GeV
        ENERGY= ENERGY*CAD_GAIN(IETA,LYR)
      ENDIF
C
C         software zero suppression except for ICD and massless gaps
      IF(DOSUPR.AND.(LYR.LT.8.OR.LYR.GT.10)) THEN
        IF(ABS(ENERGY).LT.ECUTOF) ICOND=-1
      ENDIF
C
C          set tag bits
      PAKADR=IOR(PAKADR,NEGLIM)
      PAKADR=IOR(PAKADR,2*SCALE)
      IF(.NOT.ZSUPRS.AND..NOT.DOSUPR) PAKADR=IOR(PAKADR,16)
      IF(.NOT.CONGEV) PAKADR=IOR(PAKADR,32)  ! not converted to GeV
      IF(.NOT.GNCORR) PAKADR=IOR(PAKADR,8)   ! not gains corrected
      IF(.NOT.PEDSUB) PAKADR=IOR(PAKADR,4)   ! not pedestal subtracted
C
C           pack addresses
      BYTES(BYTE4)=IETA
      BYTES(BYTE3)=IPHI
      BYTES(BYTE2)=LYR
      PHYADR=PAKADR
C
  999 RETURN
C
C
      ENTRY CUTENR(CTF,CNGV,DSPR)
C
      ECUTOF=CTF
      CONGEV=CNGV
      DOSUPR=DSPR
      RESET=.TRUE.
C
      RETURN
      END
