      SUBROUTINE CL2_SET_STP( MONTE,TBLOAD,SFTV,NOISE,PLATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      book a fake CAD1 bank (crate header only)
C-      set the version word according to inputs
C-      call initialization routines to set up CAL banks in STP:
C-          CADT for addressing
C-          CGEV for gains
C-
C-   Inputs  : see CAL_VERSION_WORD.FOR
C-   Outputs : CAD1 bank with first header word set
C-   Controls:
C-
C-   Created  27-APR-1992   James T. Linnemann  based on CADFL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MONTE,PLATE
      INTEGER TBLOAD,SFTV,NOISE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS/LIST'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'   ! for CADT_FIX
      INCLUDE 'D0$INC:CUNFLG.INC'
      INTEGER CADT_LINK(2,0:5)
      INTEGER NCAD,NCRATE,GZCADT,LCADT,LCGEV1
      INTEGER ICRATE,LCAD,BANK,IWORD
      INTEGER NW_CRATE_HEAD,ISYNC,BANKLEN,POINT,IPOINT,NWORDS,CTRAILER
      PARAMETER (CTRAILER = 4)       !number of words in crate trailer
C----------------------------------------------------------------------
C
      PARAMETER (NW_CRATE_HEAD=5)           ! number of words in crate header
      PARAMETER (ISYNC=2**16-1)             ! synch word least sig 16 bits
C
C----------------------------------------------------------------------
C
      SFTVSN = SFTV
      CALL BKCAD1(LCAD)
      BANK = 7
C
C--- NOW SET UP THE "FIXED" PART OF CAD, and the trailer

      IQ(LCAD+1) = NW_CRATE_HEAD - 1   ! Header length(not inclusive)
      IQ(LCAD+2) = ISYNC               ! SYNC word
C
C ****  Controller word
      ICRATE = 0
      IQ(LCAD+3) = (ICRATE*10+BANK)*2**24 + ! CRATE NUMBER
     &         (NADCC-1)*2**16 +                  ! ADC CARD NUMBER
     &         2**3 +                             ! PED SUBTRACTED DATA
     &         1                                  ! DATA (NOT PEDS)
C
C...  version word
      CALL CAL_VERSION_WORD(MONTE,TBLOAD,SFTVSN,NOISE,PLATE,IWORD)
      IQ(LCAD+4) = IWORD
C
C ****  STATUS/VERTEX word
      IQ(LCAD+5) = 0      ! Error indicators - no LV0 vertex yet
C
C...now set up a fake trailer if it exists on this version
C
      IF (SFTVSN.GE.4) THEN
        BANKLEN = IQ(LCAD-1)
        POINT = BANKLEN - NTRAILER + 1
        NWORDS = IQ(LCAD+POINT-CTRAILER)
        IPOINT = POINT - NWORDS - 1
        IQ(LCAD+IPOINT+1) = IQ(LCAD+1)
        IQ(LCAD+IPOINT+2) = IQ(LCAD+2)
        IQ(LCAD+IPOINT+3) = IQ(LCAD+3)
        IQ(LCAD+IPOINT+4) = IQ(LCAD+4)
        IQ(LCAD+IPOINT+5) = IQ(LCAD+5)
      ENDIF
C
C ****  SETUP CADT LOOK-UP TABLE
C
      LCADT = GZCADT ()
      DO NCAD = 1, 2
        DO NCRATE = 0, 5
C
C ****  Reserve link in STP_ZLINKA for link to ncad,ncrate
C
          CALL STP_GSLINK('CUNPAK',CADT_LINK(NCAD,NCRATE) )
          STP_LSLINK(CADT_LINK(NCAD,NCRATE)) = LCADT
          IF (LCADT.LE.0) THEN
            CALL ERRMSG('CL2HITS','CL2_OLD_DATA',
     &          'CADT CHAIN BAD','F')
            GOTO 999
          END IF
          LCADT = LC(LCADT)
        END DO
      END DO
C
C... search the crate header and sets up addressing accordingly
      CALL CADT_FIX(CADT_LINK)  ! after link areas established
C
C... now fill the CGEV bank based on header info and run number in HEAD bank
      LCGEV1 = 0    !force to initialize
      CALL CGEVFL('CAHITS_RCP',LCGEV1)
  999 RETURN
      END
