      SUBROUTINE CADT_FIX (CADT_LINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO FIX THE CADT LOOK-UP TABLE FOR
C-                         PREVIOUS SOFTWARE VERSIONS OF CAD BANKS
C-
C-   Inputs  : CADT_LINK- INDEX TO CADT ADDRESSES IN STP_ZLINKA LINK AREA
C-   Outputs : NONE
C-   Controls: STFVSN IN CUNFLG COMMON DETERMINES TYPE OF FIX
C-                     = 1; LOOP OVER PHYSICS IETA = 12 ONLY FOR CHANGES
C-
C-   Created  13-DEC-1990   Chip Stewart
C-   Updated  29-MAY-1991   James T. Linnemann  kludge for CL2HITS
C-   Updated  10-NOV-1993   James T. Linnemann  clean-up
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INTEGER VSNIN             ! input version number for entry point
C
      INTEGER CADT_LINK(2,0:5)
      INTEGER IETA,IPHI,ILYR,NCAD,NCRATE,CRATE,ADC,BLS,ROTOW,DEP,IER
      INTEGER INDEX,ADDR,PHYADR,PAKADR,LENF,ICOL,INDX
      INTEGER HEADER_LEN,SYNCH,CONTROL_WORD,VERSION,STATUS,PULSER
      CHARACTER*80 MSG,FILNAM
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
      LOGICAL CEXIST
      INTEGER GZCADT,LCADT
      INTEGER PREV_CALVSN,PREV_D0VSN,PREV_SFTVSN
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA PREV_CALVSN,PREV_D0VSN,PREV_SFTVSN/3*-999/
C----------------------------------------------------------------------
C
C...find out whether DATA thinks need FIXed CADT table

      CALL GTCAD_HEADER (0,0,HEADER_LEN,SYNCH,CONTROL_WORD,
     &    VERSION,STATUS,PULSER,IER)
      GO TO 777

C#######################################################################
      ENTRY CADT_FORCE_FIX (CADT_LINK,VSNIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : force the correction to occur for eta=+/- 12
C-                      need to fill the link table
C-   Inputs  : CADT_LINK- INDEX TO CADT ADDRESSES IN STP_ZLINKA LINK AREA
C-           : VSNIN    the SFTVSN to use
C-   Outputs : modified CADT bank
C-   Controls: none
C-
C-   Created  29-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
C
C...force parameters so that the modification WILL be done
      SFTVSN = VSNIN                        ! CLYRDP will use old addressing
      D0VSN = 0
      D0VSN = IBSET(D0VSN,5)
  777 CONTINUE
C
C ****  CHECK CAD VERSION  IF MC DATA AND SFTVSN.LT.D0_MC_SFTVSN
C
      IF (FIRST) THEN
        IF ( (PREV_CALVSN.EQ.CALVSN).AND.(PREV_D0VSN.EQ.D0VSN)
     &       .AND.(PREV_SFTVSN.EQ.SFTVSN) ) THEN
          WRITE(MSG,10)D0VSN,SFTVSN
          CALL ERRMSG('EXISTING CADT ADDRESSING OK','CADT_FIX',MSG,'I')
          FIRST = .FALSE.
          GOTO 999
        END IF
        PREV_CALVSN = CALVSN
        PREV_D0VSN  = D0VSN
        PREV_SFTVSN = SFTVSN

        IF( SFTVSN.LT.D0_MC_SFTVSN .AND. BTEST(D0VSN,5)) THEN
C
C ****  REVISE CADT if SFTVSN=1 and Monte Carlo data
C
          WRITE(MSG,10)D0VSN,SFTVSN
   10     FORMAT('D0VSN=',I3,' SFTVSN=',I3)
          CALL ERRMSG('USE OLD MC CAL ADDRESSING','CADT_FIX',MSG,'I')
          DO IETA = -NETAL, NETAL
            DO IPHI = 1, NPHIL
              DO ILYR = 1, NLYRL
                IF( CEXIST(IETA,IPHI,ILYR) ) THEN
                  CALL CPHAD(IETA,IPHI,ILYR,CRATE,ADC,BLS,ROTOW,DEP,IER)
                  CALL CADPAK (ADC,BLS,ROTOW,DEP,1,0,ADDR)
                  NCRATE = CRATE / 10
                  NCAD = ( CRATE - NCRATE*10 ) - 6
                  INDX = ADDR / 4
                  BYTES(BYTE4)=IETA
                  BYTES(BYTE3)=IPHI
                  BYTES(BYTE2)=ILYR
                  BYTES(BYTE1)= 0
                  PHYADR=PAKADR
                  IC(STP_LSLINK(CADT_LINK(NCAD,NCRATE))+INDX+3)=PHYADR
                END IF
              END DO
            END DO
          END DO
        ELSE IF( CALVSN.GT.0 .AND. BTEST(D0VSN,6)) THEN
C
C ****  REVISE CADT if D0VSN=64 (NWA DATA) 
C
          WRITE(MSG,12)D0VSN,CALVSN
   12     FORMAT('D0VSN=',I3,' CALVSN=',I3)
          CALL ERRMSG('USE NWA CAL ADDRESSING','CADT_FIX',MSG,'I')
          CALL ERRMSG('USE NWA CAL ADDRESSING','CADT_FIX',MSG,'S')
          IF( CALVSN.EQ.1) THEN
            CALL ERRMSG('NO TB90 LOAD 1 CADT FILE','CADT_FIX',
     &        'CAEP ADDRESSING IS JUNK','W')
          ELSE IF (CALVSN.EQ.2) THEN
            LCADT = GZCADT ()
            CALL EZPICK('CAHITS_RCP')
            CALL EZGETS ('CAD_STPFILE',1,FILNAM,LENF,IER)
            CALL EZRSET
            ICOL = INDEX (FILNAM(1:LENF),':')
            IF(ICOL.GT.0) THEN
              IF(LCADT.GT.0) CALL MZDROP(IXSTP,LCADT,' ') ! drop existing CADT
              MSG = ' DROP '//FILNAM(1:LENF) 
              CALL ERRMSG('TB90 LOAD 2 CADT FILE','CADT_FIX',MSG,'I')
              CALL ERRMSG('TB90 LOAD 2 CADT FILE','CADT_FIX',MSG,'S')
              FILNAM = FILNAM(1:ICOL)//'TB90L2_'//FILNAM(ICOL+1:LENF)
              LENF = LENF + 7
              MSG = ' READ '//FILNAM(1:LENF) 
              CALL ERRMSG('TB90 LOAD 2 CADT FILE','CADT_FIX',MSG,'I')
              CALL ERRMSG('TB90 LOAD 2 CADT FILE','CADT_FIX',MSG,'S')
              CALL CADSTP (FILNAM(1:LENF),IER)
              MSG = 'READ '//FILNAM(1:LENF) 
              IF(IER.NE.0) CALL ERRMSG('CADSTP FAILS','CHTINI',MSG,'W')
              LCADT = GZCADT ()
C
C ****  RESERVE LINK IN STP_ZLINKA FOR LINK TO NCAD,NCRATE
C
              STP_LSLINK(CADT_LINK(1,0)) = LCADT
              IF (LCADT.LE.0) CALL ERRMSG('TB90L2 ADDRESSING',
     &          'CADT_FIX','CADT BAD','W')
            END IF
          END IF
        ENDIF
        FIRST = .FALSE.
      ELSE
        IF (.NOT.FIRST) THEN
          IF ((PREV_CALVSN.NE.CALVSN).OR.
     &     (PREV_D0VSN.NE.D0VSN).OR.(PREV_SFTVSN.NE.SFTVSN)) 
     &     CALL ERRMSG('CADT PROBLEMS','CADT_FIX',
     &     'CADT TABLE AND CAD BANK not of this type','E')
        ENDIF
      ENDIF
  999 RETURN
      ENTRY CADT_FIX_RESET
      FIRST = .TRUE.
      END
