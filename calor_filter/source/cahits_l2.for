      FUNCTION CAHITS_L2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Substitute CAHITS bank results for CL2 conversion
C-    results for testing
C-   Inputs  : PTCAEP, CAEP and CAEH under RECO path
C-   Outputs : PTCAEP2, and CAEP under FILT path
C-   Controls: CAHITS_L2.RCP
C-
C-   Updated   5-FEB-1992   James T. Linnemann

C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'       ! CAHITS pointer to CAEP,CAEH
      INCLUDE 'D0$INC:CL2TEST_LINK.INC' ! CAHITS links
      INCLUDE 'D0$INC:CL2_LINK.INC'     ! pointers to CL2xxx banks
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'      ! Fast routine pointer
      INTEGER IETA,IPHI,ILAYER
      INTEGER GZCAEP,GZCAEH,LFILT,GZPNUT,LPNUT
      INTEGER IPOINT,JPOINT,IER,NHITS,NHITS_CAEP
      CHARACTER*4 OLD_PATH
      LOGICAL EZERROR,OK,CAHITS_L2,CL2HITS,ET_IN_CAEP
      REAL ECAHITS,ETCAHITS
      BYTE    NEWPAR
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL INRCP('CAHITS_L2_RCP',IER)
        CALL EZPICK('CAHITS_L2_RCP')       ! Select bank
        OK = .NOT.EZERROR(IER)
        IF (IER .EQ. 0) CALL EZGET('ET_IN_CAEP',ET_IN_CAEP,IER)
        IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('CAHITS_L2','CAHITS_L2_HITS',
     &      ' Error while reading CAHITS_L2_RCP','F')
        ELSE
          CALL EZRSET
        ENDIF
C
C...now force the electron code, at least, to know about the data in CAEP
        CALL EZPICK('L2_EM_RCP')       ! Select bank
        OK = .NOT.EZERROR(IER)
        IF (IER .EQ. 0) CALL EZSET('ET_IN_CAEP',ET_IN_CAEP,IER)
        IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('CAHITS_L2','CAHITS_L2',
     &      ' Error while reading L2_EM_RCP','F')
        ELSE
          NEWPAR = 1
          CALL L2_EM_PARAMETERS(NEWPAR) !force it to look at its RCP again
          CALL EZRSET
        ENDIF
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      CAHITS_L2 = .FALSE.
      CALL PATHGT(OLD_PATH)
C this next call is here only to be sure TWO FILT banks are booked here so that 
C BKFILT called from the frame creates no further bank
      CALL BKFILT(LFILT)  
      CALL PATHST('FILT')
      CALL MKPATH !this makes the FILT bank which is actually used
      CALL MZLINT(IXCOM,'/CL2TEST/',CL2TEST_LNKFLG,CL2TEST_LINK(NTLNK),
     &  CL2TEST_LNKFLG)                 ! init temp link area
      OK = CL2HITS()                  ! vzero and fill whole CAEP
      CALL PATHST('RECO')
      LCAEH = GZCAEH()
      LCAEP = GZCAEP()
      LPNUT = GZPNUT(1)
      IF ((LCAEP.LE.0).OR.(LCAEH .LE. 0).OR.(LPNUT.LE.0)) THEN
        CALL ERRMSG('CAHITS_L2','CAHITS_L2_HITS',
     &          ' CAEP or CAEH bank not found','E')
        GO TO 999
      ENDIF
      NHITS_CAEP = IQ(LCAEP+3)
      IF (.NOT.OK) THEN
        CALL PATHST('FILT')
        CALL BKCAEP(NHITS_CAEP,L2CAEP)
        CALL PATHST('RECO')
      ENDIF
      NHITS = IQ(L2CAEP+3)
      IF (NHITS_CAEP.NE.NHITS) THEN
C...2 words per hit
        CALL MZPUSH(IXCOM,L2CAEP,0,2*(NHITS_CAEP-NHITS),' ')
      ENDIF
C
C---Loop over all the indices copy CAHITS results into L2 CAEP
      LCAEH = GZCAEH()
      LCAEP = GZCAEP()
      NHITS = 0
      CALL VZERO(PTR2,(NLYRL*NPHIL*(2*NETAL+1)))
      DO IETA = -NETAL,NETAL
        DO IPHI = 1,NPHIL
          DO ILAYER = 1,NLYRL
C
C---Get ET,E from CAEH, index word fro CAEP
            IF (PTCAEP(IETA,IPHI,ILAYER) .GT. 0) THEN
              IPOINT = (PTCAEP(IETA,IPHI,ILAYER)-1)*IQ(LCAEH+2)
              ECAHITS = Q(LCAEH + IPOINT + 7)
              ETCAHITS= Q(LCAEH + IPOINT + 8)
              JPOINT = NHITS*IQ(L2CAEP+2)
              IQ(L2CAEP+4+JPOINT) = IQ(LCAEP+4+IPOINT)
              IF (ET_IN_CAEP) THEN
                Q(L2CAEP+5+JPOINT) = ETCAHITS
              ELSE
                Q(L2CAEP+5+JPOINT) = ECAHITS
              ENDIF
              NHITS = NHITS + 1
              PTR2(ILAYER,IPHI,IETA) = NHITS  !rebuild the CL2 pointers
            ENDIF
          END DO
        END DO
      END DO
      IQ(L2CAEP+3) = NHITS
C
C...now overwrite the level 2 PNUT bank with the CAHITS PNUT 1 bank 
      IF (L2PNUT.LE.0) THEN
        CALL PATHST('FILT')
        CALL BKPNUT(1)
        L2PNUT = GZPNUT(1)
        CALL PATHST('RECO')
      ENDIF
      LPNUT = GZPNUT(1)
      Q(L2PNUT+3) = Q(LPNUT+3)  !Ex
      Q(L2PNUT+4) = Q(LPNUT+4)  !Ey
      Q(L2PNUT+7) = Q(LPNUT+7)  !Et
      Q(L2PNUT+10) = Q(LPNUT+10)  !phi
      Q(L2PNUT+14) = Q(LPNUT+14)  !scalar Et
      CAHITS_L2 = .TRUE.
  999 CONTINUE
      CL2TEST_LNKFLG(1) = 0             ! deactivate temporary link area
      CALL PATHST(OLD_PATH)
      RETURN
      END
