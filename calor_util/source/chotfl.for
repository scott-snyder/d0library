      SUBROUTINE CHOTFL(NTOTHOT,ICHOT,IRUNNO,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the CHOT (calorimeter hot channel) bank.
C-
C-   Inputs  : NTOTHOT [I] - Total number of hot channels
C-             ICHOT [I](-37:37,64,17) - Hot channel flag
C-                                        dimensioned by eta, phi, layer
C-             IRUNNO   - run number
C-   Outputs : IER - Error code
C-                   -1 -> -4 error from D0DBL3_WRITFZ
C-                   -10 Cannot find parameters in RCP bank
C-                   -15 CHOT bank already exists
C-                   -20 Error booking CHOT bank
C-   Controls: none
C-
C-    Hot/bad channel flags
C-       bit  0 = bad ped value
C-       bit  1 = bad gain value
C-       bit  2 = hot or noisy channel
C-    IC(LCHOT+4) flags bits
C-       bit  0 = includes ped bad channels
C-       bit  1 = includes gain bad channels
C-       bit  2 = includes hot or noisy channels
C-       bit  7 = exceeded maximum number of hot/bad channels (500)
C-   Created  26-JAN-1993   Jan Guida
C-   Updated  28-MAY-1993   Jan Guida  Change DBCHOT to CHOT_TODO 
C-   Updated  13-NOV-1993   Jan Guida  Replace AND with IAND (FLINT) 
C-   Updated  19-NOV-1993   Jan Guida  Put hot chan flags into IWORD rather 
C-                          than IBWORD.   There are problems when setting MSB.
C-   Updated  28-MAR-1994   Jan Guida  Change BADFLG from INT*2 to INT
C-                                      and MASK from FFFF to FF (for IBM)
C-   Updated  30-DEC-1994   Jan Guida  Initialize IWORD to 0 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZCHOT.LINK'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INCLUDE 'D0$LINKS:IZCGN8.LINK'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INCLUDE 'D0$LINKS:IZCPB8.LINK'
      INCLUDE 'D0$LINKS:IZCPB1.LINK'
      INCLUDE 'D0$LINKS:IZCGB8.LINK'
      INCLUDE 'D0$LINKS:IZCGB1.LINK'
C      INCLUDE 'D0$INC:DBRSUM.INC'
      INTEGER NKYS
      PARAMETER (NKYS=15)
      INTEGER KEY(NKYS)
      INTEGER BPFLG,BGFLG
      PARAMETER (BPFLG=0)                  ! Flag for bad ped
      PARAMETER (BGFLG=1)                  ! Flag for bad gain
      CHARACTER*80 STRING,FILNAM
      CHARACTER*64 CHOT_TODO
      CHARACTER*32 PATH
      LOGICAL LFIRST,LOK
      INTEGER LCPD8,LCPD1,LCGN8,LCGN1,LCPB8,LCPB1,LCGB8,LCGB1
      INTEGER GZCPDH,GZCGNH,LZFIND
      INTEGER ICHOT(-NETAL:NETAL,NPHIL,NLYRL)
      INTEGER LCHOT,NH,NTOTHOT,IETA,IPHI,ILYR
      INTEGER GZCGEV,LCGEV
      INTEGER IWORD,IER,I,DBLEN,PLEN,IERR,LUN
      INTEGER IRUNNO,TRULEN,ILEN,FILID
      INTEGER GZCHOT
      INTEGER ICRATE,ACRATE(12),IADC,IBLS,ITWR,IDEP
      INTEGER NBAD,MSKPBD,MSKGBD,NEGLIM,ISCL,ITASK,IWRD
      INTEGER ICR,ITOT,NTOTBAD,NCLB,NTOT
      INTEGER PEDFLGS(16),GNSFLGS(16),MASK,BNKFLG
      INTEGER BADFLG
      BYTE IBWORD(4),CLBBAD(-NETAL:NETAL,NPHIL,NLYRL)
      EQUIVALENCE (IWORD,IBWORD)
      DATA LFIRST /.TRUE./
      DATA ACRATE/7,17,27,37,47,57,8,18,28,38,48,58/
      DATA MASK/z'FF'/
C
C----------------------------------------------------------------------
C
      IF (LFIRST) THEN
        CALL EZPICK('CHOTFLAG_RCP')
        CALL EZERR(IERR)
        IF (IERR.NE.0) THEN
          CALL ERRMSG('CALHOT','CHOTFL',
     &      'Error picking CHOTFLAG_RCP','W')
          IER = -10
          GO TO 999
        ENDIF
        CALL EZGETS('CHOT_TODO',1,CHOT_TODO,DBLEN,IERR)
        IF (IERR.NE.0) THEN
          CALL ERRMSG('CALHOT','CHOTFL',
     &      'Error reading CHOT_TODO from rcp file','W')
          IER = -10
          GO TO 999
        ENDIF
        IF(CHOT_TODO(DBLEN:DBLEN).NE.']')THEN
          DBLEN=DBLEN+1
          CHOT_TODO(DBLEN:DBLEN)=':'
        ENDIF
        CALL EZGETS('CHOT_PATH',1,PATH,PLEN,IERR)
        IF (IERR.NE.0) THEN
          CALL ERRMSG('CALHOT','CHOTFL',
     &      'Error reading CHOT_PATH from rcp file','W')
          IER = -10
          GO TO 999
        ENDIF
C
        CALL VZERO_i(PEDFLGS,16)
        CALL VZERO(GNSFLGS,16)
        CALL EZGET('PEDFLGS',PEDFLGS,IERR)
        IF (IERR.NE.0) THEN
          CALL ERRMSG('CALHOT','CHOTFL',
     &      'Error reading PEDFLGS from rcp file using default','W')
          PEDFLGS(1) = 3
          PEDFLGS(2) = 6
          PEDFLGS(3) = 13
          PEDFLGS(4) = 14
          PEDFLGS(5) = 15
        ENDIF
        CALL EZGET('GNSFLGS',GNSFLGS,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('CALHOT','CHOTFL',
     &      'Error reading GNSFLGS from rcp file using default','W')
          GNSFLGS(1) = 3
          GNSFLGS(2) = 6
          GNSFLGS(3) = 13
          GNSFLGS(4) = 14
          GNSFLGS(5) = 15
        ENDIF
        MSKPBD = 0
        MSKGBD = 0
        DO I=1,15
          IF (PEDFLGS(I).NE.0) THEN
            MSKPBD = IBSET(MSKPBD,PEDFLGS(I)-1)
          ENDIF
          IF (GNSFLGS(I).NE.0) THEN
            MSKGBD = IBSET(MSKGBD,GNSFLGS(I)-1)
          ENDIF
        ENDDO
C
        CALL EZRSET
        LFIRST = .FALSE.
      ENDIF
C
C***** Unpack calib bad channel banks
C
      BNKFLG = 0
      NCLB = (2*NETAL+1)*NPHIL*NLYRL/4
      CALL VZERO_b(CLBBAD,NCLB)
      DO ICR = 1,12           ! all 12 ADC crates
        ICRATE = ACRATE(ICR)
C
C****   find bad pedestals
C
        LCPDH = GZCPDH()
        LCPDH = LZFIND(IDVSTP,LCPDH,ICRATE,9)   !Finds Bank with Crate
        IF (LCPDH.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL',
     &          'Pedestal header bank does not exist','W')
          GO TO 200
        ENDIF
        LCPD8 = LC(LCPDH-IZCPD8)
        IF (LCPD8.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL','Bank CPD8 does not exist',
     &      'W')
          GO TO 200
        ENDIF
        LCPD1 = LC(LCPDH-IZCPD1)
        IF (LCPD1.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL','Bank CPD1 does not exist',
     &          'W')
          GO TO 200
        ENDIF
C
        LCPB8 = LC(LCPD8-IZCPB8)
        IF (LCPB8.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL','Bank CPB8 does not exist',
     &          'W')
          GO TO 200
        ENDIF
        LCPB1 = LC(LCPD1-IZCPB1)
        IF (LCPB1.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL','Bank CPB1 does not exist',
     &          'W')
          GO TO 200
        ENDIF
C
        BNKFLG = IBSET(BNKFLG,0)  ! Flag saying bad peds included
C
C****  x8 peds
C
        NBAD = IC(LCPB8+1)
        ISCL = 0
        DO I = 1,NBAD
          IWRD = IAND(IC(LCPB8+1+I),MSKPBD)
          IF (IWRD .NE. 0) THEN
            CALL CADUPK(ICRATE,IC(LCPB8+1+I),ICRATE,IADC,IBLS,ITWR,
     &            IDEP,ISCL,NEGLIM)
            CALL CADPH(ICRATE,IADC,IBLS,ITWR,IDEP,IETA,IPHI,ILYR,IERR)
            IF (IERR.EQ.0) THEN
              NTOTBAD = NTOTBAD + 1
              BADFLG = CLBBAD(IETA,IPHI,ILYR)
              CLBBAD(IETA,IPHI,ILYR) = IAND(IBSET(BADFLG,BPFLG),MASK)
            ENDIF
          ENDIF
        ENDDO
C
C****  x1 peds
C
        NBAD = IC(LCPB1+1)
        ISCL = 1
        DO I = 1,NBAD
          IWRD = IAND(IC(LCPB1+1+I),MSKPBD)
          IF (IWRD .NE. 0) THEN
            CALL CADUPK(ICRATE,IC(LCPB1+1+I),ICRATE,IADC,IBLS,ITWR,
     &          IDEP,ISCL,NEGLIM)
            CALL CADPH(ICRATE,IADC,IBLS,ITWR,IDEP,IETA,IPHI,ILYR,IERR)
            IF (IERR.EQ.0) THEN
              NTOTBAD = NTOTBAD + 1
              BADFLG = CLBBAD(IETA,IPHI,ILYR)
              CLBBAD(IETA,IPHI,ILYR) = IAND(IBSET(BADFLG,BPFLG),MASK)
            ENDIF
          ENDIF
        ENDDO
C
C****   find bad gains
C
  200   CONTINUE
        LCGNH = GZCGNH()
        LCGNH = LZFIND(IDVSTP,LCGNH,ICRATE,9)   !Finds Bank with Crate
        IF (LCGNH.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL',
     &        'Gain header bank does not exist','W')
          GO TO 201
        ENDIF
        LCGN8 = LC(LCGNH-IZCGN8)
        IF (LCGN8.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL','Bank CGN8 does not exist',
     &        'W')
          GO TO 201
        ENDIF
        LCGN1 = LC(LCGNH-IZCGN1)
        IF (LCGN1.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL','Bank CGN1 does not exist',
     &      'W')
          GO TO 201
        ENDIF
C
        LCGB8 = LC(LCGN8-IZCGB8)
        IF (LCPB8.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL','Bank CGB8 does not exist',
     &        'W')
          GO TO 201
        ENDIF
C
        LCGB1 = LC(LCGN1-IZCGB1)
        IF (LCPB1.LE.0) THEN
          CALL ERRMSG(' CALHOT','CHOTFL','Bank CGB1 does not exist',
     &      'W')
          GO TO 201
        ENDIF
C
        BNKFLG = IBSET(BNKFLG,1)  ! Flag saying bad gains included
C
C****  x8 gains
C
        NBAD = IC(LCGB8+1)
        ISCL = 0
        DO I = 1,NBAD
          IWRD = IAND(IC(LCGB8+1+I),MSKPBD)
          IF (IWRD .NE. 0) THEN
            CALL CADUPK(ICRATE,IC(LCGB8+1+I),ICRATE,IADC,IBLS,ITWR,
     &          IDEP,ISCL,NEGLIM)
            CALL CADPH(ICRATE,IADC,IBLS,ITWR,IDEP,IETA,IPHI,ILYR,IERR)
            IF (IERR.EQ.0) THEN
              IF (CLBBAD(IETA,IPHI,ILYR).EQ.0) NTOTBAD = NTOTBAD + 1
              BADFLG = CLBBAD(IETA,IPHI,ILYR)
              CLBBAD(IETA,IPHI,ILYR) = IAND(IBSET(BADFLG,BGFLG),MASK)
            ENDIF
          ENDIF
        ENDDO
C
C****  x1 gains
C
        NBAD = IC(LCGB1+1)
        ISCL = 1
        DO I = 1,NBAD
          IWRD = IAND(IC(LCGB1+1+I),MSKPBD)
          IF (IWRD .NE. 0) THEN
            CALL CADUPK(ICRATE,IC(LCGB1+1+I),ICRATE,IADC,IBLS,ITWR,
     &          IDEP,ISCL,NEGLIM)
            CALL CADPH(ICRATE,IADC,IBLS,ITWR,IDEP,IETA,IPHI,ILYR,IERR)
            IF (IERR.EQ.0) THEN
              IF (CLBBAD(IETA,IPHI,ILYR).EQ.0) NTOTBAD = NTOTBAD + 1
              BADFLG = CLBBAD(IETA,IPHI,ILYR)
              CLBBAD(IETA,IPHI,ILYR) = IAND(IBSET(BADFLG,BGFLG),MASK)
            ENDIF
          ENDIF
        ENDDO
  201   CONTINUE
      ENDDO
C
C****  Book the CHOT bank
C
      LCGEV = GZCGEV()
      IF (LCGEV.GT.0) THEN
        LCHOT = LC(LCGEV-IZCHOT)
      END IF
      IF (LCHOT.GT.0) THEN
        IER = -15
        CALL ERRMSG('CALHOT','CHOTFL','CHOT bank already exists','W')
        GO TO 999
      ENDIF
      NTOT = NTOTHOT + NTOTBAD
      IF (NTOT .GT. 500) NTOT = 500           ! max bank size = 500 channels
      BNKFLG = IBSET(BNKFLG,2)                ! bit 3 = hot chan
C
      CALL BKCHOT(NTOT,LCHOT)
      IF (LCHOT.LE.0) THEN
        IER = -20
        CALL ERRMSG('CALHOT','CHOTFL','Error booking CHOT bank','W')
        GO TO 999
      ENDIF
C
      NH = IC(LCHOT+1)
C
C****  Fill the CHOT bank with hot/noisy channels and clb bad channels
C
      IF (NTOT.GT.0) THEN
        I = 0
        DO IETA = -NETAL,NETAL
          IF (IETA.NE.0) THEN
            DO IPHI = 1,NPHIL
              DO ILYR = 1,NLYRL
                IF (ICHOT(IETA,IPHI,ILYR).GT.0 .OR.
     &              CLBBAD(IETA,IPHI,ILYR).GT.0) THEN
                  I = I + 1
                  IWORD = 0
                  IBWORD(BYTE4) = IETA
                  IBWORD(BYTE3) = IPHI
                  IBWORD(BYTE2) = ILYR
                  BADFLG = CLBBAD(IETA,IPHI,ILYR)
                  BADFLG = IAND(IOR(ICHOT(IETA,IPHI,ILYR),BADFLG),MASK)
                  IWORD = IOR(IWORD,BADFLG)
                  IF (I.LE.500) IC(LCHOT+NH+I) = IWORD
                ENDIF
              ENDDO     ! ILYR
            ENDDO       ! IPHI
          ENDIF         ! IF(NETAL)
        ENDDO           ! IETA
        ITOT = I
        IF (ITOT.LT.NTOTHOT) THEN
          WRITE(STRING,100) ITOT,NTOTHOT,NTOTBAD
  100     FORMAT('Inconsistent no of bad chans',I7,' no of hot chans',
     &        I7,' no of clb bad chan',I7)
          CALL ERRMSG('CALHOT','CHOTFL',STRING,'W')
        ENDIF
        IF (ITOT.GT.NTOT) THEN
          WRITE(STRING,100) ITOT,NTOTHOT,NTOTBAD
          CALL ERRMSG('CALHOT','CHOTFL',STRING,'W')
        ENDIF
      ENDIF
C
      IC(LCHOT+3) = ITOT
      IF (ITOT.GE.500) BNKFLG = IBSET(BNKFLG,7)
      IC(LCHOT+4) = BNKFLG
      IC(LCHOT+5) = IRUNNO
C
      KEY(3) = IRUNNO        ! run number
      KEY(4) = 999999999     ! end validity
      KEY(8) = IRUNNO        ! run number
C
      WRITE(FILNAM,301)CHOT_TODO(1:DBLEN),IRUNNO,IRUNNO
  301 FORMAT(A,'CAL_CHOT_',I6.6,'_',I6.6,'.FZ')
C
      CALL D0DBL3_OPENFZ(FILNAM,IDVSTP,'O',FILID,IER)
      IF (IER.NE.0) THEN
        WRITE(STRING,310)IER
  310   FORMAT(' OPENFZ returns error code ',I3)
        CALL ERRMSG('OPEN FAILURE','CHOTFL',STRING,'W')
        GOTO 999
      ENDIF
      CALL D0DBL3_OUTFZ(FILID,'OG',10,PATH,NKYS,KEY,'R-KS34',LCHOT,IER)
      IF (IER.NE.0) THEN
        WRITE(STRING,311)IER
  311   FORMAT(' OUTFZ returns error code ',I3,' - aborting')
        CALL ERRMSG('CALHOT','CHOTFL',STRING,'W')
        GO TO 999
      ENDIF
      CALL D0DBL3_CLOSEFZ (FILID,IER)
C
  999 RETURN
      END
