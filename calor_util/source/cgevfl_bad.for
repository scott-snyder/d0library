      SUBROUTINE CGEVFL_BAD(RCP_FILE,ICRATE,LCARD,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Kill CGEV cells with bad CALIB gains and pedestals
C-                          by setting K=0(CGEV bank gain word=AWCG).
C-                          CPB and CGB bank bits are masked off as specified in
C-                          \ARRAY BAD_CHANNEL_CXBX_BITS. If the bit is set
C-                          negative for CGB1 or CGB8 gains then the gain in
C-                          CGEV is set to AWC without the CALIB gain. For EM
C-                          floor 3 and 4 cells CGEV gain is set to
C-                          A*W*C*EM_GAIN where EM_GAIN is fetched from RCP_FILE
C-
C-   Inputs  :RCP_FILE [C]  RCP_FILE for control parameters
C-            ICRATE   [I]  CRATE number for electronics address
C-            LCARD    [I]  Address of bank from which bad channel bank hangs
C-
C-   Outputs : IER
C-   Controls: RCP_FILE
C-
C-   Created   7-APR-1992   Chip Stewart
C-   Updated  13-SEP-1992   Chip Stewart  - BAD_BIT START WITH 1 NOT 0 
C-   Updated  10-NOV-1992   CS            - CELL_FBKCAP 
C-   Updated   8-APR-1993   Joan Guida    - Kill both x8 and x1 cells if only 
C-                                       one is bad, only if DO_FIX_8_1 is TRUE 
C-   Updated  21-DEC-1993   Jan Guida     - If calib version .ge. 4.1, 
C-                           use calib gains even for bad channels, when flag=4
C-   Updated  11-MAR-1994   Jan Guida     - When testing for flag 4 set (gains)
C-                                            use IDATA not BITS (before masked)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_FILE
      INTEGER ICRATE,LCARD,IER
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$PARAMS:PRTPDG.DEF'
      INCLUDE 'D0$LINKS:IZCPB1.LINK'
      INTEGER ADDR,MAX_ADDR,MAX_SEQ,IB
      PARAMETER (MAX_SEQ = NDEPTC*NEFC*NBLSC)
      INTEGER I,J,K,LCGEV,GZCGEV,NR,NH,LOC,SCALE,TASK,IZ,LBAD,NBAD,BITS
      INTEGER LCGEVS(0:1),IS
      INTEGER IDATA,CRATE,ADC,BLS,ROTOW,DEPTH,NG,IETA,IPHI,ILYR
      INTEGER IAND,IBITS,BIT,CELL_FBKCAP
      LOGICAL MC,BTEST,EZERR
      LOGICAL DO_ADC_TO_GEV,TCOR,DO_FIX_8_1
      INTEGER BAD_BITS(16,2,0:1),NBITS(2,0:1),MASK(2,0:1),MASK1(0:1)
      INTEGER LCSFW,GZCSFW,LCSFC,GZCSFC,LCSFC1,LZFIND,ICSFW,ICSFC
      INTEGER MAX_LIST,NKEEP
      PARAMETER (MAX_LIST=50)
      INTEGER KEEP_LIST(3,MAX_LIST),LETA,LPHI,LLYR
      CHARACTER NAME*4,MSG*80,ERR_ID*80
      REAL    EM_GAIN,AWC(0:1)
      REAL CLBVSN
      LOGICAL FIRST,KILL,KEEP,IGNORE
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZLOC(RCP_FILE,LOC)
        IF(LOC.EQ.0) CALL INRCP(RCP_FILE,IER)
        CALL EZPICK(RCP_FILE)
        IF(EZERR(IER)) THEN
          CALL ERRMSG('NO_RCP_FILE_CGEV','CGEVFL_BAD',RCP_FILE,'W')
        ELSE
          CALL EZGET ('BAD_CHANNEL_CPB8_BITS',  BAD_BITS(1,1,0),IER)
          CALL EZGET ('BAD_CHANNEL_CPB1_BITS',  BAD_BITS(1,1,1),IER)
          CALL EZGET ('BAD_CHANNEL_CGB8_BITS',  BAD_BITS(1,2,0),IER)
          CALL EZGET ('BAD_CHANNEL_CGB1_BITS',  BAD_BITS(1,2,1),IER)
          CALL EZGET ('KEEP_CHANNELS',  KEEP_LIST,IER)
          CALL EZGET_SIZE ('BAD_CHANNEL_CPB8_BITS',NBITS(1,0),IER)
          CALL EZGET_SIZE ('BAD_CHANNEL_CPB1_BITS',NBITS(1,1),IER)
          CALL EZGET_SIZE ('BAD_CHANNEL_CGB8_BITS',NBITS(2,0),IER)
          CALL EZGET_SIZE ('BAD_CHANNEL_CGB1_BITS',NBITS(2,1),IER)
          CALL EZGET_SIZE ('KEEP_CHANNELS'    ,NKEEP,IER)
          IF(MOD(NKEEP,3).NE.0) THEN
            CALL ERRMSG('KEEP_CHANNELS_SET_WRONG','CGEVFL_BAD',
     &        'LIST IN RCP: ETA,PHI,LYR','W')
            NKEEP = 0
          END IF
          NKEEP = NKEEP / 3
          CALL EZGET ('EM_GAIN',           EM_GAIN,IER)
          CALL EZGET ('DO_ADC_TO_GEV',     DO_ADC_TO_GEV,IER)
          CALL EZGET ('CSF_CORRECTIONS',   TCOR,IER)
          CALL EZGET ('DO_FIX_8_1',        DO_FIX_8_1,IER)
          IF(IER.NE.0)DO_FIX_8_1=.FALSE.
        END IF
        CALL EZRSET
C
C ****  BUILD BAD MASK
C
        DO SCALE = 0,1
          MASK1(SCALE) = 0
          DO TASK = 1,2
            MASK(TASK,SCALE) = 0
            DO I = 1, NBITS(TASK,SCALE)
              IF (BAD_BITS(I,TASK,SCALE).GE.0) THEN
                BIT = BAD_BITS(I,TASK,SCALE) - 1
                MASK(TASK,SCALE)=MASK(TASK,SCALE) + 2**BIT
              ELSE IF(TASK.EQ.2) THEN
                BIT = - BAD_BITS(I,TASK,SCALE) - 1
                MASK1(SCALE)=MASK1(SCALE) + 2**(BIT)
              END IF
            END DO
          END DO
        END DO
        LCGEV = GZCGEV ()
        NH = IC(LCGEV+1)
        NR = IC(LCGEV+4)
        IZ = IZCPB1  ! ALL bad cell banks hang under same link slot
      END IF
C----------------------------------------------------------------------
      LBAD = LC(LCARD-IZ)
      CALL UHTOC (IC(LBAD-4),4,NAME,4)
      IF(NAME(1:3).EQ.'CPB') THEN
        TASK = 1
      ELSE IF(NAME(1:3).EQ.'CGB') THEN
        TASK = 2
      END IF
      IF(NAME(4:4).EQ.'1') THEN
        SCALE = 1
      ELSE IF(NAME(4:4).EQ.'8') THEN
        SCALE = 0
      END IF
      NBAD = IC(LBAD+1)
      LCGEV = GZCGEV()
      IF((LCGEV.GT.0)) THEN
        LCGEVS(0)=LZFIND(IDVSTP,LCGEV,0,3)
        LCGEVS(1)=LZFIND(IDVSTP,LCGEV,1,3)
      ENDIF
      CLBVSN = C(LSCAL+9)
      IF (CLBVSN.GT.10000) THEN
          CLBVSN = 4.0     ! Bug in vsn number before 4.1
      ELSEIF (CLBVSN.LT.-10000) THEN
        CLBVSN = 3.0       ! Bug in vsn number before 4.1
      ENDIF
C
      DO 40, J = 1, NBAD
        IDATA = IC(LBAD+1+J)
        CALL CADUPK(ICRATE,IDATA,CRATE,ADC,BLS,ROTOW,DEPTH,SCALE,NG)
        IF ((MASK(TASK,SCALE).EQ.0).AND.(MASK1(SCALE).EQ.0)) GOTO 40
        CALL CADT_ADCPHY(CRATE,ADC,BLS,ROTOW,DEPTH,
     &            IETA,IPHI,ILYR,IER)
        IF(IER.NE.0) GOTO 40
        I=NH+NR*(ILYR-1+(IPHI-1)*NLYRL+(IETA+NETAL)*NPHIL*NLYRL)
        AWC(0) = C(LCGEVS(0)+I+1)
        AWC(1) = C(LCGEVS(1)+I+1)
C
C ****  Check positive bits
C
        BITS = IAND(MASK(TASK,SCALE),IDATA)
        IF (BITS.NE.0) THEN
          ADDR = IBITS(IDATA,16,16)
          MSG = NAME//' BITS IN CAHITS_RCP'
          CALL CAHITS_ERRMSG(TASK,IETA,IPHI,ILYR,SCALE,CRATE,IDATA,
     &      MSG,IER)
          IS=MOD(SCALE+1,2)
          IF(DO_FIX_8_1)CALL CAHITS_ERRMSG(TASK,IETA,IPHI,ILYR,IS,CRATE,
     &      IDATA,MSG,IER)
          AWC(0) = 0.
          AWC(1) = 0.
        END IF
C
C ****  Then check negative bits (gains only)
C
        BITS = IAND(MASK1(SCALE),IDATA)
        IF ((TASK.EQ.2).AND.(BITS.NE.0)) THEN
C***  If old version do not use gain value from database
C***  If new version with bad channel flag = 4, use gain value from database
          IF (CLBVSN.LT.4.1 .OR. 
     &      (CLBVSN.GE.4.1 .AND. .NOT.BTEST(IDATA,3))) THEN
            ADDR = IBITS(IDATA,16,16)
            LCSFW = GZCSFW ()
            LCSFC = GZCSFC ()
            IF((LCSFC.GT.0).AND.TCOR) LCSFC1=LZFIND(IDVSTP,LCSFC,ILYR,2)
            ICSFW =ILYR+(IABS(IETA)-1)*NLYRL  ! 1 to 629
            ICSFC = IPHI+(IETA+NETAL)*NPHIL
            IF(DO_ADC_TO_GEV) THEN
              IF((LCSFC1.GT.0).AND.TCOR) THEN
                AWC(0) = C(LCSFW+1+ICSFW) * C(LCSFC1+2+ICSFC) !sampling weights
                AWC(1) = AWC(0)
              ELSE
                AWC(0) = C(LCSFW+1+ICSFW)  !sampling weights
                AWC(1) = AWC(0)
              END IF
            ELSE
              AWC(0) = 1.0                                  ! ADC count E scale
              AWC(1) = 1.0                                  ! ADC count E scale
            END IF
            IF (CELL_FBKCAP(IETA,IPHI,ILYR).EQ.10) THEN
              AWC(0) = AWC(0)*EM_GAIN ! 10.5pf 
              AWC(1) = AWC(1)*EM_GAIN ! 10.5pf 
            ENDIF
            DO IB = 0, 16
              IF (IAND(BITS,2**IB).GT.0) GOTO 100
            END DO
  100       WRITE(MSG,101) NAME,IB+1, C(LCGEVS(SCALE)+I+1)/AWC(SCALE)
            CALL CAHITS_ERRMSG(3,IETA,IPHI,ILYR,SCALE,CRATE,IDATA, 
     &        MSG,IER)
  101       FORMAT(A4,' BIT ',I2,' GAIN FACTOR =',G10.3)
            IF(DO_FIX_8_1)THEN
              IS=MOD(SCALE+1,2)
              WRITE(MSG,101) NAME,IB+1, C(LCGEVS(IS)+I+1)/AWC(IS)
              CALL CAHITS_ERRMSG(3,IETA,IPHI,ILYR,IS,CRATE,IDATA,
     &          MSG,IER)
            END IF
          END IF
        END IF
C
C ****  CHECK KEEP_LIST
C
        DO K = 1, NKEEP
          LETA=KEEP_LIST(1,NKEEP)
          LPHI=KEEP_LIST(2,NKEEP)
          LLYR=KEEP_LIST(3,NKEEP)
          IF( ((IETA.EQ.LETA).OR.(LETA.EQ.0)).AND.
     &          ((IPHI.EQ.LPHI).OR.(LPHI.EQ.0)).AND.
     &          ((ILYR.EQ.LLYR).OR.(LLYR.EQ.0)) ) THEN
            CALL CAHITS_ERRMSG(5,IETA,IPHI,ILYR,SCALE,0,0,
     &      'IN KEEP_CHANNELS LIST IN CAHITS_RCP',IER)
            IF(DO_FIX_8_1)THEN
              IS=MOD(SCALE+1,2)
              CALL CAHITS_ERRMSG(5,IETA,IPHI,ILYR,IS,0,0,
     &        'IN KEEP_CHANNELS LIST IN CAHITS_RCP',IER)
            ENDIF
            GOTO 40
          END IF
        END DO
C
C ****  SET K = AWC (K for no bits, 0 for postive bits, CSFW for negative bits)
C
        C(LCGEVS(SCALE)+I+1) = AWC(SCALE)
        IS=MOD(SCALE+1,2)
        IF(DO_FIX_8_1) C(LCGEVS(IS)+I+1) = AWC(IS)
C
C ****  CHECK FOR NEGATIVE BITS TO IGNORE GAIN CORRECTIONS
C
   40 CONTINUE
  999 RETURN
      END

