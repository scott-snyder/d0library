      SUBROUTINE CGEVFL_HOT(RCP_FILE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Kill CGEV cells that are "hot"
C-                          by setting K=K*HOTSUP
C-                          HOT bits are masked off as specified in
C-                          \ARRAY HOT_CHANNEL_BITS.
C-
C-   Inputs  :RCP_FILE [C]  RCP_FILE for control parameters
C-
C-   Outputs : IER
C-                     0  ok
C-                     1  error finding database name (dbmu_gtfile)
C-                    10  error reading parameters from RCP bank
C-                    11  error initializing database
C-                    12  no information in database for this run
C-                    20  LCHOT is bad
C-   Controls: RCP_FILE
C-
C-   Created  26-JAN-1993   Joan Guida based on CGEVFL_BAD
C-   Updated  26-NOV-1993   Joan Guida  fix for new CALIB database scheme of
C-                          reading database only once per validity region 
C-   Updated  19-MAR-1994   Joan Guida   fix for when CGEV has been reset
C-   Updated  21-JUL-1995   Jan Guida  Check on error code 12 from CHOT_READ
C-   Updated  21-JUL-1995   Jan Guida  Add IER=20, if LCHOT is bad 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_FILE
      INTEGER IER
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZCPB1.LINK'
      INTEGER I,J,K,LCGEV,LCGEV1,LCGEV8,GZCGEV,IRUNNO
      INTEGER NR,NH,LOC,LCHOT,NHOT,NHOTHD
      INTEGER IDATA,IETA,IPHI,ILYR,IE,IP,IL,IP0
      INTEGER ICR,ICRT,ICABLE,IPED_CRT,IGNS_CRT
      INTEGER IAND,IOR,BIT
      LOGICAL BTEST,EZERR,LHOT
      INTEGER HOT_BITS(16),NBITS,MASK
      INTEGER GZCHOT,LZFIND
      INTEGER MAX_LIST,NKEEP
      PARAMETER (MAX_LIST=50)
      INTEGER KEEP_LIST(3,MAX_LIST),LETA,LPHI,LLYR
      INTEGER IHOT_CHAN(-NETAL:NETAL,0:1,NLYRL)
      CHARACTER*80 MSG
      REAL    AWC1,AWC8
      BYTE BYTES(4)
      EQUIVALENCE (IDATA,BYTES)
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      SAVE IHOT_CHAN
      DATA IHOT_CHAN/2550*0/
C----------------------------------------------------------------------
      IER=0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZLOC(RCP_FILE,LOC)
        IF(LOC.EQ.0) CALL INRCP(RCP_FILE,IER)
        CALL EZPICK(RCP_FILE)
        IF(EZERR(IER)) THEN
          CALL ERRMSG('NO_RCP_FILE_CGEV','CGEVFL_HOT',RCP_FILE,'W')
        ELSE
          CALL EZGET ('HOT_CHANNEL_BITS',  HOT_BITS,IER)
          CALL EZGET ('KEEP_CHANNELS',  KEEP_LIST,IER)
          CALL EZGET_SIZE ('HOT_CHANNEL_BITS',NBITS,IER)
          CALL EZGET_SIZE ('KEEP_CHANNELS'    ,NKEEP,IER)
          IF(MOD(NKEEP,3).NE.0) THEN
            CALL ERRMSG('KEEP_CHANNELS_SET_WRONG','CGEVFL_HOT',
     &        'LIST IN RCP: ETA,PHI,LYR','W')
            NKEEP = 0
          END IF
          NKEEP = NKEEP / 3
        END IF
        CALL EZRSET
C
C ****  BUILD BAD MASK
C
        MASK = 0
        DO I = 1, NBITS
          BIT = HOT_BITS(I)
          MASK=MASK + ISHFT(1,BIT)
        END DO
        LCGEV = GZCGEV ()
        NH = IC(LCGEV+1)
        NR = IC(LCGEV+4)
      ELSE
C----------------------------------------------------------------------
C    FIX SINCE NO LONGER FILL CGEV BANK AT BEGINNING OF EVERY RUN
C    MUST RETURN CGEV BANK TO ORGINAL STATE WITH NO HOT CHANNELS
        LCGEV = GZCGEV ()
      END IF
C
      IF((LCGEV.GT.0)) THEN
        LCGEV1=LZFIND(IDVSTP,LCGEV,1,3)
        LCGEV8=LZFIND(IDVSTP,LCGEV,0,3)
      ENDIF
      CALL CGEVFL_CRTVAL(IPED_CRT,IGNS_CRT)
      DO IETA=-37,37
        IF(IETA.NE.0)THEN
          DO IPHI=1,64
            DO ILYR=1,17
              IP0=(IPHI-1)/32                  !0 for phi=1-32; 1 for phi=33-64
              BIT=IPHI-1-32*IP0                !phi bit 0-31
              IF(BTEST(IHOT_CHAN(IETA,IP0,ILYR),BIT))THEN
                CALL CPHCRT(IETA,IPHI,ILYR,ICRT)  ! Get crate number
                ICABLE = 0
                IF(MOD(ICRT,10).EQ.8) ICABLE = 1
                ICR=ICRT/10 + 1 + ICABLE*6         ! Get crate number 1-12
                IF(.NOT.BTEST(IGNS_CRT,ICR-1))THEN ! Check for new gains run
                  I=NH+NR*(ILYR-1+(IPHI-1)*NLYRL+
     &              (IETA+NETAL)*NPHIL*NLYRL)
                  IF (C(LCGEV1+I+1).GT.100.) THEN
                    WRITE(MSG,100) 'x1',IETA,IPHI,ILYR,C(LCGEV1+I+1)
  100               FORMAT(' For channel ',A,2X,2I4,G11.3)
                    CALL ERRMSG('BAD ENERGY','CGEVFL_HOT',MSG,'W')
                  ENDIF
                  IF (C(LCGEV8+I+1).GT.100.) THEN
                    WRITE(MSG,100) 'x1',IETA,IPHI,ILYR,C(LCGEV1+I+1)
                    CALL ERRMSG('BAD ENERGY','CGEVFL_HOT',MSG,'W')
                  ENDIF
                  C(LCGEV1+I+1)=C(LCGEV1+I+1)/HOTSUP
                  C(LCGEV8+I+1)=C(LCGEV8+I+1)/HOTSUP
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C----------------------------------------------------------------------
      CALL VZERO(IHOT_CHAN,2*(2*NETAL+1)*NLYRL)
C
      IRUNNO=IQ(LHEAD+6)
      CALL CHOT_READ(IRUNNO,IER)
      IF(IER.NE.0)THEN
        IF (IER.EQ.12) THEN
          CALL ERRMSG('NO HOT CHANNEL INFO','CGEVFL_HOT',
     &      'NO CHOT INFO','I')
        ELSE
          CALL ERRMSG('NO HOT CHANNEL INFO','CGEVFL_HOT',
     &      'FAILED GETTING CHOT BANK','W')
        ENDIF
        GOTO 999
      ENDIF
      LCHOT = GZCHOT()
      IF (LCHOT.LE.0) THEN
        IER = 20
        GO TO 999
      ENDIF
      NHOT = IC(LCHOT+3)
      NHOTHD = IC(LCHOT+1)
      LCGEV = GZCGEV()
      IF((LCGEV.GT.0)) THEN
        LCGEV1=LZFIND(IDVSTP,LCGEV,1,3)
        LCGEV8=LZFIND(IDVSTP,LCGEV,0,3)
      ENDIF
      DO 40, J = 1, NHOT
      IDATA = IC(LCHOT+NHOTHD+J)
      IETA  = BYTES(BYTE4)
      IPHI  = BYTES(BYTE3)
      ILYR  = BYTES(BYTE2)
      I=NH+NR*(ILYR-1+(IPHI-1)*NLYRL+(IETA+NETAL)*NPHIL*NLYRL)
      AWC1 = C(LCGEV1+I+1)
      AWC8 = C(LCGEV8+I+1)
C
C ****  Check bits
C
      IF (IAND(MASK,IDATA).NE.0) THEN
        MSG = 'HOT BITS IN CAHITS_RCP'
        CALL CAHITS_ERRMSG(6,IETA,IPHI,ILYR,0,0,IDATA,
     &      MSG,IER)
        IF (AWC8.LT.1.0E-06) THEN
          WRITE(MSG,100) 'x8',IETA,IPHI,ILYR,AWC8
          CALL ERRMSG('BAD ENERGY','CGEVFL_HOT',MSG,'W')
        ENDIF
        IF (AWC1.LT.1.0E-06) THEN
          WRITE(MSG,100) 'x1',IETA,IPHI,ILYR,AWC1
          CALL ERRMSG('BAD ENERGY','CGEVFL_HOT',MSG,'W')
        ENDIF
        AWC8 = AWC8*HOTSUP
        CALL CAHITS_ERRMSG(6,IETA,IPHI,ILYR,1,0,IDATA,
     &      MSG,IER)
        AWC1 = AWC1*HOTSUP
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
            CALL CAHITS_ERRMSG(5,IETA,IPHI,ILYR,0,0,0,
     &        'IN KEEP_CHANNELS LIST IN CAHITS_RCP',IER)
            CALL CAHITS_ERRMSG(5,IETA,IPHI,ILYR,1,0,0,
     &        'IN KEEP_CHANNELS LIST IN CAHITS_RCP',IER)
            GOTO 40
          END IF
        END DO
C
C ****  SET K = AWC (K for no bits, *HOTSUP otherwise)
C
        C(LCGEV1+I+1) = AWC1
        C(LCGEV8+I+1) = AWC8
        IP0=(IPHI-1)/32                       !0 for phi=1-32; 1 for phi=33-64
        BIT=IPHI-1-32*IP0
        IHOT_CHAN(IETA,IP0,ILYR)=IOR(IHOT_CHAN(IETA,IP0,ILYR),
     &                               ISHFT(1,BIT))
C
      END IF
   40 CONTINUE
  999 RETURN
C
      ENTRY CGEVFL_HOT_CHAN(IE,IP,IL,LHOT)
      IP0=(IP-1)/32                         !0 for phi=1-32; 1 for phi=33-64
      BIT=IP-1-32*IP0                       !phi bit 0-31
      LHOT=BTEST(IHOT_CHAN(IE,IP0,IL),BIT)
      RETURN
      END
