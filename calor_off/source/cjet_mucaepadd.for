      SUBROUTINE CJET_MUCAEPADD(NEW_WORD4,NUM_NEWCELLS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Book and fill CAEP bank for MUON starting from MUCA bank
C-
C-   Created: 12-21-1992  by Alex Smith
C-
C-   Outputs:
C-
C-   Comments:
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC/LIST'       ! Protected Link area
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEP.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
C
      INTEGER I,J,K
      INTEGER LMUCA,LMUON,LPMUO,ITRAK,NTRAKS
      INTEGER IETA,IPHI,ILYR,PT_CAEP,PT_MUCA
      INTEGER NCH_MUCA,NCH_CAEP,NRT_MUCA,NRT_CAEP
      INTEGER GZPMUO
      INTEGER CAEPPT,GZCAEP,PACKWD
      BYTE BYTE1(4)
      REAL PMUON,IPMUON,PATHLENGTH
      REAL IPATH,SMALL,EMIPMUON
C     INTEGER LUNIT,USUNIT
      INTEGER NUM_NEWCELLS,NEW_WORD4(1000),NEWSIZE
      REAL    NEW_WORD5(1000)
      EQUIVALENCE (BYTE1(1),PACKWD)
C
C
C----------------------------------------------------------------------
C
C *** initialize
C
C      LUNIT = USUNIT()
      PATHLENGTH = 0.0
      IPATH = 0.0
      SMALL = 0.00000001
      NUM_NEWCELLS = 0
C
C *** Loop over muons, add muon momenta to CAEP bank:
C
      NTRAKS = 0
      LPMUO=GZPMUO(0)
      IF(LPMUO.LE.0) THEN        ! Abort if PMUO does not exist
        CALL ERRMSG('NO_CAEP_W/O_PMUO','CJET_MUCAEPADD',
     &    'SKIP CJET_MUCAEPADD','W')
        GOTO 999
      ENDIF
      DO WHILE (LPMUO .GT. 0)
        NTRAKS = NTRAKS + 1
        LPMUO = LQ(LPMUO)
      ENDDO      
      DO ITRAK = 1, NTRAKS
        LPMUO = GZPMUO(ITRAK)
        LMUON = LQ(LPMUO - IQ(LPMUO-2) - 2)
        IF(LMUON.LE.0) THEN        ! Abort if MUON does not exist
          CALL ERRMSG('NO_CAEP_W/O_MUON','CJET_MUCAEPADD',
     &      'SKIP CJET_MUCAEPADD','W')
          GOTO 999
        ENDIF
        LMUCA=LQ(LMUON-2)
        IF(LMUCA.LE.0) THEN        ! Abort if MUCA does not exist
          CALL ERRMSG('NO_CAEP_W/O_MUCA','CJET_MUCAEPADD',
     &      'SKIP CJET_MUCAEPADD','W')
          GOTO 999
        ENDIF
        IF (IQ(LPMUO+9) .GT. 1) GOTO 999
C
C *** Get muon momentum from PMUO bank:
C
        PMUON = Q(LPMUO+13)
        EMIPMUON = Q(LPMUO+33)
        PMUON = ABS(PMUON) - ABS(EMIPMUON)
        IF (PMUON .LE. 0) PMUON = 0.0
C
C *** loop over MUCA hits for the first time; find total path
C     length of muon in CAL.
C
        NCH_MUCA=IQ(LMUCA+3)              ! Number of cells
        NRT_MUCA=IQ(LMUCA+2)              ! MUCA repetition number
        PT_MUCA=LMUCA
        IF(NCH_MUCA.LE.0 .OR. NRT_MUCA.LE.0) GOTO 999
        DO I=1,NCH_MUCA
          PATHLENGTH = PATHLENGTH + Q(PT_MUCA+8)
          PT_MUCA=PT_MUCA+NRT_MUCA
        END DO
C
C *** loop over MUCA hits for the second time.
C     Add muon energy to the arrays.
C
        NCH_MUCA=IQ(LMUCA+3)                   ! Number of cells
        NRT_MUCA=IQ(LMUCA+2)                   ! MUCA repetition number
        PT_MUCA=LMUCA
        IF(NCH_MUCA.LE.0 .OR. NRT_MUCA.LE.0) GOTO 999
        LCAEP = GZCAEP()
        IF(LCAEP.LE.0) THEN
          CALL ERRMSG('NO CAEP CANT ADD MUON TO CAEP','CJET_MUCAEPADD',
     &      'SKIP CJET_MUCAEPADD','W')
          GOTO 999
        ENDIF
        NRT_CAEP = IQ(LCAEP+2)
        NCH_CAEP = IQ(LCAEP+3)
        DO I=1,NCH_MUCA
          IETA=IQ(PT_MUCA+4)
          IPHI=IQ(PT_MUCA+5)
          ILYR=IQ(PT_MUCA+6)
          IPATH=Q(PT_MUCA+8)
          IPMUON = (PMUON * IPATH)/(PATHLENGTH+SMALL)
          IF (IETA.NE.0 .AND. ABS(IETA).LE.NETAL 
     &      .AND. IPHI.GE.1 .AND. IPHI.LE.NPHIL
     &      .AND. ILYR.GE.1 .AND. ILYR.LE.NLYRL) THEN
            CAEPPT=PTCAEP(IETA,IPHI,ILYR)
            PT_CAEP = LCAEP + (CAEPPT-1)*NRT_CAEP
            IF (CAEPPT.EQ.0 ) THEN 
C --- Make sure cell not already in list -------------------------------------
              IF (NUM_NEWCELLS.GT.0) THEN
                DO K = 1, NUM_NEWCELLS
                  PACKWD = NEW_WORD4(K)
                  IF ( IETA.EQ.BYTE1(4) .AND. IPHI.EQ.BYTE1(3)
     &              .AND. ILYR.NE.BYTE1(2)) THEN
                    NEW_WORD5(K) = NEW_WORD5(K) + IPMUON
                    GOTO 300    ! cell already will be added
                  END IF
                END DO
              END IF
C ----------------------------------------------------------------------------
              NUM_NEWCELLS = NUM_NEWCELLS + 1
              LCAEP = GZCAEP()
              BYTE1(1) = IAND(IQ(LCAEP+4),255)
              BYTE1(2) = ILYR
              BYTE1(3) = IPHI
              BYTE1(4) = IETA
              NEW_WORD4(NUM_NEWCELLS) = PACKWD
              NEW_WORD5(NUM_NEWCELLS) = IPMUON
  300         CONTINUE
            ELSE
              Q(PT_CAEP+5) = Q(PT_CAEP+5) + IPMUON
            END IF
          END IF
          PT_MUCA=PT_MUCA+NRT_MUCA
        END DO ! I=1, NCH_MUCA
      END DO  ! PMUO loop
C
C --- Resize the CAEP bank and add new cells -------------------------------
C
      IF (NUM_NEWCELLS.GT.0) THEN
        LCAEP = GZCAEP()
        NEWSIZE = NUM_NEWCELLS + IQ(LCAEP+2)
        IF (NEWSIZE .GT. 500) THEN  ! Protection
          NUM_NEWCELLS = 0
          CALL ERRMSG('cant add >500 words TO CAEP','CJET_MUCAEPADD',
     &      'SKIP CJET_MUCAEPADD','W')
          GOTO 999
        END IF
        CALL MZPUSH(IXMAIN,LCAEP,0,NEWSIZE,' ')            
C
        LCAEP = GZCAEP()
        NCH_CAEP = IQ(LCAEP+3)
        NRT_CAEP = IQ(LCAEP+2)
        PT_CAEP = LCAEP + (NCH_CAEP)*NRT_CAEP           ! go to end
        IQ(LCAEP+3) = IQ(LCAEP+3) + NUM_NEWCELLS
        DO K = 1, NUM_NEWCELLS
          IQ(PT_CAEP+4) = NEW_WORD4(K)
          Q(PT_CAEP+5) = NEW_WORD5(K)
          PT_CAEP = PT_CAEP + NRT_CAEP
        END DO
      ENDIF
  999 RETURN
      END
