      SUBROUTINE CATEFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Book and fill CATE bank starting from CAEH bank
C-
C-   Created   19-MAR-1989   Andrzej Zieminski
C-   MODIFIED  10-MAY-1989   R RAJA
C-   MODIFIED  15-MAY-1989   NJ HADLEY
C-   Updated  10-OCT-1989   Rajendran Raja
C-   Updated  20-DEC-1989   Serban D. Protopopescu
C-   IHD=1 sum over EM (bit list includes first FH)
C-   IHD=2 sums over all layers.
C-   MODIFIED  16-SEP-1991   Allen I. Mincer  fill Et correctly before
C-                                sorting, allow tower E<0
C-   Updated   8-JUL-1993   James T. Linnemann less storage and faster
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Protected Link area
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
C      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INCLUDE 'D0$INC:PTCAEP.INC'
C
      INCLUDE 'D0$LINKS:IZCATE.LINK'
C
      LOGICAL OK,HAVE_DATA,USE,EM_HITS,FORCE
      CHARACTER*40 MSG
      INTEGER I,J,J1,L,ID,CATE
      INTEGER GZCATE,GZCAEH
      INTEGER IETA,IPHI,ILYR,IHD,PT_CATE(2),PT_CAEH,PT_CATE_0
      INTEGER PT_CAEH_0,OFFSET,IHD1
      INTEGER NCH,NR,NCHT,NRT,NTOT
C
      INTEGER NTWMAX
      PARAMETER (NTWMAX=4800)
      REAL    ETLIST(NTWMAX,2),ET(2),EX(2),EY(2)
      INTEGER NTWRS(2),INDEX(NTWMAX,2)  !INDEX: phi =IPHI_LIST(INDEX(k,ihd),ihd)
      BYTE    IETA_LIST(NTWMAX,2),IPHI_LIST(NTWMAX,2)
C
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INTEGER BIT(1:32),MINLYR,MAXLYR
      INTEGER MAXL(NETAL),MINL(NETAL)
      LOGICAL FIRST
      SAVE FIRST,BIT,MAXL,MINL
      DATA FIRST / .TRUE. /
C
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        DO J = 1,32
          BIT(J) = 0
          BIT(J) = IBSET(BIT(J),J-1)    !initialize bit masks
        ENDDO
        DO J = 1,NETAL
          MAXL(J) = MNLYCH !stop with CH1 unless in MG region
          IF ((J.GE.MNETAICD).AND.(J.LE.MXETAICD)) MAXL(J) = NLYRL
          MINL(J) = MNLYMG !if in MG region
          IF ((J.LT.MNETAMGICD).OR.(J.GT.MXETAMGICD)) MINL(J) = MNLYFH
        ENDDO
      ENDIF
C----------------------------------------------------------------------
C
      LCATE=GZCATE()
      IF(LCATE.NE.0) GOTO 999    ! If bank exists do nothing
C
      LCAEH=GZCAEH()
      IF(LCAEH.LE.0) THEN        ! Abort if CAEH does not exist
        CALL ERRMSG('NO_CATE_W/O_CAEH','CATEFL','SKIP CATEFL','W')
        GOTO 999
      ENDIF
C
C        initialize
C
      PTTFLG=.TRUE.
      NTOT=(2*NETAL+1)*NPHIL*2
      CALL VZERO(PTCATE,NTOT)
C--
C        loop over CAEH hits for the first time; find number of towers
C
      NCH=IQ(LCAEH+3)                   ! Number of cells
      NR=IQ(LCAEH+2)                    ! CAEH repetition number
      PT_CAEH = LCAEH                   !pointer to current cell
      PT_CAEH_0 = PT_CAEH - NR          !pointer to 0th cell
      DO IHD = 1,2
        NTWRS(IHD) = 0
        ET(IHD) = 0
        EX(IHD) = 0
        EY(IHD) = 0
      ENDDO
      IF(NCH.LE.0 .OR. NR.LE.0) GOTO 999
C
C...the plan is to loop over all channels, looking for unused towers
      DO I=1,NCH
        IETA=IQ(PT_CAEH+12)
        IPHI=IQ(PT_CAEH+13)
        IF (PTCATE(IETA,IPHI,2).EQ.0) THEN
          IHD1 = 2  !only hadronic tower unless an EM layer is found too
C...    when a new tower is found, use the pointer bank to get all its cells
          DO ILYR = 1, MAXL(IABS(IETA))
            OFFSET = PTCAEP(IETA,IPHI,ILYR)
            IF (OFFSET.GT.0) THEN
              OFFSET = PT_CAEH_0 + OFFSET*NR
              IF (ILYR.LE.MXLYEM) THEN
                IHD = 1   !em layer this time
                IHD1 = 1  !be sure to include EM tower
              ELSE
                IHD = 2   !had layer this time
              ENDIF
              EX(IHD)  = EX(IHD) + Q(OFFSET+4)
              EY(IHD)  = EY(IHD) + Q(OFFSET+5)
              ET(IHD)  = ET(IHD) + Q(OFFSET+8)
            ENDIF
          ENDDO
C... add the EM to the hadronic
          EX(2) = EX(2) + EX(1)
          EY(2) = EY(2) + EY(1)
          ET(2) = ET(2) + ET(1)
C...now RECORD the ET, eta, and phi of the tower
          DO  IHD = IHD1 ,  2     !skip EM if no EM channels found
            NTWRS(IHD)=NTWRS(IHD)+1
            L=NTWRS(IHD)
            ETLIST(L,IHD)=SQRT(EX(IHD)**2+EY(IHD)**2)
            IF(ET(IHD).LT.0.0)ETLIST(L,IHD) =-ETLIST(L,IHD)
            IETA_LIST(L,IHD) = IETA
            IPHI_LIST(L,IHD) = IPHI
            PTCATE(IETA,IPHI,IHD) = - L     !flag tower as used
            INDEX(L,IHD)=L
            ET(IHD) = 0
            EX(IHD) = 0
            EY(IHD) = 0
          ENDDO
        ENDIF
        PT_CAEH=PT_CAEH+NR    !go to next hit
      ENDDO
      DO IHD = 1 ,  2
C
C ****  Order lists; fill PTCATE array
C
        CALL SRTFLT(ETLIST(1,IHD),NTWRS(IHD),INDEX(1,IHD))
        DO J = 1 ,  NTWRS(IHD)
          J1=NTWRS(IHD)+1-J   !really wanted descending
          L=INDEX(J1,IHD)
          IETA = IETA_LIST(L,IHD)
          IPHI = IPHI_LIST(L,IHD)
          ID = J
          IF(IHD.EQ.2) ID = ID + NTWRS(1) !em towers before hadronic
          PTCATE(IETA,IPHI,IHD) = ID
        ENDDO
      ENDDO
C
C ****  Book the CATE bank
C
      NCHT=NTWRS(1)+NTWRS(2)
      CALL BKCATE(NCHT,LCATE)
C
C         fill CATE bank
C
      IQ(LCATE+3)=NCHT + CATENM*NTWRS(1)  ! number of channels + CATENM*NEM
C
C
C...now loop over towers with hits
      LCAEH=GZCAEH()
      NR =IQ(LCAEH+2)
      NCH=IQ(LCAEH+3)
      PT_CAEH_0 = LCAEH - NR
      NRT=IQ(LCATE+2)
      PT_CATE_0 = LCATE - NRT                !0th tower
      DO IPHI = 1,NPHIL
        DO IETA = -NETAL,NETAL
          IF (PTCATE(IETA,IPHI,2).NE.0) THEN
C
C...something in the tower: pick up its contents
            PT_CATE(2) = PT_CATE_0 + PTCATE(IETA,IPHI,2)*NRT
            PT_CATE(1) = PT_CATE_0 + PTCATE(IETA,IPHI,1)*NRT
            EM_HITS = PTCATE(IETA,IPHI,1).NE.0
            IF (EM_HITS) THEN
              MINLYR = 1
            ELSE
              MINLYR = MINL(IABS(IETA))  !min hadronic tower
            ENDIF
            DO ILYR = MINLYR,MAXL(IABS(IETA))
              OFFSET = PTCAEP(IETA,IPHI,ILYR)
              IF (OFFSET.GT.0) THEN
C ****  IHD = 1 MEANS EM TOWER; 2 is TOTAL TOWER (will add in EM later)
                IHD = 2
                IF(ILYR.LE.MXLYEM) IHD = 1
                IQ(PT_CATE(IHD)+17)=IOR(IQ(PT_CATE(IHD)+17),BIT(ILYR))
                IQ(PT_CATE(IHD)+11) = IQ(PT_CATE(IHD)+11) + 1 ! count it
                PT_CAEH = PT_CAEH_0 + OFFSET*NR
                DO J = 4 ,  10
                  Q(PT_CATE(IHD)+J)=Q(PT_CATE(IHD)+J)+Q(PT_CAEH+J)
                ENDDO
              ENDIF
            ENDDO
C
            IHD1 = 2  !by default, only Had tower
            IF(EM_HITS) THEN
              IHD1 = 1
C
C...had EM info; copy to TOT tower
              IQ(PT_CATE(2)+17) =
     &              IOR(IQ(PT_CATE(2)+17),IQ(PT_CATE(1)+17))
              IQ(PT_CATE(2)+11)=IQ(PT_CATE(2)+11) + IQ(PT_CATE(1)+11)
              DO J = 4 ,  10
                Q(PT_CATE(2)+J)=Q(PT_CATE(2)+J)+Q(PT_CATE(1)+J)
              ENDDO
C
C ****  FH LAYER 1 IS INCLUDED IN THE BIT LIST FOR IHD=1 BUT NOT ADDED TO SUMS
C              add first fine hadronic to bit list in em tower
              IF(IAND(IQ(PT_CATE(2)+17),BIT(MNLYFH)).NE.0) THEN
                IQ(PT_CATE(1)+17)=IOR(IQ(PT_CATE(1)+17),BIT(MNLYFH))
                IQ(PT_CATE(1)+11)=IQ(PT_CATE(1)+11) + 1 ! layer
              ENDIF
            ENDIF
C
C ****  update number of towers and finish storing tower info
C
            DO IHD = IHD1 ,  2
              CATE = PT_CATE(IHD)
C                 calculate ET correctly
              IF(Q(CATE+7).GT.0.0)THEN
                Q(CATE+8) = SQRT(Q(CATE+4)**2 + Q(CATE+5)**2)
              ELSE
                Q(CATE+8) =-SQRT(Q(CATE+4)**2 + Q(CATE+5)**2)
              ENDIF
C
C protect against cases where Et > E due to round off errors
C
              IF(ABS(Q(CATE+8)).GT.ABS(Q(CATE+7))) THEN
                Q(CATE+7)=Q(CATE+8)
              ENDIF
C                 fill rest
              IQ(CATE+12)=IETA
              IQ(CATE+13)=IPHI
              IQ(CATE+14) = IHD
            ENDDO
          ENDIF
        ENDDO
      ENDDO

      PTTFLG=.FALSE.           ! set flag indicating PTCATE is not 0
C
      ENTRY CATEFL_FORCE
      FORCE = .TRUE.
C
  999 RETURN
      END
