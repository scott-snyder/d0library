      SUBROUTINE L0_PROJ_FTRAKS(CHANS_HIT,NLC,LC_HIT_POSITIONS,PADS_HIT,
     &  NSC,SC_HIT_POSITIONS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Project the FDC reconstructed tracks into
C-                         the Level 0 scintillator pads.
C-
C-   Inputs  : none
C-   Outputs : CHANS_HIT(chan no)
C-             NLC(counter no, end)
C-             LC_HIT_POSITIONS(hit no, counter no, end, data word)
C-             PADS_HIT(x bin, y bin, end)
C-             NSC(end)
C-             SC_HIT_POSITIONS(hit no, end, data word)
C-   Controls: none
C-
C-   Created  25-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ICONT(10),NTRACK,ITRACK,IB0,IB1,IB2,IB3
      INTEGER ICONT2(10)
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER LADDER(0:2)
      INTEGER IADDS(6),IHITS(6)
      INTEGER IQTRAK(26)
      INTEGER CHANS_HIT(72)
      INTEGER PADS_HIT(-4:4,-4:4,2)
      INTEGER X_BIN,Y_BIN
      INTEGER NLC(8,2),NSC(2)
      INTEGER PAD_TO_CHAN(-4:4,-4:4)
      INTEGER OVLP_TO_CHAN(-4:4,-4:4)
      INTEGER OFFSET,ILC,END
      INTEGER IX,IY
      INTEGER ERR
C
      REAL QTRAK(26),CONT(26),QHSEC(3,34),QHIT(18)
      REAL CONT2(10)
      REAL CHIDF
      REAL L0Z(2),X_L0,Y_L0
      REAL Z0,X0,Y0,DX,DY
      REAL LC_HIT_POSITIONS(10,8,2,3)
      REAL SC_HIT_POSITIONS(100,2,5)
C
      EQUIVALENCE(QTRAK,IQTRAK)
      EQUIVALENCE(ICONT2,CONT2)
C
      LOGICAL FIRST
      LOGICAL EZERROR
      LOGICAL PRODUC, PRODFL
      EXTERNAL PRODUC
C
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA PAD_TO_CHAN/ 9*32,9*24,
     &                 31,23, 0,18,12,19, 0,21,29,
     &                 31,23,17, 7, 4, 8,20,21,29,
     &                 31,23,11, 3, 0, 1, 9,21,29,
     &                 31,23,16, 6, 2, 5,13,21,29,
     &                 31,23, 0,15,10,14, 0,21,29,
     &                 9*22,9*30/
      DATA OVLP_TO_CHAN/ 31,23,5*0,21,29,
     &                  31,23,5*0,21,29,
     &                  45*0,
     &                  31,23,5*0,21,29,
     &                  31,23,5*0,21,29/
C---------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','L0_PROJ_FTRAKS',
     &                   'LEVEL0_RCP not found.','W')
        ELSE
          CALL EZGET('L0Z',L0Z,ERR)
          CALL EZRSET
        ENDIF
        FIRST=.FALSE.
      ENDIF
C
      CALL VZERO_i(NLC(1,1),16)
      CALL VZERO(LC_HIT_POSITIONS(1,1,1,1),480)
      CALL VZERO_i(NSC(1),2)
      CALL VZERO(SC_HIT_POSITIONS(1,1,1),1000)
      CALL VZERO_i(CHANS_HIT,72)
      CALL VZERO_i(PADS_HIT,162)
C
      CALL GTFTRH(ICONT2)
      NTRACK=ICONT2(2)
      IF (NTRACK.LE.0) GO TO 999
C
      DO 100 ITRACK=1,NTRACK
        CALL GTFDCT(ITRACK,CONT,QHSEC,LADDER)
        CALL UCOPY(CONT,QTRAK,26)
        IF (IQTRAK(2).EQ.0) GO TO 100
        CHIDF=QTRAK(19)/FLOAT(IQTRAK(2)-4)
        HALF=IBITS(IQTRAK(1),0,1)
        OFFSET=HALF*36
        END=HALF+1
C
        Z0=CONT2(3+IBITS(IQTRAK(1),0,1))
        X0 = QTRAK(4)
        Y0 = QTRAK(5)
        DX = QTRAK(7)
        DY = QTRAK(8)
C
        X_L0 = X0 + (L0Z(HALF+1)-Z0)*DX
        Y_L0 = Y0 + (L0Z(HALF+1)-Z0)*DY
        X_BIN = INT((ABS(X_L0)+3.4925)/6.985)
        Y_BIN = INT((ABS(Y_L0)+3.4925)/6.985)
        IF ( X_L0.LT. 0.0 ) X_BIN = X_BIN * (-1.)
        IF ( Y_L0.LT. 0.0 ) Y_BIN = Y_BIN * (-1.)
C
        IF ( ABS(X_BIN).GT.4 ) GOTO 100
        IF ( ABS(Y_BIN).GT.4 ) GOTO 100
        PADS_HIT(X_BIN,Y_BIN,HALF+1) = PADS_HIT(X_BIN,Y_BIN,HALF+1) + 1
        IF ( ABS(X_BIN).LE.2 .AND. ABS(Y_BIN).LE.2 ) THEN
          NSC(END)=NSC(END)+1
          SC_HIT_POSITIONS(NSC(END),END,1)=X_L0
          SC_HIT_POSITIONS(NSC(END),END,2)=Y_L0
          SC_HIT_POSITIONS(NSC(END),END,3)=
     &            FLOAT(PAD_TO_CHAN(X_BIN,Y_BIN)+OFFSET)
          SC_HIT_POSITIONS(NSC(END),END,4)=FLOAT(X_BIN)
          SC_HIT_POSITIONS(NSC(END),END,5)=FLOAT(Y_BIN)
          GOTO 100
        ENDIF
C
        ILC=0
        IF ( X_BIN.EQ.3 ) ILC=1
        IF ( X_BIN.EQ.-3 ) ILC=3
        IF ( X_BIN.EQ.4 ) ILC=5
        IF ( X_BIN.EQ.-4 ) ILC=7
        IF ( ILC.GT.0 ) THEN
          IF ( NLC(ILC,HALF+1).GE.10 ) GOTO 100
          NLC(ILC,HALF+1) = NLC(ILC,HALF+1)+1
          LC_HIT_POSITIONS(NLC(ILC,HALF+1),ILC,HALF+1,1) = X_L0
          LC_HIT_POSITIONS(NLC(ILC,HALF+1),ILC,HALF+1,2) = Y_L0
          LC_HIT_POSITIONS(NLC(ILC,HALF+1),ILC,HALF+1,3) = Y_BIN
        ENDIF
        ILC=0
        IF ( Y_BIN.EQ.3 ) ILC=2
        IF ( Y_BIN.EQ.-3 ) ILC=4
        IF ( Y_BIN.EQ.4 ) ILC=6
        IF ( Y_BIN.EQ.-4 ) ILC=8
        IF ( ILC.GT.0 ) THEN
          IF ( NLC(ILC,HALF+1).GE.10 ) GOTO 100
          NLC(ILC,HALF+1) = NLC(ILC,HALF+1)+1
          LC_HIT_POSITIONS(NLC(ILC,HALF+1),ILC,HALF+1,1) = X_L0
          LC_HIT_POSITIONS(NLC(ILC,HALF+1),ILC,HALF+1,2) = Y_L0
          LC_HIT_POSITIONS(NLC(ILC,HALF+1),ILC,HALF+1,3) = X_BIN
        ENDIF
C
  100 CONTINUE
C
      DO HALF=0,1
        OFFSET=HALF*36
        DO IX=-4,4
          DO IY=-4,4
            IF ( PADS_HIT(IX,IY,HALF+1).GT.0 ) THEN
              IF ( PAD_TO_CHAN(IX,IY).GT.0 ) THEN
                CHANS_HIT( PAD_TO_CHAN(IX,IY)+OFFSET ) =
     &            CHANS_HIT( PAD_TO_CHAN(IX,IY)+OFFSET )
     &            + PADS_HIT(IX,IY,HALF+1)
              ENDIF
              IF ( OVLP_TO_CHAN(IX,IY).GT.0 ) THEN
                CHANS_HIT( OVLP_TO_CHAN(IX,IY)+OFFSET ) =
     &            CHANS_HIT( OVLP_TO_CHAN(IX,IY)+OFFSET )
     &            + PADS_HIT(IX,IY,HALF+1)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
      END
