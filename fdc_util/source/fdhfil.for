      SUBROUTINE FDHFIL(H,U,QU,S,W,DRIFTT,IHITSC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Obsolete histogram filler
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-MAY-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INTEGER H, U, QU, S, W, ID, IER, LR, I
      INTEGER LRFPDA(0:35,0:1),LRFPSC(0:35,0:1)
      INTEGER LRFTDA(0:5,0:3,0:1,0:1),LRFTSC(0:5,0:3,0:1,0:1)
      INTEGER LGFPDA(0:35,0:1),LGFPSC(0:35,0:1)
      INTEGER LGFTDA(0:5,0:3,0:1,0:1),LGFTSC(0:5,0:3,0:1,0:1)
      INTEGER LKFPDA,LKFPSC,LKFTDA,LKFTSC, LJFPDA, LJFTDA
      INTEGER IDS, NCH, NWD, NHIT, NHITW, IHITDA, IHIT, ICALL
      INTEGER LUNDBG, NCHR, NWDR, NHITWR, PTGHIT
      INTEGER UN,SEC,NHITS,IHITSC,SUMHIT,IWIRE(*)
      INTEGER GZFPDA,GZFTDA,USUNIT
C
      REAL    RESOL(16),ZTOT
      REAL    THETA,PHI,SLOPEM,INTERB,VARIANCE,CHIPROB,QHIT(18),DRIFTT
C
      SAVE ICALL
      DATA ICALL / 0 /
C----------------------------------------------------------------------
C
      IF( ICALL .EQ. 0 ) THEN
        ICALL = 1
        LUNDBG=USUNIT()
C        CALL EZPICK('FTRAKS_RCP')
C        CALL EZRSET
      ENDIF
C      CALL DZSURV(' FDHFIL',IXCOM,LHEAD)
C
C **** Get pointers to the real hit banks
C
      CALL PATHST('RECO')
C
C **** Accumulate data, put into histograms
C
      IF(U.GT.1) GOTO 200
      LJFTDA= GZFTDA(H,U,QU,S)
      IHITDA= 4 + 2*10 + 9*((IHITSC-20)/12) - 1
      ID     = 0
      IF( W.EQ.0) ID = 100000
      IF( W.EQ.7) ID = 200000
C
      CALL HFILL(11000000+ID,FLOAT(IQ(LJFTDA+4+W)),0.,1.)
      CALL HFILL(19000000+ID,DRIFTT,0.,1.)
      CALL HFILL(12000000+ID,Q(LJFTDA+IHITDA+2),0.,1.)
      CALL HFILL(13000000+ID,Q(LJFTDA+IHITDA+3),0.,1.)
      CALL HFILL(14000000+ID,Q(LJFTDA+IHITDA+4),0.,1.)
      CALL HFILL(15000000+ID,Q(LJFTDA+IHITDA+5),0.,1.)
      CALL HFILL(16000000+ID,Q(LJFTDA+IHITDA+2),Q(LJFTDA+IHITDA+3),1.)
      CALL HFILL(17000000+ID,Q(LJFTDA+IHITDA+2),Q(LJFTDA+IHITDA+4),1.)
      CALL HFILL(18000000+ID,Q(LJFTDA+IHITDA+2),Q(LJFTDA+IHITDA+5),1.)
      GOTO 999
  200 CONTINUE
C
C **** Phi Chamber results
C
      LJFPDA= GZFPDA(H,S)
      IHITDA= 4 + 2*16 + 9*((IHITSC-36)/12) - 1
      ID = 300000
      IF( W.EQ.0) ID  = 400000
      IF( W.EQ.15) ID = 500000
C
      CALL HFILL(11000000+ID,FLOAT(IQ(LJFPDA+4+W)),0.,1.)
      CALL HFILL(19000000+ID,DRIFTT,0.,1.)
      CALL HFILL(12000000+ID,Q(LJFPDA+IHITDA+2),0.,1.)
      CALL HFILL(13000000+ID,Q(LJFPDA+IHITDA+3),0.,1.)
      CALL HFILL(14000000+ID,Q(LJFPDA+IHITDA+4),0.,1.)
      CALL HFILL(15000000+ID,Q(LJFPDA+IHITDA+5),0.,1.)
      CALL HFILL(16000000+ID,Q(LJFPDA+IHITDA+2),Q(LJFPDA+IHITDA+3),1.)
      CALL HFILL(17000000+ID,Q(LJFPDA+IHITDA+2),Q(LJFPDA+IHITDA+4),1.)
      CALL HFILL(18000000+ID,Q(LJFPDA+IHITDA+2),Q(LJFPDA+IHITDA+5),1.)
      GOTO 999
C----------------------------------------------------------------------
      ENTRY FDHFL2(UN,SEC,NHITS,THETA,PHI)
C
      CALL HFILL(50000000,FLOAT(UN*6+SEC),0.,1.)
      CALL HFILL(50000001,FLOAT(NHITS),0.,1.)
      IF(UN.EQ. 0) CALL HFILL(50000002,THETA,0.,1.)
      IF(UN.EQ. 0) CALL HFILL(50000003,PHI,0.,1.)
      IF(UN.EQ. 1) CALL HFILL(50000004,THETA,0.,1.)
      IF(UN.EQ. 1) CALL HFILL(50000005,PHI,0.,1.)
      IF(UN.EQ. 2) CALL HFILL(50000006,THETA,0.,1.)
      IF(UN.EQ. 2) CALL HFILL(50000007,PHI,0.,1.)
C
C      CALL INTMSG(' End of FDHFL2')
      GOTO 999
C----------------------------------------------------------------------
      ENTRY FDHFL3(UN,SEC,SLOPEM,INTERB,VARIANCE,CHIPROB,
     &                        NHIT,RESOL,IWIRE)
C
      CALL HFILL(60000000,FLOAT(UN*6+SEC),0.,1.)
      CALL HFILL(60000001+UN*4,SLOPEM,0.,1.)
C      CALL HFILL(60000002+UN*4,INTERB,0.,1.)
      CALL HFILL(60000003+UN*4,VARIANCE,0.,1.)
      CALL HFILL(60000004+UN*4,CHIPROB,0.,1.)
C
      DO 300 I = 1,NHIT
        IF( IWIRE(I) .EQ. 0 ) THEN
          CALL HFILL(70000002+UN*3,RESOL(I),0.,1.)
        ELSEIF( IWIRE(I) .EQ. 7) THEN
          CALL HFILL(70000003+UN*3,RESOL(I),0.,1.)
        ELSE
          CALL HFILL(70000001+UN*3,RESOL(I),0.,1.)
        ENDIF
  300 CONTINUE
C      CALL INTMSG(' End of FDHFL3')
      GOTO 999
C----------------------------------------------------------------------
      ENTRY FDHFL4(UN,SEC,NHIT,SUMHIT)
C
      CALL HFILL(70000000,FLOAT(SUMHIT),FLOAT(NHIT),1.)
C      CALL INTMSG(' End of FDHFL4')
      GOTO 999
C----------------------------------------------------------------------
      ENTRY FDHFL5(UN,SEC,NHIT)
C
      CALL HFILL(50000008+UN,FLOAT(NHIT),0.,1.)
C      CALL INTMSG(' End of FDHFL5')
      GOTO 999
C----------------------------------------------------------------------
      ENTRY FDHFL6(UN,SEC,ZTOT)
C
      IF(UN .EQ. 0) CALL HFILL(80000000,ZTOT,FLOAT(SEC),1.)
      IF(UN .EQ. 1) CALL HFILL(80000001,ZTOT,FLOAT(SEC),1.)
      GOTO 999
C----------------------------------------------------------------------
  999 CONTINUE
C      CALL INTMSG(' End of FDHFIL')
      RETURN
      END
