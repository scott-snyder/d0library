      SUBROUTINE MU_HITS_UNPACK(SKIP_LEVEL)
C =============================================================
C   PURPOSE:  This is a short version of DH's MUANLZ routine in
C             order to simply unpack muon hit data for the muon
C             trigger calculation.
C             It fills the muon data banks MUHT, MUOF SAHH
C                                     - - Kamel Bazizi  5-26-92
C     INPUTS :
C             SKIP_LEVEL = 0  No hit unpacking
C                        = 1  Unpacking for WAMUS only
C                        = 2  Unpacking for SAMUS only
C                        = 3  Unpacking for WAMUS and SAMUS
C     =========================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LMUHT,LSAHH,LMUOH,LMUOF,NMUONH,NMAXH,NMAXF,L,LMUD1
      INTEGER GZMUHT,GZMUOF,GZMUD1,GZMUOH,GZSAHH,NMUSRT,NMUMOD,NTIME
      INTEGER IT,NON,ION(10),NOFF,IOFF(10),LMUOT,GZMUOT,GZMTRH
      INTEGER LMTRH,LT,IER,IA,NTRACKS,II1,II2
      INTEGER IERR,GZRECO,I,IQUAD,NQUAD,JQUAD(12),NMOD,JMOD(170)
      INTEGER NTRK1,NTRK2,NTRK3,SKIP_LEVEL

      REAL CUT1,CUT2,CUT3
      LOGICAL SAHITS,OK,EZERROR
      IERR=0
C-
C- no unpacking if SKIP_LEVEL=0
      IF(SKIP_LEVEL.EQ.0) GOTO 999
C-
CCCC   CREATE MUON HIT BANK HEADER
      IF(GZMUHT(0).EQ.0) CALL BKMUHT(0,0,LMUHT)
      LMUHT=GZMUHT(0)
      IF(IQ(LMUHT+1).EQ.-1) THEN    ! BAD EVENT
        IERR=-4
        GO TO 999
      ENDIF
CCCC  CREATE FLAGGING BANK; SKIP HIT UNPACKING IF MUOF EXISTS
      IF(GZSAHH().EQ.0) CALL BKSAHH(0,0,LSAHH)
      LMUOF=GZMUOF(0)
C- skip WAMUS if SKIP_LEVEL=2
C-
      IF(LMUOF.EQ.0) THEN
        LMUD1=GZMUD1(0)
        NMUONH=IQ(LMUD1-1)/9     ! ASSUMES THAT ALL HEADER WORDS ARE
CC                        REALLY DATA SO UPPER BOUND
        NMAXF=10*NMUONH     ! MAX SIZE OF FLAGGING BANK
        CALL BKMUOF(0,NMAXF,LMUOF)
        LMUHT=GZMUHT(0)
        LMUD1=GZMUD1(0)
        LSAHH=GZSAHH()
        CALL MUSRT1(LMUD1,LMUHT,LMUOF,LSAHH,IERR) ! INITIAL SORT THROUGH RAW DATA
        IF (IERR.LT.0) THEN
          IQ(LMUHT+1)=-1      ! NO. RAW HITS AND MODULES SET TO -1
          IQ(LMUHT+3)=-1
          NMAXF=IQ(LMUOF-1)
          IF(NMAXF.GT.0) CALL MZPUSH(IXCOM,LMUOF,0,-NMAXF,' ')
          GO TO 999
        ENDIF
C--
        NMUMOD=IQ(LMUHT+3)
        L=IQ(LMUOF-1)-10*NMUMOD
        IF(L.GT.0) CALL MZPUSH(IXCOM,LMUOF,0,-L,' ')
      ENDIF
C-
  998 CONTINUE
      IF (SKIP_LEVEL.GE.2) THEN
        LSAHH=GZSAHH()                 ! here
        IF (LSAHH.EQ.0) THEN 
          CALL BKSAHH(0,0,LSAHH)
          CALL BKMUHT(0,0,LMUHT)
          LMUD1=GZMUD1()
          NMUONH=IQ(LMUD1-1)/9
          NMAXF=10*NMUONH
          CALL BKMUOF(0,NMAXF,LMUOF)
          LMUD1=GZMUD1()
          LMUHT=GZMUHT()
          LSAHH=GZSAHH()
          CALL MUSRT1(LMUD1,LMUHT,LMUOF,LSAHH,IERR)  
        END IF
        CALL SAMHFL(OK)                ! to here
      ELSE
        OK=.FALSE.
      ENDIF
C--
  999 RETURN
      END
