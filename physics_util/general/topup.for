      PROGRAM TOPUP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates top upperlimit cross sections.
C-                         Uses TOPUP.DAT file as input. Will combine
C-                         limits if ICOMB > 0. To combine limits from
C-                         various decay channels the program
C-                         works with a joint probobility distribution with
C-                         correct correlations built in. If METH is given
C-                         negative, then errors are not propagated and ignored,
C-                         otherwise errors are considered in derivation of
C-                         limits. Expected background is also considered in
C-                         evaluation of limits. Results are printed out and
C-                         usefull histograms are produced in TOPUP.HST4 file
C-                         if IHST flag is set to 1.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   02-MAR-1993   SHAHRIAR ABACHI
C-   Updated   18-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL H(1800000)
      COMMON /PAWC/H
      REAL MU,SIG,MUP,XGAUSD,G,XX0,DNMT,S2
      INTEGER IPOISD,IP,ICYCLE,NT,I,J,M,N,K,KK,IBSZ,IJK
      INTEGER NBNZ,N0,NE,NUMR,IHST,METH,ICOMB,IFLG,I0
      INTEGER MAXNC,MAXNC2,MZMAX,NC,NC1,NMAS1
      PARAMETER (MAXNC=8,MZMAX=5,MAXNC2=40)
      LOGICAL FIRST
      REAL XNOBS1(MAXNC2),XM1(MAXNC2),MUS2(MAXNC2)
      REAL BF1(MAXNC2),DEU1(MAXNC2),DEC1(MAXNC2),L1(MAXNC2),DL1(MAXNC2)
      REAL MUS1(MAXNC2),RDUM1(MAXNC2),MUBU1(MAXNC2),MUBC1(MAXNC2)
      REAL DMUBU1(MAXNC2),DMUBC1(MAXNC2),EC1(MAXNC2),EU1(MAXNC2)
      INTEGER NOBS1(MAXNC2)
      CHARACTER*20 CMNT1(MAXNC2)
      REAL XNOBS(MAXNC,MZMAX),STEP
      REAL BF(MAXNC),DEU(MAXNC,MZMAX),DEC(MAXNC,MZMAX)
      REAL L(MAXNC,MZMAX),DL(MAXNC,MZMAX)
      REAL MUS(MAXNC,MZMAX),RDUM(MAXNC,MZMAX),MUBU(MAXNC,MZMAX)
      REAL MUBC(MAXNC,MZMAX)
      REAL DMUBU(MAXNC,MZMAX),DMUBC(MAXNC,MZMAX),EC(MAXNC,MZMAX)
      REAL EU(MAXNC,MZMAX),XSC,LL,XNS
      INTEGER NOBS(MAXNC,MZMAX),IJ
      CHARACTER*20 CMNT(MAXNC,MZMAX)
      LOGICAL OK
      REAL XS,XS2,DLB,S,LB,XS0,S0
      REAL DU0,DEND,U1
      INTEGER NN,NMAS
      INTEGER IU,NT1,NT2,NT3
      REAL XX,DU,CL,DD
      REAL NEG,POS,RINT,RINT0,RX,C,BSZ
      DATA FIRST /.TRUE./
      DATA NT1 /15000/
C
      XNS = 0.
      LL = 0.
C
      CALL D0OPEN(11,'TOPUP_DATA','FI',OK)
      IF(.NOT.OK)CALL D0_ABORT('D0OPEN failed in TOPUP')
      READ(11,1) STEP,METH,IHST,ICOMB,CL,NMAS,NC
      REWIND(UNIT=11)
C&IF LINUX
C&      READ(11,*) STEP,METH,IHST,ICOMB,CL,NMAS,NC,(BF(I),I=1,NC)
C&ELSE
      READ(11,2) STEP,METH,IHST,ICOMB,CL,NMAS,NC,(BF(I),I=1,NC)
C&ENDIF
C
      IF(IHST .GT. 0) THEN
C      CALL MZEBRA(0)
C      CALL INPAWC
        CALL HLIMIT(1800000)
        CALL HCDIR('//PAWC',' ')
        CALL HMDIR('TOPUP','S')
        CALL HCDIR('//PAWC/TOPUP',' ')
        CALL D0RZOPEN(1,'TOPUP.HST4','OU',4096,OK)
        IF(.NOT.OK)CALL D0_ABORT('D0RZOPEN failed in TOPUP')
      ENDIF
C
      DO J=1,NMAS
        DO I=1,NC
          RDUM(I,J) = 0.
          CMNT(I,J) = ' '
          IF(I .EQ. 1) THEN
            READ(11,3) EU(I,J),DEU(I,J),EC(I,J),DEC(I,J),L(I,J),
     &        DL(I,J),XNOBS(I,J),MUS(I,J),MUBU(I,J),DMUBU(I,J),
     &        MUBC(I,J),DMUBC(I,J),CMNT(I,J)
          ELSE
            READ(11,4) EU(I,J),DEU(I,J),EC(I,J),DEC(I,J),L(I,J),
     &        DL(I,J),XNOBS(I,J),MUS(I,J),MUBU(I,J),DMUBU(I,J),
     &        MUBC(I,J),DMUBC(I,J),CMNT(I,J)
          ENDIF
        ENDDO
      ENDDO
C
      IF (ICOMB .GT. 0) THEN
        NMAS1 = NMAS
        NC1 = NC
      ELSE
        NMAS1 = NMAS * NC
        NC1 = 1
      ENDIF
C
C - Loop over masses
C
      DO J=1,NMAS1
C
        DO I=1,NC1
          IF(ICOMB .GT. 0) THEN
            N = I
            M = J
          ELSE
            N = MOD(J,NC)
            IF(N .EQ. 0) N = NC
            M = (J - 1) / NC + 1
          ENDIF
          BF1(I) = BF(N)
          EU1(I) = EU(N,M)
          DEU1(I) = DEU(N,M)
          EC1(I) = EC(N,M)
          DEC1(I) = DEC(N,M)
          L1(I) = L(N,M)
          DL1(I) = DL(N,M)
          XNOBS1(I) = XNOBS(N,M)
          NOBS1(I) = XNOBS(N,M)
          MUS1(I) = MUS(N,M)
          MUBU1(I) = MUBU(N,M)
          DMUBU1(I) = DMUBU(N,M)
          MUBC1(I) = MUBC(N,M)
          DMUBC1(I) = DMUBC(N,M)
          RDUM1(I) = RDUM(N,M)
          CMNT1(I) = CMNT(N,M)
        ENDDO
C
C - Account for background and combined if asked, but exclude errors.
C
        IF(METH .LE. 0) THEN
C
          BSZ = 1.0
          CALL TOPUP_BKG_ERR(J,STEP,NC1,NT1,METH,IHST,ICOMB,BF1,
     &              EU1,RDUM1,EC1,RDUM1,L1,RDUM1,NOBS1,MUS1,MUBU1,
     &              RDUM1,MUBC1,RDUM1,CL,BSZ,MUS2,XM1)
          PRINT *, '||||||||||||||||||||||||||||||||||||||||||||||||||'
          PRINT *,'RESULTS WITH ERRORS EXCLUDED @ ',CL,' CL'
C
        ELSE
C
C - Account for background and combined if asked, include all errors.
C
          IF(METH .EQ. 2) THEN
            BSZ = 1.0
          ELSE
            BSZ = 0.5
          ENDIF
          CALL TOPUP_BKG_ERR(J,STEP,NC1,NT1,METH,IHST,ICOMB,BF1,
     &              EU1,DEU1,EC1,DEC1,L1,DL1,NOBS1,MUS1,MUBU1,
     &              DMUBU1,MUBC1,DMUBC1,CL,BSZ,MUS2,XM1)
          PRINT *, ' '
          PRINT *, '||||||||||||||||||||||||||||||||||||||||||||||||||'
          PRINT *,'RESULTS WITH ERRORS INCLUDED @ ',CL,' CL'
C
        ENDIF
C
C - print out final results
C
        DO I=1,NC1
          PRINT *,
     &      'UPPERLIMIT CROSS SECTION IS ',XM1(I),'  FOR',CMNT1(I)
        ENDDO
        PRINT *, 'METHOD =', METH
        PRINT *, 'CONFIDENCE LEVEL =',CL
        IF(ICOMB .GT. 0) THEN
          IF(IABS(METH) .EQ. 1) THEN
            PRINT *, '////////////////////////'
            PRINT *, 'COMBINED CROSS SECTION =', XM1(1)
          ENDIF
        ELSE
          PRINT *, 'COMBINED CROSS SECTION NOT REQUESTED'
        ENDIF
        PRINT *, '||||||||||||||||||||||||||||||||||||||||||||||||||'
        PRINT *, ' '
C
      ENDDO
C
      PRINT *, '******************** END ************************'
C
      CLOSE(UNIT=11)
      IF(IHST .GT. 0) THEN
        CALL HRFILE(1,'TOPUP.HST4','N')
        CALL HROUT(0,ICYCLE,' ')
        CALL HREND('TOPUP.HST4')
      ENDIF
C
    1 FORMAT(/F19.9,3I12,F19.9,2I12)
C&IF LINUX
C&ELSE
    2 FORMAT(/F19.9,3I12,F19.9,2I12,<NC>F19.9,/)
C&ENDIF
    3 FORMAT(/12F19.9,A20)
    4 FORMAT(12F19.9,A20)
C
C----------------------------------------------------------------------
      END
