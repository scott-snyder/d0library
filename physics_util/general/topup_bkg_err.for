      SUBROUTINE TOPUP_BKG_ERR(NMAS,STEP,NC,NT,METH,IHST,ICOMB,
     &                     BF,EFU,DEFU,EFC,DEFC,LUM,DLUM,N0,MUS,
     &                     MUBU,DMUBU,MUBC,DMUBC,CL,BSZ,MUS1,UXS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called by TOPUP program and calculates effect
C-                         of background in limit calculations and also
C-                         convolutes errors in limits. The limits from
C-                         various decay channels are combined with
C-                         correlations properly taken into account.
C-
C-   Inputs  : NMAS    Index refering to assumed mass (or any other parameter).
C-             STEP    Step size in mc.
C-             NC      No. of channels to consider (emu,ee,mumu,...).
C-             NT      No. of monte carlo events.
C-             METH    Method chosen (1 available)
C-                     if < 0, then do not propagate errors.
C-             ICOMB   1=Combine results from various channels,0=do not.
C-             BF(*)   branching fractions.
C-             EFU(*)  Efficiency (uncorrelated).
C-             DEFU(*) Error in uncorrelated efficiency.
C-             EFC(*)  Efficiency (correlated).
C-             DEFc(*) Error in correlated efficiency.
C-             LUM(*)  Luminosity.
C-             DLUM(*) Error in luminosity.
C-             N0(*)   Number of events observed.
C-             MUS(*)  Upperlimit numbers for total events observered.
C-             MUBU(*) Expected # of background events (uncorrelated).
C-             DMUBU(*)Error on above.
C-             MUBC(*) Expected # of background events (correlated).
C-             DMUBC(*)Error on above.
C-             CL      Confidence level of interest.
C-             BSZ     Bin size
C-
C-   Outputs : MUS1(*) Upperlimit numbers for signals
C-             UXS(*)  Upper-limit cross secctions
C-   Controls:
C-
C-   Created   03-MAR-1993   SHAHRIAR ABACHI
C-   Updated   20-MAY-1993   SHAHRIAR ABACHI  Added capabilities upto 4 channels
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL H(1800000)
      COMMON /PAWC/H
      INTEGER IERR
      INTEGER NMAS,NC,NT,METH,IHST,ICOMB,N0(NC)
      REAL STEP,BF(NC),EFU(NC),EFC(NC),CL,BSZ,MUBU(NC),MUBC(NC),MUS(NC)
      REAL DMUBU(NC),DMUBC(NC),DEFU(NC),DEFC(NC),LUM(NC),DLUM(NC)
      REAL A,B,C,UXS(NC)
      INTEGER I,J,K,L,NN,NM,IPOISD,PM,NCOLUM,NN2
      PARAMETER (NCOLUM=4,NN=9,NN2=40,PM=30)
      REAL FS(NN2),XMINSF,XMAXSF,XGAUSD,FAC
      REAL TVN,TVB,RAT,RRAT,MUS0,MUB(NN2)
      REAL DB1(0:PM),DN1(0:PM)
      REAL DB2(0:PM,0:PM),DN2(0:PM,0:PM)
      REAL DB3(0:PM,0:PM,0:PM),DN3(0:PM,0:PM,0:PM)
      REAL DB4(0:PM,0:PM,0:PM,0:PM),DN4(0:PM,0:PM,0:PM,0:PM)
      REAL DS0,DS,DS2,DS3,MUB0(NN2),RH(NN2),XTUPLE(NCOLUM)
      CHARACTER*8 CHTAGS(NCOLUM)
      INTEGER IN(NN2),IB(NN2),JN,JB,PB(NN2),PN(NN2),IFLG
      REAL RPB(NN2),RPN(NN2),XSB(NN2),XSN(NN2),XS,CP,EPS
      REAL RPB2,RPN2,IN2,IB2,SAFE
      INTEGER NH(NN2),NPRIME,IMUS0,MIN_TVN
      REAL MUB1,MUB1C,DLTC,PCB,PCE,PCL,MUBT(NN2),LUMT,RDUM,DG
      REAL EF1C,LUMB(NN2),LB0(NN2),EFT,ACPT,ACPT2,ACPT3,MUS1(NN2)
      INTEGER NT1,IN1,IB1,IS1
      LOGICAL FIRST,FIRST1,FIRST2
      DATA CHTAGS /'SBCH1','SBCH2','BCH1','BCH2'/
      DATA FIRST/.TRUE./
      DATA NPRIME /1000/
      DATA EPS,SAFE /1.0E-4,.002/
C
      FIRST1 = .TRUE.
      FIRST2 = .TRUE.
      CP = 1.0 - CL - SAFE
      MUS0 = 1.0
      IF(STEP .GT. 0.0) THEN
        DS0 = STEP
      ELSE
        DS0 = 0.3 / NC**2
      ENDIF
      DS = DS0
      DS2 = DS / 5.0
      DS3 = DS / 10.0
      ACPT = 0.5
      ACPT2 = 0.2
      ACPT3 = 0.02
      IFLG = 0
      NT1 = NT
      MIN_TVN = 1000
C
      IF(IHST .GT. 0) THEN
        DO I=1,NC
          MUB(I) = MUBC(I) + MUBU(I)
          NH(I) = SQRT(FLOAT(N0(I)) + MUB(I) + 1.0) * 10
          RH(I) = FLOAT(NH(I))
          CALL HBOOK1(I+NMAS*1000,'NS + NB$',NH(1),-1.,RH(1)-1.,0.)
          CALL HBOOK1(I+50+NMAS*1000,'NB$',NH(1),-1.,RH(1)-1.,0.)
          CALL HBOOK1(I+100+NMAS*1000,'NS$',NH(1),-1.,RH(1)-1.,0.)
        ENDDO
C
        IF(NC .GT. 1) THEN
          CALL HBOOK2(200+NMAS*1000,'NS1 + NB1  VS  NS2 + NB2$',
     &                   NH(1),-1.,RH(1)-1.,NH(2),-1.,RH(2)-1.,0.)
          CALL HBOOK2(201+NMAS*1000,'NB1 VS NB2$',NH(1),-1.,
     &                     RH(1)-1.,NH(2),-1.,RH(2)-1.,0.)
          CALL HBOOKN(500+NMAS*1000,'TOP_LIMITS',NCOLUM,' ',
     &                     NPRIME,CHTAGS)
        ENDIF
      ENDIF
C
      PRINT *, ' '
      PRINT *, 'XX-----------------------------XX FROM TOPUP_BKG_ERR:'
      PRINT *, '--- Start with initial step size of',DS0
C
C - Poisson table for up to N0=10 for CL's of 90% and 95%
C
      DO I=1,NC
        IF(ABS(CL-0.95) .LE. EPS) THEN         !95% confidence level
          IF(N0(I) .EQ. 0) MUS(I) = 3.0
          IF(N0(I) .EQ. 1) MUS(I) = 4.74
          IF(N0(I) .EQ. 2) MUS(I) = 6.3
          IF(N0(I) .EQ. 3) MUS(I) = 7.75
          IF(N0(I) .EQ. 4) MUS(I) = 9.15
          IF(N0(I) .EQ. 5) MUS(I) = 10.51
          IF(N0(I) .EQ. 6) MUS(I) = 11.84
          IF(N0(I) .EQ. 7) MUS(I) = 13.15
          IF(N0(I) .EQ. 8) MUS(I) = 14.44
          IF(N0(I) .EQ. 9) MUS(I) = 15.71
          IF(N0(I) .EQ. 10) MUS(I) = 16.96
        ELSEIF(ABS(CL-0.90) .LE. EPS) THEN     !90% confidence level
          IF(N0(I) .EQ. 0) MUS(I) = 2.3
          IF(N0(I) .EQ. 1) MUS(I) = 3.89
          IF(N0(I) .EQ. 2) MUS(I) = 5.32
          IF(N0(I) .EQ. 3) MUS(I) = 6.68
          IF(N0(I) .EQ. 4) MUS(I) = 7.99
          IF(N0(I) .EQ. 5) MUS(I) = 9.27
          IF(N0(I) .EQ. 6) MUS(I) = 10.53
          IF(N0(I) .EQ. 7) MUS(I) = 11.77
          IF(N0(I) .EQ. 8) MUS(I) = 13.00
          IF(N0(I) .EQ. 9) MUS(I) = 14.21
          IF(N0(I) .EQ. 10) MUS(I) = 15.41
        ENDIF
      ENDDO
C
C - Do not combine and do not include errors;
C
      IF(ICOMB .EQ. 0 .AND. METH .LT. 0) THEN
        DO K=1,NC
          IF(MUB(K) .LE. 0) THEN
            MUS1(K) = MUS(K)
            GOTO 11
          ENDIF
          MUS1(K) = FLOAT(N0(K)) - MUB(K)
          IF(MUS1(K) .LE. 0.0) MUS1(K) = 0.01
   10     CONTINUE
          B = 0.0
          C = 0.0
          J = 1
          DO I=0,N0(K)
            J = J * I
            IF(J .EQ. 0) J = 1
            B = B + ((MUS1(K) + MUB(K))**I) / J
            C = C + ((MUB(K))**I) / J
          ENDDO
          B = EXP(-MUS1(K)) * B
          A = B / C
          IF(A .GT. CP) THEN
            IF(A - CP .LT. 0.04) THEN
              PRINT *, 'MUS0 = ', MUS1(K)
              PRINT *, 'TVN, TVB, RAT = ', B,C,A
            ENDIF
            MUS1(K) = MUS1(K) + 0.1
            GOTO 10
          ELSE
            PRINT *, 'MUS0 = ', MUS1(K)
            PRINT *, 'TVN, TVB, RAT = ', B,C,A
            PRINT *, '-----  '
          ENDIF
   11   ENDDO
C
        DO I=1,NC
          IF(IHST .GT. 0) THEN
            DO J=1,NT1*NC
C            IN1 = IPOISD(MUS1(I) + MUB(I))
C            IB1 = IPOISD(MUB(I))
C            IS1 = IPOISD(MUS1(I))
              CALL POISSN(MUS1(I) + MUB(I), IN1, IERR)
              CALL POISSN(MUB(I), IB1, IERR)
              CALL POISSN(MUS1(I), IS1, IERR)
              CALL HFILL(I+NMAS*1000, FLOAT(IN1), 0., 1.0)
              CALL HFILL(I+50+NMAS*1000, FLOAT(IB1), 0., 1.0)
              CALL HFILL(I+100+NMAS*1000, FLOAT(IS1), 0., 1.0)
            ENDDO
          ENDIF
          PRINT *, ' MUS(',I,'  ) =', MUS1(I)
          LB0(I) = LUM(I) * EFU(I) * EFC(I) * BF(I)
          UXS(I) = MUS1(I) / LB0(I)
        ENDDO
        PRINT *, 'XX-----------------------------XX  END TOPUP_BKG_ERR:'
        GOTO 999
C
C - Combine and include errors if asked;
C
      ELSE
C
        XMINSF = 9999999.
        XMAXSF = 0.
        DO I=1,NC
          LB0(I) = LUM(I) * EFU(I) * EFC(I) * BF(I)
          MUB0(I) = MUBU(I) + MUBC(I)
          IF(MUB0(I) .LE. 0.) MUB0(I) = 0.
          IF(LB0(I) .LT. XMINSF) THEN
            XMINSF = LB0(I)
          ENDIF
          IF(LB0(I) .GT. XMAXSF) THEN
            XMAXSF = LB0(I)
          ENDIF
   12   ENDDO
        FAC = XMAXSF / XMINSF
        MUS0 = 0.06 * (1.0 / CP) * MUS0 / FAC
C -                     start mc ;
  100   CONTINUE
        TVN = 0.
        TVB = 0.
        IF(NC .EQ. 1 .OR. ICOMB .EQ. 2) THEN
          NM = 0
          IF(ICOMB .LT. 2 .OR. NC .LT. 2) THEN
            NM = N0(1)
          ELSE
            DO I=1,NC
              NM = NM + N0(I)
            ENDDO
          ENDIF
          DO I=0,NM/BSZ
            DN1(I) = 0.
            DB1(I) = 0.
          ENDDO
        ELSEIF(NC .EQ. 2) THEN
          DO I=0,N0(1)/BSZ
            DO J=0,N0(2)/BSZ
              DN2(I,J) = 0.
              DB2(I,J) = 0.
            ENDDO
          ENDDO
        ELSEIF(NC .EQ. 3) THEN
          DO I=0,N0(1)/BSZ
            DO J=0,N0(2)/BSZ
              DO K=0,N0(3)/BSZ
                DN3(I,J,K) = 0.
                DB3(I,J,K) = 0.
              ENDDO
            ENDDO
          ENDDO
        ELSEIF(NC .EQ. 4) THEN
          DO I=0,N0(1)/BSZ
            DO J=0,N0(2)/BSZ
              DO K=0,N0(3)/BSZ
                DO L=0,N0(4)/BSZ
                  DN4(I,J,K,L) = 0.
                  DB4(I,J,K,L) = 0.
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDIF
        DO K=1,NT1
          RPN2 = 0.
          RPB2 = 0.
          DO I=1,NC
            LB0(I) = LUM(I) * EFU(I) * EFC(I) * BF(I)
            IF(I .EQ. 1) THEN               ! error U&C
              IF(DMUBC(I) .GT. 0.) THEN
C                DLTC = XGAUSD(0.,DMUBC(I))
                CALL NORMCO(DLTC,RDUM,0.,0.,DMUBC(I),0.,0.)
                PCB = DLTC / DMUBC(I)
              ELSE
                PCB = 0.
              ENDIF
              IF(DEFC(I) .GT. 0.) THEN
C                DLTC = XGAUSD(0.,DEFC(I))
                CALL NORMCO(DLTC,RDUM,0.,0.,DEFC(I),0.,0.)
                PCE = DLTC / DEFC(I)
              ELSE
                PCE = 0.
              ENDIF
              IF(DLUM(I) .GT. 0.) THEN
C                DLTC = XGAUSD(0.,DLUM(I))
                CALL NORMCO(DLTC,RDUM,0.,0.,DLUM(I),0.,0.)
                PCL = DLTC / DLUM(I)
              ELSE
                PCL = 0.
              ENDIF
            ENDIF
            MUB1C = PCB * DMUBC(I) + MUBC(I)
C            MUBT(I) = MUB1C(I) + XGAUSD(MUBU(I),DMUBU(I))
            CALL NORMCO(DG,RDUM,MUBU(I),0.,DMUBU(I),0.,0.)
            MUBT(I) = MUB1C + DG
            EF1C = PCE * DEFC(I) + EFC(I)
C            EFT = EF1C * XGAUSD(EFU(I),DEFU(I))
            CALL NORMCO(DG,RDUM,EFU(I),0.,DEFU(I),0.,0.)
            EFT = EF1C * DG
            LUMT = PCL * DLUM(I) + LUM(I)
            LUMB(I) = LUMT * EFT * BF(I)
            FS(I) = LB0(I) / XMINSF
C            PN(I) = IPOISD(MUS0 * FS(I) + MUBT(I))
C            PB(I) = IPOISD(MUBT(I))
            CALL POISSN(MUS0 * FS(I) + MUBT(I), PN(I), IERR)
            CALL POISSN(MUBT(I), PB(I), IERR)
            XSN(I) = PN(I) / LUMB(I)
            XSB(I) = PB(I) / LUMB(I)
            RPN(I) = XSN(I) * LB0(I)
            RPB(I) = XSB(I) * LB0(I)
            IF(IHST .GT. 0 .AND. IFLG .EQ. 1) THEN
              CALL HFILL(I+NMAS*1000, RPN(I), 0., 1.0)
              CALL HFILL(I+50+NMAS*1000, RPB(I), 0., 1.0)
C              IMUS0 = IPOISD(MUS0)
              CALL POISSN(MUS0, IMUS0, IERR)
              CALL HFILL(I+100+NMAS*1000, FLOAT(IMUS0), 0., 1.0)
            ENDIF
            IN(I) = (RPN(I) / BSZ)
            IB(I) = (RPB(I) / BSZ)
            IF(ICOMB .EQ. 2 .AND. NC .GT. 1) THEN
              RPN2 = RPN2 + PN(I) * LB0(I) / LUMB(I)
              RPB2 = RPB2 + PB(I) * LB0(I) / LUMB(I)
              IF(I .GE. NC) THEN
                IN2 = (RPN2 / BSZ)
                IB2 = (RPB2 / BSZ)
              ENDIF
            ENDIF
            IF(IFLG .EQ. 1 .AND. I .LE. 2) THEN
              XTUPLE(I) = RPN(I)
              XTUPLE(I+2) = RPB(I)
            ENDIF
          ENDDO
          IF(IHST .GT. 0 .AND. IFLG .EQ. 1 .AND. NC .GT. 1) THEN
            CALL HF2(200+NMAS*1000, RPN(1), RPN(2), 1.0)
            CALL HF2(201+NMAS*1000, RPB(1), RPB(2), 1.0)
            CALL HFN(500+NMAS*1000,XTUPLE)
          ENDIF
          IF(NC .EQ. 4 .AND. IN(1) .LE. PM .AND. IN(2) .LE. PM .AND.
     &        IN(3) .LE. PM .AND. IN(4) .LE. PM .AND.
     &        IN(1) .GE. 0 .AND. IN(2) .GE. 0 .AND.
     &        IN(3) .GE. 0 .AND. IN(4) .GE. 0 .AND.
     &        IB(1) .LE. PM .AND. IB(2) .LE. PM .AND.
     &        IB(3) .LE. PM .AND. IB(4) .LE. PM .AND.
     &        IB(1) .GE. 0 .AND. IB(2) .GE. 0 .AND.
     &        IB(3) .GE. 0 .AND. IB(4) .GE. 0) THEN
            IF(ICOMB .LT. 2) THEN
              DN4(IN(1),IN(2),IN(3),IN(4)) =
     &                       DN4(IN(1),IN(2),IN(3),IN(4)) + 1.0
              DB4(IB(1),IB(2),IB(3),IB(4)) =
     &                       DB4(IB(1),IB(2),IB(3),IB(4)) + 1.0
            ELSE
              DN1(IN2) = DN1(IN2) + 1.0
              DB1(IB2) = DB1(IB2) + 1.0
            ENDIF
          ELSEIF(NC .EQ. 3 .AND. IN(1) .LE. PM .AND. IN(2) .LE. PM
     &        .AND. IN(3) .LE. PM .AND. IN(1) .GE. 0 .AND.
     &        IN(2) .GE. 0 .AND. IN(3) .GE. 0 .AND.
     &        IB(1) .LE. PM .AND. IB(2) .LE. PM .AND.
     &        IB(3) .LE. PM .AND. IB(1) .GE. 0 .AND.
     &        IB(2) .GE. 0 .AND. IB(3) .GE. 0) THEN
            IF(ICOMB .LT. 2) THEN
              DN3(IN(1),IN(2),IN(3)) = DN3(IN(1),IN(2),IN(3)) + 1.0
              DB3(IB(1),IB(2),IB(3)) = DB3(IB(1),IB(2),IB(3)) + 1.0
            ELSE
              DN1(IN2) = DN1(IN2) + 1.0
              DB1(IB2) = DB1(IB2) + 1.0
            ENDIF
          ELSEIF(NC .EQ. 2 .AND. IN(1) .LE. PM .AND.IN(2) .LE. PM
     &           .AND. IN(1) .GE. 0 .AND. IN(2) .GE. 0 .AND.
     &           IB(1) .LE. PM .AND.IB(2) .LE. PM .AND.
     &           IB(1) .GE. 0 .AND. IB(2) .GE. 0) THEN
            IF(ICOMB .LT. 2) THEN
              DN2(IN(1),IN(2)) = DN2(IN(1),IN(2)) + 1.0
              DB2(IB(1),IB(2)) = DB2(IB(1),IB(2)) + 1.0
            ELSE
              DN1(IN2) = DN1(IN2) + 1.0
              DB1(IB2) = DB1(IB2) + 1.0
            ENDIF
          ELSEIF(NC .EQ. 1 .AND. IN(1) .LE. PM .AND.
     &           IN(1) .GE. 0 .AND. IB(1) .LE. PM .AND.
     &           IB(1) .GE. 0) THEN
            DN1(IN(1)) = DN1(IN(1)) + 1.0
            DB1(IB(1)) = DB1(IB(1)) + 1.0
          ENDIF
        ENDDO
C
C - Now find the nD volumes
C
        IF(NC .EQ. 1 .OR. ICOMB .EQ. 2) THEN
          DO I=0,NM/BSZ
            TVN = TVN + DN1(I)
            TVB = TVB + DB1(I)
          ENDDO
        ELSEIF(NC .EQ. 2) THEN
          DO I=0,N0(1)/BSZ
            DO J=0,N0(2)/BSZ
              TVN = TVN + DN2(I,J)
              TVB = TVB + DB2(I,J)
            ENDDO
          ENDDO
        ELSEIF(NC .EQ. 3) THEN
          DO I=0,N0(1)/BSZ
            DO J=0,N0(2)/BSZ
              DO K=0,N0(3)/BSZ
                TVN = TVN + DN3(I,J,K)
                TVB = TVB + DB3(I,J,K)
              ENDDO
            ENDDO
          ENDDO
        ELSEIF(NC .EQ. 4) THEN
          DO I=0,N0(1)/BSZ
            DO J=0,N0(2)/BSZ
              DO K=0,N0(3)/BSZ
                DO L=0,N0(4)/BSZ
                  TVN = TVN + DN4(I,J,K,L)
                  TVB = TVB + DB4(I,J,K,L)
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDIF
C
C - check ratios
C
        RAT = TVN / TVB
        RRAT = (RAT - CP) / CP
        NT1 = NT1 * MIN_TVN / (NC *TVN)
        PRINT *, 'MUS0 = ', MUS0
        PRINT *, 'TVN, TVB, RAT = ', TVN,TVB,RAT
C
        IF(IFLG .EQ. 0) THEN
          IF(RRAT .GT. ACPT) THEN
            DS = DS0
            MUS0 = MUS0 + DS
            GOTO 100
          ELSEIF(RRAT .GT. ACPT2 .AND. RRAT .LE. ACPT) THEN
            DS = DS2
            MUS0 = MUS0 + DS
            IF(FIRST1) THEN
              FIRST1 = .FALSE.
              PRINT *, '--- Next, small steps of', DS,' ;'
            ENDIF
            GOTO 100
          ELSEIF(RRAT .GT. ACPT3 .AND. RRAT .LE. ACPT2) THEN
            DS = DS3
            MUS0 = MUS0 + DS
            IF(FIRST2) THEN
              FIRST2 = .FALSE.
              PRINT *, '--- Next, smaller steps of', DS,' ;'
            ENDIF
            GOTO 100
          ELSE
            IFLG = 1
            PRINT *, '------------  Converged.'
            GOTO 100
          ENDIF
        ELSE
          GOTO 110
        ENDIF
C
      ENDIF
C
  110 CONTINUE
      DO I=1,NC
        MUS1(I) = MUS0 * FS(I)
        PRINT *, '  MUS1(',I,'  ) =', MUS1(I)
        UXS(I) = MUS1(I) / LB0(I)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
