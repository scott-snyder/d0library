      SUBROUTINE L2_COMP_JETS(LUN,LJAUX,LJPAR,IB,OKOK,
     &  JAUX_ET,JAUX_ETA,JAUX_PHI,JAUX_EMF,JAUX_ETAS,JAUX_PHIS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        compare two versions of the output banks of l2jets
C
C     grabs info on L2JETS from JAUX and JPAR bank - SHOULD ONLY BE
C     CALLED FOR IB = 2
C
C     JAUX* are the tolerances for the cuts '*'
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-APR-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:L2JETS_PAR.INC'
C
      INTEGER MAXPAR,MAXCAND,MAXBANK
      PARAMETER (MAXPAR = 10)
      PARAMETER (MAXCAND = 20)
      PARAMETER (MAXBANK = 4)
      INTEGER LJAUX(*),LJPAR,IB,TYPE,LUN
      LOGICAL OKOK
C
      INTEGER I,BIT,ICAND,IPAR,IP,IP1,IP2,NREP,NREPJ,NRUN,NEV
      REAL JAUX_ETA,JAUX_PHI,JAUX_ET,JAUX_EMF,JAUX_ETAS,JAUX_PHIS
C
      INTEGER JCAND(MAXBANK),NPARS(MAXBANK)
      CHARACTER*1 CSTAT,CIETA,CIPHI,CETA,CPHI,CET,CEMF,CETAS,CPHIS
      CHARACTER*1 CPAR(4)
      INTEGER PS1(4),PS2(4)
      LOGICAL LDUMP,FIRST
      LOGICAL LSTAT,LIETA,LIPHI,LETA,LPHI,LET,LEMF,LETAS,LPHIS,LPAR
C
C     JPAR stuff....
C
      INTEGER NJPAR,NJCUT(MAXPAR),JPARSET(MAXPAR)
      REAL JETMIN(MAXPAR),JCONCEN(MAXPAR),JCONTOT(MAXPAR)
      REAL JMINRAD(MAXPAR),JMAXRAD(MAXPAR)
      REAL JEMFMIN(MAXPAR),JEMFMAX(MAXPAR)
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      OKOK = .TRUE.
C
      IF (IB.NE.2) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMP_JETS',
     &    'L2_COMP_JETS called for IB.NE.2 ! Not allowed!!!','F')
        RETURN
      ENDIF
C
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
C
C     loop over the parameter sets in JPAR to extract info into
C     a convenient form (known throughout the world as common blocks)
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NJPAR = IC(LJPAR+3)
        NREPJ = IC(LJPAR+2) + 1
        IP = LJPAR + 3
        DO IPAR=1,NJPAR
          IP = IP + 1
          NJCUT(IPAR) = C(IP)
          IP = IP + 1
          JETMIN(IPAR) = C(IP)
          IP = IP + 1
          JCONCEN(IPAR) = C(IP)
          IP = IP + 1
          JCONTOT(IPAR) = C(IP)
          IP = IP + 1
          JMAXRAD(IPAR) = C(IP)
          IP = IP + 1
          JMINRAD(IPAR) = C(IP)
          IP = IP + 1
          JEMFMAX(IPAR) = C(IP)
          IP = IP + 1
          JEMFMIN(IPAR) = C(IP)
          IP = IP + 1
          JPARSET(IPAR) = C(IP)
        ENDDO
      ENDIF
C
C     make sure there is JAUX and JPAR - no mistakes
C
      IF (LJPAR.LE.0) THEN
        WRITE(LUN,'('' L2_COMP_JETS: Run/Event '',2I7,
     &      '' has NO JPAR bank in STP'')') NRUN,NEV
        GOTO 999
      ENDIF
      IF (LJAUX(1).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_JETS: Run/Event '',2I7,
     &      '' has NO JAUX bank for SIMULATION'')') NRUN,NEV
        GOTO 999
      ENDIF
      IF (LJAUX(2).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_JETS: Run/Event '',2I7,
     &      '' has NO JAUX bank for DATA'')')
        GOTO 999
      ENDIF
C
C     now compare JAUX entries.  we HAVE to assume that the two L2SIM runs
C     were done with the same configuration since we only have JPAR from
C     the last run of L2SIM
C
      DO I=1,IB
        NPARS(I) = IQ(LJAUX(I)+4)
        JCAND(I) = IQ(LJAUX(I)+3)
      ENDDO
C
C     check on number of independent parameter sets (i.e. cone sizes)
C
      IF (NPARS(1).NE.NPARS(2)) THEN
        WRITE(LUN,'('' L2_COMP_JETS: Run/Event '',2I7,
     &      '' JAUX discrepancy in #Parsets: '',2I6)') NRUN,NEV,NPARS
        GOTO 999
      ENDIF
C
C     check on number of candidates found (e.g. L1 hot towers)
C
      IF (JCAND(1).NE.JCAND(2)) THEN
        WRITE(LUN,'('' L2_COMP_JETS: Run/Event '',2I7,
     &      '' JAUX discrepancy in #Candidates (S,D): '',2I6)')
     &      NRUN,NEV,JCAND(1),JCAND(2)
        GOTO 999
      ENDIF
C
C     just to be anal, check that each JAUX bank has the same rep number
C
      IF (IQ(LJAUX(1)+2).NE.IQ(LJAUX(2)+2)) THEN
        WRITE(LUN,'('' L2_COMP_JETS: Run/Event '',2I7,
     &      '' JAUX discrepancy in repetition length (S,D):'',2I7)')
     &      NRUN,NEV,IQ(LJAUX(1)+2),IQ(LJAUX(2)+2)
        GOTO 999
      ENDIF
C
C     ok, check word for word
C
      CSTAT = ' '
      CIETA = ' '
      CIPHI = ' '
      CETA = ' '
      CPHI = ' '
      CET = ' '
      CEMF = ' '
      CETAS = ' '
      CPHIS = ' '
      NREP = IQ(LJAUX(IB)+2)
      DO IPAR=1,NPARS(IB)
        DO ICAND=1,JCAND(IB)
          IP1 = (IPAR-1)*NREP*JCAND(1) + (ICAND-1)*NREP + LJAUX(1)
          IP2 = (IPAR-1)*NREP*JCAND(2) + (ICAND-1)*NREP + LJAUX(2)
          LSTAT = IQ(IP1+PJEVT).NE.IQ(IP2+PJEVT)
          IF (LSTAT) CSTAT = '*'
          LIETA = IQ(IP1+PJIETA).NE.IQ(IP2+PJIETA)
          IF (LIETA) CIETA = '*'
          LIPHI = IQ(IP1+PJIPHI).NE.IQ(IP2+PJIPHI)
          IF (LIPHI) CIPHI = '*'
          LETA = ABS(Q(IP1+PJETA)-Q(IP2+PJETA)).GT.JAUX_ETA
          IF (LETA) CETA = '*'
          LPHI = ABS(Q(IP1+PJPHI)-Q(IP2+PJPHI)).GT.JAUX_PHI
          IF (LPHI) CPHI = '*'
          LET = ABS(Q(IP1+PJET)-Q(IP2+PJET)).GT.JAUX_ET
          IF (LET) CET = '*'
          LEMF = ABS(Q(IP1+PJEMFR)-Q(IP2+PJEMFR)).GT.JAUX_EMF
          IF (LEMF) CEMF = '*'
          LETAS = ABS(Q(IP1+PJETASIZ)-Q(IP2+PJETASIZ)).GT.JAUX_ETAS
          IF (LETAS) CETAS = '*'
          LPHIS = ABS(Q(IP1+PJPHISIZ)-Q(IP2+PJPHISIZ)).GT.JAUX_PHIS
          IF (LPHIS) CPHIS = '*'
          DO I=0,3
            PS1(I+1) = IQ(IP1+14+I)
          ENDDO
          DO I=0,3
            PS2(I+1) = IQ(IP2+14+I)
          ENDDO
          LPAR = .FALSE.
          DO I=1,4
            CPAR(I) = ' '
            IF (PS1(I).NE.PS2(I)) THEN
              LPAR = .TRUE.
              CPAR(I) = '*'
            ENDIF
          ENDDO
C
C         now, check to see if these two entries are equal....
C
          LDUMP = LSTAT.OR.LIETA.OR.LIPHI.OR.LETA.OR.LPHI.OR.LET.OR.
     &      LEMF.OR.LETAS.OR.LPHIS
          IF (LDUMP.OR.LPAR) THEN
            OKOK = .FALSE.
            WRITE(LUN,'(/,'' L2_COMP_JETS:  Run/Event '',2I7,
     &        '' JAUX entry discrepancy'',/,
     &        '' "*" denote variables whose differences is '',
     &        ''outside of tolerances'')') NRUN,NEV
            WRITE(LUN,'(
     &        ''     CAND PAR   STAT   IETA   ETA IPHI   '',
     &        ''PHI     ET   EMFR   ETAS   PHIS'')')
            WRITE(LUN,'('' SIM'',I4,'':'',I4,I6,A1,1X,I5,A1,F6.2,A1,1X,
     &        I2,A1,F6.2,A1,F6.1,A1,F6.1,A1,F6.2,A1,F6.2,A1)')
     &        ICAND,IPAR,
     &        IQ(IP1+PJEVT),CSTAT,IQ(IP1+PJIETA),CIETA,
     &        Q(IP1+PJETA),CETA,IQ(IP1+PJIPHI),CIPHI,Q(IP1+PJPHI),CPHI,
     &        Q(IP1+PJET),CET,Q(IP1+PJEMFR),CEMF,
     &        Q(IP1+PJETASIZ),CETAS,Q(IP1+PJPHISIZ),CPHIS
            WRITE(LUN,'('' DAT'',I4,'':'',I4,I6,A1,1X,I5,A1,F6.2,A1,1X,
     &        I2,A1,F6.2,A1,F6.1,A1,F6.1,A1,F6.2,A1,F6.2,A1)')
     &        ICAND,IPAR,
     &        IQ(IP2+PJEVT),CSTAT,IQ(IP2+PJIETA),CIETA,
     &        Q(IP2+PJETA),CETA,IQ(IP2+PJIPHI),CIPHI,Q(IP2+PJPHI),CPHI,
     &        Q(IP2+PJET),CET,Q(IP2+PJEMFR),CEMF,
     &        Q(IP2+PJETASIZ),CETAS,Q(IP2+PJPHISIZ),CPHIS
            IF (LPAR) THEN
              WRITE(LUN,'(
     &          ''       Masks:        1         2         '',
     &          ''3         4 '')')
              WRITE(LUN,'(''         SIM:'',4(1X,Z8.8,A1))')
     &          (PS1(I),CPAR(I),I=1,4)
              WRITE(LUN,'(''         DAT:'',4(1X,Z8.8,A1))')
     &          (PS2(I),CPAR(I),I=1,4)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
C
  999 CONTINUE
C
C     things are amiss
C
      OKOK = .FALSE.
      RETURN
C
      ENTRY L2_COMP_JETS_OUT(LUN)
C
      WRITE(LUN,'('' JPAR parameters/cuts: '',/,
     &    ''    #Js      ET        Cones          Jet'',
     &    '' Width                EMF           PAR'')')
      DO I=1,NJPAR
        WRITE(LUN,'(
     &      I13,F7.2,1X,E8.1,''-'',E8.1,2x,E8.1,''-'',E8.1,2X,
     &      E8.1,''-'',E8.1,1x,I12)') NJCUT(I),JETMIN(I),
     &      JCONCEN(I),JCONTOT(I),JMAXRAD(I),JMINRAD(I),
     &      JEMFMAX(I),JEMFMIN(I),JPARSET(I)
      ENDDO
C
      RETURN
      END
