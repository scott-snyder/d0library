      SUBROUTINE GET_L1JETS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Load L2JETS found jets into common
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-JUN-1991   Richard Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
C      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INTEGER NHOT_MAX
      PARAMETER(NHOT_MAX = 16)
      INCLUDE 'D0$PARAMS:JETS.PARAMS'
      INCLUDE 'D0$INC:JETVAR.INC'
      INCLUDE 'D0$INC:L2JETS_HOT.INC'
      INTEGER IETA,IPHI,I, L1ESUM, GZESUM, NR, NFIX, NJ,POINT
      LOGICAL OK
      REAL EMET,TOTET
      CHARACTER*4 PATH
      INTEGER GZJUTL, LJUTL
      INCLUDE 'D0$INC:QCDJ.DEF'
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF ( PATH .EQ. 'MDST') GOTO 7000
C---Call L2 routine to get L1 jet seeds
      IF ( LQ( LHEAD - 1) .LE. 0) THEN ! NO TRGR
        NJET(NOWLIST) = 0
        GOTO 950
      END IF
      CALL L2JETS_HOTFL                 ! Ignore value of this function
      NJET(NOWLIST) = NJTHOT
      JETFLAG(NOWLIST) = 'L1JT'
      DO I = 1,NJTHOT
        CALL CDBITT(IHOT_ADR_JT(I),IETA,IPHI,OK)
        CALL CL1PHET(IPHI,IETA,EMET,TOTET)
        IDJET(I,NOWLIST) = I            ! Good ?
        ETLST(I,NOWLIST) = TOTET
        ETALST(I,NOWLIST) = ETA_FROM_JETA(IETA,.2)
        PHILST(I,NOWLIST) = PHI_FROM_JPHI(IPHI,32)
        ENGLST(I,NOWLIST) = ETLST(I,NOWLIST)/
     &    SIN(THETA_FROM_ETA(ETALST(I,NOWLIST) ))
c        IPHILST(I,NOWLIST) = JPHI_FROM_PHI(PHILST(I,NOWLIST),64)
c        IETALST(I,NOWLIST) = JETA_FROM_ETA(ETALST(I,NOWLIST),.1)
        IETALST(I,NOWLIST) = IETA
        IPHILST(I,NOWLIST) = IPHI
        ETASIZ(I,NOWLIST) = 0.
        PHISIZ(I,NOWLIST) = 0.
        EMFRLST(I,NOWLIST) = -999.
        IF ( ABS( TOTET ) .GT. .200) EMFRLST(I,NOWLIST) = EMET/TOTET
      END DO
      GOTO 999
C
C: No trigger bank. Look in ESUM
C
  950 CONTINUE
      L1ESUM = GZESUM('TRGR')
      IF ( L1ESUM .LE. 0 ) GOTO 999
      NR =   IQ(L1ESUM + 3 )
      NFIX = IQ(L1ESUM + 2 )
      NJ = IQ(L1ESUM + 4 )
      DO I = 1, NJ
        POINT = L1ESUM + NFIX + (I-1)*NR
        IF ( IQ(POINT+1) .EQ. 5 ) THEN
          NJET(NOWLIST) = NJET(NOWLIST) + 1
          ETLST(NJET(NOWLIST),NOWLIST) = Q(POINT+3)
          ETALST(NJET(NOWLIST),NOWLIST)= Q(POINT+5)
          PHILST(NJET(NOWLIST),NOWLIST)= Q(POINT+6)
          IDJET(NJET(NOWLIST),NOWLIST) = I
        ENDIF
      ENDDO
  999 RETURN
C
C: FOR MDST
C
 7000 CONTINUE
      LJUTL = GZJUTL()
      IF ( LJUTL .LE. 0 ) RETURN
      POINT = LJUTL + 41
      IF ( IQ(LJUTL + 1 ) .EQ. 2 ) POINT = LJUTL + 21
      NJET(NOWLIST) = MIN( INT(Q( POINT )), MAXLIST)
      JETFLAG(NOWLIST) = 'L1JT'
      DO I = 1, NJET(NOWLIST)
        IF ( I .LE. MAXLIST ) THEN
          IDJET( I, NOWLIST ) = I
          ETALST( I, NOWLIST ) = Q( POINT + 1 )
          PHILST( I, NOWLIST ) = Q( POINT + 2 )
          ETLST( I, NOWLIST ) = Q( POINT + 3 )
          ENGLST(I,NOWLIST) = ETLST(I,NOWLIST)/
     &      SIN(THETA_FROM_ETA(ETALST(I,NOWLIST) ))
          IETALST(I,NOWLIST) = JETA_FROM_ETA(ETALST(I,NOWLIST),.2)
          IPHILST(I,NOWLIST) = JPHI_FROM_PHI(PHILST(I,NOWLIST),32)
          ETASIZ(I,NOWLIST) = -999.
          PHISIZ(I,NOWLIST) = -999.
          EMFRLST(I,NOWLIST) = -999.
          POINT = POINT + 3
        ENDIF
      ENDDO
      RETURN
      END
