      SUBROUTINE SAVE_RESULTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Work out maximum likelihood value,
C-   95% CL's and make banks
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-JUN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:EVENT_QUAN1.INC'
      INCLUDE 'D0$INC:BTAG_ISAJ.INC'
      LOGICAL first
      SAVE first
      DATA first / .true. /
      REAL    BUFFER(MAXT+10)
      INTEGER IRR
      INTEGER NDATA
      INTEGER LMASS
      EQUIVALENCE (LBANK,LMASS)
      REAL    XLO,XHI,XMAX,YMAX
      REAL    XLOLIM,XHILIM
      REAL    X1,X2,SUM
      REAL    Y(MAXT),CUM(MAXT)
      INTEGER ILO,IHI,IMAX,NX
      LOGICAL READ_RCP
      INTEGER IER
      INTEGER SSUNIT
      INTEGER RUNNO,EVONUM
      INTEGER IOFF
      INTEGER NDUMP
      INTEGER IDUMP
      SAVE IDUMP
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL DO_HBOOK('LIKELY')
        CALL EZGET('READ_EVENT_FROM_RCP',READ_RCP,IER)
        CALL EZGET('NUMBER_DUMP_EVENTS',NDUMP,IER)
        CALL EZRSET
        XLOLIM = .05
        XHILIM = .95
        IDUMP = 0
      ENDIF
C
      IF(NCNFGE.EQ.0)GO TO 777   !NO CONFIGS BEING GENERATED
C
      IF ( READ_RCP ) THEN
        IOFF = (COMBNUM-1)*10
        CALL SUMUP(2002+IOFF,XLOLIM,XHILIM)
        CALL SUMUP(2003+IOFF,XLOLIM,XHILIM)
        CALL SUMUP(2005+IOFF,XLOLIM,XHILIM)
        CALL SUMUP(2006+IOFF,XLOLIM,XHILIM)
!
      ENDIF
C
C ****  now to book bank on DST and save.
C

      X1 = TMASS_LO
      X2 = TMASS_HI+1   !PRETEND END OF HISTOGRAM LIMIT
      NX = NTOPS
C
      DO ICMB = 1 ,2
        IRR = 1   !RR METHOD
        CALL UCOPYDS(LIKELY(1,ICMB,IRR),Y,NTOPS)
        CALL GET_LIMITS(XLOLIM,XHILIM,X1,X2,NX,Y,
     &    NCNFGE,CUM,SUM,XLO,XHI,XMAX,YMAX,ILO,IHI,IMAX)
C
        RR_LO(ICMB) = XLO
        RR_HI(ICMB) = XHI
        RR_MASS(ICMB) = XMAX
        RR_LIKELY(ICMB) = YMAX
C
        IRR = 2
        CALL UCOPYDS(LIKELY(1,ICMB,IRR),Y,NTOPS)
        CALL GET_LIMITS(XLOLIM,XHILIM,X1,X2,NX,Y,
     &    NCNFGE,CUM,SUM,XLO,XHI,XMAX,YMAX,ILO,IHI,IMAX)
C
        DG_LO(ICMB) = XLO
        DG_HI(ICMB) = XHI
        DG_MASS(ICMB) = XMAX
        DG_LIKELY(ICMB) = YMAX
C
C MAKE NTUPLE
C
        BUFFER(1) = RUNC
        BUFFER(2) = EVENTC
        BUFFER(3) = ICMB
        DO IRR = 1 , 2
          BUFFER(4) = IRR
          BUFFER(5) = COMBNUM
          IF ( IRR.EQ.1 ) THEN
            BUFFER(6) = RR_MASS(ICMB)
            BUFFER(7) = RR_LIKELY(ICMB)
            BUFFER(8) = RR_LO(ICMB)
            BUFFER(9) = RR_HI(ICMB)
          ELSE
            BUFFER(6) = DG_MASS(ICMB)
            BUFFER(7) = DG_LIKELY(ICMB)
            BUFFER(8) = DG_LO(ICMB)
            BUFFER(9) = DG_HI(ICMB)
          ENDIF
C BTAG INFO
C 'BTFL' 'NTAG' 'JTAG1' 'JTAG2' 'DIFR1' 'DIFR2' 'DIFET1'  'DIFET2'
          BUFFER(10) = BTAG_FL
          BUFFER(11) = NTAG
          BUFFER(12) = JET_TAGGED(1)
          BUFFER(13) = JET_TAGGED(2)
          BUFFER(14) = DIF_R(1)
          BUFFER(15) = DIF_R(2)
          BUFFER(16) = DIF_ET(1)
          BUFFER(17) = DIF_ET(2)
          BUFFER(18) = TMASS_ON(1)
          BUFFER(19) = TMASS_ON(2)
          BUFFER(20) = NJETS
          BUFFER(21) = IFSRA+IFSRT
C
          CALL UCOPYDS(LIKELY(1,ICMB,IRR),BUFFER(22),NTOPS)
          CALL DO_HFN('DILEPTON',400,BUFFER)
        ENDDO
      ENDDO
      NDATA = 43
      CALL BKMASS(LPROC,LMASS,NDATA)
      CALL MASSFL(LMASS)
C
  777 IF ( IDUMP .LE.NDUMP) THEN
        IF ( COMBNUM.EQ.1 ) THEN
          CALL DUMP_ISAJET_TOP
          CALL DUMP_MASS_EVENT
          IDUMP = IDUMP+1
        ENDIF
C
        IF ( NCNFGE.NE.0 ) CALL PRMASS(SSUNIT(),LMASS,0,'ALL',0)
      ENDIF
C
  999 RETURN
      END
