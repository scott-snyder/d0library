      SUBROUTINE TRDTFIL (ITR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill TRD ZEBRA banks TRDT
C-
C-   Inputs  : ITR = TRACK NUMBER
C-   Outputs :
C-   Controls:
C-
C-   Created  15-NOV-1989   A. Zylberstejn
C-   Updated  28-NOV-1990   A. Zylberstejn   :UPDATE VERSION NB TO 1.0
C-   Updated  17-JUL-1991   A. Zylberstejn   : fill +10 with Tmin as determined
C-                                             by CDC (for cosmic runs)
C-   Updated  29-JAN-1993   Alain PLUQUET     Simplification.
C-   Updated  30-SEP-1994   Alain PLUQUET  Version 2 (new pressure format)
C-   Updated  30-JAN-1995   Lewis Taylor Goss   Version 3 (use uranium)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRDT_VERSION_NB.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:TRDEFF.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:VARTRD.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:URAN_COR.INC'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INTEGER I,IER,ITR,DBM_FIRST_RUN,ILAY
      INTEGER LTRDT,LTROP,RUNNB
      LOGICAL FIRST,READ_DBMON,RUN1A
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        READ_DBMON=.FALSE.
        CALL EZGET('READ_DBMON',I, IER)
        IF (IER.EQ.0.AND.I.NE.0) READ_DBMON=.TRUE.
        CALL EZGET('DBM_FIRST_RUN', DBM_FIRST_RUN, IER)
        CALL EZRSET
      END IF
      RUNNB=IQ(LHEAD+12)
      CALL BKTRDT(LTRDT)
      IF(READ_DBMON .AND.RUNNB.GT.DBM_FIRST_RUN) LTROP=LC(LTGEN-IZTROP)
      Q(LTRDT+1)=1.0
      IQ(LTRDT-5)=ITR  ! track number
      IQ(LTRDT+3)=NTWIRE(1,ITR) ! total nb. of hit layers on the track
      Q(LTRDT+4)=ETOTAL(1,ITR) ! Total energy 3 layers
      Q(LTRDT+5)=ETRUNC(1,ITR) ! Truncated mean
      Q(LTRDT+6)=LIKET(1,ITR)  ! Likelihood E tot
      Q(LTRDT+7)=LIKECL(1,ITR) ! Likelihood Etot/nb. of clusters thresh. 0
      Q(LTRDT+8)=LIKECL(1,ITR) ! Likelihood Etot/nb. of clusters thresh. 30
      Q(LTRDT+9)=LIKECL(1,ITR) ! Likelihood Etot/nb. of clusters thresh. 60
      IF(LTROP.NE.0)THEN   ! fill things from TROP bank
        Q(LTRDT+1)= TRDT_VERS ! Version number (use uranium info.)
        Q(LTRDT+10)=0.
        Q(LTRDT+11)=C(LTROP+5)  !canary signal 80%
        Q(LTRDT+12)=0.
        Q(LTRDT+21)=C(LTROP+56) ! Temperature VOL. 0
        Q(LTRDT+22)=C(LTROP+57) ! Temperature ( Collision Hall)
        IF (RUN1A()) THEN
          Q(LTRDT+13)=C(LTROP+55)/10.+900. ! Atmospheric pressure
        ELSE
          Q(LTRDT+13)=C(LTROP+54)+C(LTROP+60)
        ENDIF
C take care of uranium stuff
        Q(LTRDT+26) = QJT_UR(1)   ! Lower uranium run packed time
        Q(LTRDT+27) = QJT_UR(2)   ! Upper uranium run packed time
        Q(LTRDT+28) = CORPT(1)    ! Lower uranium run p,t correction 
        Q(LTRDT+29) = CORPT(2)    ! Upper uranium run p,t correction
        DO I = 1,2
          DO ILAY=1,3
            Q(LTRDT+29+ILAY+(I-1)*3) = CORHV(I,ILAY)
            Q(LTRDT+35+ILAY+(I-1)*3) = UR_COR(I,ILAY)
          ENDDO
        ENDDO
      ENDIF
C  Efficiencies ANODES
      Q(LTRDT+14)=ELEFF(1)       ! Electron effficiency E tot
      Q(LTRDT+15)=ELEFF(3)       ! Electron effficiency trunc. mean
      Q(LTRDT+16)=ELEFF(5)       ! Electron effficiency likel. E tot
      Q(LTRDT+20)=PIREJ(1)       ! 5 GeV/c pion rejection (Etot)
      Q(LTRDT+42)=ITR            ! Track number
      IF(IQ(LTRDT+6).EQ.3)THEN   ! EFFICIENCIES FOR 3 HIT PLANES
        Q(LTRDT+17)=ELEFF(2)     ! ELECTRON EFFFICIENCY E TOT
        Q(LTRDT+18)=ELEFF(4)     ! ELECTRON EFFFICIENCY TRUNC. MEAN
      END IF
C Fill quantities related to cathodes
      IQ(LTRDT+23)=NTWIRE(2,ITR)
      Q(LTRDT+24)=ETOTAL(2,ITR)  ! Total energy 3 layers
      Q(LTRDT+25)=ETRUNC(2,ITR)  ! Truncated mean
      END
