      SUBROUTINE TRGVER(ITRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : D0 histos to check the geometry of the TRD
C-
C-   Inputs  : ITRA= TRACK NUMBER
C-   Outputs :
C-   Controls:
C-
C-   Created  13-JUN-1990   A. Zylberstejn
C-   Updated   3-SEP-1992   A. Zylberstejn
C-   Updated  30-DEC-1993   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:GEOMTR.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      REAL DPHI,DPHI1,PHITR,DANG(3)
      INTEGER ICH,IER,IH,ITRA,IWG,IW,LOUT,TRUNIT
      INTEGER NHIT,NMISS(3)
      CHARACTER*3 C3
      LOGICAL FIRST,DO_HISTO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        LOUT=TRUNIT()
        CALL EZPICK('TRD_RCP')
        DO_HISTO=.FALSE.
C        CALL EZGET('GENERAL_TRD_HISTOS',IH,IER)
C        IF(IER.EQ.0)THEN
C          CALL UHTOC(IH,3,C3,3)
C          DO_HISTO=C3.EQ.'Y' .OR. C3.EQ.'y' .OR. C3.EQ.'YES'
C        END IF
        CALL EZGET('HSTBOK',IWS,IER)
        IF(IER.EQ.0)THEN
          CALL UHTOC(IWS(1),3,C3,3)
          DO_HISTO=(C3.EQ.'Y' .OR. C3.EQ.'y' .OR. C3.EQ.'YES')
          CALL UHTOC(IWS(4),3,C3,3)
          DO_HISTO=(C3.EQ.'Y' .OR. C3.EQ.'y' .OR. C3.EQ.'YES') .AND.
     &      DO_HISTO
        END IF
        DO IH=1,3
          DANG(IH)=2.*PI/FLOAT(NWIRE_PER_LAYER(IH))
        END DO
        FIRST=.FALSE.
        CALL EZRSET
      END IF
      IF (.NOT.DO_HISTO) GOTO 999       ! No histogram required
      CALL HCDIR('//PAWC/TRD',' ')  ! go to TRD directory
      NHIT=0
      DO 40 ICH=1,6
        IF(ICH.LE.3)THEN
          NMISS(ICH)=1
          DPHI=PHITRA(ICH,ITRA)/RADAN(ICH)
        END IF
        IF(ICH.LE.3)THEN
          IF(NBHWIR(ICH,ITRA).LE.0)THEN
            IF(ICH.EQ.3)THEN
              IW=PHIP(ITRA)/DANG(ICH)+1
C              print*,'dphi,dang,iw',dphi,dang(ich),iw
              CALL HF1(FIRSHT+537,FLOAT(IW),1.)
              GO TO 40
            END IF
          END IF
          NMISS(ICH)=0
          NHIT=NHIT+1
          CALL HF1(FIRSHT+500+ICH,DPHI*RADDEG,1.)
          CALL HFILL(FIRSHT+550+ICH,PHIP(ITRA)*RADDEG,DPHI*RADDEG,1.)
          CALL HF1(FIRSHT+525+ICH,2.*DPHI/DANG(ICH),1.)
        END IF
        DO 20 IH=1,NBHWIR(ICH,ITRA)
          IWG=WIRNBH(IH,ICH,ITRA)
          CALL HF1(FIRSHT+530+ICH,FLOAT(IWG),1.)
   20   CONTINUE
   40 CONTINUE
      CALL HF1(FIRSHT+522,FLOAT(NHIT),1.)
      DO 50 ICH =  1,  3
        IF(NMISS(ICH).NE.1)THEN
C                           ! Nb. of times the layer is hit
          CALL HF1(FIRSHT+523,FLOAT( ICH),1.)
        ELSE                ! missing layer but the 2 others are there
          IF(NHIT.EQ.2) CALL HF1(FIRSHT+521,FLOAT( ICH),1.)
        END IF
   50 CONTINUE
  999 RETURN
      END
