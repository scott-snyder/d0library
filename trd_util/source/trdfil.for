      SUBROUTINE TRDFIL(VIN,PHI,IDENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :         fills TRINTR.INC
C-
C-   Inputs  :
C-             VIN         :       position of vertex, direction of track
C-             PHI         =       ATAN(VIN(5)/VIN(6)) not recomputed
C-             IDENT       :       = 1 for e-like particles, 0 otherwise
C-                                      +coded particle number
C-   Outputs :
C-   Controls:
C-
C-   Created   2-NOV-1989   J.Fr. Glicenstein
C-   Updated   1-FEB-1990   a. Zylberstejn  :increment ngoodt only if
C-                                           cells are hit
C-   Updated  13-SEP-1990   A. Zylberstejn: updated for 512 wires in layer 3
C-   Updated  22-NOV-1994   A. Zylberstejn  Change max nb. of allowed wires
C-   Updated  23-NOV-1994   Qizhong Li-Demarteau  fixed crash due to write
C-                                                outside of array
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:GEOMTR.INC'
      INCLUDE 'D0$INC:TCLUS_PER_WIRE.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TR_CROSS_CELL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:TRENER.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INTEGER CHA1,IERR,IDENT,IKK1,IKK2,IREG0,ITR,NTOTW,ICAS,NWMAX(2)
      INTEGER I,IS,IW,WI,IWIRE,ICH,CHAMB,IINF,INFO,ISUP,NERR,UBIT
      INTEGER TRANOD(2,3),TRCATH(2,3),DW,DWIR,IAIN,LOUT,TRUNIT
      INTEGER IUCOMP,TDATA(NMFADC+10),NG,TCHNB,TWIRCOR
      REAL DANG(3),DCELL,DPHI,DPHI1,FTDATA(NMFADC+10),PEDES,VASUM,SG,WG
      LOGICAL DOPRINT,FIRST,HEXIST,DO_HISTO,TRD_DO_PRINT
      CHARACTER*3 C3
      EQUIVALENCE(FTDATA(1),TDATA(1))
      REAL VIN(6),PHI,PHITR
      DATA FIRST/.TRUE./
C ****
      IF(FIRST)THEN
        DANG(1)=DPHIAN(1)
        DANG(2)=DPHIAN(2)
        DANG(3)=DPHIAN(3)
        NWMAX(1)=4 ! MAX. NB. OF adjacent allowed anode wires
        NWMAX(2)=MIN0(NBWIRE_MAX,NBINFO-2)
        IF(NWIRE_PER_LAYER(3).LE.256)DANG(3)=2.*DANG(3)
        LOUT=TRUNIT()
        CALL EZPICK('TRD_RCP')
        DO_HISTO=.FALSE.
        CALL EZGET('HSTBOK',IWS,IERR)
        CALL UHTOC(IWS(1),3,C3,3)
        IF(IERR.EQ.0)DO_HISTO=C3.EQ.'y' .OR. C3.EQ.'Y'
     &    .OR. C3.EQ.'YES'
        CALL EZRSET
        NERR=0
        FIRST=.FALSE.
      END IF
C ****
C    Compute hit anodes and cathodes
C
      DOPRINT=TRD_DO_PRINT()
      IF(DOPRINT)
     +  WRITE(LOUT,'(a30,/,a30)')
     &  ' ----------------',' enter TRDFIL'
      CALL TRCELL(VIN,TRANOD,TRCATH,IERR)
      IF(DOPRINT)WRITE(LOUT,*)' In TRDFIL,call TRCELL VIN',VIN,
     +' TRANOD',TRANOD,'TRCATH',TRCATH
      IF(IERR.NE.0)THEN
        IF(NERR.LE.20) CALL ERRMSG
     &    ('TRD','TRDFIL','ERROR COMPUTING HIT TRD CELL ','W')
        NERR=NERR+1
        GO TO 999
      END IF
   68 CONTINUE
      IF(DO_HISTO)CALL HCDIR('//PAWC/TRD',' ')  ! go to TRD directory
      NGOODT = NGOODT+1
      IF(NGOODT.GE.TTRAKG)THEN  ! protection against too many tracks
        CALL ERRMSG('TRD','TRDFIL',' Too many tracks in the TRD','W')
        IDENT=-1000
        NGOODT=NGOODT-1
        GO TO 999
      END IF
      STHETA(NGOODT)=AMIN1(SQRT(VIN(4)**2+VIN(5)**2),1.)
      PHIP  (NGOODT)=PHI
      IDENTR(NGOODT)=IDENT
      DO 20 ICH=1,3
        ANOINF(ICH,NGOODT)=TRANOD(1,ICH)
        ANOSUP(ICH,NGOODT)=TRANOD(2,ICH)
        CATINF(ICH,NGOODT)=TRCATH(1,ICH)
        CATSUP(ICH,NGOODT)=TRCATH(2,ICH)
        NANOD(ICH,NGOODT)=IABS(ANOINF(ICH,NGOODT)-ANOSUP(ICH,NGOODT))
     &                      +1
        IF(NANOD(ICH,NGOODT).GT.10)NANOD(ICH,NGOODT)=
     &             NWIRE_PER_LAYER(ICH)+2 -NANOD(ICH,NGOODT)
        NCATO(ICH,NGOODT)=IABS(CATINF(ICH,NGOODT)-CATSUP(ICH,NGOODT))
     &                      +1
        IF(NCATO(ICH,NGOODT).GT.10)NCATO(ICH,NGOODT)=
     &             258-NCATO(ICH,NGOODT)
        IF(ANOINF(ICH,NGOODT)*ANOSUP(ICH,NGOODT).LE.0)THEN
          NANOD(ICH,NGOODT)=0
          NCATO(ICH,NGOODT)=0
          IF(DOPRINT)WRITE(LOUT,*)' In TRDFIL nanod=0 for track',
     &      NGOODT,' ich',ICH
        END IF
   20 CONTINUE
      ITR=NGOODT
      NTOTW=0
      DO 62 ICAS=1,2
        DO 60 ICH=1,3 ! Loop on layers
          CHAMB=(ICAS-1)*3+ICH
          NBHWIR(CHAMB,ITR) = 0
          IF(NANOD(ICH,ITR).LE.0)GO TO 60
          IF(CHAMB.LE.3)THEN  !Anodes
            IINF=ANOINF(ICH,ITR)
            ISUP=ANOSUP(ICH,ITR)
          ELSE                 !Cathodes
            IINF=CATINF(ICH,ITR)
            ISUP=CATSUP(ICH,ITR)
          END IF
          IREG0=0
          IF(IABS(IINF-ISUP).GT.50)IREG0=1
          IF (IREG0.EQ.0) THEN
            IKK1 = MIN0(IINF,ISUP)
            IKK2 = MAX0(IINF,ISUP)
          ELSE
            IKK1 = MAX0(IINF,ISUP)
            IKK2 =  NWIRE_PER_LAYER(CHAMB) + MIN0(IINF,ISUP)
          ENDIF
          DPHI=1000.
          DO 40 IW = IKK1,IKK2
            IWIRE=IW
            IF(IWIRE.GT.NWIRE_PER_LAYER(CHAMB))
     &        IWIRE=IWIRE- NWIRE_PER_LAYER(CHAMB)
c            IF(.NOT.MCDATA)THEN
c              IF (ICH.EQ.3 .AND.NWIRE_PER_LAYER(3).EQ.256)
c     &          IWIRE=TWIRCOR(IWIRE)
c            END IF
            IF (.NOT.TWCOD(TCHNB(IWIRE,CHAMB))) GOTO 40 ! Wire not coded
            IF(NBHWIR(CHAMB,ITR).GE.NWMAX(ICAS))GO TO 40
            NBHWIR(CHAMB,ITR) = NBHWIR(CHAMB,ITR)+1 ! Number of hit wires
            WIRNBH(NBHWIR(CHAMB,ITR),CHAMB,ITR) = IWIRE
            MULT_CELL(NBHWIR(CHAMB,ITR),CHAMB,ITR) = 0
            IF(IUCOMP(IWIRE,ICROSS(1,CHAMB),NCROSS(CHAMB)).NE.0)
     &        MULT_CELL(NBHWIR(CHAMB,ITR),CHAMB,ITR) = 1
C          if(doprint)write(lout,3047)itr,chamb,iwire,
C     &      mult_cell(NBHWIR(CHAMB,ITR),CHAMB,ITR),
C     &      (icross(i,chamb),i=1,ncross(chamb))
 3047       FORMAT(' in trdfil,track',I3,' chamb',I2,' wire',I4,
     &        'mult_cell',I2,' icross',8I4)
            IF(DOPRINT .OR.
     &         (IWIRE.LE.0 .OR. IWIRE.GT.NWIRE_PER_LAYER(CHAMB)))THEN
              IF(LOUT.NE.0)
     +          WRITE(LOUT,*)' In TRDFIL,NBHWIR,WIRNBH',NBHWIR(CHAMB,
     +          ITR),
     &          IWIRE,' ITR',ITR,'ikk1,ikk2',IKK1,IKK2,' iinf,isup',
     &          IINF,
     +          ISUP,'ANOINF, ANOSUP',ANOINF(ICH,ITR), ANOSUP(ICH,ITR)
            END IF
            IF(CHAMB.GT.3)GO TO 40
C    ANG. DIST. TO THE WIRE
            PHITR=WS(4500+ICH) ! phi of the intersection of the track and
                               ! the anode plane
            DPHI1=PHITR-(FLOAT(IWIRE)-.5)*DANG(ICH)
            IF(ABS(DPHI).LT.ABS(DPHI1))GO TO 40
            DPHI=DPHI1
   40     CONTINUE
          IF(CHAMB.LE.3)THEN
            NTOTW=NTOTW+NBHWIR(CHAMB,ITR)
            PHITRA(ICH,NGOODT)=DPHI*RADAN(CHAMB)
          END IF
C
C  Histo of distance betwwen coded cell nearest to the track and crossed cell
          IF(.NOT.DO_HISTO .OR. .NOT.HEXIST(FIRSHT+610+CHAMB))
     &      GO TO 60
          IAIN=IWS(4503+CHAMB)
C    Discard  sector 11 layer 3 IF COSMIC1
          IF(COSMIC1 .AND. CHAMB.EQ.3 .AND. IAIN/16.EQ.11)GO TO 60
          IF(TWCOD(TCHNB(IAIN,CHAMB)))THEN
            DWIR=0
            GO TO 46
          END IF
          DWIR=100
          DO 44 IWIRE=1, NWIRE_PER_LAYER(CHAMB)
            IF(.NOT.TWCOD(TCHNB(IWIRE,CHAMB)))GO TO 44
            DW=IABS(IWIRE-IAIN)
            IF(DW.GT.250)DW= NWIRE_PER_LAYER(CHAMB)-DW
            IF(DW.GT.IABS(DWIR))GO TO 44
            DWIR=IWIRE-IAIN
            IF(DWIR.EQ.0)GO TO 46
            IF(DWIR.GT.250)DWIR= NWIRE_PER_LAYER(CHAMB)-DWIR
            IF(DWIR.LT.-250)DWIR=-DWIR- NWIRE_PER_LAYER(CHAMB)
   44     CONTINUE
   46     CONTINUE
          IF(DWIR.EQ.100)GO TO 60
          IF(DOPRINT)THEN
            IF(CHAMB.EQ.3 .AND. (IAIN.GE.81 .AND. IAIN.LE.88))THEN
              WRITE(LOUT,*)' Chamber',CHAMB,' wire track',IAIN,
     &            ' coded wire code',IAIN+DWIR
            END IF
          END IF
          CALL HF1(FIRSHT+610+CHAMB,FLOAT(DWIR),1.)
          IF(HEXIST(FIRSHT+537+CHAMB).AND. HEXIST(FIRSHT+557+CHAMB).
     &        AND.
     &        CHAMB.LE.3 .AND.IABS(DWIR).LE.20)THEN
            CALL HF1(FIRSHT+537+CHAMB,FLOAT(IAIN),FLOAT(DWIR))
            CALL HF1(FIRSHT+557+CHAMB,FLOAT(IAIN),1.)
          END IF
   60   CONTINUE
   62 CONTINUE
C
C  PRINTS
C
      IF( NTOTW.GE.1)THEN ! At least one anode plane with coded wires
C        DOPRINT=trd_do_print
        IF (DOPRINT )THEN
          DO 70 CHAMB =  1,  6
            WRITE(LOUT,1010)ITR,CHAMB, NBHWIR(CHAMB,ITR),
     &        (WIRNBH(IWIRE,CHAMB,ITR),IWIRE=1,NBHWIR(CHAMB,ITR))
   70     CONTINUE
        END IF
      ELSE
        IF(DOPRINT)WRITE(LOUT,*)' Track',ITR,' No coded wire'
        NGOODT=NGOODT-1
      END IF
 1010 FORMAT( ' In TRDFIL,Track',I3,' Chamber',I2,' Nb. of hit wires',
     &  I2, ' Coded wires',16I4)
  999 CONTINUE
      IF(DOPRINT)
     +  WRITE(LOUT,'(a30,/,a30)')' Exit TRDFIL',
     &  ' ----------------'
      RETURN
      END
