      SUBROUTINE TRDHIT_U
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Look for hits in TRD for Uranium signal.
C-     If wires are aligned in several layers consider FADC bins above
C-     150. I wires not aligned consider full range of FADC
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  30-MAY-1990   J.Fr.Glicenstein
C-   Updated  29-JUN-1992   A. Zylberstejn  Use THIT  bank
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FADCCN.INC'
      INCLUDE 'D0$INC:UNPTRD.INC'
C      INCLUDE 'd0$inc:TRD_GROUP.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:TRHITW.INC'
C      INCLUDE 'D0$INC:trwcod.INC'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
C----------------------------------------------------------------------
C-
C-   Created  24-MAY-1992   JFG
C-                          Information on clustering in TRD layers
C-                          Maximum 30 groups of maximum 50 hits (?)
C- NGROUP_PER_LAYER(L):number of  grouped set of wires found in layer L
C- NWIRE_PER_GROUP(NW,L): number of wires in group NW in layer L
C- LISTW_INCL(List,NW,L): list of wires for group NW in layer L
C----------------------------------------------------------------------
      INTEGER NWRMAX,NCLMAX
      INTEGER NLM,NTM
      PARAMETER (NLM=6)
      PARAMETER (NWRMAX = 50)
      PARAMETER (NCLMAX = 30)
      PARAMETER (NTM=NLM*NTOT_WIRE_TRD)
      COMMON /TRD_GROUPS/ NGROUP_PER_LAYER(NLM),NWIRE_PER_GROUP
     &(NCLMAX,NLM),LISTW_INCL(NWRMAX,NCLMAX,NLM),GOOD_WIRE(NTM)
      INTEGER NGROUP_PER_LAYER,NWIRE_PER_GROUP,LISTW_INCL
      LOGICAL GOOD_WIRE
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER DT0,ISUPFLAT,IPRIME,IER,NBMOT,IMOT,TCHNB
      INTEGER CHA1,ICH,IWIRE,UBIT,TDATA(260),IERR,IW,KKK
      INTEGER LOUT,TRUNIT,NGOOD31,NGOOD31P
      INTEGER I,II,GZTHIT,LSH,NCL,JBYT,JJ,LTHIT,NTOT,TDMIN,TDMAX
      REAL WG,PEDES,FDATA(260),CORE(3)
      real TRHITS(256,6),GAIN_TRD(256,3)  ! faux a changer
C
      REAL VMAX,VMIN
      REAL VASUM
      LOGICAL DOCOR,FIRST
      INTEGER J,K,DEPTH,IMI,IMS
      CHARACTER*3 C3
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        NGOOD31=0
        NGOOD31P=0
        print*,' in trdhit_u, nb de layers',nlm
        CALL EZPICK('TRDHIT_RCP')
        CALL EZGET_i('TRIGGER_TIME',DT0,IER)
        CALL EZGET_i('SUPPRESS_FLAT_CHAN',ISUPFLAT,IER)
        CALL EZGET_i('CORRECTION_<EPI>',I,IER)
        CALL UHTOC(I,3,C3,3)
        DOCOR=C3.EQ.'y' .OR. C3.EQ.'Y' .OR. C3.EQ.'YES'
        CALL VFILL(GAIN_TRD,768,1.)
        IF(DOCOR) THEN
          CALL EZGET('EPICOR',CORE(1),IER)
          DO ICH=1,3  ! Get gains
            DO IWIRE=1,256 ! faux a changer
              CALL TRGGN('BID',IWIRE,ICH,WG,IERR)
              IF(IERR.EQ.0)GAIN_TRD(IWIRE,ICH)=WG
            END DO
          END DO
        ELSE
          CORE(1)=200.
          CORE(2)=200.
          CORE(3)=200.
        END IF
        CALL EZRSET
        DEPTH = DT0 + NTFADC
        KKK = DT0 + 3
        LOUT=TRUNIT()
        LTHIT = GZTHIT( )
        IF(LTHIT.LE.0)
     +    CALL ERRMSG('Bank THIT not booked','TRDHIT',' use CDD4','W')
        FIRST = .FALSE.
      ENDIF
      CALL VZERO_i(NBTHIT,6)
      CALL VZERO(NUMTWH,6*NMWHIT)
      CALL VZERO(ENTWH,6*NMWHIT)
C  Initialize array good_wire
      DO ICH=1,3
        DO  I=1,nwire_per_layer(ich)
          GOOD_WIRE(TCHNB(I,ICH))=.FALSE.
          IF(TWCOD(TCHNB(I,ICH)))GOOD_WIRE(TCHNB(I,ICH))=.TRUE.
C          IF(ICH.EQ.3 .AND. I.LT.16 .and. twcod(i,ich))then
C           PRINT*,' LAYER',ICH,' WIRE',I,
C     &      ' GOOD_WIRE(tchnb(i,ich)) ', GOOD_WIRE(tchnb(i,ich))
C           ngood31=ngood31+1
C           end if
        END DO
      END DO
C  Check if wires with same number are coded in different layers
C  Good_wire(i,j)=.true. if wire i in layer j has no corresponding wire in other
C                         layer.
      DO 40 I=1,nwire_per_layer(1)  ! loop on hit wires in layer 1
        IMI=I-1
        IF(IMI.LE.0)IMI=nwire_per_layer(1)-IMI
        IMS=I+1
        IF(IMS.GE.257)IMS=IMS-nwire_per_layer(1)
        IF(TWCOD(TCHNB(I,1)))THEN ! Check if wire I is coded
          DO 20 J=2,3 ! check if same wires in layer 2, 3
            GOOD_WIRE(TCHNB(I,J))  =.FALSE.
            GOOD_WIRE(TCHNB(IMI,J))=.FALSE.
            GOOD_WIRE(TCHNB(IMS,J))=.FALSE.
            IF(TWCOD(TCHNB(IMI,J)) .OR. TWCOD(TCHNB(I,J)) .OR.
     &         TWCOD(TCHNB(IMS,J)))THEN
              GOOD_WIRE(TCHNB(I,1))=.FALSE.
              GOOD_WIRE(TCHNB(IMI,1))=.FALSE.
              GOOD_WIRE(TCHNB(IMS,1))=.FALSE.
C              IF (I.LE.16 .AND. J.EQ.3)PRINT*,' FIL',I,'CHAMBRE 1
C     & REJETE   CAR PRESENT DANS CHAMBRE ',J,
C     +       TWCOD(tchnb(IMI,J)), TWCOD(tchnb(I,J)),TWCOD(tchnb(IMS,J))
            END IF
   20     CONTINUE
        ELSE! 1 NON CODE
C        ! Check layer 2 against layer 3 (if wire I not coded in layer 1)
          IF(.NOT.TWCOD(TCHNB(I,2)))GO TO 40
          GOOD_WIRE(TCHNB(I,3))=.FALSE.
          GOOD_WIRE(TCHNB(IMI,3))=.FALSE.
          GOOD_WIRE(TCHNB(IMS,3))=.FALSE.
          IF(.NOT.TWCOD(TCHNB(I,3)))GO TO 40
          GOOD_WIRE(TCHNB(I,2))=.FALSE.
          GOOD_WIRE(TCHNB(IMI,2))=.FALSE.
          GOOD_WIRE(TCHNB(IMS,2))=.FALSE.
C          if(i.le.16)
C     +         PRINT*,' FIL',I,'CHAMBRE 2 REJETE
C     &          CAR PRESENT DANS CHAMBRE 3'
        END IF
   40 CONTINUE
   60 CONTINUE
C  Now check for adjacent wires in each layer
      PEDES = 0.
      WG    = 1.
      K = 1
C
      DO 100 ICH = 1,3 !Start loop on the 3 layers
        IPRIME = ICH
        J = ICH
        IF(ICH .GE. 4) THEN
          K = 2
          J = ICH-3
        ENDIF
        DO 80 IW = 1, nwire_per_layer(ich)
          IWIRE = IW
          IF(.NOT.TWCOD(TCHNB(IW,ICH)))GO TO 80 ! Check if there is
                                                !a signal on the wire
          CALL TCODER(CHA1,IPRIME-1,IWIRE-1,UBIT,2)
          CALL ZDEXPD(4,CHA1,TDATA)
          NBMOT = TDATA(1)
          PRINT*,'ICH,WIRE',ICH,IWIRE,
     &      ' nbmot',nbmot
          CALL VFLOAT(TDATA(3),FDATA,NBMOT) ! transform to real
          IMI=1
          IF(.NOT.GOOD_WIRE(TCHNB(IW,ICH)))IMI=150
          IMS=NBMOT-IMI+1
          PRINT*,'ICH,WIRE',ICH,IWIRE,
     &      ' GOOD_WIRE ',GOOD_WIRE(tchnb(IW,ICH)),' IMI',IMI,
     &      ' IMS',IMS,
     &      ' nbmot',nbmot
          IF(IMS.LE.0)GO TO 80
          IF(VMAX(FDATA(IMI),IMS)-VMIN(FDATA(IMI),IMS).LE.ISUPFLAT)THEN
            print*,' layer',ich,' fil',iw,
     &        ' rejete: vmax',VMAX(FDATA(IMI),IMS),' vmin',
     &        VMIN(FDATA(IMI),IMS),' isupflat',isupflat
            GO TO 80 ! Check for some signal in the selected range of the
                     ! FADC
          END IF
          NBTHIT(J,K) = NBTHIT(J,K)+1
          NUMTWH(NBTHIT(J,K),J,K) = IWIRE
          IF(ICH.EQ.3 .AND. IWIRE.LE.16)NGOOD31P=NGOOD31P+1
   80   CONTINUE
  100 CONTINUE
c      IF(MOD(NGOOD31,50).EQ.0)
c     +  PRINT*,' ngood31 dans trdhit_u',NGOOD31,' ngood31p',NGOOD31P
        PRINT*,' ngood31 dans trdhit_u',NGOOD31,' ngood31p',NGOOD31P
  999 RETURN
      END
