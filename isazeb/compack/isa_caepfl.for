      FUNCTION ISA_CAEPFL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Generate a CAEP bank using ISAJET banks
C-       default is not to smear for resolution
C-
C-   Returned value  : true if succesfull
C-
C-   ENTRY ISA_CAEP_DIA
C-     dialog to change cutoffs and request smearing
C-
C-   Created  27-JUN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ISA_CAEPFL,ISA_CAEP_DIA
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INTEGER NCLMAX
      PARAMETER (NCLMAX=20)
      INTEGER IETAC(NCLMAX),IPHIC(NCLMAX),LAYERC(NCLMAX),PAKADR
      INTEGER LISV1,ID,NCELL,IETA,IPHI,LAYER,LOW,HIGH,NR
      INTEGER NALOC,NCH,LCAEP,GZCAEP,LDCAEP,IDABS,I,IDV,LISP1
      INTEGER OK,IEH,IPRFIL
      REAL    P(4),PHI,TH,ETA,PTOT,DIR(3),VTX(3),PCUT(2)
      CHARACTER*4 PATH
      LOGICAL SMEAR,DO_SHOWER
C&IF VAXVMS
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C&ENDIF
      SAVE PCUT,SMEAR,DO_SHOWER
      DATA PCUT/.05,.20/
      DATA SMEAR,DO_SHOWER/.FALSE.,.TRUE./
C----------------------------------------------------------------------
C
      ISA_CAEPFL=.FALSE.
      CALL PATHGT(PATH)
      IF(PATH.NE.'FAKE') THEN
        CALL ERRMSG('Path not set to FAKE','ISA_CAEPFL',
     &    'Cannot proceed to make CAEP','W')
        GOTO 999
      ENDIF
C
      NCH=0
      NALOC=25000
      LCAEP=GZCAEP()
      IF(LCAEP.GT.0) CALL MZDROP(IXCOM,LCAEP,' ')  ! will remake CAEP
      CALL BKCAEP(NALOC,LCAEP)
      NR=IQ(LCAEP+2)
C
C              loop through vertices and particles
C
      LISV1=0
C
   50 CALL GTISV1(LISV1,LISV1,IDV,P,VTX(1),VTX(2),VTX(3)) 
      IF(LISV1.GT.0) THEN 
        LISP1=LISV1-IZISP1
C
   60   CALL GTISP1(LISP1,LISP1,ID,P,PHI,TH,ETA) ! loop over particles
        IF(LISP1.GT.0) THEN 
          IDABS=IABS(ID)
C                  skip neutrinos and muons
          IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15) GOTO 60
          IF(IDABS.EQ.14) GOTO 60
C                  find whether e.m. or hadronic energy
          IF(IDABS.EQ.10.OR.IDABS.EQ.12) THEN  ! e.m. energy
            LOW=2
            HIGH=7
            IEH=0
          ELSE                                 ! had. energy
            LOW=7
            HIGH=14
            IEH=1
          ENDIF
C
          IF(P(4).LT.PCUT(2-IEH)) GOTO 60   
          PTOT=SQRT(P(1)**2+P(2)**2+P(3)**2)
          DIR(1)=P(1)/PTOT
          DIR(2)=P(2)/PTOT
          DIR(3)=P(3)/PTOT
          CALL CLINPH(VTX,DIR,NCLMAX,NCELL,IETAC,IPHIC,LAYERC,OK)
          IF(NCELL.LE.0) GOTO 60
C
          IF(DO_SHOWER) THEN          ! longitudinal profile per Bock
            CALL ISA_PROFIL(NCELL,IETAC,IPHIC,LAYERC,IEH,TH,P,NCH)
C
          ELSE                        ! all energy in one cell 
            CALL ISA_ONE_CELL(NCELL,IETAC,IPHIC,LAYERC,LOW,HIGH,P,NCH)
          ENDIF
          IF(NCH.GT.NALOC) THEN         ! no more room
            CALL ERRMSG('Not enough room','ISA_CAEP',
     &      'Not enough channels booked for CAEP','W')
            GOTO 999
          ENDIF
C
          GOTO 60
        ENDIF
C
        GOTO 50
      ENDIF
C
      IQ(LCAEP+3)=NCH  ! no. of channels
      IF(NCH.LT.NALOC) THEN        ! reduce bank size if warranted
        NALOC=NR*NCH+3-NR*NALOC-3
        CALL MZPUSH(IXCOM,LCAEP,0,NALOC,'I')
      ENDIF
C
C         smear if requested
      IF(SMEAR) THEN
        LCAEP=GZCAEP()
        DO NCELL=1,NCH
          LDCAEP=LCAEP+(NCELL-1)*NR
          PAKADR=IQ(LDCAEP+4)
C&IF VAXVMS
          LAYER=BYTES(2)
          BYTES(1)=1                ! smeared
C&ENDIF
          IQ(LDCAEP+4)=PAKADR
          I=1                                  ! e.m.
          IF(LAYER.GT.10) I=2                  ! hadronic
          IF(LAYER.GT.7.AND.LAYER.LT.11) I=3   ! EC/CC transition
          CALL ISA_SMEAR(I,Q(LDCAEP+5))
        ENDDO
      ENDIF
      PTZFLG=.FALSE.
      ISA_CAEPFL=.TRUE.
C
        RETURN
C
        ENTRY ISA_CAEP_DIA
C
        ISA_CAEP_DIA=.TRUE.
        SMEAR=.FALSE.
        CALL GETPAR(1,' Smear pseudo-calorimeter data? [N] >',
     &  'L',SMEAR)
        PCUT(1)=.05
        CALL GETPAR(1,' e.m. energy cutoff [.05]>','R',PCUT(1))
        PCUT(2)=.20
        CALL GETPAR(-1,' had. energy cutoff [.20]>','R',PCUT(2))
        DO_SHOWER=.TRUE.
        CALL GETPAR(1,' Generate long. showers? [Y]>','L',DO_SHOWER)
 999    RETURN
        END
