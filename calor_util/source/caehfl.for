      SUBROUTINE CAEHFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Book and fill CAEH bank starting from CAEP bank
C-
C-   Created   1-DEC-1988   Serban D. Protopopescu
C-   Updated   9-APR-1992   Harrison B. Prosper
C-      Add better error calculation
C-   Updated  30-OCT-1992   Chip Stewart  Add LV0 vertex switch
C-   Updated  16-DEC-1992   Stan M. Krzywdzinski, Harrison B. Prosper
C-      Add code to handle windows
C-   Updated   4-JAN-1993   Stan M. Krzywdzinski
C-      Add layers selection for windows
C-   Updated  26-MAR-1993   Norman Graf
C-      Allows for nonzero xy of beam spot.
C-   Updated   2-APR-1993   Stan M. Krzywdzinski
C-      Modified calls to CAEHFL_ERROR according to the changes in its
C-      argument list.
C-      Added storing sig**2(Et) in CAEH bank.
C-      Replaced vertex code by a call to CAEHFL_VERTEX routine.
C-   Updated  17-MAY-1993   Stan M. Krzywdzinski
C-      Added storing the remaining elements of error matrix:
C-       sig**2(Ez), <dExdEy>, <dExdEz>, <dEydEz>
C-   Updated  27-MAY-1993   Harrison B. Prosper
C-      Add entry point to allow creation of more than one bank
C-   Updated  25-FEB-1994   Ian Adam find LCAEH if CAEH already there;
C-      needed to load LCAEH into CSTLNK.
C-   Updated   8-SEP-1994   Meenakshi Narain
C-                          Replace CELXYZ by CELXYZ_FAST
C-   Updated  26-SEP-1995   Dhiman Chakraborty
C-                          Book and fill PNU1 bank
C-   Updated   1-OCT-1995   Dhiman Chakraborty
C-                          Apply cryostat-corrections to CAEH
C-   Updated   2-OCT-1995   Dhiman Chakraborty
C-                          Include object vertices in PNU1
C-   Updated  26-NOV-1995   Rajendran Raja
C-                          Introduce phi corrections
C-   Updated  28-NOV-1995   Dhiman Chakraborty
C-                          PNU1 bug-fix + some tune-up
C-   Updated  14-DEC-1995   Dhiman Chakraborty   
C-                          Fixed bug that led to crash in events with no
C-                          primary vertex
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:PNU1.INC'
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      INTEGER PACKED_ADDRESS
      BYTE BYTES(4)
      EQUIVALENCE (PACKED_ADDRESS,BYTES)
C----------------------------------------------------------------------
      INTEGER ICH,IER,IVADD,JJ
      INTEGER GZCAHT,GZCAEH,GZCAEP,GZPNU1,LDCAEP,LDCAEH,LPNU1
      EXTERNAL GZCAHT,GZCAEH,GZCAEP,GZPNU1
      INTEGER IETA,IPHI,LAYER,IOK,SCALE,INFO
      INTEGER NCH,NR,NRP,NV,NVADD,NVMORE,NVERSN
      INTEGER CRYO_INDEX,ICRYO
      INTEGER ND_CAHT,MIN_ND_CAHT
      PARAMETER( MIN_ND_CAHT = 4 )
C
      REAL    E,XC,YC,ZC,DIST,WT,SCALE_ICD
      REAL    XYZV(3,NVMAX),DXYZV(3,NVMAX)
      REAL    XYZV_ADD(3,NVMAX),DXYZV_ADD(3,NVMAX)
      REAL    COSX,COSY,SEXSQ,SEYSQ,SIGZV
      REAL    EXYZMAT(3,3),SETSQ,ET_CELL,XCELL,YCELL,ZCELL
      REAL    EX(NVMAX),EY(NVMAX),ERRET_SQ(NVMAX),ETSC_SAVE(NVMAX)
      REAL    ET(NVMAX),PHI(NVMAX),ET_SCALAR(NVMAX)
      REAL    ERRMAT(6,NVMAX)
      REAL    CRYO_FACTOR(3)
C
      INTEGER STATUS
      INTEGER*2 STAT2(2)
      EQUIVALENCE (STATUS,STAT2)
      LOGICAL FORCE
      LOGICAL FIRST
C----------------------------------------------------------------------
      REAL    ESUM(-NETAL:NETAL,NPHIL)
      LOGICAL APPLY_WINDOW,IN_IETA_WINDOW,IN_IPHI_WINDOW,IN_WINDOW
      INTEGER NTOT,JETA,JPHI,DETA_DPHI(2)
      LOGICAL LAYR_OK(NLYRL)
      REAL HOTTEST
      LOGICAL STORS2ET,STORMAT
      SAVE FIRST,FORCE
      LOGICAL DO_CRYO_CORR,DO_PNU1
      SAVE DO_CRYO_CORR,DO_PNU1
      LOGICAL DO_PHI_CORRECTIONS
      SAVE DO_PHI_CORRECTIONS
      LOGICAL DO_PHI_HISTS
      SAVE DO_PHI_HISTS
      REAL    ENEW
C----------------------------------------------------------------------
      DATA FIRST  /.TRUE./
      DATA FORCE  /.FALSE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('CELL_WEIGHT',WT,IER)
        CALL EZGET('APPLY_WINDOW',APPLY_WINDOW,IER)
        CALL EZGET('DO_CRYO_CORR',DO_CRYO_CORR,IER)
        CALL EZGET('DO_PNU1',DO_PNU1,IER)
        CALL EZGET('CRYO_FACTOR',CRYO_FACTOR,IER)
        CALL EZGET('DO_PHI_CORRECTIONS',DO_PHI_CORRECTIONS,IER)
        CALL EZGET('DO_PHI_HISTS',DO_PHI_HISTS,IER)
        IF ( IER .NE. 0) THEN
          APPLY_WINDOW = .FALSE.
        ELSE
          IF ( APPLY_WINDOW ) THEN
            CALL EZGET('DETA_DPHI_WINDOW',DETA_DPHI,IER)
            IF ( IER. NE. 0) APPLY_WINDOW = .FALSE.
            CALL EZGET('LAYERS_TO_BE_USED',LAYR_OK,IER)
            IF ( IER. NE. 0) APPLY_WINDOW = .FALSE.
          ENDIF
        ENDIF
        CALL EZRSET
      ENDIF
C
C ****  Check to make sure that proper CAHT exists
C
      LCAHT = GZCAHT()
      IF(LCAHT.LE.0) THEN
        CALL ERRMSG('No CAHT','CAEHFL',
     &    'Cannot create CAEH without CAHT','W')
        GOTO 888
      ENDIF
C
      ND_CAHT = IQ(LCAHT-1)
      IF(ND_CAHT.LT.MIN_ND_CAHT)THEN
        CALL MZPUSH(IXCOM,LCAHT,0,MIN_ND_CAHT-ND_CAHT,' ')
        IF(IQ(LCAHT).LT.2) IQ(LCAHT) = 2
      ENDIF
C
C ****  If FORCE is TRUE then skip the following test
C
      IF ( .NOT. FORCE ) THEN
        LCAEH=GZCAEH()
        IF(LCAEH.NE.0) GOTO 888 ! If bank exists do nothing
      ENDIF
      FORCE = .FALSE.
C
      LCAEP=GZCAEP()
      IF(LCAEP.LE.0) THEN        ! Abort if CAEP does not exist
        CALL ERRMSG('No CAEP','CAEHFL',
     &    'Cannot create CAEH without CAEP','W')
        GOTO 888
      ENDIF
C
C **** put cryo factors in CAHT
C
      IF(DO_CRYO_CORR) THEN
        CALL UCOPY(CRYO_FACTOR,Q(LCAHT+2),3)
      ELSE
        CALL VFILL(Q(LCAHT+2),3,1.0)
      ENDIF
C
      NRP=IQ(LCAEP+2)
      NCH=IQ(LCAEP+3)
C
C          book CAEH bank
      CALL BKCAEH(NCH,LCAEH)
      NVERSN=IQ(LCAEH+1)
      NR    =IQ(LCAEH+2)
      STORS2ET=(NVERSN.GE.2).AND.(NR.GE.13)
      STORMAT =(NVERSN.GE.3).AND.(NR.GE.17)
C
C *** Get position of vertices and their errors
C
      CALL CAEHFL_VERTEX(NVMAX,XYZV,DXYZV,NV,IER)
C
      IF(DO_PNU1) THEN    ! Want MWT info w.r.t. additional vertices ?
C
C***  initialize EX,EY,ET_SCALAR,ERRs and PHI
C
        CALL VZERO(EX,NVMAX)
        CALL VZERO(EY,NVMAX)
        CALL VZERO(ET_SCALAR,NVMAX)
        CALL VZERO(ETSC_SAVE,NVMAX)
        CALL VZERO(ERRET_SQ,NVMAX)
        CALL VZERO(ERRMAT,6*NVMAX)
        CALL VFILL(PHI,NVMAX,999.)
C
C *** get position of additional vertices and their errors
C
        CALL FLPNU1_VERTEX(XYZV_ADD,DXYZV_ADD,NVMORE,IER)
      ELSE
        NVMORE = 0
      ENDIF
      NVADD = MIN(NVMAX,NVMORE)
C
      IF ( APPLY_WINDOW ) THEN
C
C *** Loop over CAEP bank and find IETA, IPHI of hottest tower
C
        LCAEP=GZCAEP()
C
        NTOT=(2*NETAL+1)*NPHIL
        CALL VZERO(ESUM,NTOT)
        DO ICH=1,NCH
          LDCAEP=LCAEP+(ICH-1)*NRP
C
          PACKED_ADDRESS = IQ(LDCAEP+4)
          IETA = BYTES(BYTE4)
          IPHI = BYTES(BYTE3)
          LAYER= BYTES(BYTE2)
          IF (LAYR_OK(LAYER)) THEN
            E    = Q(LDCAEP+5)
            ESUM(IETA,IPHI) = ESUM(IETA,IPHI) + E
          ENDIF
        ENDDO
C
        HOTTEST =-999999.0
        JETA = 0
        JPHI = 0
        DO IETA = -NETAL ,  NETAL
          DO IPHI = 1 ,  NPHIL
            IF ( ESUM(IETA,IPHI) .GT. HOTTEST ) THEN
              HOTTEST = ESUM(IETA,IPHI)
              JETA = IETA
              JPHI = IPHI
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
      LCAEP = GZCAEP()
C
C ****  fill CAEH bank
C
      DO ICH = 1,NCH
        LDCAEP = LCAEP+(ICH-1)*NRP
C
        PACKED_ADDRESS = IQ(LDCAEP+4)
        IETA  = BYTES(BYTE4)
        IPHI  = BYTES(BYTE3)
        LAYER = BYTES(BYTE2)
        INFO  = BYTES(BYTE1)
C
C ****  Get scale (x8 or x1)
C
        IF ( BTEST(INFO,1) ) THEN
          SCALE = 1
        ELSE
          SCALE = 0
        ENDIF
C
        E = Q(LDCAEP+5)
        IF ( DO_CRYO_CORR ) THEN
          CRYO_INDEX = ICRYO(IETA,IPHI,LAYER)
          IF((CRYO_INDEX.GE.1).AND.(CRYO_INDEX.LE.3))THEN
            E = E*CRYO_FACTOR(CRYO_INDEX)
          ENDIF
        ENDIF
C
        IF ( DO_PHI_HISTS ) THEN
          CALL CSF_HISTS(ICH,NCH,IETA,IPHI,LAYER,E)
        ENDIF
C
        IF ( DO_PHI_CORRECTIONS ) THEN
          CALL CSF_SCALE(ICH,IETA,IPHI,LAYER,E,ENEW)
          E=ENEW
        ENDIF
C
        PTCAEP(IETA,IPHI,LAYER) = ICH  ! set pointers
        LDCAEH = LCAEH+(ICH-1)*NR
C
        CALL CELXYZ_FAST(IETA,IPHI,LAYER,XCELL,YCELL,ZCELL,IOK)
C
C
C ***       Use 1-st vertex to offset
C
        XC = XCELL-XYZV(1,1)
        YC = YCELL-XYZV(2,1)
        ZC = ZCELL-XYZV(3,1)
C
        IF(IOK.EQ.0) THEN
          IF (NV .GT. 0) THEN
            SIGZV = DXYZV(3,1)*DXYZV(3,1)
          ELSE
            SIGZV = 0.
          ENDIF
C
          CALL VZERO(EXYZMAT,9)
          IF ( APPLY_WINDOW ) THEN
            IN_WINDOW = IN_IETA_WINDOW(IETA,JETA,DETA_DPHI(1)) .AND.
     &                  IN_IPHI_WINDOW(IPHI,JPHI,DETA_DPHI(2)) .AND.
     &                  LAYR_OK(LAYER)
            IF ( IN_WINDOW ) THEN
              CALL CAEHFL_ERROR(IETA,IPHI,LAYER,SCALE,E,XC,YC,ZC,SIGZV,
     &          EXYZMAT,SETSQ,IER)
              IF(IER.EQ.0) THEN
                SEXSQ = EXYZMAT(1,1)
                SEYSQ = EXYZMAT(2,2)
              ELSE
                SEXSQ = 0.
                SEYSQ = 0.
                SETSQ = 0.
                CALL VZERO(EXYZMAT,9)
              END IF
            ELSE
              SEXSQ = 0.0
              SEYSQ = 0.0
              SETSQ = 0.0
              CALL VZERO(EXYZMAT,9)
              E   = 0.0
            ENDIF
          ELSE
            CALL CAEHFL_ERROR(IETA,IPHI,LAYER,SCALE,E,XC,YC,ZC,SIGZV,
     &        EXYZMAT,SETSQ,IER)
            IF(IER.EQ.0) THEN
              SEXSQ = EXYZMAT(1,1)
              SEYSQ = EXYZMAT(2,2)
            ELSE
              SEXSQ = 0.
              SEYSQ = 0.
              SETSQ = 0.
              CALL VZERO(EXYZMAT,9)
            END IF
          ENDIF
C
          DIST=SQRT(XC*XC+YC*YC+ZC*ZC)
          COSX=XC/DIST
          COSY=YC/DIST
C
          Q(LDCAEH+4)=E*COSX
          Q(LDCAEH+5)=E*COSY
          Q(LDCAEH+6)=E*ZC/DIST
          Q(LDCAEH+7)=E
          Q(LDCAEH+8)=E*SQRT(XC*XC+YC*YC)/DIST
          Q(LDCAEH+9) = SEXSQ
          Q(LDCAEH+10)= SEYSQ
          STATUS=0
          IF(STORS2ET) Q(LDCAEH+16)= SETSQ
          IF(STORMAT) THEN
            Q(LDCAEH+17) = EXYZMAT(3,3)
            Q(LDCAEH+18) = EXYZMAT(1,2)
            Q(LDCAEH+19) = EXYZMAT(1,3)
            Q(LDCAEH+20) = EXYZMAT(2,3)
          ENDIF
        ELSE
          Q(LDCAEH+4) = 0.0
          Q(LDCAEH+5) = 0.0
          Q(LDCAEH+6) = 0.0
          Q(LDCAEH+7) = E
          Q(LDCAEH+8) = 0.0
          Q(LDCAEH+9) = 0.0
          Q(LDCAEH+10)= 0.0
          STAT2(WORD2)=1
          IF(STORS2ET) Q(LDCAEH+16)= 0.0
          IF(STORMAT) THEN
            Q(LDCAEH+17) = 0.0
            Q(LDCAEH+18) = 0.0
            Q(LDCAEH+19) = 0.0
            Q(LDCAEH+20) = 0.0
          ENDIF
        ENDIF
C
        Q(LDCAEH+11)  = WT
        IQ(LDCAEH+12) = IETA
        IQ(LDCAEH+13) = IPHI
        IQ(LDCAEH+14) = LAYER
        IQ(LDCAEH+15) = STATUS
        IF((NVADD.LE.0).OR.(.NOT.DO_PNU1))GOTO 777  ! No PNU1 to be formed
C if not desired or there are no additional Primary Vertices in the event
C
C ***  calculate MET for each additional primary vertex
C
        DO IVADD = 1,NVADD
C
          XC = XCELL-XYZV_ADD(1,IVADD)
          YC = YCELL-XYZV_ADD(2,IVADD)
          ZC = ZCELL-XYZV_ADD(3,IVADD)
          SIGZV = DXYZV(3,IVADD)*DXYZV(3,IVADD)
C
          CALL VZERO(EXYZMAT,9)
          CALL CAEHFL_ERROR(IETA,IPHI,LAYER,SCALE,E,XC,YC,ZC,SIGZV,
     &          EXYZMAT,SETSQ,IER)
          IF(IER.NE.0) CALL VZERO(EXYZMAT,9)
C
          DIST=SQRT(XC*XC+YC*YC+ZC*ZC)
          COSX=XC/DIST
          COSY=YC/DIST
          ET_CELL = E*SQRT(XC*XC+YC*YC)/DIST
          EX(IVADD) = EX(IVADD) + E*COSX
          EY(IVADD) = EY(IVADD) + E*COSY
          ET_SCALAR(IVADD) = ET_SCALAR(IVADD) + ET_CELL
          IF((CRYO_INDEX.GE.1).AND.(CRYO_INDEX.LE.3))THEN
            ETSC_SAVE(IVADD) = ETSC_SAVE(IVADD) + ET_CELL
            ERRET_SQ(IVADD) = ERRET_SQ(IVADD) + SETSQ
            ERRMAT(1,IVADD) = ERRMAT(1,IVADD) + EXYZMAT(1,1)
            ERRMAT(2,IVADD) = ERRMAT(2,IVADD) + EXYZMAT(2,2)
            ERRMAT(3,IVADD) = ERRMAT(3,IVADD) + EXYZMAT(3,3)
            ERRMAT(4,IVADD) = ERRMAT(4,IVADD) + EXYZMAT(1,2)
            ERRMAT(5,IVADD) = ERRMAT(5,IVADD) + EXYZMAT(1,3)
            ERRMAT(6,IVADD) = ERRMAT(6,IVADD) + EXYZMAT(2,3)
          ENDIF   ! For error matrix, scale non-ICD/MG values by ETSC ratio
        ENDDO
  777   CONTINUE
      ENDDO
C
  750 LPNU1 = GZPNU1()
      IF(LPNU1.GE.0)CALL MZDROP(IXCOM,LPNU1,' ')  ! Drop if already exists
      IF(NVADD.LE.0) GOTO 887           !  No need to make an empty bank
C
      CALL BKPNU1(NVADD,LPNU1)          !  Book PNU1 bank ...
      IF(LPNU1.LE.0) THEN
        CALL ERRMSG('BKPNU1 failed','CAEHFL',
     &    'Could not book PNU1, continuing','W')
        GOTO 887
      ENDIF
C
C ... and fill it
C
      IQ(LPNU1+1) = 1        !  Version number
      IQ(LPNU1+2) = NR_PNU1       !  Repetition number
      IQ(LPNU1+3) = NVADD    !  Number of additional primary vertices
      DO IVADD = 1,NVADD
        ET(IVADD) = SQRT(EX(IVADD)**2 + EY(IVADD)**2 + .0001)
        IF(EX(IVADD).NE.0.) PHI(IVADD) = ATAN2(-EY(IVADD),-EX(IVADD))
        IF(PHI(IVADD).LT.0.) PHI(IVADD) = PHI(IVADD) + TWOPI
        Q(LPNU1+NOFF_PNU1+(IVADD-1)*NR_PNU1+1) = XYZV_ADD(3,IVADD)
        Q(LPNU1+NOFF_PNU1+(IVADD-1)*NR_PNU1+2) = ET(IVADD)
        Q(LPNU1+NOFF_PNU1+(IVADD-1)*NR_PNU1+3) = PHI(IVADD)
        Q(LPNU1+NOFF_PNU1+(IVADD-1)*NR_PNU1+4) = ET_SCALAR(IVADD)
        SCALE_ICD = ABS(ET_SCALAR(IVADD)/ETSC_SAVE(IVADD))
        IF(ERRET_SQ(IVADD).GT.0.)THEN
          Q(LPNU1+NOFF_PNU1+(IVADD-1)*NR_PNU1+5) =
     &      SQRT(ERRET_SQ(IVADD) * SCALE_ICD)
        ELSE  ! Watch out for -ve values of this word
          Q(LPNU1+NOFF_PNU1+(IVADD-1)*NR_PNU1+5) =
     &      ERRET_SQ(IVADD) * SCALE_ICD
        ENDIF
        DO JJ = 1,6
          Q(LPNU1+NOFF_PNU1+(IVADD-1)*NR_PNU1+5+JJ) =
     &      ERRMAT(JJ,IVADD) * SCALE_ICD
        ENDDO
      ENDDO
C
  887 PTZFLG=.FALSE.           ! set flag indicating PTCAEP is not 0
C
  888 RETURN
C
      ENTRY CAEHFL_FORCE
      FORCE = .TRUE.
  999 RETURN
      END
