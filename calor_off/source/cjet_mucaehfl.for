      SUBROUTINE CJET_MUCAEHFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Book and fill CAEH bank starting from CAEP bank. Same as CAEHFL 
C-     except CAEH will be filled regardless of whether CAEH already 
C-    exists
C-
C-   CREATED : 21-FEB-93 : ALEX SMITH 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZPLV0.LINK'
C----------------------------------------------------------------------
      INTEGER PACKED_ADDRESS
      BYTE BYTES(4)
      EQUIVALENCE (PACKED_ADDRESS,BYTES)
C----------------------------------------------------------------------
      INTEGER I,J,IER
      INTEGER GZCAEH,GZCAEP,LDCAEP,LDCAEH
      INTEGER IETA,IPHI,LAYER,IOK,SCALE,INFO
      INTEGER NCH,NR,NALOC,NRP,NV,GZISV1,GZPROC,LPLV0
      REAL    E,XC,YC,ZC,DIST,WT
      REAL    ZV(10),DZV(10)
      REAL    COSX,COSY,EABS,SEX,SEY,SIGZV
C
      INTEGER STATUS
      INTEGER*2 STAT2(2)
      EQUIVALENCE (STATUS,STAT2)
      LOGICAL FIRST,LMONTE,LV0
C
C----------------------------------------------------------------------
      REAL    ESUM(-NETAL:NETAL,NPHIL)
      LOGICAL APPLY_WINDOW,IN_IETA_WINDOW,IN_IPHI_WINDOW,IN_WINDOW
      INTEGER NTOT,JETA,JPHI,DETA_DPHI(2)
      LOGICAL LAYR_OK(NLYRL)
      REAL HOTTEST
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('CELL_WEIGHT',WT,IER)
        CALL EZGET_l('USE_MONTE_CARLO_VERTEX',LMONTE,IER)
        CALL EZGET_l('USE_LEVEL_ZERO_VERTEX',LV0,IER)
        CALL EZGET_l('APPLY_WINDOW',APPLY_WINDOW,IER)
        IF ( IER .NE. 0) THEN
          APPLY_WINDOW = .FALSE.
        ELSE
          IF ( APPLY_WINDOW ) THEN
            CALL EZGET_iarr('DETA_DPHI_WINDOW',DETA_DPHI,IER)
            IF ( IER. NE. 0) APPLY_WINDOW = .FALSE.
            CALL EZGET_larr('LAYERS_TO_BE_USED',LAYR_OK,IER)
            IF ( IER. NE. 0) APPLY_WINDOW = .FALSE.
          ENDIF
        ENDIF
        CALL EZRSET
      ENDIF
C
CC      IF(GZCAEH().NE.0) GOTO 999    ! If bank exists do nothing
C
      LCAEP=GZCAEP()
      IF(LCAEP.LE.0) THEN        ! Abort if CAEP does not exist
        CALL ERRMSG('No CAEP','CJET_MUCAEHFL',
     &    ' Trying to create CAEH without CAEP','W')
        GOTO 999
      ENDIF
      NRP=IQ(LCAEP+2)
      NCH=IQ(LCAEP+3)
C
C          book CAEH bank
      CALL BKCAEH(NCH,LCAEH)
      NR=IQ(LCAEH+2)
C
C ****  check LMONTE switch to use ISV1 vertex or VERT bank vertex
C
      IF ( LMONTE ) THEN
        IF(GZISV1().LE.0) THEN
          LMONTE = .FALSE.
          CALL EZPICK('CAHITS_RCP')
          CALL EZSET('USE_MONTE_CARLO_VERTEX',LMONTE,IER)
          CALL EZRSET
          CALL ERRMSG('No ISV1 Vertices','CJET_MUCAEHFL',
     &      ' TRYING VERT BANK','W')
        ELSE
          CALL ZVERTX(ZV,DZV)                ! Isajet Vertex
        END IF
      ELSE IF (LV0) THEN
        LPROC = GZPROC ()
        ZV(1)=0.0
        LV0 = .FALSE.
        IF( LPROC.LE.0) THEN
          CALL ERRMSG('No PROC','CJET_MUCAEHFL','PLV0 
     &      missing -try VERT','W')
        ELSE
          LPLV0 = LQ(LPROC-IZPLV0)
          IF(LPLV0.LE.0) THEN
            CALL ERRMSG('No PLV0','CJET_MUCAEHFL','PLV0 missing- 
     &        try VERT','W')
          ELSE
            LV0 = .TRUE.
            IF (BTEST(IQ(LPLV0+1),5)) THEN  ! GOOD SLOW Z
              ZV(1) = Q(LPLV0+3)  !slow Z
              DZV(1) = 7.0        !Jeff Bantley's guess 10/22/92
            ELSE                  !fast Z
              ZV(1) = Q(LPLV0+2)
              DZV(1) = 15.0
            END IF
          END IF
        END IF
      END IF
      IF (.NOT. LMONTE .AND. .NOT.LV0) THEN
        CALL ZVERTE(NV,ZV,DZV)                ! Vertex from tracking
        IF(NV.EQ.0) THEN
          CALL ERRMSG('No Vertices','CJET_MUCAEHFL','z set to 0','W')
          ZV(1)=0.0
        ENDIF
      ENDIF
C
C ****  Loop over CAEP bank and find IETA, IPHI of hottest
C ****  tower
C
      IF ( APPLY_WINDOW ) THEN
        LCAEP=GZCAEP()
C
        NTOT=(2*NETAL+1)*NPHIL
        CALL VZERO(ESUM,NTOT)
        DO I=1,NCH
          LDCAEP=LCAEP+(I-1)*NRP
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
      LCAEP=GZCAEP()
C
C
C ****  fill CAEH bank
C
      DO I=1,NCH
        LDCAEP=LCAEP+(I-1)*NRP
C
        PACKED_ADDRESS = IQ(LDCAEP+4)
        IETA = BYTES(BYTE4)
        IPHI = BYTES(BYTE3)
        LAYER= BYTES(BYTE2)
        INFO = BYTES(BYTE1)
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
        PTCAEP(IETA,IPHI,LAYER)=I  ! set pointers
        LDCAEH=LCAEH+(I-1)*NR
C
        CALL CELXYZ(IETA,IPHI,LAYER,XC,YC,ZC,IOK)
C
        IF(IOK.EQ.0) THEN
C
          ZC    = ZC-ZV(1)
          SIGZV = DZV(1)*DZV(1)
C
          IF ( APPLY_WINDOW ) THEN
            IN_WINDOW = IN_IETA_WINDOW(IETA,JETA,DETA_DPHI(1)) .AND.
     &                  IN_IPHI_WINDOW(IPHI,JPHI,DETA_DPHI(2)) .AND.
     &                  LAYR_OK(LAYER)
            IF ( IN_WINDOW ) THEN
              CALL CAEHFL_ERROR(IETA,IPHI,LAYER,SCALE,E,XC,YC,ZC,SIGZV,
     &          SEX,SEY,IER)
              IF(IER.NE.0) THEN
                SEX = 0
                SEY = 0
              END IF
            ELSE
              SEX = 0.0
              SEY = 0.0
              E   = 0.0
            ENDIF
          ELSE
            CALL CAEHFL_ERROR(IETA,IPHI,LAYER,SCALE,E,XC,YC,ZC,SIGZV,
     &        SEX,SEY,IER)
            IF(IER.NE.0) THEN
              SEX = 0
              SEY = 0
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
          Q(LDCAEH+9) = SEX
          Q(LDCAEH+10)= SEY
          STATUS=0
        ELSE
          Q(LDCAEH+4) = 0.0
          Q(LDCAEH+5) = 0.0
          Q(LDCAEH+6) = 0.0
          Q(LDCAEH+7) = E
          Q(LDCAEH+8) = 0.0
          Q(LDCAEH+9) = 0.0
          Q(LDCAEH+10)= 0.0
          STAT2(WORD2)=1
        ENDIF
C
        Q(LDCAEH+11)  = WT
        IQ(LDCAEH+12) = IETA
        IQ(LDCAEH+13) = IPHI
        IQ(LDCAEH+14) = LAYER
        IQ(LDCAEH+15) = STATUS
C
      ENDDO
C
      PTZFLG=.FALSE.           ! set flag indicating PTCAEP is not 0
  999 RETURN
      END
