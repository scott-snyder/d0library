      SUBROUTINE MU_ASTUB
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Locate and list A-layer stubs.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   3-JUN-1993   Tom Diehl
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
     
      INTEGER PDT(28)
      INTEGER NUMCENT,NMUOH_CENT(20),MUOH_CENT(20,16),ICENT(28)
      INTEGER IHIT(28,20,16),NHITS(28,20)
      INTEGER I,J,K,L
      INTEGER IPADHIT,IBEST      !Pad soln stuff.
      INTEGER NUM_STUBS,NUM_STUBS_MULTCOPIES,NUM_STUBS_ABCCOPIES
      INTEGER IVER,NV
      INTEGER MUQUAD,NH,IFW1,IFW2,IFW3,IQUAD

      INTEGER IDSOL(40),JADD(40),IPOINT(40),IPAD(40),IDELT(40) ! MHTT

      REAL X,Y,Z,DX,DY,DZ
      REAL XCENT(28,20),YCENT(28,20),ZCENT(28,20)
      REAL DXCENT(28,20),DYCENT(28,20),DZCENT(28,20)

      REAL P,PX,PY,PZ,PT,DP
      REAL VERTEX(3)
      REAL SLIX,SLIY,COSTHX,COSTHY,COSTHZ
      REAL PHI,TWOPI,THETA,ETA

      REAL CT,BDL,ELFE,ELCAL,CHI2N,CHI2D
      REAL COSTHX_OUT,COSTHY_OUT,COSTHZ_OUT,XINTMAG,YINTMAG,ZINTMAG

      INTEGER IWADD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1
      REAL DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR    !GTMUOH

      LOGICAL OK,IAMCOPY,MAKE_ASTUB_DIAGNOSTICS,DIAG_SETUP,WRITE_MUOT
      LOGICAL IAMPUNCH

      DATA TWOPI/6.283185307/

      DATA PDT/016,026,036,010,020,030,011,021,031,
     $         015,025,035,013,023,033,012,022,032,
     $         061,062,064,065,067,
     $         091,092,094,095,097/

      DATA MAKE_ASTUB_DIAGNOSTICS/.truE./,DIAG_SETUP/.falsE./
      DATA WRITE_MUOT/.TRUE./
C----------------------------------------------------------------------
C
C This section for onetime setup calculations. It is commented out to
C save space in Level 2. 
C
C      IF(MAKE_ASTUB_DIAGNOSTICS.AND..NOT.DIAG_SETUP) THEN
C        CALL HCDIR('//PAWC',' ')       ! go to top directory
C        CALL HMDIR('//PAWC/MSTU','S')  ! create SUB-DIRECTORY
C        CALL HCDIR('//PAWC/MSTU',' ')  ! go to MUON directory
C        CALL HBOOK1(1,'NUMBER STUBS$',20,0.,20.,0.)
C        CALL HBOOK1(2,'NUMBER STUBS ABC COPIES$',20,0.,20.,0.)
C        CALL HBOOK1(3,'NUMBER HITS PER STUB$',12,0.,12.,0.)
C        CALL HBOOK1(4,'STUB PDT$',100,0.,100.,0.)
C        CALL HBOOK1(5,'NUMBER STUBS MULT COPIES$',20,0.,20.,0.)
C        CALL HBOOK1(11,'STUB MATCH$',2,0.,2.,0.)
C        CALL HBOOK1(12,'STUB MISSED$',2.,0.,2.,0.)
C        DIAG_SETUP = .TRUE.
C      ENDIF
C
C      CALL HCDIR('//PAWC/MSTU',' ')  ! go to MUON directory
C
C This section for once per event setups.
C
      NUM_STUBS = 0                 
      NUM_STUBS_MULTCOPIES = 0      
      NUM_STUBS_ABCCOPIES = 0       
C
C Loop over 28 A-layer PDTs. Loop over centroids. Store results in arrays.
C
      DO I = 1,28
        ICENT(I) = 0
        CALL CENT_FIND(PDT(I),NUMCENT,NMUOH_CENT,MUOH_CENT,OK)
        DO J = 1,NUMCENT
C          IF(MAKE_ASTUB_DIAGNOSTICS) THEN
C            CALL HFILL(3,FLOAT(NMUOH_CENT(J)),0.,1.)
C            CALL HFILL(4,FLOAT(PDT(I)),0.,1.)
C          ENDIF
          NUM_STUBS = NUM_STUBS + 1
          CALL MU_SCA_CEN_POS(J,NMUOH_CENT(J),MUOH_CENT,
     $                  IPADHIT,IBEST,X,Y,Z,DX,DY,DZ,OK)
          IF(OK) THEN
            ICENT(I) = ICENT(I) + 1
            XCENT(I,ICENT(I)) = X
            YCENT(I,ICENT(I)) = Y
            ZCENT(I,ICENT(I)) = Z
            DXCENT(I,ICENT(I)) = DX
            DYCENT(I,ICENT(I)) = DY
            DZCENT(I,ICENT(I)) = DZ
            NHITS(I,ICENT(I)) = NMUOH_CENT(J)
            DO K = 1,NMUOH_CENT(J)
              IHIT(I,ICENT(I),K) = MUOH_CENT(J,K)
            ENDDO
          ENDIF                   ! OK
        ENDDO                     ! Loop over NUM_CENT.
      ENDDO                       ! Loop over 28 PDTs.
C
C Loop over PDTS and stubs. Quality checks on stubs.
C Calculate some physical params, stuff into MUOT
C
      DO I = 1,28
        DO J = 1,ICENT(I)
          CALL VERXYZ(IVER,VERTEX,NV)
          SLIX = XCENT(I,J)/(ZCENT(I,J)-VERTEX(3))
          SLIY = YCENT(I,J)/(ZCENT(I,J)-VERTEX(3))
          P = 1.0
          DP = 1.
          COSTHZ = 1./SQRT(1.+SLIX**2.+SLIY**2.)
          IF(VERTEX(3).GT.ZCENT(I,J)) THEN
            COSTHZ = -COSTHZ
          ENDIF
          COSTHX = SLIX*COSTHZ
          COSTHY = SLIY*COSTHZ
          PX = P * COSTHX
          PY = P * COSTHY
          PZ = P * COSTHZ
          PT = SQRT(PX**2. + PY**2.)
          PHI = ATAN2(PY,PX)
          IF(PHI.LT.0.0) PHI = PHI + TWOPI
          THETA = ATAN2(PT,PZ)
          ETA = -ALOG(TAN(THETA/2.))
          IQUAD = MUQUAD(PDT(I))
          CALL MU_STUB_FLAG_ABCCOPY(I,J,NHITS,IHIT,IAMCOPY)
          IF(IAMCOPY) NUM_STUBS_ABCCOPIES = NUM_STUBS_ABCCOPIES + 1
          CALL MU_STUB_FLAG_PUNCH(I,J,NHITS,IHIT,IAMPUNCH)
          IFW1 = 5
          IFW2 = 0
          IF(IAMCOPY) THEN
            IFW2 = IFW2 + 4096 
          ENDIF
          IFW3 = 0

          IF(WRITE_MUOT) THEN
            NH = NHITS(I,J)
            XINTMAG = 0.
            YINTMAG = 0.
            ZINTMAG = 0.
            COSTHX_OUT = 0.
            COSTHY_OUT = 0.
            COSTHZ_OUT = 0.
            CHI2D = 0.
            CHI2N = 0.
            ELCAL = 0.
            ELFE = 0.
            BDL = 0.
            CT = 0.
            DO L = 1,NH
              IPOINT(L) = IHIT(I,J,L)
              CALL GTMUOH(IHIT(I,J,L),IWADD,IFWH1,IFWH2,INRAW,IORIEN,
     $            NHWIR,CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     $            DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
              JADD(L) = IWADD
              IDSOL(L) = 0
              IDELT(L) = 0
              IPAD(L) = 0
            ENDDO                     ! End substitutes.

            CALL MUMUOT(NH,IQUAD,IFW1,IFW2,IFW3,XCENT(I,J),YCENT(I,J),
     $             ZCENT(I,J),XINTMAG,YINTMAG,ZINTMAG,COSTHX,
     $             COSTHY,COSTHZ,COSTHX_OUT,COSTHY_OUT,
     $             COSTHZ_OUT,CHI2D,CHI2N,P,DP,ELCAL,
     $             ELFE,BDL,CT,JADD,IPOINT,IDSOL,IDELT,IPAD)
          ENDIF                       ! WRITE_MUOT

C          IF(MAKE_ASTUB_DIAGNOSTICS) THEN
C            CALL HFILL(1,FLOAT(NUM_STUBS),0.,1.)
C            CALL HFILL(2,FLOAT(NUM_STUBS_ABCCOPIES),0.,1.)
C            CALL HFILL(5,FLOAT(NUM_STUBS_MULTCOPIES),0.,1.)
C            CALL HCDIR('//PAWC',' ') !RETURN TO TOP LEVEL DIRECTORY
C          ENDIF
        ENDDO                         ! Stubs
      ENDDO                           ! PDTs

  999 RETURN
      END
