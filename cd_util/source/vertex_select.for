      SUBROUTINE VERTEX_SELECT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select primary vertex based on information
C-                         from particle objects.
C-
C-   Inputs  : VERT banks
C-   Outputs : Flag 'VERTEX_SWAP'
C-             The Flag 'VERTEX_SWAP' is set if the vertices *need* to be
C-             swapped. This does not imply that the vertices ARE swapped.
C-             The swap is based on the weight associated with each vertex
C-             (word Q(LVERT+19)).
C-             The swap is controlled by the RCP switch 'DO_SWAP_VERTICES'.
C-
C-   Controls:
C-
C-   Created   5-AUG-1995   Srini Rajagopalan
C-   Updated  24-SEP-1995   Srini Rajagopalan  Add em_vertex info into VERT
C-   Updated  11-OCT-1995   Srini Rajagopalan  Add vert_mtc argument 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
C
      LOGICAL   USE_EM,USE_MU,USE_JET
      LOGICAL   DO_SWAP_VERTICES
      LOGICAL   EZERROR
      LOGICAL   FIRST /.TRUE./
C
      INTEGER   I,K,NR,IER
      INTEGER   LVERH,GZVERH,LVERT,GZVERT
      INTEGER   NEM,NMU,NJET
      INTEGER   EM_VERT_ID(30),MU_VERT_ID(30),JET_VERT_ID(30)
      INTEGER   OBJ_VERT,TRK_VERT
C
      REAL      VERT_MTC(30)
      REAL      WT_EM_VERT,WT_MU_VERT,WT_JET_VERT
      REAL      SUM_EM_VERT,SUM_MU_VERT,SUM_JET_VERT
      REAL      NUM_OBJ,FNTRK
      REAL      MAXWT,ZCLUS
      REAL      EM_VERT(2,3)
      REAL      BEAM_POS(3),BEAM_SLOP(2),BEAM_ERR(2)
      REAL      BMSL_ERR(2), IP_SIGMA(2)
      REAL      XVER,YVER,ZVER,DXVER,DYVER,DZVER
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VERTEX_FIX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('BAD RCP','VERTEX_SELECT',
     &                'Unable to find bank VERTEX_FIX_RCP','F')
        ENDIF
        CALL EZGET('DO_SWAP_VERTICES',DO_SWAP_VERTICES,IER)
        IF (IER.EQ.0) CALL EZGET('USE_EM',USE_EM,IER)
        IF (IER.EQ.0) CALL EZGET('USE_MU',USE_MU,IER)
        IF (IER.EQ.0) CALL EZGET('USE_JET',USE_JET,IER)
        IF (IER.EQ.0) CALL EZGET('WT_EM_VERT',WT_EM_VERT,IER)
        IF (IER.EQ.0) CALL EZGET('WT_MU_VERT',WT_MU_VERT,IER)
        IF (IER.EQ.0) CALL EZGET('WT_JET_VERT',WT_JET_VERT,IER)
        IF (IER.NE.0) CALL ERRMSG('Bad VERTEX_FIX RCP','VERTEX_SELECT',
     &                'RCP parameter not found','F')
        CALL EZRSET
C
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('BAD RCP','VERTEX_SELECT',
     &                'Unable to find bank VERTEX_RCP','F')
        ENDIF
        CALL EZGET('NR',NR,IER)
        IF (IER.EQ.0) CALL EZGET('BEAM_POS',BEAM_POS,IER)
        IF (IER.EQ.0) CALL EZGET('BEAM_ERR',BEAM_ERR,IER)
        IF (IER.EQ.0) CALL EZGET('BEAM_SLOP',BEAM_SLOP,IER)
        IF (IER.EQ.0) CALL EZGET('BMSL_ERR',BMSL_ERR,IER)
        IF (IER.EQ.0) CALL EZGET('IP_SIGMA',IP_SIGMA, IER)
        IF (IER.NE.0) CALL ERRMSG('Bad VERTEX RCP','VERTEX_SELECT',
     &                'RCP parameter not found','F')
        CALL EZRSET
C
      ENDIF
C
C  set SWAP flag to false on entry
C
      CALL FLGSET('VERTEX_SWAP',.FALSE.)
C
C Information from electrons...
C
      IF (USE_EM) THEN
        CALL EM_VERTEX_INFO(NEM,EM_VERT_ID,EM_VERT)
        SUM_EM_VERT = 0.
        DO I = 1,NEM
          IF (EM_VERT_ID(I).GT.0) THEN
            LVERT = GZVERT(EM_VERT_ID(I))
            IF (LVERT.GT.0) THEN
              Q(LVERT+19) = Q(LVERT+19) + WT_EM_VERT
              SUM_EM_VERT = SUM_EM_VERT + WT_EM_VERT
              IQ(LVERT) = IQ(LVERT) + 1       ! Increment bits 0-4
            ELSE
              CALL ERRMSG('BAD LINK','VERTEX_FIX',
     &                    'EM object points to unknown VERT bank','W')
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C Information from Muons...
C
      IF (USE_MU) THEN
        CALL MU_VERTEX_INFO(NMU,MU_VERT_ID,VERT_MTC)
        SUM_MU_VERT = 0.
        DO I = 1,NMU
          IF (MU_VERT_ID(I).GT.0) THEN
            LVERT = GZVERT(MU_VERT_ID(I))
            IF (LVERT.GT.0) THEN
              Q(LVERT+19) = Q(LVERT+19) + WT_MU_VERT
              SUM_MU_VERT = SUM_MU_VERT + WT_MU_VERT
              IQ(LVERT) = IQ(LVERT) + 32       ! Increment bits 5-9
            ELSE
              CALL ERRMSG('BAD LINK','VERTEX_FIX',
     &                    'MU object points to unknown VERT bank','W')
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C Information from Jets...
C
      IF (USE_JET) THEN
        CALL JET_VERTEX_INFO(NJET,JET_VERT_ID)
        SUM_JET_VERT = 0.
        DO I = 1,NJET
          IF (JET_VERT_ID(I).GT.0) THEN
            LVERT = GZVERT(JET_VERT_ID(I))
            IF (LVERT.GT.0) THEN
              Q(LVERT+19) = Q(LVERT+19) + WT_JET_VERT
              SUM_JET_VERT = SUM_JET_VERT + WT_JET_VERT
              IQ(LVERT) = IQ(LVERT) + 1024       ! Increment bits 10-15
            ELSE
              CALL ERRMSG('BAD LINK','VERTEX_FIX',
     &                    'JET object points to unknown VERT bank','W')
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C Sum up all objects (weighted) determine the primary vertex
C The vertex with larger number of tracks is picked if two vertices have
C equal number of objects pointing to it.
C
      NUM_OBJ = SUM_EM_VERT + SUM_MU_VERT + SUM_JET_VERT
C
      TRK_VERT = 1                      ! Primary z ala tracking
      OBJ_VERT = 0                      ! Primary z ala objects
      LVERH = GZVERH()
      IF (NUM_OBJ .GT. 0.0) THEN
        LVERT = LQ(LVERH - IZVERT)
        DO WHILE (LVERT.GT.0)
          FNTRK = FLOAT(IBITS(IQ(LVERT+2),8,8))/100.
          Q(LVERT+19) = Q(LVERT+19) + FNTRK
          LVERT = LQ(LVERT)
        ENDDO
C
C Loop over vertices, determine primary vertex based on word 19 (max wt)
C
        MAXWT = 0.0
        LVERT = LQ(LVERH - IZVERT)
C
        DO WHILE (LVERT.GT.0)
          IF (Q(LVERT+19).GT.MAXWT) THEN
            MAXWT = Q(LVERT+19)
            OBJ_VERT = IQ(LVERT - 5)
          ENDIF
          LVERT = LQ(LVERT)
        ENDDO
C
C If primary vertex has changed, set  swap flag
C
        IF (OBJ_VERT.NE.1) THEN
          CALL FLGSET('VERTEX_SWAP',.TRUE.)
          CALL MVBITS(1,0,1,IQ(LVERH),0)
        ENDIF
C
C Swap vertices based on objects if needed
C
        IF (DO_SWAP_VERTICES) THEN
          LVERT = LQ(LVERH - IZVERT)
          CALL ZSORT(IXCOM,LVERT,19)
          CALL ZTOPSY(IXCOM,LVERT)
C
C Renumber the vertices
C
          K = 0
          DO WHILE (LVERT.GT.0)
            K = K + 1
            IF (IQ(LVERT - 5).EQ.1) TRK_VERT = K
            IQ(LVERT - 5) = K
            LVERT = LQ(LVERT)
          ENDDO
C
          OBJ_VERT = 1                   !  Effect of reordering
          CALL MVBITS(1,0,1,IQ(LVERH),1)
        ENDIF
C
      ELSE
        CALL ERRMSG('No objects','VERTEX_FIX',
     &              'No EM, MU or JET objects in this event','I')
      ENDIF
C
C
C fill in the primary vertex in VERH
C
      CALL MVBITS(TRK_VERT,0,4,IQ(LVERH),8)
      CALL MVBITS(OBJ_VERT,0,4,IQ(LVERH),12)
C
C
C book and fill "other" vertices:
C
C  Add EM_VERT information into VERT banks
C
      IF (USE_EM) THEN
        DO I = 1,2
          IF (EM_VERT(I,1).NE.-999.) THEN
            IF (I.EQ.2 .AND.
     &         (EM_VERT(1,1).EQ.EM_VERT(2,1)) ) THEN
              Q(LVERT+19) = 2.0               ! double weight if same electron
              GO TO 100
            ENDIF
            CALL BKVERT(LVERT,NR)
            IF (LVERT.LE.0) GO TO 100
C
            CALL MVBITS(1,0,5,IQ(LVERT),0)
            CALL MVBITS(1,0,3,IQ(LVERT+2),16) ! set electron vertex bit

            XVER = BEAM_POS(1) + BEAM_SLOP(1)*ZVER
            YVER = BEAM_POS(2) + BEAM_SLOP(2)*ZVER
            ZVER = EM_VERT(I,1)
            DXVER = SQRT(BEAM_ERR(1)**2 + ((ZVER - BEAM_POS(3)) *
     &                   BMSL_ERR(1))**2 + IP_SIGMA(1)**2)
            DYVER = SQRT(BEAM_ERR(2)**2 + ((ZVER - BEAM_POS(3)) *
     &                   BMSL_ERR(2))**2 + IP_SIGMA(2)**2)
            DZVER = EM_VERT(I,2)
            ZCLUS = EM_VERT(I,3)
C
            Q(LVERT+3) = XVER                 ! x
            Q(LVERT+4) = YVER                 ! y
            Q(LVERT+5) = ZVER                 ! z
            Q(LVERT+6) = DXVER                ! err_x
            Q(LVERT+7) = DYVER                ! err_y
            Q(LVERT+8) = DZVER                ! err_z
            Q(LVERT+9) =  1.                  ! correlation matrix elem. cxx
            Q(LVERT+10) = 0.                  ! cxy
            Q(LVERT+11) = 0.                  ! cxz
            Q(LVERT+12) = 1.                  ! cyy
            Q(LVERT+13) = 0.                  ! cyz
            Q(LVERT+14) = 1.                  ! czz
            Q(LVERT+15) = ZCLUS               ! Cal. Cluster Position
            Q(LVERT+19) = 1.0                 ! weight
          ENDIF
        ENDDO
      ENDIF
  100 CONTINUE
C
  999 RETURN
      END
