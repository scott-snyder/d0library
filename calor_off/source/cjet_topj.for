      FUNCTION CJET_TOPJ ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Hybrid NN/Kt algorithm for multijet finding
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: CAJETS_RCP
C-
C-   Created 1-JUN-1993   Chip Stewart
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CJET_TOPJ
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER GZCAEP,GZCATE
      INTEGER IER,NJETS,NCLUST,NUMBER,I
C
      INTEGER MXCACL,NREP
      REAL SCEXP,THREN,THRET,THREM,ETMIN,RNUMBER,X
      INTEGER HADRONIC,ND
      PARAMETER( HADRONIC = 2  )        ! Hadronic clusters
C
      INTEGER ISCEXP,ITHREN,ITHREM,IETMIN,ITHRET
      EQUIVALENCE ( ISCEXP , SCEXP )
      EQUIVALENCE ( ITHREN, THREN )
      EQUIVALENCE ( ITHRET, THRET )
      EQUIVALENCE ( ITHREM, THREM )
      EQUIVALENCE ( IETMIN, ETMIN )
C
      EQUIVALENCE (NUMBER,RNUMBER)
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
      SAVE FIRST,THREN,THRET,THREM,ETMIN,MXCACL
C----------------------------------------------------------------------
C
      OK = .TRUE.
      IF(FIRST) THEN
        FIRST = .FALSE.
C
        IF ( NPARAMS.NE.NPARAMS_TOPJ) THEN
          CALL ERRMSG('CJET_TOPJ PARAMETERS WRONG','CJET_TOPJ',
     &    'Unable to FILL CAPH','W')
          OK = .FALSE.
          GOTO 999
        ENDIF
C
      END IF
      ITHRET = ALG_PARAMS(5)
      IETMIN = ALG_PARAMS(6)
      ISCEXP = ALG_PARAMS(7)
C
C ****  Check for CAEP (Unpacked energy by cell)
C
      LCAEP = GZCAEP()
      IF ( LCAEP.EQ.0 ) THEN
        CALL ERRMSG('CALORIMETER','CJET_TOPJ',
     &    'CAEP BANK NOT SET UP','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
C
C ****  Check for Tower bank
C
      LCATE = GZCATE()
      IF ( LCATE.EQ.0 ) THEN
        CALL ERRMSG('CALORIMETER','CJET_TOPJ',
     &    'CATE BANK NOT SET UP','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
C
C ****  Book/Fill CAPH bank for Nearest-Neighbor algorithm
C
      CALL BKCAPH(LCAPH)
      ND = IQ(LCAPH-1)
C      IF(ND.LT.16) CALL MZPUSH(IXCOM,LCAPH,0,16-ND,'I')  ! push to 16 words
      IF ( LCAPH .LE. 0 ) THEN
        CALL ERRMSG
     &    ('CALORIMETER','CJET_TOPJ','Unable to book CAPH','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
      CALL CAPHFL_INT(K_ALGORITHM,A_TOPJ_JET)
      CALL CAPHFL_INT(K_VERSION,1)
C
C ****  FILL CAPH JET PARAMETERS
C
      DO I = 2, NPARAMS     !only 15 parameters
        NUMBER  = ALG_PARAMS(I)
        X = 0
        IF(TYPE_PARAMS(I).EQ.1) THEN
          X = NUMBER
        ELSE IF(TYPE_PARAMS(I).EQ.4) THEN
          X = -1
        ELSE IF(TYPE_PARAMS(I).LT.4) THEN
          X = RNUMBER
        END IF
        CALL CAPHFL_REAL (K_BASE + I - 1 , X )
      END DO
C
C ****  Find pre-clusters in CATE bank
C
      CALL CJET_TOPJNN
C
      CALL CACLFL(HADRONIC,SCEXP,NCLUST)      ! Fill CACL,CACH BANKS
C
C Drop tiny CACL's 
C
      CALL CLDROP(255,THRET,THRET,0.0)     ! drop unwanted clusters
C
C ****  Connect pre-clusters into jet-clusters
C
      CALL CJET_TOPJKT
C
C ****  Cut jets below ETMIN & Make JETS banks using  CMAP
C
      CALL CACL_TO_JETS (ETMIN,NJETS)
C
  999 CONTINUE
      CJET_TOPJ = OK
      RETURN
      END
