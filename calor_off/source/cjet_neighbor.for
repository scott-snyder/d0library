      FUNCTION CJET_NEIGHBOR ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Shell for neighbor algorithm jet finder.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-OCT-1989   Gerald C. Blazey, Harrison B. Prosper
C-   Modified 10-OCT-1989   Gerald C.Blazey
C-                          Add SCEXP to CACLFL list.
C-   Updated  11-DEC-1989   Harrison B. Prosper
C-      Added call to CJET_POSTCLUSTER
C-   Updated  11-JAN-1990   Harrison B. Prosper
C-      Removed arguments from CJET_POSTCLUSTER etc.
C-   Updated 16-MAR-1990  N.A. Graf Added EM ratio cut variables
C-   Updated  13-SEP-1990   Harrison B. Prosper
C-      Added code for CAPH
C-   Updated   2-OCT-1990   Chip Stewart
C-      Added new CAJETS RCP format
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CJET_NEIGHBOR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
C----------------------------------------------------------------------
      INTEGER GZCAEP,GZCATE
      INTEGER IER,NJETS,NCLUST,NUMBER,I
C
      INTEGER MXCACL,NREP
      REAL SCEXP,THRET,THREM,ETMIN,RNUMBER,X
      INTEGER HADRONIC
      PARAMETER( HADRONIC = 2  )        ! Hadronic clusters
C
      INTEGER ISCEXP,ITHREM,IETMIN,ITHRET
      EQUIVALENCE ( ISCEXP , SCEXP )
      EQUIVALENCE ( ITHRET, THRET )
      EQUIVALENCE ( ITHREM, THREM )
      EQUIVALENCE ( IETMIN, ETMIN )
C
      EQUIVALENCE (NUMBER,RNUMBER)
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
      OK = .TRUE.
C
      IF ( NPARAMS.NE.NPARAMS_NN) THEN
        CALL ERRMSG
     &    ('CJET_NEIGHBOR PARAMETERS WRONG','CJET_NEIGHBOR',
     &    'Unable to FILL CAPH','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
C
      ITHRET = ALG_PARAMS(8)       ! EZGSET('CLUSTER_ET_THRESHOLD',THRET,1)
      ITHREM = ALG_PARAMS(9)       ! EZGET('CLUSTER_EM_RATIO_THRESHOLD',THREM
      IETMIN = ALG_PARAMS(10)      ! EZGSET('JET_ET_THRESHOLD',ETMIN,1)
      ISCEXP = ALG_PARAMS(11)      ! EZGSET('POWER_FOR_SHOWER_CENTER',SCEXP,1)
      MXCACL = ALG_PARAMS(12)      ! EZGSET('MAXIMUM_CAL_CLUSTERS',MXCACL,1)
C
C ****  Check for CAEP (Unpacked energy by cell)
C
      LCAEP = GZCAEP()
      IF ( LCAEP.EQ.0 ) THEN
        CALL ERRMSG('CALORIMETER','CJET_NEIGHBOR',
     &    'CAEP BANK NOT SET UP','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
C
C ****  Check for Tower bank
C
      LCATE = GZCATE()
      IF ( LCATE.EQ.0 ) THEN
        CALL ERRMSG('CALORIMETER','CJET_NEIGHBOR',
     &    'CATE BANK NOT SET UP','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
C
C ****  Book/Fill CAPH bank for Nearest-Neighbor algorithm
C
      CALL BKCAPH(LCAPH)
      IF ( LCAPH .LE. 0 ) THEN
        CALL ERRMSG
     &    ('CALORIMETER','CJET_NEIGHBOR','Unable to book CAPH','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
      CALL CAPHFL_INT(K_ALGORITHM,A_NN_JET)
C
C ****  FILL CAPH JET PARAMETERS 
C
      DO I = 3, NPARAMS
        NUMBER  = ALG_PARAMS(I)
        X = 0
        IF(TYPE_PARAMS(I).EQ.1) THEN
          X = NUMBER
        ELSE IF(TYPE_PARAMS(I).EQ.4) THEN
          X = -1
        ELSE IF(TYPE_PARAMS(I).LT.4) THEN        
          X = RNUMBER
        END IF
        CALL CAPHFL_REAL (K_BASE + I - 2 , X )
      END DO
C
C ****  Find pre-clusters in CATE bank
C
      CALL CJET_PRECLUSTER
C
      CALL CACLFL(HADRONIC,SCEXP,NCLUST)      ! Fill CACL,CACH BANKS
C
      CALL CLDROP(MXCACL,THRET,THRET,THREM)     ! drop unwanted clusters
C
C ****  Connect pre-clusters into jet-clusters
C
      CALL CJET_POSTCLUSTER
C
C ****  Cut jets below ETMIN & Make JETS banks using  CMAP
C
      CALL CACL_TO_JETS (ETMIN,NJETS)
C
  999 CONTINUE
      CJET_NEIGHBOR = OK
      RETURN
      END
