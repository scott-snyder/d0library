      SUBROUTINE GTESUM_MATCH(REF,DTA,ID,CSP,NMX,MRF,MDT,NMC,MATCH,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Match objects of a given type in one ESUM
C-                          bank with those in another
C-
C-   Inputs  :
C-              REF   [C*4]   ESUM bank from which objects are to be matched
C-                            (REF bank is the one where the best-quality
C-                            objects are expected to be found, eg ISAE or RECO)
C-              DTA   [C*4]   ESUM bank from which objects are to be matched
C-              ID(2) [I]     ID of objects to match:
C-                              ID(1) - object type from REF bank
C-                              ID(2) - object type from DTA bank
C-                                See ESUM.PARAMS for object types:
C-                                INCLUDE 'D0$PARAMS:ESUM.PARAMS'
C-              CSP   [R]     Cosine of angle within which objects
C-                            are considered to be matched
C-              NMX   [I]     Maximum # of objects to match
C-
C-   Outputs :
C-         MRF(NMX)   [L]     Status of REF objects --- .TRUE., if matched
C-                                                      .FALSE. otherwise
C-         MDT(NMX)   [L]     Status of DTA objects --- .TRUE., if matched
C-                                                      .FALSE. otherwise
C-              NMC   [I]     Total # of matches found
C-   MATCH(NMX*NMX,2) [I]     Matrix with serial #s of matched objects
C-                              MATCH(I,1) -   Serial # of object from REF
C-                              MATCH(I,2) -   Serial # of object from DTA
C-                                You can call GTESUM(REF,ID(1),MATCH(I,1),...)
C-                                to find the matched object from the REF ESUM
C-                                bank
C-
C-              IER   [I]     Error code:
C-                               0 --- OK
C-                              -1 --- No ESUM bank of DTA type
C-                              -2 --- No ESUM bank of REF type
C-                              -3 --- # of objects in DTA > NMX
C-                              -4 --- # of objects in REF > NMX
C-                              -5 --- No objects of type requested in DTA
C-                              -6 --- No objects of type requested in REF
C-                              -7 --- Can't ask for matching all objects
C-                              -8 --- Vertices and ETSUM are not included
C-                              -9 --- ETMISS cannot be matched with other
C-                                     objects
C-
C-   Note:                      more than one from one bank may
C-                              match one object in the other
C-
C-   Created  18-FEB-1992   M.V.S. Rao
C-   Updated  27-FEB-1992   M.V.S. Rao  elaborate 'inputs' and 'outputs'
C-   Updated  28-FEB-1992   M.V.S. Rao  different object IDs for REF and DTA
C-   Updated  26-JUN-1992   Meenakshi Narain  change for compatibilty with V3
C-                          *NOTE* no backwards compatibility with V2.
C-   Updated  20-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*(*) REF,DTA
      CHARACTER*80 MSG
      INTEGER ID(2),NMC,NMX,MATCH(NMX*NMX,2),IER,I
      INTEGER NFOUND_REF( ID_ALL : LAST_TYPE ),FLAGR,FLAGD,IER_SUB
      INTEGER NFOUND_DTA( ID_ALL : LAST_TYPE ),J,K
      LOGICAL MRF(NMX),MDT(NMX)
      REAL    CSP,COSP
      REAL    ETR,PHYS_ETAR,DET_ETAR,PHIR,THETAR
      REAL    ETD,PHYS_ETAD,DET_ETAD,PHID,THETAD
      REAL    EL_REF,EM_REF,EN_REF,EL_DTA,EM_DTA,EN_DTA
      REAL    EL,EM,EN,THETA,PHI
      REAL    ETA_FROM_THETA, THETA_FROM_ETA, RETA, RTHETA
C
C----------------------------------------------------------------------
C
C ****  Statement functions
C
      EL(THETA,PHI) = SIN(THETA)*COS(PHI)
      EM(THETA,PHI) = SIN(THETA)*SIN(PHI)
      EN(THETA) = COS(THETA)
      THETA_FROM_ETA(RETA ) = 2*ATAN(EXP(-(RETA)))
      ETA_FROM_THETA(RTHETA) =  -ALOG(MAX(TAN((RTHETA)/2.),1.E-9))
C
C----------------------------------------------------------------------
C
C ****  Initialize
C
      IER = 0                           ! OK
      DO I=1,2
C
C ****  check if ID = -1                ! all objects
C
        IF ( ID(I).EQ.-1 ) THEN
          IER = -7                        ! can't ask for matching all objects
          GO TO 999
        ENDIF
C
C ****  VERTEX and ETSUM
C

        IF ( ID(I).EQ.0.OR.ID(I).EQ.7 ) THEN
          IER = -8                        ! vertices and ETSUM are not included
          GO TO 999
        ENDIF

      ENDDO

C
C ****  MISSING ET
C
      IF (ID(1).EQ.6 .OR.ID(2).EQ.6) THEN
        IF (ID(1).NE.ID(2)) THEN
          IER = -9
          GOTO 999
        END IF
      END IF
C
C ****  Initialize arrays
C
      NMC = 0
      DO I = 1,NMX
        MRF(I) = .FALSE.
        MDT(I) = .FALSE.
        DO J = 1,NMX
          DO K = 1,2
            MATCH(NMX*(I-1)+J,K) = 0
          ENDDO
        ENDDO
      ENDDO
C
C ****  Fetch # of objects from REF bank
C
      CALL GTESUM_COUNTS (REF,NFOUND_REF,IER_SUB)
      IF ( IER_SUB.NE.0 ) THEN
        IER = -2                        ! no ESUM bank of REF type
        WRITE(MSG,'(A,I5)')'IER from GTESUM_COUNTS = ',IER_SUB
        CALL ERRMSG('GTESUM_COUNTS_IER','GTESUM_MATCH',MSG,'W')
      ENDIF
      IF ( NFOUND_REF(ID(1)).EQ.0 ) THEN
        IER = -6                        ! no objects of type requested in REF
        GO TO 999
      ENDIF
      IF ( NFOUND_REF(ID(1)).GT.NMX ) THEN
        IER = -4                        ! # of objects in REF > NMX
        GO TO 999
      ENDIF
C
C ****  Fetch # of objects from DTA bank
C
      CALL GTESUM_COUNTS (DTA,NFOUND_DTA,IER_SUB)
      IF ( IER_SUB.NE.0 ) THEN
        IER = -1                        ! no ESUM bank of DTA type
        WRITE(MSG,'(A,I5)')'IER from GTESUM_COUNTS = ',IER_SUB
        CALL ERRMSG('GTESUM_COUNTS_IER','GTESUM_MATCH',MSG,'W')
       ENDIF
      IF ( NFOUND_DTA(ID(2)).EQ.0 ) THEN
        IER = -5                        ! no objects of type requested in DTA
        GO TO 999
      ENDIF
      IF ( NFOUND_DTA(ID(2)).GT.NMX ) THEN
        IER = -3                        ! # of objects in DTA > NMX
        GO TO 999
      ENDIF
C
C ****  Loop over objects in REF bank
C
      DO I = 1,NFOUND_REF(ID(1))
        CALL GTESUM (REF,ID(1),I,ETR,PHYS_ETAR,DET_ETAR,PHIR,
     &               FLAGR,IER_SUB)
C
C ****  Direction cosines of the REF object
C ****  reset eta = 0. for MISSING_ET    
C
        IF (ID(1).EQ.6) DET_ETAR = 0.      
        THETAR = THETA_FROM_ETA(DET_ETAR)
        EL_REF = EL(THETAR,PHIR)
        EM_REF = EM(THETAR,PHIR)
        EN_REF = EN(THETAR)

C
C ****  Loop over objects in DTA bank
C
        DO J = 1,NFOUND_DTA(ID(2))
          CALL GTESUM (DTA,ID(2),J,ETD,PHYS_ETAD,DET_ETAD,PHID,
     &                 FLAGD,IER_SUB)
C
C ****  Direction cosines of the DTA object
C ****  reset eta = 0. for MISSING_ET    
C
          IF (ID(2).EQ.6) DET_ETAD = 0.      
          THETAD = THETA_FROM_ETA(DET_ETAD)
          EL_DTA = EL(THETAD,PHID)
          EM_DTA = EM(THETAD,PHID)
          EN_DTA = EN(THETAD)
C
C ****  Check if objects match and fill output matrices
C
          COSP = EL_REF*EL_DTA+EM_REF*EM_DTA+EN_REF*EN_DTA
          IF ( COSP.GE.CSP ) THEN
            NMC = NMC+1
            MATCH(NMC,1) = I
            MATCH(NMC,2) = J
            MRF(I) = .TRUE.
            MDT(J) = .TRUE.
          ENDIF
        ENDDO
      ENDDO
  999 RETURN
      END
