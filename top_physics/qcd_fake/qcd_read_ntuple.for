      SUBROUTINE QCD_READ_NTUPLE(TOP_DIRECTORY,NTUPLE_ID,IEVENT,EOF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : READ IN NTUPLE
C-
C-   Inputs  : TOP_DIRECTORY = DIRECTORY NAME
C-             NTUPLE_ID = ID OF NTUPLE
C-   Outputs : EOF = TRUE WHEN NO MORE
C-   Controls: 
C-
C-   Created  24-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOP_DIRECTORY
      INTEGER NTUPLE_ID
      INTEGER IEVENT
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE.INC'
C
      INTEGER STATUS
      LOGICAL EOF
C
      INTEGER I,J
C----------------------------------------------------------------------
      CALL NTUPLE_GET(TOP_DIRECTORY,NTUPLE_ID,IEVENT,XDUMMY,STATUS)
      IF ( STATUS.NE.0 ) THEN
        EOF = .TRUE.
      ELSE
C LOAD COMMON BLOCK.
        NELE = NPELC
        NJETS = UNJETS  !JNEP number of jets
        ELE(1,1) = EE1
        ELE(2,1) = ETAE1
        ELE(3,1) = PHIE1
        ELE(1,2) = EE2
        ELE(2,2) = ETAE2
        ELE(3,2) = PHIE2
        NEUT(1) = METC1*COS(METPHIC1)
        NEUT(2) = METC1*SIN(METPHIC1)
C
        JET(1,1) = EJ51
        JET(2,1) = ETAJ51
        JET(3,1) = PHIJ51
        JET(1,2) = EJ52
        JET(2,2) = ETAJ52
        JET(3,2) = PHIJ52
        JET(1,3) = EJ53
        JET(2,3) = ETAJ53
        JET(3,3) = PHIJ53
        JET(1,4) = EJ54
        JET(2,4) = ETAJ54
        JET(3,4) = PHIJ54
        JET(1,5) = EJ55
        JET(2,5) = ETAJ55
        JET(3,5) = PHIJ55
C
        NELE = MIN(NELE,NELMX)
        NJETS = MIN(NJETS,NJTMX)
C
        DO I = 1 , NELE
          CALL GET_CART(ELE(1,I),ELEC(1,I),0.0)
        ENDDO
        DO I = 1 , NJETS
          CALL GET_CART(JET(1,I),JETS(1,I),0.0)
        ENDDO
        DO I = 1 , 2
          REST(I) = 0.0
          DO J = 1 , NELE
            REST(I) = REST(I) + ELEC(I,J)
          ENDDO
          DO J = 1 , NJETS
            REST(I) = REST(I) + JETS(I,J)
          ENDDO
          REST(I) = REST(I) + NEUT(I)
          REST(I) = - REST(I)
        ENDDO
      REST_PT = SQRT(REST(1)**2 + REST(2)**2)
C
      ENDIF
  999 RETURN
      END
