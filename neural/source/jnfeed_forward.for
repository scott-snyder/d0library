      SUBROUTINE JNFEED_FORWARD
     &  (M,MV0,MM0,NG,NL,DTINV,TINV,T,W,A,O,OIN,OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the weights and other JETNET V3.0
C-   parameters for a feed-forward neural network compute the outputs
C-   O(*) for a given set of inputs OIN(*). See JETNET V3.0
C-   documentation for details.
C-
C-   Inputs  : M(0:10)  [I]
C-             MV0(11)  [I]
C-             MM0(11)  [I]
C-             NG(10)   [I]   Sigmoid type (by layer)
C-             NL       [I]   Number of hidden layers
C-
C-             DTINV    [F]   Default inverse temperature (PARJN(3))
C-             TINV(10) [F]   Inverse temperature (by layer)
C-             T(*)     [F]   Thresholds
C-             W(*)     [F]   Weights
C-             A(*)     [F]   Work array
C-             O(*)     [F]   Work array
C-
C-             OIN(*)   [F]   Inputs
C-
C-   Outputs : OUT(*)   [F]   Outputs
C-   Controls:
C-
C-   Created   5-FEB-1995   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER M(0:10), MV0(11), MM0(11), NG(10), NL
      REAL    DTINV, TINV(10), A(*), T(*), W(*), O(*), OIN(*), OUT(*)
C----------------------------------------------------------------------
      INTEGER I,J,IL,MI,MIJ
      REAL    GJN, BETA
C----------------------------------------------------------------------
C
C ****  Get inverse temperature for first hidden layer
C
      IF ( TINV(1).EQ.0.0 ) THEN
        BETA = DTINV
      ELSE
        BETA = ABS(TINV(1))
      ENDIF
C
C ****  Compute outputs for first hidden layer
C
      DO I = 1, M(1)
        A(I)=T(I)
        MIJ=I-M(1)
        DO J=1,M(0)
          MIJ=MIJ+M(1)
          A(I)=A(I)+W(MIJ)*OIN(J)
        ENDDO
        O(I)=GJN(I,BETA*A(I),NG(1))
      ENDDO
C
C ****  Feed outputs through the net
C
      DO IL = 2, NL
C
C **** Set beta in layer IL
C
        IF ( TINV(IL).EQ.0.0 ) THEN
          BETA = DTINV
        ELSE
          BETA = ABS(TINV(IL))
        ENDIF
C
C **** Compute outputs for nodes in layer IL
C
        DO I=1, M(IL)
          MI=MV0(IL)+I
          A(MI)=T(MI)
          MIJ=MM0(IL)-M(IL)+I
          DO J=MV0(IL-1)+1, MV0(IL-1)+M(IL-1)
            MIJ=MIJ+M(IL)
            A(MI)=A(MI)+W(MIJ)*O(J)
          ENDDO
          O(MI)=GJN(MI,BETA*A(MI),NG(IL))
        ENDDO
      ENDDO
C
      DO I=1,M(NL)
        OUT(I)=O(MV0(NL)+I)
      ENDDO
C
  999 RETURN
      END
