      SUBROUTINE GET_PARTONS_IN_RECO(CONSIZ,MAXPAR,
     &                           NRECO,RETA,RPHI,NPART,PETA,PPHI,
     &                           NP,PTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find partons belonging to jets, within a
C-                         specified cone size
C-
C-   Inputs  :
C-              CONSIZ        [R] - cone size in (eta,phi) space
C-              MAXPAR        [I] - maximum number of partons belonging
C-                                  with a jet
C-              NRECO         [I] - number of reconstructed jets
C-              RETA(*)       [R] - eta of reconstructed jets
C-              RPHI(*)       [R] - phi of reconstructed jets
C-              NPART         [I] - number of partons
C-              PETA(*)       [R] - eta of partons
C-              PPHI(*)       [R] - phi of partons
C-   Outputs :
C-              NP(IR)        [I] - number of partons belonging to
C-                                  jet IR  ( IR = 1, NRECO )
C-              PTR(JP,IR)    [I] - JP pointer to parton arrays for
C-                                  a jet IR ( JP = 1, NP(IR) )
C-   Controls:
C-
C-   Created  13-OCT-1994   Stan M. Krzywdzinski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER MAXPAR,NRECO,NPART,NP(*),PTR(MAXPAR,*)
      REAL    CONSIZ,RETA(*),RPHI(*),PETA(*),PPHI(*)
C----------------------------------------------------------------------
      INTEGER IR,JP
      REAL    DR,PHI_DIFFERENCE
C----------------------------------------------------------------------
C     IF (MAXPAR.LE. 0) GO TO 999
      IF (NRECO .LE. 0) GO TO 999
      IF (NPART .LE. 0) GO TO 999
C
      CALL VZERO(NP,NRECO)
C
      DO IR = 1, NRECO
        DO JP = 1, NPART
          DR = SQRT( (RETA(IR)-PETA(JP))**2 +
     &               PHI_DIFFERENCE(RPHI(IR),PPHI(JP))**2 )
          IF (DR .LE. CONSIZ) THEN
            NP(IR) = NP(IR) + 1
            IF ( NP(IR) .LE. MAXPAR ) THEN
              PTR(NP(IR),IR) = JP
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
  999 RETURN
      END
