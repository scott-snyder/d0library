      SUBROUTINE PJPGET(N,ALG,ETCUT,DRCUT,MAXIT,IR,MUON,SPLMRG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GETS PJET PARAMETERS - SETS PARAMETERS TO DEFAULTS
C-   IF PJPSET IS NOT CALLED PRIOR TO PJPGET
C-
C-   Inputs  : NONE
C-   Outputs : N     = NUMBER OF PJET ALGORITHMS
C-             ALG   = ALGORITHM TPYE 'CONE' OR 'ANGLE'
C-             ETCUT = ET CUT FR PJET JETS
C-             DRCUT = CONE CUT (IN R OR ANGLE)
C-             MAXIT = MAXIMUM NUMBER OF ITERATIONS
C-             IR    = INIT RADIATION FLAG 0=USE IR,   1=NO IR
C-             MUON  = MUON FLAG           0=NO MUONS, 1=use MUONS in PJET
C-             SPLMRG = EKS MERGING CONE CUT (SPLMRG > 0.)
C-                    = SHARED ET FRACTION CUT (-SPLMRG > 0.)
C-   Controls: NONE
C-
C-   Created  14-DEC-1989   Chip Stewart
C-   Updated  13-NOV-1992   Brent May / Andy Milder
C-      added SPLMRG (splitting/merging cut)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXALG
      PARAMETER( MAXALG = 30 )
      REAL ETCUT(*),DRCUT(*),SPLMRG(*)
      INTEGER MAXIT(*), IR(*), MUON(*)
      CHARACTER ALG(*)*(*)
      INTEGER N,I
      REAL ETCUTL(MAXALG),DRCUTL(MAXALG),SPLMRGL(MAXALG)
      INTEGER MAXITL(MAXALG), IRL(MAXALG), MUONL(MAXALG)
      CHARACTER ALGL(MAXALG)*8
      INTEGER NL
C----------------------------------------------------------------------
      SAVE ETCUTL,DRCUTL,MAXITL,IRL,ALGL,NL,MUONL,SPLMRGL
      DATA ETCUTL(1),DRCUTL(1),MAXITL(1),IRL(1),ALGL(1),MUONL(1),NL,
     &  SPLMRGL(1)
     &  /        5.0,     0.45,        1,    0 , 'CONE',      0, 1 , 0./
      N = NL
      DO I = 1, N
        IF ( I .LE. MAXALG)   THEN
          ETCUT(I) = ETCUTL(I)
          DRCUT(I) = DRCUTL(I)
          SPLMRG(I) = SPLMRGL(I)
          MAXIT(I) = MAXITL(I)
          IR(I)    = IRL(I)
          MUON(I)  = MUONL(I)
          ALG(I)   = ALGL(I)
        END IF
      END DO
      RETURN
C
C ****  ENTRY PJPSET SET PJET PARAMETERS TO VALUES INPUT.
C
      ENTRY PJPSET(N,ALG,ETCUT,DRCUT,MAXIT,IR,MUON,SPLMRG)
      NL = N
      DO I = 1, NL
        IF ( I .LE. MAXALG)   THEN
          ETCUTL(I) = ETCUT(I)
          DRCUTL(I) = DRCUT(I)
          SPLMRGL(I) = SPLMRG(I)
          MAXITL(I) = MAXIT(I)
          IRL(I)    = IR(I)
          MUONL(I)    = MUON(I)
          ALGL(I)   = ALG(I)
        END IF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
