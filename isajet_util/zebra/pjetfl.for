      SUBROUTINE PJETFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank PJET
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   7-NOV-1989 18:10:09.84  Chip Stewart
C-   Updated  13-NOV-1992 Andy Milder - Added CAJETS like jet finding
C-      options 'PTCL' (particle cone) and 'PRTN' (parton cone)
C-   Updated  13-NOV-1992 Brent May  - Added call to PJ_EKS to apply
C-      optional EKS merging to PJETS (after PJCONE and PJPART only)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKPJET.INC'
C
      INTEGER I,J,NJET,GZISAQ
      LOGICAL FIRST,OPEN_ANGLE,DR_CONE
      INTEGER MAXALG
      PARAMETER( MAXALG = 20 )
      REAL ETCUT(MAXALG),DRCUT(MAXALG),SPLMRG(MAXALG)
      INTEGER MAXIT(MAXALG), IR(MAXALG), MUON(MAXALG)
      CHARACTER ALG(MAXALG)*20
      INTEGER N
C----------------------------------------------------------------------
C
C ****  Activate PJET temporary link area
C
      CALL PJLINK (PJLON)
C
C
C ****  GET ALGORITHM PARAMETERS FROM PJPGET
C
      CALL PJPGET(N,ALG,ETCUT,DRCUT,MAXIT,IR,MUON,SPLMRG)
C
C ****  DON'T DO PJET IF NO ISAQ BANKS OR NO PJET PARAMETERS
C
      LISAQ = GZISAQ()
      IF (N.EQ.0 .OR. LISAQ.LE.0) GOTO 999
C
C ****  LOOP OVER ALGORITHMS
C
      DO I = 1, N
C
C ****  Book next PJHD bank to hang this PJET from
C
        CALL BKPJHD(LPJHD)
        IF (INDEX(ALG(I),'CONE').GT.0) THEN
          CALL PJCONE(LPJHD,DRCUT(I),ETCUT(I),MAXIT(I),
     &      IR(I),MUON(I),NJET)
          IF (SPLMRG(I).GT.0.) THEN       ! Do EKS Merging
            CALL PJ_EKS(LPJHD,ETCUT(I),SPLMRG(I),NJET)
          ENDIF
          IQ(LPJHD+2) = 1                 ! Delta R CONE ALGORITHM
        ELSE IF (INDEX(ALG(I),'ANGL').GT.0) THEN
          CALL PJANGL(LPJHD,DRCUT(I),ETCUT(I),MAXIT(I),
     &      IR(I),MUON(I),NJET)
          IQ(LPJHD+2) = 2                 ! CMS OPENING ANGLE ALGORITHM
        ELSE IF (INDEX(ALG(I),'PART').GT.0) THEN
          CALL PJPART(LPJHD,DRCUT(I),ETCUT(I),MAXIT(I),
     &      IR(I),MUON(I),NJET)
          IF (SPLMRG(I).GT.0.) THEN       ! Do EKS Merging
            CALL PJ_EKS(LPJHD,ETCUT(I),SPLMRG(I),NJET)
          ENDIF
          IQ(LPJHD+2) = 3                 ! ISP1 PARTICLE CONE ALG
        ELSE IF (INDEX(ALG(I),'PTCL').GT.0) THEN
          CALL PJUSER(.FALSE.,LPJHD,DRCUT(I),ETCUT(I),MAXIT(I),
     &      IR(I),MUON(I),SPLMRG(I),NJET)
          IQ(LPJHD+2) = 4                 ! PARTICLE CONE ALG WITH SPLIT/MERG
        ELSE IF (INDEX(ALG(I),'PRTN').GT.0) THEN
          CALL PJUSER(.TRUE.,LPJHD,DRCUT(I),ETCUT(I),MAXIT(I),
     &      IR(I),MUON(I),SPLMRG(I),NJET)
          IQ(LPJHD+2) = 5                 ! PARTON CONE ALG WITH SPLIT/MERG
        ELSE
          CALL ERRMSG('PJET','PJETFL',
     &          'NOT A VALID ALGORITHM - USE CONE OR ANGLE','T')
        END IF
        IQ(LPJHD+3) = NJET                 ! NUMBER OF JETS FROM CONE ALGORITHM
        Q(LPJHD+4) = DRCUT(I)              ! Delta R /Opening Angle CUT
        Q(LPJHD+5) = ETCUT(I)              ! JET ET CUT
        IQ(LPJHD+6) = MAXIT(I)             ! MAX_PASS = 1 FOR DR CONE
        IQ(LPJHD+7) = IR(I)                ! INIT RAD SWITCH  = NOT 0 FOR NO IR
        IQ(LPJHD+8) = MUON(I)              ! MUON SWITCH  = 0 for NO MUONS
        Q(LPJHD+9) = SPLMRG(I)   ! MERGING CRITERION:
C                                   SPLMRG = 0. NO MERGING
C                                   SPLMRG > 0. EKS MERGING: dR cut = SPLMRG
C                                   SPLMRG < 0. SHARED ET MERGING:
C                                               shared et fraction = -SPLMRG
      END DO
      CALL PJISAQ(0)
C
C ****  De-activate temporary link area
C
      CALL PJLINK (PJLOFF)
C-
C----------------------------------------------------------------------
  999 RETURN
      END
