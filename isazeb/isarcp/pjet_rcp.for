      SUBROUTINE PJET_RCP(RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FILLS PJET COMMON WITH ALGORITHM PARAMETERS READ
C-   FROM PJET_RCP
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  14-DEC-1989   Chip Stewart
C-   Updated  13-NOV-1992   Andy Milder, Brent May
C-      Added new parameter SPLMRG for splitting and merging
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXALG
      PARAMETER( MAXALG = 30 )
      REAL ETCUTL(MAXALG),DRCUTL(MAXALG),SPL(MAXALG)
      INTEGER MAXITL(MAXALG), IRL(MAXALG), IML(MAXALG)
      CHARACTER ALGL(MAXALG)*8
      INTEGER N_ALGORITHM
C
      CHARACTER NAME*32,TMP*8,RCP_BANK*(*)
      INTEGER ALGORITHM_PARAMS(10),IER,MP,IALG(2)
      INTEGER TYPE_PARAMS(10),N_PARAMS,I,J,IR,IM,NJET
      LOGICAL FIRST,OPEN_ANGLE,DR_CONE,EZERR
      REAL ET,DR,SPLMRG(3)
C
      DATA FIRST/.TRUE./
      EQUIVALENCE (ALGORITHM_PARAMS(1),DR)
      EQUIVALENCE (ALGORITHM_PARAMS(2),ET)
      EQUIVALENCE (ALGORITHM_PARAMS(3),MP)
      EQUIVALENCE (ALGORITHM_PARAMS(4),IR)
      EQUIVALENCE (ALGORITHM_PARAMS(5),IM)
      EQUIVALENCE (ALGORITHM_PARAMS(6),IALG) !IALG MAY OVERLAP SPLMRG
      EQUIVALENCE (ALGORITHM_PARAMS(7),SPLMRG(1))  ! SPLMRG IS
      EQUIVALENCE (ALGORITHM_PARAMS(8),SPLMRG(2))  !  ONE OF
      EQUIVALENCE (ALGORITHM_PARAMS(9),SPLMRG(3))  !  THESE
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK(RCP_BANK)       ! SELECT JETS RCP BANK
        IF (EZERR(IER)) THEN
          CALL ERRMSG('PJET','PJET_RCP',
     &      'RCP bank for PJET not found - USE DEFAULTS','W')
        ELSE
C
C ****  READ RUNTIME PJET AGLORITHM CONSTANTS
C
          CALL EZGET('NUMBER_ALGORITHMS',N_ALGORITHM,IER)
          IF ( IER.NE.0 ) THEN
             CALL ERRMSG('PJET','PJET_RCP',
     &      'RCP bank has no PJET parameters - USE DEFAULTS','W')
            GOTO 999
          END IF
          DO I = 1, N_ALGORITHM
            WRITE(NAME,10)I
   10       FORMAT('ALGORITHM_',I2.2)
            CALL EZGET_VALUE_TYPE(NAME(1:12), ALGORITHM_PARAMS,
     &        TYPE_PARAMS,N_PARAMS, IER)
            SPL(I) = 0.        !DEFAULT - NO MERGING
            DO J = 1, N_PARAMS-6  !FIND SPLMRG IN ALGORITHM_PARAMS(>=7)
              IF (TYPE_PARAMS(6+J).EQ.2) THEN
                SPL(I) = SPLMRG(J)
                GOTO 13
              ENDIF
            ENDDO
   13       CONTINUE
            ETCUTL(I) = ET
            DRCUTL(I) = DR
            MAXITL(I) = MP
            IRL(I) = IR
            IML(I) = IM
            J = 8 ! TYPE_PARAMS(6) - 10
            CALL UHTOC(IALG,J,TMP,8)
            ALGL(I) = TMP
            WRITE(2,15)I,
     &        ALGL(I),DRCUTL(I),ETCUTL(I),MAXITL(I),IRL(I),IML(I),
     &        SPL(I)
   15       FORMAT(/1X,I5,A20,2X,2F10.5,3I10,F10.5)
          END DO
C
C ****  SET PARAMETERS
C
          CALL PJPSET(N_ALGORITHM,ALGL,ETCUTL,DRCUTL,MAXITL,IRL,IML,
     &      SPL)
        END IF
      END IF
      CALL EZRSET
  999 RETURN
      END
