      SUBROUTINE GETARR(NUMPAR,LABELS,TYPARR,LIMITS,PARAMS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get each of an array of parameters for a command 
C-                         by using a screen display such that the user can 
C-                         move to the parameter he/she wants to change without
C-                         affecting the other parameters.
C-
C-   Inputs :  NUMPAR: Number of parameters to get
C              LABELS: Array of LABEL strings
C              TYPARR: Array of parameter types
C              LIMITS: Array of parameter limits for REAL and INTEGER types
C-   Outputs:  PARAMS: Array of parameters to be filled.
C-   Controls: None
C-
C-
C-   Created 25-APR-1989   Jan S. Hoftun  
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMPAR
      CHARACTER*(*) LABELS(NUMPAR)
      CHARACTER*1 TYPARR(NUMPAR)
      INTEGER LIMITS(2,NUMPAR)
      INTEGER PARAMS(NUMPAR)
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER MAXPAR
      PARAMETER (MAXPAR=60)
      INTEGER LOCLIM(2,MAXPAR)
      REAL  RLIMIT(2,MAXPAR)
      EQUIVALENCE (LOCLIM,RLIMIT)
      CHARACTER*40 CHARS,OUTSTR,INPARA,PARSTR,PARINT,PAREAL,
     *             PARLOG,pareal_i
      CHARACTER*8 PFSTR(4)
      CHARACTER*80 TRANUP,TOPIN
      REAL XIN,TRAREA
      LOGICAL LIN,PARSET,FLGPAR(MAXPAR),CHKLIM(MAXPAR),LIMIOK,
     &  ABRTIT
      INTEGER COMUSE,LOCPOS,LINTOP,LIBSCR,LENINT,POSO,LIBCUR
      INTEGER M,J,TRULEN,IN,TRAINT,ISTAT,K,LIBPUT,LIBERL
      INTEGER READPF,LIBGET,ULEN,LINGET,LIBBIG
      EQUIVALENCE (IN,LIN), (IN,XIN)
      CHARACTER*40 LABLIN(MAXPAR),PARLIN(MAXPAR)
      CHARACTER*40 C1,C2
C----------------------------------------------------------------------
      IF(NUMPAR.GT.MAXPAR) THEN
        CALL INTMSG('0Too many parameters asked for in GETARR')
        RETURN
      ENDIF
      ABRTIT=.FALSE.
C
C     Loop over parameters
C
      DO M=1,NUMPAR
        FLGPAR(M)=.FALSE.
C
C     Set up limits and labels
C
        LOCLIM(1,M)=LIMITS(1,M)
        LOCLIM(2,M)=LIMITS(2,M)
        CHKLIM(M)=.FALSE.
        IF(TYPARR(M).EQ.'I') THEN
          IF(LOCLIM(1,M).LT.LOCLIM(2,M)) THEN
            CHKLIM(M)=.TRUE.
            CHKLIM(M)=.TRUE.
            C1=PARINT(LOCLIM(1,M))
            C2=PARINT(LOCLIM(2,M))
            ULEN=MIN(40-TRULEN(C1)-TRULEN(C2)-9,TRULEN(LABELS(M)))
            WRITE(LABLIN(M),1020) LABELS(M)(1:ULEN),
     *               C1(1:TRULEN(C1)),C2(1:TRULEN(C2))
 1020       FORMAT(A:,' [',A,'<=>',A,'] ')
          ELSE
            ULEN=MIN(40,TRULEN(LABELS(M)))
            WRITE(LABLIN(M),1020) LABELS(M)(1:ULEN)
          ENDIF
          PARLIN(M)=PARINT(PARAMS(M))
        ELSEIF(TYPARR(M).EQ.'R') THEN
          IF(RLIMIT(1,M).LT.RLIMIT(2,M)) THEN
            CHKLIM(M)=.TRUE.
            C1=PAREAL(RLIMIT(1,M))
            C2=PAREAL(RLIMIT(2,M))
            ULEN=MIN(40-TRULEN(C1)-TRULEN(C2)-9,TRULEN(LABELS(M)))
            WRITE(LABLIN(M),1020) LABELS(M)(1:ULEN),
     *               C1(1:TRULEN(C1)),C2(1:TRULEN(C2))
          ELSE
            ULEN=MIN(40,TRULEN(LABELS(M)))
            WRITE(LABLIN(M),1020) LABELS(M)(1:ULEN)
          ENDIF
          PARLIN(M)=PAREAL_i(PARAMS(M))
        ELSEIF(TYPARR(M).EQ.'L') THEN
          ULEN=MIN(36,TRULEN(LABELS(M)))
          WRITE(LABLIN(M),1030) LABELS(M)(1:ULEN)
 1030     FORMAT(A,' (T,F) ')
          PARLIN(M)=PARLOG(PARAMS(M))
        ELSE
C&IF VAXVMS
          LABLIN(M)=LABELS(M)(1:TRULEN(LABELS(M)))
          CALL MOVSTR(PARAMS(M),PARLIN(M))
C&ELSE
C&          CALL outmsg(
C&     &      ' GETARR: Character arguments not implemented in UNIX')
C&ENDIF
        ENDIF
      ENDDO
C
C     Start loop to read new parameter values into the PARLIN array
C
      PF=0
      LOCPOS=1
      LINTOP=0
      DO WHILE (PF.EQ.0)
        CALL CURLIN('{#+val, LIST, BACK, ABORT}',PF,LOCPOS,NUMPAR,
     &       INPARA)
        IF(PF.EQ.1) THEN
          CHARS=INPARA(2:)           ! EXCLUDE BLANK
          CALL NEWPAR(TYPARR(LOCPOS),CHARS,LOCLIM(1,LOCPOS),
     *            LOCLIM(2,LOCPOS),CHKLIM(LOCPOS),PARLIN(LOCPOS),
     *            LIMIOK)
          IF(LIMIOK) FLGPAR(LOCPOS)=.TRUE.
          PF=0
        ELSEIF(PF.EQ.2) THEN
          CALL GLINE0(LABLIN,PARLIN,NUMPAR)
          PF=0
        ELSEIF(PF.EQ.3) THEN
          CALL GLINE0(LABLIN,PARLIN,NUMPAR)
          PF=0
        ELSEIF(PF.EQ.4) THEN
          IF(TRANUP(INPARA(1:1)).EQ.'A') THEN      !A for ABORT
            ABRTIT=.TRUE.
C            DO M=1,NUMPAR
C              FLGPAR(M)=.FALSE.
C            ENDDO
          ENDIF
        ENDIF
      ENDDO
      IF(.NOT.ABRTIT) THEN
        DO M=1,NUMPAR
          IF(FLGPAR(M)) THEN
            IF((SETUP.OR.LOGUP).AND..NOT.ASTFLG) THEN
              WRITE(COMUNI,2) M,PARLIN(M)(1:TRULEN(PARLIN(M))),
     *               LABLIN(M)
    2         FORMAT(I4,' ',A,'    !-<',A)
            ENDIF
            IF(TYPARR(M).EQ.'R') THEN
              IF(TRULEN(PARLIN(M)).GT.0) THEN
                XIN=TRAREA(PARLIN(M))
                PARAMS(M)=IN
              ENDIF
            ELSEIF(TYPARR(M).EQ.'I') THEN
              IF(TRULEN(PARLIN(M)).GT.0) THEN
                PARAMS(M)=TRAINT(PARLIN(M))
              ENDIF
            ELSEIF(TYPARR(M).EQ.'L') THEN
              IF(TRULEN(PARLIN(M)).GT.0) THEN
                PARLIN(M)=TRANUP(PARLIN(M))
                DO J=1,TRULEN(PARLIN(M))
                  IF (PARLIN(M)(1:1).EQ.' ') PARLIN(M)=PARLIN(M)(2:)
                ENDDO
C
C     Assume that T for .true. or Y for 'yes' are the only valid .TRUE. codes.
C
                IF(PARLIN(M)(1:1).EQ.'T'.OR.
     *                  PARLIN(M)(1:1).EQ.'Y') THEN
                  LIN=.TRUE.
                ELSE
                  LIN=.FALSE.
                ENDIF
                PARAMS(M)=IN
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      IF((SETUP.OR.LOGUP).AND..NOT.ASTFLG) THEN
        WRITE(COMUNI,3)
    3   FORMAT('BACK')
      ENDIF
      IF(.NOT.ABRTIT) THEN
        PF=0
      ENDIF
      RETURN
      END
