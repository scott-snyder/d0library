      SUBROUTINE GETDIS77(NUMPAR,LABELS,TYPARR,LIMITS,PTON,PTOC,PAR
     &  ,CPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a set of parameters for a command by using
C-                         a screen display such that the user can move
C-                         to the parameter he/she wants to change without
C-                         affecting the other parameters.
C-
C-                         This is a fortran-77 version of GETDIS.  
C-                         Parameters are returned in either PAR or CPAR, 
C-                         depending on type.  The calling program must
C-                         supply the mapping from parameter number to
C-                         array index.
C-
C-   Inputs :  NUMPAR: Number of parameters to get
C              LABELS: Array of LABEL strings
C              TYPARR: Array of parameter types
C              LIMITS: Array of parameter limits for REAL and INTEGER types
C-             PTON:   Mapping of parameter number to numeric array index.
C-             PTOC:   Mapping of parameter number to character array index.
C-   Outputs:  PAR:    Array of numeric parameters to be filled.
C-             CPAR:   Array of character parameters to be filled.
C-   Controls: PF may be changed inside this routine
C-
C-   Created  1-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMPAR
      CHARACTER*(*) LABELS(NUMPAR)
      CHARACTER*1 TYPARR(NUMPAR)
      INTEGER LIMITS(2,*)
      INTEGER PAR(*)
      CHARACTER*(*) CPAR(*)
      INTEGER PTON(NUMPAR), PTOC(NUMPAR)
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LOCLIM(2,30)
      REAL  RLIMIT(2,30)
      EQUIVALENCE (LOCLIM,RLIMIT)
      CHARACTER*40 CHARS,OUTSTR,INPARA,PARSTR,PARINT,PAREAL,
     *             PARLOG
      CHARACTER*8 PFSTR(4)
      CHARACTER*80 TRANUP,TOPIN
      REAL XIN,TRAREA
      LOGICAL LIN,GETDEV,PARSET,FLGPAR(30),CHKLIM(30),LIMIOK,ABRTIT
      INTEGER COMUSE,LOCPOS,LINTOP,LIBSCR,LENINT,POSO,LIBCUR
      INTEGER M,J,TRULEN,IN,TRAINT,ISTAT,K,LIBPUT,LIBERL
      INTEGER READPF,LIBGET,ULEN,LINGET,LIBBIG
      EQUIVALENCE (IN,LIN), (IN,XIN)
      CHARACTER*40 LABLIN(30),PARLIN(30)
      CHARACTER*40 C1,C2
      CHARACTER*132 CTEMP
C----------------------------------------------------------------------
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
          PARLIN(M) = PARINT(PAR(PTON(M)))
  190     CONTINUE
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
          PARLIN(M) = PAREAL(PAR(PTON(M)))
  290     CONTINUE
        ELSEIF(TYPARR(M).EQ.'L') THEN
          ULEN=MIN(36,TRULEN(LABELS(M)))
          WRITE(LABLIN(M),1030) LABELS(M)(1:ULEN)
 1030     FORMAT(A,' (T,F) ')
          PARLIN(M) = PARLOG(PAR(PTON(M)))
  390     CONTINUE
        ELSE
          LABLIN(M)=LABELS(M)(1:TRULEN(LABELS(M)))
          PARLIN(M) = CPAR(PTOC(M))
  490     CONTINUE
        ENDIF
      ENDDO
C
C     Start loop to read new parameter values into the PARLIN array
C
      PF=0
      LOCPOS=1
      LINTOP=0
      IF(FULSCR) THEN
        CALL OUTMSG('1')         ! Erase display region
        CALL PFGET(PFSTR)
        ISTAT=LINGET(1,TOPIN)    ! For later display when screen changes
    1   CONTINUE
        ISTAT=LIBBIG(TOPIN(1:PBCOLS),1,1,3)
        ISTAT=LIBSCR(3,PBROWS-2)
        CALL PFLABL('CHANGE',' ',' ','ABORT')
        CALL GLINE1(LOCPOS,LABLIN,PARLIN,NUMPAR,LINTOP)
        DO WHILE (PF.EQ.0)
          POSO=LOCPOS
          ISTAT=LIBGET(M,J)
          ISTAT=LIBCUR(M,1)
          CALL GREADS(NUMPAR,PARLIN(LOCPOS),CHARS,PF,LOCPOS)
          CALL NEWPAR(TYPARR(POSO),CHARS,LOCLIM(1,POSO),
     *              LOCLIM(2,POSO),CHKLIM(POSO),PARLIN(POSO),LIMIOK)
          IF(PF.EQ.30) THEN
            IF (LIMIOK) FLGPAR(POSO) = .TRUE.
            PF=0
            GOTO 1
          ENDIF
          IF(LIMIOK) THEN
            FLGPAR(POSO)=.TRUE.
          ELSE
            PF=0                     !Refuse to go anywhere else
          ENDIF
          CALL GLINES(POSO,LOCPOS,LABLIN,PARLIN,NUMPAR,LINTOP)
          IF(PF.EQ.2.OR.PF.EQ.3) THEN
            PF=0
          ENDIF
          IF(PF.EQ.4) THEN
            ISTAT=LIBGET(M,J)
            CTEMP = ' Hit PF1 to confirm ABORT without'//
     *              ' changing parameters'//CHAR(7)
            ISTAT=LIBPUT(CTEMP,PBROWS-2,1,0)
            PF=READPF()
            IF(PF.NE.1) THEN
              PF=0
              ISTAT=LIBERL(PBROWS-2,1)
              ISTAT=LIBCUR(M,J)
            ELSE
              ABRTIT=.TRUE.
            ENDIF
          ENDIF
        ENDDO
      ELSE
        IF(TRMFLG) THEN
          CALL GLINE0(LABLIN,PARLIN,NUMPAR)
        ENDIF
        DO WHILE (PF.EQ.0)
          CALL CURLIN('{#+val, LIST, BACK, ABORT}',PF,LOCPOS,NUMPAR,
     &         INPARA)
          IF(PF.EQ.1) THEN
            CHARS=INPARA(2:)           ! EXCLUDE BLANK
            CALL NEWPAR(TYPARR(LOCPOS),CHARS,LOCLIM(1,LOCPOS),
     *              LOCLIM(2,LOCPOS),CHKLIM(LOCPOS),PARLIN(LOCPOS),
     *              LIMIOK)
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
            ENDIF
          ENDIF
        ENDDO
      ENDIF
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
                PAR(PTON(M)) = IN
  590           CONTINUE
              ENDIF
            ELSEIF(TYPARR(M).EQ.'I') THEN
              IF(TRULEN(PARLIN(M)).GT.0) THEN
                IN=TRAINT(PARLIN(M))
                PAR(PTON(M)) = IN
  690           CONTINUE
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
                PAR(PTON(M)) = IN
  790           CONTINUE
              ENDIF
            ELSEIF(TYPARR(M).EQ.'C'.OR.TYPARR(M).EQ.'U') THEN
              IF(TYPARR(M).EQ.'U') THEN
                PARLIN(M)=TRANUP(PARLIN(M))
              ENDIF
              CPAR(PTOC(M)) = PARLIN(M)
  890         CONTINUE
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
      IF(FULSCR) THEN
        CALL OUTMSG('1')
        CALL PFLABL(PFSTR,PFSTR(2),PFSTR(3),PFSTR(4))
        ISTAT=LIBCUR(3,1)
      ENDIF
      RETURN
      END
