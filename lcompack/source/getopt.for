      SUBROUTINE GETOPT(NUMPAR,LABELS,OPTNUM,OPTION,OUTNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up a display of options for a user to choose
C-                         one for each parameter.
C-
C-   Inputs  : NUMPAR: Number of parameters to be used.
C-             LABELS: Labels for each parameter.
C-             OPTNUM: Array of number of options for each parameter.
C-             OPTION: Character array of options
C-             OUTNUM: Starting default number of the option.
C-
C-   Outputs : OUTNUM: Current number of the option choosen.
C-
C-   Created   4-FEB-1988   Jan S. Hoftun
C-   Modifyed  29-Sept-1990 Steve Adler
C-             found bug when selecting one item in full screen mode.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMPAR,OPTNUM(NUMPAR),OUTNUM(NUMPAR)
      CHARACTER*(*) LABELS(NUMPAR),OPTION(10,NUMPAR)
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER M,LOCNUM(100),LOCPOS,LINTOP,LINOLD,LINGET,J,OLDOPT,LIBERL
      INTEGER LIBBIG,LIBSCR,POSO,ISTAT,READPF,LIBCUR,LIBPUT,I
      INTEGER MAXLEN(100),MAXLAB,TRULEN,PFNUM,LINFLG(100),LINUM(100)
      INTEGER CONINT,MAXITM
      CHARACTER*8 PFSTR(4)
      CHARACTER*80 TRANUP,TOPIN,INPARA
      LOGICAL CHGPAR,LOCAST
C----------------------------------------------------------------------
      MAXLAB=0
      DO M=1,NUMPAR
        MAXLEN(M)=0
        IF(OUTNUM(M).EQ.0) THEN
          OUTNUM(M)=1
        ENDIF
        LOCNUM(M)=OUTNUM(M)
        DO I=1,OPTNUM(M)
          DO J=1,TRULEN(OPTION(I,M))
            IF(OPTION(I,M)(1:1).EQ.' ') THEN
              OPTION(I,M)=OPTION(I,M)(2:)
            ELSE
              GOTO 99
            ENDIF
          ENDDO
   99     CONTINUE
          J=TRULEN(OPTION(I,M))
          IF(J.GT.MAXLEN(M)) THEN
            MAXLEN(M)=J
          ENDIF
        ENDDO
        J=TRULEN(LABELS(M))
        IF(J.GT.MAXLAB) THEN
          MAXLAB=J
        ENDIF
        LINFLG(M)=0
        MAXLEN(M)=MAXLEN(M)+4        !Four spaces between options
      ENDDO
      MAXLAB=MAXLAB+3+3              !Three spaces before and after label
      DO I=1,NUMPAR
        LINUM(I)=OPTNUM(I)
        DO WHILE (MAXLAB+LINUM(I)*MAXLEN(I).GT.PBCOLS)
          LINUM(I)=LINUM(I)/2+MOD(LINUM(I),2)
          LINFLG(I)=LINFLG(I)+1
        ENDDO
      ENDDO
C
C     Start loop to SET new values in the LOCNUM array
C
      PFNUM=0
      LOCPOS=1
      LINTOP=0
      LINOLD=0
      CHGPAR=.FALSE.
      DO WHILE (PFNUM.EQ.0)
        IF(NUMPAR.GT.1) THEN
          IF(TRMFLG) THEN
            CALL OLINE0(NUMPAR,LABELS,MAXLAB-6,OPTION,OPTNUM,LOCNUM)
          ENDIF
          LOCAST=ASTFLG
          ASTFLG=.FALSE.            !Make sure prompt will be displayed
          CALL CURLIN('{#+#, LIST, BACK, ABORT}',PFNUM,LOCPOS,NUMPAR,
     &         INPARA)
          ASTFLG=LOCAST
        ELSE
          IF(COMNUM.EQ.0) THEN
            IF(TRMFLG) THEN
              CALL OLINE0(NUMPAR,LABELS,MAXLAB-6,OPTION,OPTNUM,LOCNUM)
            ENDIF
            LOCAST=ASTFLG
            ASTFLG=.FALSE.            !Make sure prompt will be displayed
            CALL CURLIN('{#, LIST, ABORT}',PFNUM,LOCNUM(1),OPTNUM(1),
     &           INPARA)
            ASTFLG=LOCAST
          ELSE
            PF=1
            M=CONINT(COMPRT(1))
            IF(M.GT.0.AND.M.LE.OPTNUM(1)) THEN
              LOCNUM(1)=M
              CHGPAR=.TRUE.
            ELSE
              OUTNUM(1)=0
              CALL OUTMSG('0Invalid option number found!'//CHAR(7))
              CALL OUTMSG(' ')
            ENDIF
            GOTO 987
          ENDIF
        ENDIF
        IF(PFNUM.EQ.1) THEN
          IF (NUMPAR.GT.1) THEN
            IF(LOCPOS.GT.0) THEN
              LOCNUM(LOCPOS)=CONINT(INPARA(2:))
              IF(LOCNUM(LOCPOS).LE.0.OR.LOCNUM(LOCPOS).GT.
     &           OPTNUM(LOCPOS)) THEN
                CALL OUTMSG('0Invalid option number found!'//CHAR(7))
                CALL OUTMSG(' ')
                LOCNUM(I)=OUTNUM(I)
              ELSE
                CHGPAR=.TRUE.
              ENDIF
            ELSE
              CALL OUTMSG('0Illegal character in integer!'//CHAR(7))
              CALL OUTMSG(' ')
            ENDIF
            PFNUM=0
          ELSEIF(LOCNUM(1).GT.0.AND.LOCNUM(1).LE.OPTNUM(1)) THEN
            PFNUM=4
            CHGPAR=.TRUE.
          ELSE
            CALL OUTMSG('0Invalid option number found!'//CHAR(7))
            CALL OUTMSG(' ')
            LOCNUM(I)=OUTNUM(I)
            PFNUM=0
          ENDIF
        ELSEIF(PFNUM.EQ.2) THEN
          CALL OLINE0(NUMPAR,LABELS,MAXLAB-6,OPTION,OPTNUM,LOCNUM)
          PFNUM=0
        ELSEIF(PFNUM.EQ.3) THEN
          CALL OLINE0(NUMPAR,LABELS,MAXLAB-6,OPTION,OPTNUM,LOCNUM)
          PFNUM=0
        ELSEIF(PFNUM.EQ.4) THEN
          IF(TRANUP(INPARA(1:1)).EQ.'A') THEN      !A for ABORT
            CHGPAR=.FALSE.
          ENDIF
        ENDIF
      ENDDO
  987 CONTINUE
      IF(CHGPAR) THEN
        IF(NUMPAR.GT.1) THEN
          DO M=1,NUMPAR
            OUTNUM(M)=LOCNUM(M)
            IF((SETUP.OR.LOGUP).AND..NOT.ASTFLG) THEN
              WRITE(COMUNI,2) M,OUTNUM(M),LABELS(M)
    2         FORMAT(I4,' ',I4,'    !-<',A)
            ENDIF
          ENDDO
        ELSEIF(NUMPAR.EQ.1) THEN
          OUTNUM(1)=LOCNUM(1)
          IF((SETUP.OR.LOGUP).AND..NOT.ASTFLG) THEN
            WRITE(COMUNI,3) OUTNUM(1),LABELS(1)
    3       FORMAT(I4,'    !-<',A)
          ENDIF
        ENDIF
      ELSE
        IF(NUMPAR.EQ.1) THEN
          OUTNUM(1)=0
        ENDIF
      ENDIF
      IF((SETUP.OR.LOGUP).AND..NOT.ASTFLG.AND.NUMPAR.GT.1) THEN
        WRITE(COMUNI,4)
    4   FORMAT('BACK')
      ENDIF
  999 RETURN
      END
