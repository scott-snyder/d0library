      SUBROUTINE OLINE1(POS,LABLIN,OPTION,NUMPAR,OPTNUM,OPTCUR,
     *           LINTOP,LINOLD,MAXLAB,MAXLEN,LINFLG,MAXITM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display whole new page of options for GETOPT
C-
C-   Inputs  : POS:    Current position in display
C-             LABLIN: Array of parameter labels
C-             OPTION: Array of options (two dimensional)
C-             NUMPAR: Number of parameters currently used
C-             OPTNUM: Number of options for each parameter
C-             OPTCUR: Currently selected option for each parameter
C-             MAXLAB: Maximum length of labels
C-             MAXLEN: Maximum length of options for each parameter
C-             LINFLG: Number of lines to use for each parameter
C-             MAXITM: Maximum number of items per screen
C-   Outputs : LINTOP: New number of lines scrolled from top
C-             LINOLD: Old number of lines scrolled from top
C-   Controls: None
C-
C-   Documented  22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER POS,NUMPAR,LINTOP,LINOLD,OPTNUM(NUMPAR),OPTCUR(NUMPAR)
      INTEGER MAXLAB,MAXLEN(NUMPAR),LINFLG(NUMPAR),MAXITM
      CHARACTER*(*) LABLIN(1:NUMPAR),OPTION(10,NUMPAR)
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LINE,COLUMN,ISTAT,I,LIBCAR,LI,CU,LM,NUM1,LISAV,CUSAV
      PARAMETER (COLUMN=3)
      INTEGER LIBERL,LIBPUT,LIBCUR,J,LOWCAR,TOPCAR,TRULEN
      LINE(I)=2*I+TOPCAR
C----------------------------------------------------------------------
      IF(PBROWS.GT.20) THEN
        MAXITM=PBROWS/2-4
        TOPCAR=3
        LOWCAR=PBROWS-3
      ELSE
        MAXITM=PBROWS/2-3
        TOPCAR=2
        LOWCAR=PBROWS-3
      ENDIF
      DO J=LINTOP+1,LINTOP+MAXITM
        MAXITM=MAXITM-LINFLG(J)
      ENDDO
C      DO I=1,NUMPAR
C        MAXITM=MAXITM-LINFLG(I)
C      ENDDO
      MAXITM=MAX(1,MAXITM)
      IF(POS.EQ.1) THEN
        LINTOP=0                            ! Reset lintop at the top of the menu
c      ELSEIF(POS.GT.MAXITM) THEN
c        LINTOP=POS-MAXITM
      ENDIF
      ISTAT=LIBERL(TOPCAR,COLUMN)
      IF(LINTOP.GT.0) THEN
        IF(LINTOP.GT.LINOLD) THEN
          MAXITM=POS-LINTOP
        ENDIF
        ISTAT=LIBCAR(TOPCAR,COLUMN,1)      ! Put carat on top
      ENDIF
      DO I=1,MIN(NUMPAR,MAXITM)
        ISTAT=LIBERL(LINE(I),COLUMN)
        ISTAT=LIBERL(LINE(I)+1,COLUMN)
        IF(MOD(ISTAT,2).EQ.0) CALL MSGSCR(ISTAT,' ')
        LI=LINE(I)
        DO J=LINTOP+1,I-LINTOP-1
          LI=LI+LINFLG(J)
        ENDDO
        ISTAT=LIBPUT(LABLIN(I+LINTOP),LI,COLUMN,0)
        ISTAT=LIBERL(LI+1,1)
        IF(MOD(ISTAT,2).EQ.0) CALL MSGSCR(ISTAT,' ')
        NUM1=OPTNUM(I+LINTOP)
   10   CONTINUE
        IF(MAXLAB+NUM1*MAXLEN(I+LINTOP).GT.PBCOLS) THEN
          NUM1=NUM1/2+MOD(NUM1,2)
          GOTO 10
        ENDIF
        LM=0
        DO J=1,OPTNUM(I+LINTOP)
          IF(J.GT.NUM1*(LM+1)) THEN
            LI=LI+1
            LM=LM+1
          ENDIF
          CU=MAXLAB+(J-NUM1*LM-1)*MAXLEN(I+LINTOP)
          IF(OPTCUR(I+LINTOP).EQ.J) THEN
            ISTAT=LIBPUT(OPTION(J,I+LINTOP),LI,CU,1)
            IF(I+LINTOP.EQ.POS) THEN                       !Save cursor
C                                                   ! position of selected item
              LISAV=LI
              CUSAV=CU
            ENDIF
          ELSE
            ISTAT=LIBPUT(OPTION(J,I+LINTOP),LI,CU,0)
          ENDIF
          IF(MOD(ISTAT,2).EQ.0) CALL MSGSCR(ISTAT,' ')
        ENDDO
      ENDDO
      IF((LINTOP+MAXITM).LT.NUMPAR) THEN
        ISTAT=LIBERL(LOWCAR,COLUMN)
        IF(MOD(ISTAT,2).EQ.0) CALL MSGSCR(ISTAT,' ')
        ISTAT=LIBCAR(LOWCAR,COLUMN,1)             ! Put carat on bottom
        IF(MOD(ISTAT,2).EQ.0) CALL MSGSCR(ISTAT,' ')
      ELSE
        ISTAT=LIBERL(LOWCAR,COLUMN)
        IF(MOD(ISTAT,2).EQ.0) CALL MSGSCR(ISTAT,' ')
      ENDIF
      ISTAT=LIBCUR(LISAV,CUSAV)                   !Put cursor on
C                                                 ! selected item
      IF(MOD(ISTAT,2).EQ.0) CALL MSGSCR(ISTAT,' ')
      RETURN
      END
