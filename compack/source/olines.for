      SUBROUTINE OLINES(OLDPOS,POS,LABLIN,OPTION,NUMPAR,OPTNUM,OPTCUR,
     *           OPTOLD,LINTOP,LINOLD,MAXLAB,MAXLEN,LINFLG,MAXITM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Update display when moving between options 
C-                         for GETOPT.
C-
C-   Inputs  : OLDPOS: Old position in display to be un-selected
C-             POS:    New position in display to be selected
C-             LABLIN: Array of parameter labels
C-             OPTION: Array of options (two dimensional)
C-             NUMPAR: Number of parameters currently used
C-             OPTNUM: Number of options for each parameter
C-             OPTCUR: Currently selected option for each parameter
C-             OPTOLD: Old option selected option for parameter POS
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
      INTEGER OLDPOS,POS,NUMPAR,LINTOP,LINOLD,OPTNUM(NUMPAR),
     *   OPTCUR(NUMPAR),OPTOLD,MAXLAB,MAXLEN(NUMPAR),LINFLG(NUMPAR),
     *   MAXITM
      CHARACTER*(*) LABLIN(1:NUMPAR),OPTION(10,NUMPAR)
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LINE,ISTAT,I,POSMAX,CU,LI,LM,LISAV,NUM1
      INTEGER LIBPUT,LIBCUR,J,TOPCAR,TRULEN,LIBERL
      LINE(I)=2*I+TOPCAR
      IF(PBROWS.GT.20) THEN
        TOPCAR=3
      ELSE
        TOPCAR=2
      ENDIF
      IF(POS.GT.OLDPOS.AND.POS.LE.2) THEN
        POSMAX=1                            ! Reset posmax at the top of the menu
        LINTOP=0                            ! Reset lintop at the top of the menu
      ENDIF
      IF(POS.LE.LINTOP) THEN
        LINTOP=LINTOP-1                     ! Scroll down
        POSMAX=POSMAX-1
      ENDIF
      IF(POS.GT.MAXITM.AND.POS.GT.OLDPOS.AND.POSMAX.LT.POS) THEN     ! Scroll lines when POS is increasing
        LINTOP=LINTOP+1
        POSMAX=POS
      ENDIF
      IF(LINTOP.NE.LINOLD) THEN                             ! Redraw menu when needed
         CALL OLINE1(POS,LABLIN,OPTION,NUMPAR,OPTNUM,OPTCUR,LINTOP,
     *       LINOLD,MAXLAB,MAXLEN,LINFLG,MAXITM)
        LINOLD=LINTOP
      ELSE
        LI=LINE(POS-LINTOP)
        DO J=LINTOP+1,POS-LINTOP-1
          LI=LI+LINFLG(J)
        ENDDO
        LISAV=LI
        NUM1=OPTNUM(POS)
        DO WHILE (MAXLAB+NUM1*MAXLEN(POS).GT.PBCOLS)
          NUM1=NUM1/2+MOD(NUM1,2)
        ENDDO
        IF(POS.EQ.OLDPOS.AND.OPTCUR(POS).NE.OPTOLD) THEN
          LM=0
          DO J=1,OPTOLD
            IF(J.GT.NUM1*(LM+1)) THEN
              LI=LI+1
              LM=LM+1
            ENDIF
          ENDDO
          CU=MAXLAB+(OPTOLD-NUM1*LM-1)*MAXLEN(POS)
          ISTAT=LIBPUT(OPTION(OPTOLD,POS)(1:TRULEN(OPTION(OPTOLD,POS))),
     &                 LI,CU,0)
        ENDIF
        LM=0
        LI=LISAV
        DO J=1,OPTCUR(POS)
          IF(J.GT.NUM1*(LM+1)) THEN
            LI=LI+1
            LM=LM+1
          ENDIF
        ENDDO
        CU=MAXLAB+(OPTCUR(POS)-NUM1*LM-1)*MAXLEN(POS)
        IF(POS.EQ.OLDPOS.AND.OPTCUR(POS).NE.OPTOLD) THEN
          ISTAT=LIBPUT(OPTION(OPTCUR(POS),POS)
     &      (1:TRULEN(OPTION(OPTCUR(POS),POS))),LI,CU,1)
C          ISTAT=LIBERL(LI+1,1)
        ENDIF
        ISTAT=LIBCUR(LI,CU)
      ENDIF
      RETURN
      END
