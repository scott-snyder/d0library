      SUBROUTINE OLINE0(NUMPAR,LABLIN,MAXLAB,OPTION,OPTNUM,OPTCUR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display option lines in LINE mode
C-
C-   Inputs  : NUMPAR: Number of parameters
C-             LABLIN: Lines of labels
C-             MAXLAB: Maximum length of any LABLIN
C-             OPTION: The actual options
C-             OPTNUM: Number of options for each parameter
C-             OPTCUR: Currently selected options
C-   Outputs : None
C-
C-   Created   4-FEB-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMPAR,OPTNUM(NUMPAR),OPTCUR(NUMPAR),MAXLAB
      CHARACTER*(*) LABLIN(NUMPAR),OPTION(10,NUMPAR)
      CHARACTER*64 OPTLOC(10,100)
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER I,TRULEN,LIBGET,LIBLIN,ISTAT,LIBPUT,K,J,LI,CU,L,PMAX
      INTEGER NUM1,LM
      LOGICAL GETDEV
      CHARACTER*132 MSGLIN,BLNK
      DATA BLNK/' '/
C----------------------------------------------------------------------
      CALL OUTMSG(' ')
      DO I=1,NUMPAR
        K=MIN0(TRULEN(LABLIN(I)),MAXLAB)
        IF(NUMPAR.GT.1) THEN
          WRITE(MSGLIN,980) I,LABLIN(I)(1:K),BLNK(1:MAXLAB-K)
  980     FORMAT(I3,': ',2A)
        ELSE
          MSGLIN='    '//LABLIN(I)(1:K)
        ENDIF
          DO J=1,OPTNUM(I)
            WRITE(OPTLOC(J,I),982) J,OPTION(J,I)(1:TRULEN(OPTION(J,I)))
  982       FORMAT(I2,': ',A)
          ENDDO
        IF(TRMFLG.AND.GETDEV()) THEN
          ISTAT=LIBGET(LI,CU)
          IF(LI.GE.PBROWS) THEN
            ISTAT=LIBLIN(' ',0)
            LI=PBROWS
          ENDIF
          ISTAT=LIBPUT(MSGLIN(1:MAXLAB+7),LI,1,0)
        ENDIF
        PMAX=0
        DO J=1,OPTNUM(I)
          L=TRULEN(OPTLOC(J,I))
          IF(L.GT.PMAX) THEN
            PMAX=L
          ENDIF
        ENDDO
        PMAX=PMAX+4                   !For four spaces between each option
        NUM1=OPTNUM(I)
        DO WHILE (MAXLAB+4+NUM1*PMAX.GT.PBCOLS)
          NUM1=NUM1/2+MOD(NUM1,2)
        ENDDO
        LM=0
        DO J=1,OPTNUM(I)
          IF(J.GT.NUM1*(LM+1)) THEN
            LI=LI+1
            LM=LM+1
            IF(.NOT.(TRMFLG.AND.GETDEV())) THEN
              CALL OUTMSG(MSGLIN(1:TRULEN(MSGLIN)))
              MSGLIN=' '
            ELSE
              IF(LI.GE.PBROWS) THEN
                ISTAT=LIBLIN(' ',0)
                ISTAT=LIBLIN(' ',0)
                LI=PBROWS
              ENDIF
            ENDIF
          ENDIF
          CU=MAXLAB+10+(J-NUM1*LM-1)*PMAX
          IF(TRMFLG.AND.GETDEV()) THEN
            IF(OPTCUR(I).EQ.J) THEN
              ISTAT=LIBPUT(OPTLOC(J,I)(1:TRULEN(OPTLOC(J,I))),LI,CU,1)
            ELSE
              ISTAT=LIBPUT(OPTLOC(J,I)(1:TRULEN(OPTLOC(J,I))),LI,CU,0)
            ENDIF
          ELSE
            IF(OPTCUR(I).EQ.J) THEN
              MSGLIN(CU-2:)='->'//OPTLOC(J,I)(1:TRULEN(OPTLOC(J,I)))
            ELSE
              MSGLIN(CU:)=OPTLOC(J,I)(1:TRULEN(OPTLOC(J,I)))
            ENDIF
          ENDIF
        ENDDO
        IF(TRMFLG.AND.GETDEV()) THEN
          ISTAT=LIBLIN(' ',0)
        ELSE
          CALL OUTMSG(MSGLIN(1:TRULEN(MSGLIN)))
        ENDIF
        CALL OUTMSG(' ')
      ENDDO
      CALL OUTMSG(' ')
      RETURN
      END
