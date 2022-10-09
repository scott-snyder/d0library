C&IF VAXVMS
      SUBROUTINE GETDIS(NUMPAR,LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
     *           PAR4,PAR5,PAR6,PAR7,PAR8,PAR9,PAR10,
     *           PAR11,PAR12,PAR13,PAR14,PAR15,PAR16,PAR17,
     *           PAR18,PAR19,PAR20,PAR21,PAR22,PAR23,PAR24,
     *           PAR25,PAR26,PAR27,PAR28,PAR29,PAR30)
C&ELSE
C&      SUBROUTINE GETDIS(NUMPAR,LABELS,TYPARR,LIMITS,PAR1,PAR2,PAR3,
C&     *           PAR4,PAR5,PAR6,PAR7,PAR8,PAR9,PAR10,
C&     *           PAR11,PAR12,PAR13,PAR14,PAR15,PAR16,PAR17,
C&     *           PAR18,PAR19,PAR20,PAR21,PAR22,PAR23,PAR24,
C&     *           PAR25,PAR26,PAR27,PAR28,PAR29,PAR30,PAR31,
C&     *           PAR32,PAR33,PAR34,PAR35,PAR36,PAR37,PAR38,
C&     *           PAR39,PAR40,PAR41,PAR42,PAR43,PAR44,PAR45,
C&     *           PAR46,PAR47,PAR48,PAR49,PAR50,PAR51,PAR52,
C&     *           PAR53,PAR54,PAR55,PAR56,PAR57,PAR58,PAR59,
C&     *           PAR60,PAR61,PAR62)
C&ENDIF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a set of parameters for a command by using
C-                         a screen display such that the user can move
C-                         to the parameter he/she wants to change without
C-                         affecting the other parameters.
C-
C-   Inputs :  NUMPAR: Number of parameters to get
C              LABELS: Array of LABEL strings
C              TYPARR: Array of parameter types
C              LIMITS: Array of parameter limits for REAL and INTEGER types
C-   Outputs:  PAR1-PAR30:  Parameters to be filled.
C-   Controls: PF may be changed inside this routine
C-
C-     NOTE
C-
C-       To INCREASE the number of parameters possible, add PAR31 etc. here
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Modified    5-FEB-1991   Scott Snyder
C-    If the user terminates an input with KP0 or ENTER, the input gets
C-    thrown away. Fix by moving the PF==30 test to after the call to
C-    NEWPAR.
C-   Updated   1-OCT-1991  Herbert Greenlee
C-      Added a machine dependent block to act as an interface to getdis77.
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMPAR
      CHARACTER*(*) LABELS(NUMPAR)
      CHARACTER*1 TYPARR(NUMPAR)
      INTEGER LIMITS(2,NUMPAR)
      INTEGER PAR1,PAR2,PAR3,PAR4,PAR5,PAR6,PAR7
      INTEGER PAR8,PAR9,PAR10,PAR11,PAR12,PAR13
      INTEGER PAR14,PAR15,PAR16,PAR17,PAR18,PAR19
      INTEGER PAR20,PAR21,PAR22,PAR23,PAR24,PAR25
      INTEGER PAR26,PAR27,PAR28,PAR29,PAR30
C&IF VAXVMS
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
      LOGICAL LIN,PARSET,FLGPAR(30),CHKLIM(30),LIMIOK,ABRTIT
      INTEGER COMUSE,LOCPOS,LINTOP,LIBSCR,LENINT,POSO,LIBCUR
      INTEGER M,J,TRULEN,IN,TRAINT,ISTAT,K,LIBPUT,LIBERL
      INTEGER READPF,LIBGET,ULEN,LINGET,LIBBIG
      EQUIVALENCE (IN,LIN), (IN,XIN)
      CHARACTER*40 LABLIN(30),PARLIN(30)
      CHARACTER*40 C1,C2
C&ELSE
C&      INTEGER  PAR31
C&      INTEGER  PAR32,PAR33,PAR34,PAR35,PAR36,PAR37,PAR38
C&      INTEGER  PAR39,PAR40,PAR41,PAR42,PAR43,PAR44,PAR45
C&      INTEGER  PAR46,PAR47,PAR48,PAR49,PAR50,PAR51,PAR52
C&      INTEGER  PAR53,PAR54,PAR55,PAR56,PAR57,PAR58,PAR59
C&      INTEGER  PAR60,PAR61,PAR62
C&      INTEGER LEN_LABELS
C&      INTEGER I, J, NNUM, NCHAR
C&      INTEGER PAR(30), PTON(30), PTOC(30)
C&      CHARACTER*80 CPAR(30)
C&      INTEGER RETURN, ADDR
C&      INTEGER ADDNPAR(30), ADDCPAR(30), LENCPAR(30)
C&      INTEGER D0_LOC
C&ENDIF
C----------------------------------------------------------------------
C&IF VAXVMS
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
          GOTO (101,102,103,104,105,106,107,108,109,110,111,112,113,
     *            114,115,116,117,118,119,120,121,122,123,124,125,126,
     *            127,128,129,130), M
  101     PARLIN(M)=PARINT(PAR1)
          GOTO 190
  102     PARLIN(M)=PARINT(PAR2)
          GOTO 190
  103     PARLIN(M)=PARINT(PAR3)
          GOTO 190
  104     PARLIN(M)=PARINT(PAR4)
          GOTO 190
  105     PARLIN(M)=PARINT(PAR5)
          GOTO 190
  106     PARLIN(M)=PARINT(PAR6)
          GOTO 190
  107     PARLIN(M)=PARINT(PAR7)
          GOTO 190
  108     PARLIN(M)=PARINT(PAR8)
          GOTO 190
  109     PARLIN(M)=PARINT(PAR9)
          GOTO 190
  110     PARLIN(M)=PARINT(PAR10)
          GOTO 190
  111     PARLIN(M)=PARINT(PAR11)
          GOTO 190
  112     PARLIN(M)=PARINT(PAR12)
          GOTO 190
  113     PARLIN(M)=PARINT(PAR13)
          GOTO 190
  114     PARLIN(M)=PARINT(PAR14)
          GOTO 190
  115     PARLIN(M)=PARINT(PAR15)
          GOTO 190
  116     PARLIN(M)=PARINT(PAR16)
          GOTO 190
  117     PARLIN(M)=PARINT(PAR17)
          GOTO 190
  118     PARLIN(M)=PARINT(PAR18)
          GOTO 190
  119     PARLIN(M)=PARINT(PAR19)
          GOTO 190
  120     PARLIN(M)=PARINT(PAR20)
          GOTO 190
  121     PARLIN(M)=PARINT(PAR21)
          GOTO 190
  122     PARLIN(M)=PARINT(PAR22)
          GOTO 190
  123     PARLIN(M)=PARINT(PAR23)
          GOTO 190
  124     PARLIN(M)=PARINT(PAR24)
          GOTO 190
  125     PARLIN(M)=PARINT(PAR25)
          GOTO 190
  126     PARLIN(M)=PARINT(PAR26)
          GOTO 190
  127     PARLIN(M)=PARINT(PAR27)
          GOTO 190
  128     PARLIN(M)=PARINT(PAR28)
          GOTO 190
  129     PARLIN(M)=PARINT(PAR29)
          GOTO 190
  130     PARLIN(M)=PARINT(PAR30)
          GOTO 190
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
          GOTO (201,202,203,204,205,206,207,208,209,210,211,212,213,
     *            214,215,216,217,218,219,220,221,222,223,224,225,226,
     *            227,228,229,230), M
  201     PARLIN(M)=PAREAL(PAR1)
          GOTO 290
  202     PARLIN(M)=PAREAL(PAR2)
          GOTO 290
  203     PARLIN(M)=PAREAL(PAR3)
          GOTO 290
  204     PARLIN(M)=PAREAL(PAR4)
          GOTO 290
  205     PARLIN(M)=PAREAL(PAR5)
          GOTO 290
  206     PARLIN(M)=PAREAL(PAR6)
          GOTO 290
  207     PARLIN(M)=PAREAL(PAR7)
          GOTO 290
  208     PARLIN(M)=PAREAL(PAR8)
          GOTO 290
  209     PARLIN(M)=PAREAL(PAR9)
          GOTO 290
  210     PARLIN(M)=PAREAL(PAR10)
          GOTO 290
  211     PARLIN(M)=PAREAL(PAR11)
          GOTO 290
  212     PARLIN(M)=PAREAL(PAR12)
          GOTO 290
  213     PARLIN(M)=PAREAL(PAR13)
          GOTO 290
  214     PARLIN(M)=PAREAL(PAR14)
          GOTO 290
  215     PARLIN(M)=PAREAL(PAR15)
          GOTO 290
  216     PARLIN(M)=PAREAL(PAR16)
          GOTO 290
  217     PARLIN(M)=PAREAL(PAR17)
          GOTO 290
  218     PARLIN(M)=PAREAL(PAR18)
          GOTO 290
  219     PARLIN(M)=PAREAL(PAR19)
          GOTO 290
  220     PARLIN(M)=PAREAL(PAR20)
          GOTO 290
  221     PARLIN(M)=PAREAL(PAR21)
          GOTO 290
  222     PARLIN(M)=PAREAL(PAR22)
          GOTO 290
  223     PARLIN(M)=PAREAL(PAR23)
          GOTO 290
  224     PARLIN(M)=PAREAL(PAR24)
          GOTO 290
  225     PARLIN(M)=PAREAL(PAR25)
          GOTO 290
  226     PARLIN(M)=PAREAL(PAR26)
          GOTO 290
  227     PARLIN(M)=PAREAL(PAR27)
          GOTO 290
  228     PARLIN(M)=PAREAL(PAR28)
          GOTO 290
  229     PARLIN(M)=PAREAL(PAR29)
          GOTO 290
  230     PARLIN(M)=PAREAL(PAR30)
          GOTO 290
  290     CONTINUE
        ELSEIF(TYPARR(M).EQ.'L') THEN
          ULEN=MIN(36,TRULEN(LABELS(M)))
          WRITE(LABLIN(M),1030) LABELS(M)(1:ULEN)
 1030     FORMAT(A,' (T,F) ')
          GOTO (301,302,303,304,305,306,307,308,309,310,311,312,313,
     *            314,315,316,317,318,319,320,321,322,323,324,325,326,
     *            327,328,329,330), M
  301     PARLIN(M)=PARLOG(PAR1)
          GOTO 390
  302     PARLIN(M)=PARLOG(PAR2)
          GOTO 390
  303     PARLIN(M)=PARLOG(PAR3)
          GOTO 390
  304     PARLIN(M)=PARLOG(PAR4)
          GOTO 390
  305     PARLIN(M)=PARLOG(PAR5)
          GOTO 390
  306     PARLIN(M)=PARLOG(PAR6)
          GOTO 390
  307     PARLIN(M)=PARLOG(PAR7)
          GOTO 390
  308     PARLIN(M)=PARLOG(PAR8)
          GOTO 390
  309     PARLIN(M)=PARLOG(PAR9)
          GOTO 390
  310     PARLIN(M)=PARLOG(PAR10)
          GOTO 390
  311     PARLIN(M)=PARLOG(PAR11)
          GOTO 390
  312     PARLIN(M)=PARLOG(PAR12)
          GOTO 390
  313     PARLIN(M)=PARLOG(PAR13)
          GOTO 390
  314     PARLIN(M)=PARLOG(PAR14)
          GOTO 390
  315     PARLIN(M)=PARLOG(PAR15)
          GOTO 390
  316     PARLIN(M)=PARLOG(PAR16)
          GOTO 390
  317     PARLIN(M)=PARLOG(PAR17)
          GOTO 390
  318     PARLIN(M)=PARLOG(PAR18)
          GOTO 390
  319     PARLIN(M)=PARLOG(PAR19)
          GOTO 390
  320     PARLIN(M)=PARLOG(PAR20)
          GOTO 390
  321     PARLIN(M)=PARLOG(PAR21)
          GOTO 390
  322     PARLIN(M)=PARLOG(PAR22)
          GOTO 390
  323     PARLIN(M)=PARLOG(PAR23)
          GOTO 390
  324     PARLIN(M)=PARLOG(PAR24)
          GOTO 390
  325     PARLIN(M)=PARLOG(PAR25)
          GOTO 390
  326     PARLIN(M)=PARLOG(PAR26)
          GOTO 390
  327     PARLIN(M)=PARLOG(PAR27)
          GOTO 390
  328     PARLIN(M)=PARLOG(PAR28)
          GOTO 390
  329     PARLIN(M)=PARLOG(PAR29)
          GOTO 390
  330     PARLIN(M)=PARLOG(PAR30)
          GOTO 390
  390     CONTINUE
        ELSE
          LABLIN(M)=LABELS(M)(1:TRULEN(LABELS(M)))
          GOTO (401,402,403,404,405,406,407,408,409,410,411,412,413,
     *            414,415,416,417,418,419,420,421,422,423,424,425,426,
     *            427,428,429,430), M
  401     CALL MOVSTR(PAR1,PARLIN(M))
          GOTO 490
  402     CALL MOVSTR(PAR2,PARLIN(M))
          GOTO 490
  403     CALL MOVSTR(PAR3,PARLIN(M))
          GOTO 490
  404     CALL MOVSTR(PAR4,PARLIN(M))
          GOTO 490
  405     CALL MOVSTR(PAR5,PARLIN(M))
          GOTO 490
  406     CALL MOVSTR(PAR6,PARLIN(M))
          GOTO 490
  407     CALL MOVSTR(PAR7,PARLIN(M))
          GOTO 490
  408     CALL MOVSTR(PAR8,PARLIN(M))
          GOTO 490
  409     CALL MOVSTR(PAR9,PARLIN(M))
          GOTO 490
  410     CALL MOVSTR(PAR10,PARLIN(M))
          GOTO 490
  411     CALL MOVSTR(PAR11,PARLIN(M))
          GOTO 490
  412     CALL MOVSTR(PAR12,PARLIN(M))
          GOTO 490
  413     CALL MOVSTR(PAR13,PARLIN(M))
          GOTO 490
  414     CALL MOVSTR(PAR14,PARLIN(M))
          GOTO 490
  415     CALL MOVSTR(PAR15,PARLIN(M))
          GOTO 490
  416     CALL MOVSTR(PAR16,PARLIN(M))
          GOTO 490
  417     CALL MOVSTR(PAR17,PARLIN(M))
          GOTO 490
  418     CALL MOVSTR(PAR18,PARLIN(M))
          GOTO 490
  419     CALL MOVSTR(PAR19,PARLIN(M))
          GOTO 490
  420     CALL MOVSTR(PAR20,PARLIN(M))
          GOTO 490
  421     CALL MOVSTR(PAR21,PARLIN(M))
          GOTO 490
  422     CALL MOVSTR(PAR22,PARLIN(M))
          GOTO 490
  423     CALL MOVSTR(PAR23,PARLIN(M))
          GOTO 490
  424     CALL MOVSTR(PAR24,PARLIN(M))
          GOTO 490
  425     CALL MOVSTR(PAR25,PARLIN(M))
          GOTO 490
  426     CALL MOVSTR(PAR26,PARLIN(M))
          GOTO 490
  427     CALL MOVSTR(PAR27,PARLIN(M))
          GOTO 490
  428     CALL MOVSTR(PAR28,PARLIN(M))
          GOTO 490
  429     CALL MOVSTR(PAR29,PARLIN(M))
          GOTO 490
  430     CALL MOVSTR(PAR30,PARLIN(M))
          GOTO 490
  490     CONTINUE
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
                GOTO(501,502,503,504,505,506,507,508,509,510,511,512,
     *                 513,514,515,516,517,518,519,520,521,522,523,524,
     *                 525,526,527,528,529,530), M
  501           PAR1=IN
                GOTO 590
  502           PAR2=IN
                GOTO 590
  503           PAR3=IN
                GOTO 590
  504           PAR4=IN
                GOTO 590
  505           PAR5=IN
                GOTO 590
  506           PAR6=IN
                GOTO 590
  507           PAR7=IN
                GOTO 590
  508           PAR8=IN
                GOTO 590
  509           PAR9=IN
                GOTO 590
  510           PAR10=IN
                GOTO 590
  511           PAR11=IN
                GOTO 590
  512           PAR12=IN
                GOTO 590
  513           PAR13=IN
                GOTO 590
  514           PAR14=IN
                GOTO 590
  515           PAR15=IN
                GOTO 590
  516           PAR16=IN
                GOTO 590
  517           PAR17=IN
                GOTO 590
  518           PAR18=IN
                GOTO 590
  519           PAR19=IN
                GOTO 590
  520           PAR20=IN
                GOTO 590
  521           PAR21=IN
                GOTO 590
  522           PAR22=IN
                GOTO 590
  523           PAR23=IN
                GOTO 590
  524           PAR24=IN
                GOTO 590
  525           PAR25=IN
                GOTO 590
  526           PAR26=IN
                GOTO 590
  527           PAR27=IN
                GOTO 590
  528           PAR28=IN
                GOTO 590
  529           PAR29=IN
                GOTO 590
  530           PAR30=IN
                GOTO 590
  590           CONTINUE
              ENDIF
            ELSEIF(TYPARR(M).EQ.'I') THEN
              IF(TRULEN(PARLIN(M)).GT.0) THEN
                IN=TRAINT(PARLIN(M))
                GOTO(601,602,603,604,605,606,607,608,609,610,611,612,
     *                 613,614,615,616,617,618,619,620,621,622,623,624,
     *                 625,626,627,628,629,630), M
  601           PAR1=IN
                GOTO 690
  602           PAR2=IN
                GOTO 690
  603           PAR3=IN
                GOTO 690
  604           PAR4=IN
                GOTO 690
  605           PAR5=IN
                GOTO 690
  606           PAR6=IN
                GOTO 690
  607           PAR7=IN
                GOTO 690
  608           PAR8=IN
                GOTO 690
  609           PAR9=IN
                GOTO 690
  610           PAR10=IN
                GOTO 690
  611           PAR11=IN
                GOTO 690
  612           PAR12=IN
                GOTO 690
  613           PAR13=IN
                GOTO 690
  614           PAR14=IN
                GOTO 690
  615           PAR15=IN
                GOTO 690
  616           PAR16=IN
                GOTO 690
  617           PAR17=IN
                GOTO 690
  618           PAR18=IN
                GOTO 690
  619           PAR19=IN
                GOTO 690
  620           PAR20=IN
                GOTO 690
  621           PAR21=IN
                GOTO 690
  622           PAR22=IN
                GOTO 690
  623           PAR23=IN
                GOTO 690
  624           PAR24=IN
                GOTO 690
  625           PAR25=IN
                GOTO 690
  626           PAR26=IN
                GOTO 690
  627           PAR27=IN
                GOTO 690
  628           PAR28=IN
                GOTO 690
  629           PAR29=IN
                GOTO 690
  630           PAR30=IN
                GOTO 690
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
                GOTO(701,702,703,704,705,706,707,708,709,710,711,712,
     *                 713,714,715,716,717,718,719,720,721,722,723,724,
     *                 725,726,727,728,729,730), M
  701           PAR1=IN
                GOTO 790
  702           PAR2=IN
                GOTO 790
  703           PAR3=IN
                GOTO 790
  704           PAR4=IN
                GOTO 790
  705           PAR5=IN
                GOTO 790
  706           PAR6=IN
                GOTO 790
  707           PAR7=IN
                GOTO 790
  708           PAR8=IN
                GOTO 790
  709           PAR9=IN
                GOTO 790
  710           PAR10=IN
                GOTO 790
  711           PAR11=IN
                GOTO 790
  712           PAR12=IN
                GOTO 790
  713           PAR13=IN
                GOTO 790
  714           PAR14=IN
                GOTO 790
  715           PAR15=IN
                GOTO 790
  716           PAR16=IN
                GOTO 790
  717           PAR17=IN
                GOTO 790
  718           PAR18=IN
                GOTO 790
  719           PAR19=IN
                GOTO 790
  720           PAR20=IN
                GOTO 790
  721           PAR21=IN
                GOTO 790
  722           PAR22=IN
                GOTO 790
  723           PAR23=IN
                GOTO 790
  724           PAR24=IN
                GOTO 790
  725           PAR25=IN
                GOTO 790
  726           PAR26=IN
                GOTO 790
  727           PAR27=IN
                GOTO 790
  728           PAR28=IN
                GOTO 790
  729           PAR29=IN
                GOTO 790
  730           PAR30=IN
                GOTO 790
  790           CONTINUE
              ENDIF
            ELSEIF(TYPARR(M).EQ.'C'.OR.TYPARR(M).EQ.'U') THEN
              IF(TYPARR(M).EQ.'U') THEN
                PARLIN(M)=TRANUP(PARLIN(M))
              ENDIF
              GOTO (801,802,803,804,805,806,807,808,809,810,811,812,
     *               813,814,815,816,817,818,819,820,821,822,823,824,
     *               825,826,827,828,829,830), M
  801         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR1)
              GOTO 890
  802         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR2)
              GOTO 890
  803         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR3)
              GOTO 890
  804         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR4)
              GOTO 890
  805         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR5)
              GOTO 890
  806         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR6)
              GOTO 890
  807         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR7)
              GOTO 890
  808         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR8)
              GOTO 890
  809         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR9)
              GOTO 890
  810         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR10)
              GOTO 890
  811         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR11)
              GOTO 890
  812         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR12)
              GOTO 890
  813         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR13)
              GOTO 890
  814         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR14)
              GOTO 890
  815         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR15)
              GOTO 890
  816         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR16)
              GOTO 890
  817         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR17)
              GOTO 890
  818         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR18)
              GOTO 890
  819         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR19)
              GOTO 890
  820         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR20)
              GOTO 890
  821         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR21)
              GOTO 890
  822         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR22)
              GOTO 890
  823         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR23)
              GOTO 890
  824         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR24)
              GOTO 890
  825         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR25)
              GOTO 890
  826         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR26)
              GOTO 890
  827         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR27)
              GOTO 890
  828         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR28)
              GOTO 890
  829         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR29)
              GOTO 890
  830         CALL MOVSTR(PARLIN(M)(1:TRULEN(PARLIN(M))),PAR30)
              GOTO 890
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
C&ELSE
C&C-
C&C-  Construct mapping of parameter number to array index for numeric and 
C&C-  character parameters.  Copy parameters.
C&C-
C&      NNUM = 0
C&      NCHAR = 0
C&      DO 50 I = 1, NUMPAR
C&        IF(TYPARR(I).EQ.'I' .OR. TYPARR(I).EQ.'R' .OR. 
C&     &    TYPARR(I).EQ.'L')THEN
C&          NNUM = NNUM + 1
C&          PTON(I) = NNUM
C&          J = I
C&          ASSIGN 10 TO RETURN
C&          GO TO 100
C& 10       CONTINUE
C&          ADDNPAR(I) = ADDR
C&          CALL MOVINT(%VAL(ADDR), PAR(NNUM))
C&        ELSE
C&          NCHAR = NCHAR + 1
C&          PTOC(I) = NCHAR
C&          J = NCHAR + NUMPAR + 2
C&          ASSIGN 20 TO RETURN
C&          GO TO 100
C& 20       CONTINUE
C&          LENCPAR(I) = ADDR
C&          J = I
C&          ASSIGN 30 TO RETURN
C&          GO TO 100
C& 30       CONTINUE
C&          ADDCPAR(I) = ADDR
C&          CALL MOVSTR1(%VAL(ADDR), CPAR(NCHAR), %VAL(LENCPAR(I)))
C&        ENDIF
C& 50   CONTINUE
C&C-
C&C- Get passed length of LABELS
C&C-
C&      J = NUMPAR + 1
C&      ASSIGN 60 TO RETURN
C&      GO TO 100
C& 60   CONTINUE
C&      LEN_LABELS = ADDR
C&C-
C&C- Interact with user
C&C-
C&      CALL GETDIS77(NUMPAR, %REF(LABELS), %REF(TYPARR), LIMITS, 
C&     &  PTON, PTOC, PAR, %REF(CPAR), %VAL(LEN_LABELS), %VAL(1),
C&     &  %VAL(LEN(CPAR(1))))
C&C-
C&C- Copy parameters back to calling program
C&C-
C&      DO 90 I=1,NUMPAR
C&        IF(TYPARR(I).EQ.'I' .OR. TYPARR(I).EQ.'R' .OR.
C&     &    TYPARR(I).EQ.'L')THEN
C&          CALL MOVINT(PAR(PTON(I)), %VAL(ADDNPAR(I)))
C&        ELSE
C&          CALL MOVSTR(%REF(CPAR(PTOC(I))), %VAL(ADDCPAR(I)), 
C&     &      %VAL(LEN(CPAR(PTOC(I)))), %VAL(LENCPAR(I)))
C&        ENDIF
C& 90   CONTINUE
C&      GO TO 999    
C&C-
C&C- The following code is a "subroutine" to return the address of parameter
C&C- J (1-62) in the variable ADDR.
C& 100  CONTINUE
C&      GO TO (101,102,103,104,105,106,107,108,109,110,
C&     &       111,112,113,114,115,116,117,118,119,120,
C&     &       121,122,123,124,125,126,127,128,129,130,
C&     &       131,132,133,134,135,136,137,138,139,140,
C&     &       141,142,143,144,145,146,147,148,149,150,
C&     &       151,152,153,154,155,156,157,158,159,160,
C&     &       161,162), J
C& 101  ADDR = D0_LOC(PAR1)
C&      GO TO RETURN
C& 102  ADDR = D0_LOC(PAR2)
C&      GO TO RETURN
C& 103  ADDR = D0_LOC(PAR3)
C&      GO TO RETURN
C& 104  ADDR = D0_LOC(PAR4)
C&      GO TO RETURN
C& 105  ADDR = D0_LOC(PAR5)
C&      GO TO RETURN
C& 106  ADDR = D0_LOC(PAR6)
C&      GO TO RETURN
C& 107  ADDR = D0_LOC(PAR7)
C&      GO TO RETURN
C& 108  ADDR = D0_LOC(PAR8)
C&      GO TO RETURN
C& 109  ADDR = D0_LOC(PAR9)
C&      GO TO RETURN
C& 110  ADDR = D0_LOC(PAR10)
C&      GO TO RETURN
C& 111  ADDR = D0_LOC(PAR11)
C&      GO TO RETURN
C& 112  ADDR = D0_LOC(PAR12)
C&      GO TO RETURN
C& 113  ADDR = D0_LOC(PAR13)
C&      GO TO RETURN
C& 114  ADDR = D0_LOC(PAR14)
C&      GO TO RETURN
C& 115  ADDR = D0_LOC(PAR15)
C&      GO TO RETURN
C& 116  ADDR = D0_LOC(PAR16)
C&      GO TO RETURN
C& 117  ADDR = D0_LOC(PAR17)
C&      GO TO RETURN
C& 118  ADDR = D0_LOC(PAR18)
C&      GO TO RETURN
C& 119  ADDR = D0_LOC(PAR19)
C&      GO TO RETURN
C& 120  ADDR = D0_LOC(PAR20)
C&      GO TO RETURN
C& 121  ADDR = D0_LOC(PAR21)
C&      GO TO RETURN
C& 122  ADDR = D0_LOC(PAR22)
C&      GO TO RETURN
C& 123  ADDR = D0_LOC(PAR23)
C&      GO TO RETURN
C& 124  ADDR = D0_LOC(PAR24)
C&      GO TO RETURN
C& 125  ADDR = D0_LOC(PAR25)
C&      GO TO RETURN
C& 126  ADDR = D0_LOC(PAR26)
C&      GO TO RETURN
C& 127  ADDR = D0_LOC(PAR27)
C&      GO TO RETURN
C& 128  ADDR = D0_LOC(PAR28)
C&      GO TO RETURN
C& 129  ADDR = D0_LOC(PAR29)
C&      GO TO RETURN
C& 130  ADDR = D0_LOC(PAR30)
C&      GO TO RETURN
C& 131  ADDR = D0_LOC(PAR31)
C&      GO TO RETURN
C& 132  ADDR = D0_LOC(PAR32)
C&      GO TO RETURN
C& 133  ADDR = D0_LOC(PAR33)
C&      GO TO RETURN
C& 134  ADDR = D0_LOC(PAR34)
C&      GO TO RETURN
C& 135  ADDR = D0_LOC(PAR35)
C&      GO TO RETURN
C& 136  ADDR = D0_LOC(PAR36)
C&      GO TO RETURN
C& 137  ADDR = D0_LOC(PAR37)
C&      GO TO RETURN
C& 138  ADDR = D0_LOC(PAR38)
C&      GO TO RETURN
C& 139  ADDR = D0_LOC(PAR39)
C&      GO TO RETURN
C& 140  ADDR = D0_LOC(PAR40)
C&      GO TO RETURN
C& 141  ADDR = D0_LOC(PAR44)
C&      GO TO RETURN
C& 142  ADDR = D0_LOC(PAR42)
C&      GO TO RETURN
C& 143  ADDR = D0_LOC(PAR43)
C&      GO TO RETURN
C& 144  ADDR = D0_LOC(PAR44)
C&      GO TO RETURN
C& 145  ADDR = D0_LOC(PAR44)
C&      GO TO RETURN
C& 146  ADDR = D0_LOC(PAR46)
C&      GO TO RETURN
C& 147  ADDR = D0_LOC(PAR47)
C&      GO TO RETURN
C& 148  ADDR = D0_LOC(PAR48)
C&      GO TO RETURN
C& 149  ADDR = D0_LOC(PAR49)
C&      GO TO RETURN
C& 150  ADDR = D0_LOC(PAR50)
C&      GO TO RETURN
C& 151  ADDR = D0_LOC(PAR51)
C&      GO TO RETURN
C& 152  ADDR = D0_LOC(PAR52)
C&      GO TO RETURN
C& 153  ADDR = D0_LOC(PAR53)
C&      GO TO RETURN
C& 154  ADDR = D0_LOC(PAR54)
C&      GO TO RETURN
C& 155  ADDR = D0_LOC(PAR55)
C&      GO TO RETURN
C& 156  ADDR = D0_LOC(PAR56)
C&      GO TO RETURN
C& 157  ADDR = D0_LOC(PAR57)
C&      GO TO RETURN
C& 158  ADDR = D0_LOC(PAR58)
C&      GO TO RETURN
C& 159  ADDR = D0_LOC(PAR59)
C&      GO TO RETURN
C& 160  ADDR = D0_LOC(PAR60)
C&      GO TO RETURN
C& 161  ADDR = D0_LOC(PAR61)
C&      GO TO RETURN
C& 162  ADDR = D0_LOC(PAR62)
C&      GO TO RETURN
C&ENDIF
 999  RETURN
      END
