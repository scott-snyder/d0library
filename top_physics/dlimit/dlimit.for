      SUBROUTINE DLIMIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Computesd the discovery limit assuming
C-   efficiencies that vary with top mass .
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Updated  23-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_CROSS.INC'
C
      INTEGER I
      REAL    DISC_LIM
C
      REAL    DIVDIF
      REAL    LUM
      REAL EE_SIGBR,EMU_SIGBR,EJETS_SIGBR,EJETS_TAG_SIGBR,
     &               MUMU_SIGBR,MUJETS_SIGBR,MUJETS_TAG_SIGBR
      REAL EE_SERR,EMU_SERR,EJETS_SERR,EJETS_TAG_SERR,
     &               MUMU_SERR,MUJETS_SERR,MUJETS_TAG_SERR
      REAL    CROSS_SECT,C18,C20
C
      LOGICAL OK,DLIMIT_INI,DLIMIT_FIN
C
      INCLUDE 'D0$INC:SIG_EFF.INC'
C      INTEGER LREC,ISTAT
      LOGICAL DO_CONSISTENCY,DO_HIGGS
      INTEGER IER
C----------------------------------------------------------------------
C      OK = DLIMIT_INI()  !INITIALIZE DLIMIT
C
      OPEN(UNIT=28,FILE='[-.cross_section]TOP_CROSS_OUTPUT_TEV18.DAT',
     &  STATUS='OLD',
     &  FORM='UNFORMATTED')
      OPEN(UNIT=30,FILE='[-.cross_section]TOP_CROSS_OUTPUT_TEV20.DAT',
     &  STATUS='OLD',
     &  FORM='UNFORMATTED')
C
      OPEN(UNIT=31,FILE='[-.cross_section]nnlo_central_18tev.DAT',
     &  STATUS='OLD',
     &  FORM='FORMATTED')
      OPEN(UNIT=32,FILE='[-.cross_section]nnlo_central_20tev.DAT',
     &  STATUS='OLD',
     &  FORM='FORMATTED')
C
C      LREC = 1024
C      CALL HROPEN(55,'DLIMIT','DLIMIT_HBOOK.DAT','NX',LREC,ISTAT)
C
      DO 10 I = 1 , ND
        READ(28)TMASS_18(I),TTB_CROSS_18(I),TTB_GG_18(I),
     &    TTB_QQB_18(I)
        READ(30)TMASS_20(I),TTB_CROSS_20(I),TTB_GG_20(I),
     &    TTB_QQB_20(I)
        TM(I) = TMASS_18(I)
        RAT_BORN(I) = TTB_CROSS_20(I)/TTB_CROSS_18(I)
        WRITE(41,1)TM(I),RAT_BORN(I)
    1   FORMAT(1X,2F15.5)
  10  CONTINUE
C
      DO 20 I = 1 , ND
        READ(31,*,END=21)TM_N18(I),TTB_CROSS_N18(I)
        TTB_CROSS_N18(I) = TTB_CROSS_N18(I)*1.E6
   20 CONTINUE
   21 CONTINUE
      N18 = I
      TM_N18(N18) = 220.0
      TTB_CROSS_N18(N18) = 1.213  !OBTAINED BY RATIO METHOD
      N18 = N18 + 1
      TM_N18(N18) = 240.0
      TTB_CROSS_N18(N18) = 0.6512  !OBTAINED BY RATIO METHOD
C
      DO 30 I = 1 , ND
        READ(32,*,END=31)TM_N20(I),TTB_CROSS_N20(I)
        TTB_CROSS_N20(I) = TTB_CROSS_N20(I)*1.E6
   30 CONTINUE
   31 CONTINUE
      N20 = I
      TM_N20(N20) = 220.0
      TTB_CROSS_N20(N20) = 1.559
      N20 = N20 + 1
      TM_N20(N20) = 240.0
      TTB_CROSS_N20(N20) = 0.811
C
      DO 40 I = 1 , ND
        RAT_NNLO(I) = DIVDIF(TTB_CROSS_N20,TM_N20,N20,TM(I),M20)/
     &     DIVDIF(TTB_CROSS_N18,TM_N18,N18,TM(I),M18)
        WRITE(42,1)TM(I),RAT_NNLO(I)
   40 CONTINUE
C
      DO 71 I = 1 , NMASS 
        WRITE(45,12)TM_EFF(I),EE_SBR(I),EMU_SBR(I),EJETS_SBR(I),
     &    EJETS_TAG_SBR(I),MUMU_SBR(I),MUJETS_SBR(I),MUJETS_TAG_SBR(I)
   12   FORMAT(' MASS ,EE SBR,EMU SBR,EJETS SBR,EJETS_TAG SBR,MUMU_SBR',
     &    ' MUJETS_SBR,MUJETS_TAG_SBR '
     &    /,(7E10.4/))
   71 CONTINUE
C
      DO 60 I = 1 , NDISC
        LUM = I
        IEN=18
        DISC_LIMIT_18(I) = DISC_LIM(LUM)
        IEN=20
        DISC_LIMIT_20(I) = DISC_LIM(LUM)
        WRITE(43,4)LUM,DISC_LIMIT_18(I),DISC_LIMIT_20(I)
    4   FORMAT(1X,3F15.5)
   60 CONTINUE
C
      DO 70 I = 1 , ND
        IEN=18
        C18 = CROSS_SECT(TM(I))
        IEN=20
        C20 = CROSS_SECT(TM(I))
        WRITE(44,2)TM(I),
     &    EE_SIGBR(TM(I)),EMU_SIGBR(TM(I)),
     &    EJETS_SIGBR(TM(I)),EJETS_TAG_SIGBR(TM(I)),
     &    MUMU_SIGBR(TM(I)),MUJETS_SIGBR(TM(I)),
     &    MUJETS_TAG_SIGBR(TM(I)),
     &    C18,C20
    2   FORMAT(1X,10E12.4)
    3   FORMAT(1X,8E12.4)
        WRITE(46,3)TM(I),
     &    EE_SERR(TM(I)),EMU_SERR(TM(I)),
     &    EJETS_SERR(TM(I)),EJETS_TAG_SERR(TM(I)),
     &    MUMU_SERR(TM(I)),MUJETS_SERR(TM(I)),
     &    MUJETS_TAG_SERR(TM(I))
   70 CONTINUE
C
      CALL EZPICK('DLIMIT_RCP')
      CALL EZGET('DO_CONSISTENCY',DO_CONSISTENCY,IER)
      CALL EZGET('DO_HIGGS',DO_HIGGS,IER)
      CALL EZRSET
      IF ( DO_CONSISTENCY ) THEN
        CALL CONSISTENCY
      ENDIF
      IF ( DO_HIGGS ) THEN
        CALL HIGGS
      ENDIF
C
C      OK = DLIMIT_FIN()
      CLOSE(UNIT=28)
      CLOSE(UNIT=30)
      CLOSE(UNIT=31)
      CLOSE(UNIT=32)
      CLOSE(UNIT=41)
      CLOSE(UNIT=42)
      CLOSE(UNIT=43)
      CLOSE(UNIT=44)
      CLOSE(UNIT=45)
C
  999 RETURN
      END
