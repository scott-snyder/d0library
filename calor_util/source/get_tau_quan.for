      SUBROUTINE GET_TAU_QUAN (LPTAU,PROB,CHISQ,FVAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to get tau signal and bkg chisq's
C-                         for a given PTAU
C-
C-   Inputs  : LPTAU       (I)  address of PTAU BANK
C-   Outputs : CHISQ(1)    (R)  signal chisquared
C-             CHISQ(2)    (R)  background chisquared
C-             PROB(1)     (R)  event probability of being signal
C-             PROB(2)     (R)  event probability of being background
C-             FVAR  (R)  Fisher variable for PTAU
C-
C-   Controls: tau_hmatrix.rcp
C-             hmatrix.rcp
C-             cahits.rcp  (if use dst data)
C-
C-             to call this code, one needs to include "hmatrix,calor,
C-             cahits,cajets" in the pbd. (the last two for dst data).
C-
C-   Created  10-MAY-1995   Johannes V. (Djoko) Wirjawan
C-   Updated  21-JUL-1995   Qizhong Li-Demarteau 
C-   Updated   7-DEC-1995   Johannes V. (Djoko) Wirjawan  split central and
C-                                                        forward ptau 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:EVENT_QUAN.INC'
      INCLUDE 'D0$LINKS:IZQUAN.LINK'
      INCLUDE 'D0$LINKS:IZISAQ.LINK'
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$LINKS:IZCAEH.LINK'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INTEGER I,IER
      REAL    QUAN(20)
      INTEGER  NQUAN,NVIS,INVS,NVTX,NZERO
      CHARACTER*(16) VIS_NAME(20),INVIS_NAME(20)
      REAL    ZV,DZ,ZVTX_INFO(3,1),PETA,DETA
      REAL    PETA_TO_DETA
      INTEGER LAYER,MAXLAY
      PARAMETER (MAXLAY=17)
      REAL    ET(MAXLAY),E(MAXLAY)
      REAL     PROB(2),CHISQ(2),FVAR
      INTEGER NCELL
      INTEGER GZCAEH
      REAL    PTAU_THE,PTAU_PHI
      REAL    ETSUM,ESUM
      LOGICAL FIRST,FLAG,SELECT,OK,DST,HMATRIX_EVENT
      INTEGER IND
      CHARACTER*32 USE_HMATRIX(10)
      INTEGER NUSE,NMARK
      INTEGER LPTR,LPTAU,LQUAN_SAVE
      EQUIVALENCE (LQUAN_SAVE,HSTLNK(HST_LNKMX))
      INTEGER NDATAQ
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST  ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZ_GET_CHARS('VISIBLE_QUANTITIES',NVIS,VIS_NAME,IER)
        CALL EZ_GET_CHARS('INVISIBLE_QUANTITIES',INVS,INVIS_NAME,IER)
        CALL EZGET('FLAG',FLAG,IER)
        CALL EZGET('DO_SELECTION',SELECT,IER)
        CALL EZGET('DST_DATA',DST,IER)
        CALL EZ_GET_CHARS('USE_HMATRIX',NUSE,USE_HMATRIX,IER)
        CALL EZRSET
      ENDIF
C
      NQUAN = NVIS + INVS
      CALL VZERO(ET(1),MAXLAY)
      CALL VZERO(E(1),MAXLAY)
      IF (DST) CALL CAEQ_TO_CAEP()
      CHISQ(1) = 9999.9
      CHISQ(2) = 9999.9
      PROB(1)  = 0
      PROB(2)  = 0
      FVAR     = -9999.9
      PTAU_THE = 90.
      PTAU_PHI = 90.
      LBANK = LPTAU                  ! to reserve the original LPTAU
      PTAU_THE = Q(LBANK+8)
      PTAU_PHI = Q(LBANK+9)
C
      LJETS = LQ(LPTAU-2)
      IF (LJETS .LE. 0) RETURN
      ETSUM = Q(LJETS+6)
      ESUM  = Q(LJETS+5)
      LJPTS = LQ(LJETS-IZJPTS)
C
C *** Calculate deposition jet energy in each layer
C
      IF (LJPTS.GT.0) THEN
        NCELL = IQ(LJPTS+2)
C              LCAHT = GZCAHT()
C              IF(LCAHT.GT.0) LCAEH = LQ(LCAHT-IZCAEH)
        LCAEH = GZCAEH()
        IF (LCAEH.GT.0) THEN
          DO I = 1,NCELL
            LPTR = LCAEH+IQ(LCAEH+2)*(IQ(LJPTS+I+2)-1)
            LAYER   = IQ(LPTR+14)
            ET(LAYER) = ET(LAYER) + Q(LPTR+8)
            E(LAYER) = E(LAYER) + Q(LPTR+7)
          ENDDO
          GOTO 300
        ENDIF
      ENDIF
C
C ****  now to fill QUAN
C
 300  CALL VZERO(QUAN(1),NQUAN)      ! initialized to zero
      IF (ETSUM.GT.0.0 .AND. ESUM.GT.0.0) THEN
        IF (FLAG.EQ.0) THEN
          QUAN(1) = ET(1)/ETSUM
          QUAN(2) = ET(2)/ETSUM
          QUAN(3) = (ET(3)+ET(4)+ET(5)+ET(6))/ETSUM
          QUAN(4) = ET(7)/ETSUM
          QUAN(5) = (ET(11)+ET(12)+ET(13)+ET(14))/ETSUM
          QUAN(6) = LOG(ETSUM)
        ELSE
          QUAN(1) = E(1)/ESUM
          QUAN(2) = E(2)/ESUM
          QUAN(3) = (E(3)+E(4)+E(5)+E(6))/ESUM
          QUAN(4) = E(7)/ESUM
          QUAN(5) = (E(11)+E(12)+E(13)+E(14))/ESUM
          QUAN(6) = LOG(ESUM)
        ENDIF
C
C   get Z vertex, and consider the main primary vertex only
C
        CALL VERTEX_INFO(1,NVTX,ZVTX_INFO,OK)
        IF ( OK ) THEN
          ZV = ZVTX_INFO(1,1)
          DZ = ZVTX_INFO(2,1)
        ELSE
          ZV = 0.0
          DZ = 0.0
        ENDIF
        IF (DZ.NE.0) QUAN(7) = ZV/DZ
        QUAN(8)  = Q(LPTAU+17)/ESUM                 ! E fraction in 3x3 window
        QUAN(9)  = Q(LPTAU+18)/ESUM                 ! E fraction in 5x5 window
        QUAN(10) = (Q(LPTAU+12)+Q(LPTAU+13))/ETSUM  ! Hottest + next Et fract.
      ENDIF
C
C ****  now fill quantities for new H matrix 
C
      NZERO = 0
      DO I = 1, NQUAN
        C(LQUAN+I) = QUAN(I)
        IF (QUAN(I).EQ.0.)  NZERO = NZERO+1
      ENDDO
      IF (NZERO.EQ.NQUAN) GOTO 999       ! reject event if all quans are zero's
C
      OK = HMATRIX_EVENT()
      IF (.NOT.OK) GOTO 999
      LQUAN_SAVE = LQUAN                 ! SAVE QUAN BANK
C 
C **** get the detector eta for PTAU
C
      PETA = Q(LPTAU+10)
      DETA = PETA_TO_DETA(PETA,ZV)*10.
C
      IND = 1
      DO I = 1 , 2
        IF (ABS(DETA).LE.10) THEN                   ! central ptau
          CALL HMATRIX_SET(USE_HMATRIX(I),IER)
        ELSE
          CALL HMATRIX_SET(USE_HMATRIX(I+2),IER)    ! forward ptau
        ENDIF
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('TAU_HMATRIX','TAU_HMATRIX_ANAL',
     &          'ERROR SETTING TO H MATRIX ','W')
        ELSE
          IF ( LQUAN.NE.LQUAN_SAVE ) THEN
C COPY SAVED QUAN BANK TO QUAN
            NDATAQ = IC(LQUAN_SAVE-1)
            CALL UCOPY(C(LQUAN_SAVE+1),C(LQUAN+1),NDATAQ)
          ENDIF
          CALL HMATRIX_CHISQUARED
          CHISQ(IND) = C(LHMTR+3)           !Chisq
          PROB(IND) = C(LHMTR+5)            !Prob
          NMARK = IND+2
          CALL HMATRIX_RESET
        ENDIF
        IND = IND + 1  !Increment pointer anyway
      ENDDO
      FVAR = (CHISQ(2)-CHISQ(1))/2
C
C----------------------------------------------------------------------
C
  999 RETURN
      END
