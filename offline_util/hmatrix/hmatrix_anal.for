      SUBROUTINE HMATRIX_ANAL(IUSE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Analyze hmatrix
C-
C-   Inputs  :IUSE = 1, DO HISTOGRAMS ETC. OTHERWISE RETURN
C-            IUSE =1 IS THE H MATRIX RELEVANT TO PRESENT DATA SAMPLE.
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:EVENT_QUAN.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER WLINK,TLINK
      EQUIVALENCE (CRFLNK(3),WLINK),(CRFLNK(4),TLINK)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IER,I,J,ITWO
      INTEGER MAX_SCATTER_DIAG
      REAL    TOP_MASS_PRED,DEL_PRED_GEN
      LOGICAL USE_ISAJET_INFO
      REAL    CHISQ,PROBL,PROB_CUT
      INTEGER IUSE
C----------------------------------------------------------------------
C
      IF(IUSE.NE.1)RETURN
C
      CALL DHDIR('HMATRIX_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('HMATRIX','HMATRIX_ANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
C
C ****  BOOK HISTOGRAMS HERE
C
        CALL DO_HBOOK('VISIBLE_HISTOGRAMS')     ! BOOK THEM
        CALL DO_HBOOK('INVISIBLE_HISTOGRAMS')     ! BOOK THEM
        IF(.NOT.ACCUMULATE)THEN
          CALL DO_HBOOK('USAGE_PHASE_HISTOGRAMS')     ! BOOK THEM
        ENDIF
        CALL EZGET('MAX_SCATTER_DIAG',MAX_SCATTER_DIAG,IER)
        CALL EZGET('W_MASS',WMASS,IER)
        CALL EZGET('USE_ISAJET_INFO',USE_ISAJET_INFO,IER)
        CALL EZGET('TOP_PROB_CUT',PROB_CUT,IER)
C
        CALL EZRSET
C
      ENDIF
C
      DO I = 1 , VIS_DIM
        CALL HFILL(100 + I, C(LQUAN+I),0.0,1.0)
      ENDDO
C
      IF ( INVIS_DIM.GT.0 ) THEN
        DO I = 1, INVIS_DIM
          CALL HFILL(200 + I, C(LQUAN+VIS_DIM+I),0.0,1.0)
        ENDDO
      ENDIF
C
C ****  USAGE HISTOGRAMS NEXT. USER WILL NEED TO CHANGE CODE HERE.
C
      IF ( .NOT.ACCUMULATE ) THEN
        CALL HFILL(301,C(LHMTR+3),0.0,1.0)    ! Chisquared
        CALL HFILL(302,C(LHMTR+5),0.0,1.0)    ! Probability
        CALL HFILL(303,C(LHMTR+6),0.0,1.0)    ! Chisquared TRUNCATED
        CALL HFILL(304,C(LHMTR+8),0.0,1.0)    ! Probability
C
C ****  now to histogram diagonalized quantities
C
        DO I = 1 , VIS_DIM
          CALL HFILL(350+I,C(LDIAG+I),0.,1.0)
        ENDDO
C
C ****  now to scattergram diaganalized normalised quantities.
C
        IF ( VIS_DIM.GT.1 ) THEN
          ITWO = 0
          DO I = 1 , VIS_DIM-1
            DO J = I+1 , VIS_DIM
              ITWO = ITWO + 1
              IF ( ITWO.LE.MAX_SCATTER_DIAG ) THEN
                CALL HFILL(400+ITWO,C(LDIAG+J),C(LDIAG+I),1.0)
              ELSE
                GO TO 950               ! NO MORE
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C
C ****  do TOP ISAJET mass versus TOP predicted mass
C
  950   CONTINUE
C
        IF ( USE_ISAJET_INFO  ) THEN
          WLINK = CSTLNK(LNKMX-1)           ! Link to W ISAJ
          TLINK = LQ(WLINK-1)               ! link to parent Top ISAJ
          TOP_MASS = Q(TLINK + 6)              ! Generated top mass
          TOP_MASS_PRED = C(LQUAN+VIS_DIM+1)*WMASS  ! UNREDUCED
          DEL_PRED_GEN = TOP_MASS_PRED - TOP_MASS
          CALL HFILL(701,DEL_PRED_GEN,0.,1.0)
          CALL HFILL(702,TOP_MASS,TOP_MASS_PRED,1.0)
        ENDIF
C
C ****  DO SOME CHISQUARED BASED ANALYSIS
C
        CHISQ = C(LHMTR+3)
        PROBL = C(LHMTR+5)
C
        IF(PROBL.GT.PROB_CUT)THEN
          CALL HFILL(703,TRMASS,0.,1.0)
        ENDIF
C
      ENDIF
C
  999 RETURN
      END
