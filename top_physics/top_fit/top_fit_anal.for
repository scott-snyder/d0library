      SUBROUTINE TOP_FIT_ANAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ANALYZE DILEPTON MASSES HERE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KINE_FIT.INC'
      INCLUDE 'D0$INC:EVENT_QUAN_2C.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      INTEGER LISV1,LISP1
      EQUIVALENCE (LISV1,CSTLNK(LNKMX)),(LISP1,CSTLNK(LNKMX-1))
      INTEGER GZISV1
C
      INTEGER STATUS
      LOGICAL MORE
      LOGICAL FIT_EV    !JUST TO COMPILE
C
      INTEGER IER
      LOGICAL first
      LOGICAL DO_FIT_EVENT,DO_MATCH_JETS,READ_RCP
      SAVE first
      DATA first / .true. /
C
C----------------------------------------------------------------------
C
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_FIT_RCP')
        CALL EZGET('DO_FIT_EVENT',DO_FIT_EVENT,IER)
        CALL EZGET('DO_MATCH_JETS',DO_MATCH_JETS,IER)
        CALL EZGET('READ_EVENT_FROM_RCP',READ_RCP,IER)
        CALL DO_HBOOK_OPEN('HBOOK_OPEN',STATUS)
        CALL DHDIR('TOP_FIT_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('TOP_FIT','TOP_FIT_ANAL',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        CALL DO_HBOOK('CONFIGE_NTUPLE')
        CALL EZRSET
      ENDIF
C
      ITOT_EV = ITOT_EV + 1
      CALL DHDIR_DECLARE_FILE('FIT2C')
C
      CALL DHDIR('TOP_FIT_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('TOP_FIT','TOP_FIT_ANAL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      MORE =  .TRUE.
      COMBNUM = 0
C
      DO WHILE (MORE)
        IF ( DO_FIT_EVENT ) THEN
          CALL TOP_FIT_GET_EVENT(MORE,IER)
        ELSE
          MORE = .false.
          COMBNUM=1
        ENDIF
C
        IF(IER.NE.0)GO TO 888   !NOT A GOOD EVENT
C
        IF ( COMBNUM.EQ.1 ) THEN
          IACC_EV = IACC_EV + 1
          IF ( DO_MATCH_JETS ) THEN
            CALL TOP_FIT_MATCH_JETS
          ENDIF
        ENDIF
C
        CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        IF ( DO_FIT_EVENT ) THEN
          FIT_EV = .FALSE.
C
          CALL TOP_FIT_INIVAL         !Initial values setup
C
          CALL  TOP_FIT_DO            !do top fit
C
          FIT_EV = .TRUE.
          CALL TOP_FIT_SAVE_RESULTS           !Put out results on to banks
C
        ENDIF
      ENDDO
C
      CALL TOP_FIT_WRITE_NTUPLE
C
  888 CONTINUE
C
      IF ( READ_RCP.AND.IER.NE.0 ) THEN
C END OF DATA
        CALL CEND
        STOP
      ENDIF
      IF(IER.NE.4)GO TO 999
C PERMIT EVENTS HERE FROM NON TTBAR SAMPLES
      IF ( FIT_EV ) THEN
        IFIT_EV = IFIT_EV + 1  !this event has solutions
      ENDIF
C
      LISV1 = GZISV1()
      DO WHILE (LISV1.NE.0)
        CALL MZDROP(IXMAIN,LISV1,'V')   !DROP STRUCTUREs BELOW
        LISV1 = LQ(LISV1)
      ENDDO
C
      RETURN
  999 CALL FLGSET('WRITE_THIS_EVENT',.FALSE.)
      RETURN
      END
