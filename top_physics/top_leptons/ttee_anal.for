      SUBROUTINE TTEE_ANAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VERT.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IER
C
      LOGICAL MET_CUT,JET_CUTS,FAKE_BKG,REJ_MBLANK
      INTEGER NELEC,NFELEC,NPHOT,NELEC_REQ
C----------------------------------------------------------------------
C
      IF(FIRST)THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGET('FAKE_BKG',FAKE_BKG,IER)
        CALL EZGET('REJ_MBLANK',REJ_MBLANK,IER)
        CALL EZGET('NELEC_REQ',NELEC_REQ,IER)
        CALL EZRSET
      ENDIF
C
C **First get general event and vertex information
C
      CALL GET_VERT(IER)
      IF(REJ_MBLANK .AND.(RUN(9).NE.0))GOTO 999
C
C **Get electrons and  photons.
C
      CALL GET_ELECTRONS(IER)
      CALL GET_TTEE_PHOTONS(IER)
C
C **Apply electron and  photon cuts 
C
      CALL ELECTRON_CUTS(NELEC,NFELEC)
      IF((NELEC-NFELEC).LT.NELEC_REQ)GOTO 999
      CALL PHOTON_CUTS(NPHOT)
      IF(.NOT. FAKE_BKG)THEN
        IF((NELEC+NPHOT).LT.2)GOTO 999
      ENDIF
C
C **Now get Missing ET
C
      CALL GET_MET(IER)
C
C **Now get Jet energy
C
      CALL GET_TTEE_JETS(IER)
C
C **Apply Missing et cut
C
      IF(.NOT. MET_CUT())GOTO 999
C
C **Apply jet cuts
C
      IF(.NOT. JET_CUTS())GOTO 999
      CALL MAKE_TTEE_NTUPLE
C
  999 RETURN
      END
