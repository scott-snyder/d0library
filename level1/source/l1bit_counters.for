      SUBROUTINE L1BIT_COUNTERS(NTRIG_S,NTRIG_H)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do the trigger statistics
C-
C-   Inputs  :
C-   Outputs : NTRIG_S - SIMULATOR trigger multiplicity
C-             NTRIG_H -  HARDWARE trigger multiplicity
C-                       The multiplicity can be: 0 = no muons
C-                                                1 = one muon
C-                                                2 = two muons
C-                                                3 = three or more muons
C-   Controls:
C-
C-   Original    JUN-1992   Kamel Bazizi
C-   Created  30-AUG-1992   Guilherme Lima
C-   Updated       jan-94   Jussara - include hardware statistics
C-   Updated  22-MAR-2004   sss - compile with g77
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL S_L1BIT(16), H_L1BIT(16),HFW_L1BIT(16)
      INTEGER H_CCT_LATCH(7)
      INTEGER BIT1,BIT2,BIT3,NTRIG_S(7)
      INTEGER BIT1W,BIT2W,BIT3W,NTRIG_H(7)
      INTEGER I,II,IPW(8,2),ISW(8)
      INTEGER Y1,Y2,Y3,Y4,X1,X2,X3
      INTEGER NCCT,NOMU,ONEMU,TWOMUS,THREEMUS
      INTEGER NCCTW,NOMUW,ONEMUW,TWOMUSW,THREEMUSW
      INTEGER ICOUNT,IER,IBIT,IERP
      LOGICAL FIRST, IPRSTAT
	CHARACTER*72 STRING
      SAVE FIRST
      DATA FIRST / .TRUE. /

      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('MUSIM_RCP')
	CALL EZERR(IERP)     ! Check if error
	IF(IERP.EQ.0)THEN
          CALL EZGET('IPR_STAT',IPRSTAT,IER)
          CALL EZRSET()
	ELSE
           CALL EZGET_ERROR_TEXT(IER,STRING)
           CALL ERRMSG(' CANNOT PICK MUSIM.RCP',
     &          'L1BIT_COUNTERS',STRING,'F')
           GOTO 999
	ENDIF

        DO I = 1 , 7
          NTRIG_S(I) = 0
          NTRIG_H(I) = 0
        ENDDO
        NOMU = 0
        ONEMU = 0
        TWOMUS = 0
        THREEMUS = 0
        NOMUW = 0
        ONEMUW = 0
        TWOMUSW = 0
        THREEMUSW = 0
      ENDIF

C----------------------------------------------------------------------

C-  Get hardware info in TRGR
      CALL HARD_INFO(H_CCT_LATCH,H_L1BIT,HFW_L1BIT)

C- Gets raw L1 muon raw trigger bits from simulator
      CALL MU_L1_RAW_BITS(S_L1BIT)

C- CF multiplicity
      BIT1=0
      BIT2=0
      IF( S_L1BIT(2) ) BIT1 = 1
      IF( S_L1BIT(3) ) BIT2 = 1
      NTRIG_S(1) = 2*BIT2 + BIT1
      BIT1W=0
      BIT2W=0
      IF( H_L1BIT(2) ) BIT1W = 1
      IF( H_L1BIT(3) ) BIT2W = 1
      NTRIG_H(1) = 2*BIT2W + BIT1W

C- WN multiplicity
      BIT1=0
      BIT2=0
      IF( S_L1BIT(4) ) BIT1 = 1
      IF( S_L1BIT(5) ) BIT2 = 1
      NTRIG_S(2) = 2*BIT2 + BIT1
      BIT1W=0
      BIT2W=0
      IF( H_L1BIT(4) ) BIT1W = 1
      IF( H_L1BIT(5) ) BIT2W = 1
      NTRIG_H(2) = 2*BIT2W + BIT1W

C- WS multiplicity
      BIT1=0
      BIT2=0
      IF( S_L1BIT(6) ) BIT1 = 1
      IF( S_L1BIT(7) ) BIT2 = 1
      NTRIG_S(3) = 2*BIT2 + BIT1
      BIT1W=0
      BIT2W=0
      IF( H_L1BIT(6) ) BIT1W = 1
      IF( H_L1BIT(7) ) BIT2W = 1
      NTRIG_H(3) = 2*BIT2W + BIT1W

C- ON multiplicity
      BIT1=0
      BIT2=0
      IF( S_L1BIT(8) ) BIT1 = 1
      IF( S_L1BIT(9) ) BIT2 = 1
      NTRIG_S(4) = 2*BIT2 + BIT1
      BIT1W=0
      BIT2W=0
      IF( H_L1BIT(8) ) BIT1W = 1
      IF( H_L1BIT(9) ) BIT2W = 1
      NTRIG_H(4) = 2*BIT2W + BIT1W

C- OS multiplicity
      BIT1=0
      BIT2=0
      IF( S_L1BIT(10) ) BIT1 = 1
      IF( S_L1BIT(11) ) BIT2 = 1
      NTRIG_S(5) = 2*BIT2 + BIT1
      BIT1W=0
      BIT2W=0
      IF( H_L1BIT(10) ) BIT1W = 1
      IF( H_L1BIT(11) ) BIT2W = 1
      NTRIG_H(5) = 2*BIT2W + BIT1W

C- SAMUS N. multiplicity
      BIT1=0
      BIT2=0
      IF( S_L1BIT(12) ) BIT1=1
      IF( S_L1BIT(13) ) BIT2=1
      NTRIG_S(6) = 2*BIT2 + BIT1
      BIT1W=0
      BIT2W=0
      IF( H_L1BIT(12) ) BIT1W = 1
      IF( H_L1BIT(13) ) BIT2W = 1
      NTRIG_H(6) = 2*BIT2W + BIT1W

C- SAMUS S. multiplicity
      BIT1=0
      BIT2=0
      IF( S_L1BIT(14) ) BIT1=1
      IF( S_L1BIT(15) ) BIT2=1
      NTRIG_S(7) = 2*BIT2 + BIT1
      BIT1W=0
      BIT2W=0
      IF( H_L1BIT(14) ) BIT1W = 1
      IF( H_L1BIT(15) ) BIT2W = 1
      NTRIG_H(7) = 2*BIT2W + BIT1W

      NCCT=0
      NCCTW=0
      DO ICOUNT=1,7
        NCCT = NCCT + NTRIG_S(ICOUNT)
        NCCTW = NCCTW + NTRIG_H(ICOUNT)
      ENDDO

      IF(NCCT.EQ.0) NOMU = NOMU+1
      IF(NCCT.GE.1) ONEMU = ONEMU+1
      IF(NCCT.GE.2) TWOMUS = TWOMUS+1
      IF(NCCT.GE.3) THREEMUS = THREEMUS+1

      IF(NCCTW.EQ.0) NOMUW = NOMUW+1
      IF(NCCTW.GE.1) ONEMUW = ONEMUW+1
      IF(NCCTW.GE.2) TWOMUSW = TWOMUSW+1
      IF(NCCTW.GE.3) THREEMUSW = THREEMUSW+1

      IF(IPRSTAT) THEN

        WRITE(6,910) NCCT,(NTRIG_S(I),I=1,7)
  910   FORMAT(' *** This event SIM : TOTAL,CF,WN,WS,ON,OS,SN,SS ',
     &    2X,8I3)
        WRITE(6,912) NCCTW,(NTRIG_H(I),I=1,7)
  912   FORMAT(' *** This event HARD: TOTAL,CF,WN,WS,ON,OS,SN,SS ',
     &    2X,8I3)

        WRITE(6,911) NOMU,ONEMU,TWOMUS,THREEMUS
  911   FORMAT(1X,
     &    ' L1 TRIGGERS SIMULATOR === ',' 0MU:',I6,'  1MU:',I6,
     &    '  2MU:',I4,'  3MU:',I4)
        WRITE(6,913) NOMUW,ONEMUW,TWOMUSW,THREEMUSW
  913   FORMAT(1X,
     &    ' L1 TRIGGERS HARDWARE  === ',' 0MU: ',I6,'  1MU:',I6,
     &    '  2MU:',I4,'  3MU:',I4)
      ENDIF

  999 RETURN
C------------------------------------------------------------------------------
C--   Printout final statistics
      ENTRY FINAL_STATS
      WRITE(6,*) ' >>>>> FINAL L1 STATISTICS '
      WRITE(6,911) NOMU,ONEMU,TWOMUS,THREEMUS
      WRITE(6,913) NOMUW,ONEMUW,TWOMUSW,THREEMUSW
      RETURN

      END
