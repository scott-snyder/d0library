      LOGICAL FUNCTION GAP_CHECK_HOT_CELL(IETA,IPHI,ILYR,IRUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns true if cell is in hot cell list
C-
C-   Returned value  :
C-   Inputs  :          IETA,IPHI,ILYR of cell to check
C-                      IRUN = run number for correct hot list
C-
C-   Controls:          ILYR  =   1 thru 17  ->   ILYR must match
C-                      ILYR  =   0          ->   no LYR match required
C-                      ILYR  =  -1          ->   match EM only (CATD EM)
C-                      ILYR  =  -8          ->   match ICD,MG only
C-                      ILYR  =  -11         ->   match HAD only
C-                      ILYR  =  -17         ->   match HAD+ICD+MG (CATD HD)
C-
C-
C-   Created  30-JUN-1993   Brent J. May
C-   Modified 20-NOV-1993   BJM - read in new list by run range
C-   Modified  7-FEB-1994   BJM - Adapt for ntuple maker
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GAP_CATD_INFO.INC'
      INCLUDE 'D0$INC:GAP_HOT_CATD_INFO.INC'
      INTEGER IETA, IPHI, ILYR, I, NUM, IRUN, IVER
      REAL SIG
      LOGICAL FIRST, HOT_LIST, HOT_CHOT, HOT_LHOT
      DATA FIRST/.TRUE./
C
      INTEGER IPHIW, OLDRUN, RUNINDX
      INTEGER ICAL
      SAVE OLDRUN, RUNINDX
C----------------------------------------------------------------------
      IF (FIRST) THEN
        IF (DO_HOT_KILL) CALL GAP_GET_HOT_INFO
        FIRST = .FALSE.
        OLDRUN = 0
      ENDIF
      HOT_LIST = .FALSE.
      HOT_CHOT = .FALSE.
      HOT_LHOT = .FALSE.
      IF (.NOT.DO_HOT_KILL) GOTO 998
      SIG = 0.
      NUM = 0
C
C Do run based list
C
      IF (.NOT.DO_BY_RUN) GOTO 10
      IF (ABS(IETA).GT.32) GOTO 10   ! IETA>32 not in this list
      IF (ILYR.EQ.-1)  ICAL = 1
      IF (ILYR.EQ.-17) ICAL = 2
      IF (IRUN.NE.OLDRUN) THEN  ! find new run index
        I = 1
        DO WHILE (IRUN.NE.LRUN(I).AND.I.LE.NLRUN)
          I = I + 1
        ENDDO
        IF (I.LE.NLRUN) THEN
          OLDRUN = IRUN
          RUNINDX = I
        ELSE
          OLDRUN = 0
          CALL ERRMSG('HOT_CELL','HOT_CELL',
     &      'Run not in RUN_LIST_FILE list - not killed','W')
          GOTO 10
        ENDIF
      ENDIF
      IPHIW = (IPHI-1)/32
      IF (BTEST(LHOT(ICAL,RUNINDX,IETA,IPHIW),MOD(IPHI-1,32))) THEN
        HOT_LHOT = .TRUE.
        GOTO 998
      ENDIF
C
C
  10  IF (DO_FIXED_LIST) THEN    ! do hot list suppression
        DO I = 1, NVER          ! find correct list according to run number
          IF (IRUN.GT.HRUN(I).AND.IRUN.LE.HRUN(I+1)) GOTO 50
        ENDDO
   50   IVER = I
        DO I = 1, NLIST(IVER)
          IF (IHOT_LIST(1,I,IVER).EQ.IETA .AND.
     &        IHOT_LIST(2,I,IVER).EQ.IPHI .AND.
     &        IHOT_LIST(3,I,IVER).GT.INT(ABS(100.*HOT_SIGMA_CUT)))THEN
            HOT_LIST = .TRUE.
            SIG = FLOAT(IHOT_LIST(3,I,IVER))/100.
            NUM = I
            GOTO 998
          ENDIF
        ENDDO
      ENDIF
C
  998 GAP_CHECK_HOT_CELL = HOT_CHOT.OR.HOT_LIST.OR.HOT_LHOT
  999 RETURN
      END
