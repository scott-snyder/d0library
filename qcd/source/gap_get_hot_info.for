      SUBROUTINE GAP_GET_HOT_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in both fixed and variable run CATD
C-                         hot lists
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-FEB-1994   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GAP_CATD_INFO.INC'
      INCLUDE 'D0$INC:GAP_HOT_CATD_INFO.INC'
      INTEGER IUSER,U,IER
      LOGICAL OK
      INTEGER I, ITYP, IETA, IPHI, ISIG, IRUNE, IRUNB, ISIZ, IPHIW
C----------------------------------------------------------------------
C
C Read in list of global run numbers
C
      IUSER = 49
      CALL GTUNIT(IUSER,U,IER)
      CALL D0OPEN(U,RUN_LIST_FILE,'FI',OK)
      IF (.NOT.OK) THEN
        CALL ERRMSG('GAP_GET_HOT','GAP_GET_HOT',
     &      'Could not open RUN_LIST_FILE file','F')
          GOTO 999
      ENDIF
      NLRUN = 0
      DO WHILE (.TRUE.)
        NLRUN = NLRUN + 1
        READ(U,*,END=5) LRUN(NLRUN)
      ENDDO
    5 CONTINUE
      NLRUN = NLRUN - 1
      IF (NLRUN.LE.0) THEN
         CALL ERRMSG('GAP_GET_HOT','GAP_GET_HOT',
     &      'empty RUN_LIST_FILE file','F')
          GOTO 999
      ENDIF
      CALL D0CLOSE(U,' ',OK)
      CALL RLUNIT(IUSER,U,IER)

C
C Read in Hot CATD list and match to run numbers
C
      CALL GTUNIT(IUSER,U,IER)
      CALL D0OPEN(U,HOT_RUN_FILE,'FI',OK)
      IF (.NOT.OK) THEN
        CALL ERRMSG('GAP_GET_HOT','GAP_GET_HOT',
     &      'Could not open HOT_FILE file','F')
          GOTO 999
      ENDIF
      CALL VZERO(LHOT,2000*65*2*2) !zero hot catd bitmap
      DO WHILE (.TRUE.)
        READ(U,*,END=20) ITYP, IETA, IPHI, ISIG, IRUNE, IRUNB, ISIZ
        IF (ABS(IETA).LE.32.AND.ISIZ.GE.LOCUT.AND.ISIZ.LE.HICUT.AND.
     &      FLOAT(ISIG)/10.0.GE.HOT_SIGMA_CUT) THEN
          DO I = 1, NLRUN
            IF (LRUN(I).GE.IRUNB.AND.LRUN(I).LE.IRUNE) THEN
              IPHIW = (IPHI-1)/32
              LHOT(ITYP,I,IETA,IPHIW) =   ! set IETA,IPHI bit for run I
     &           IBSET( LHOT(ITYP,I,IETA,IPHIW), MOD(IPHI-1,32) )
            ENDIF
          ENDDO
        ENDIF
      ENDDO
   20 CONTINUE
      CALL D0CLOSE(U,' ',OK)
      CALL RLUNIT(IUSER,U,IER)
  999 RETURN
      END
