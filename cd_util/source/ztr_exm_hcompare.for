      FUNCTION ZTR_EXM_HCOMPARE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Submit histogram comparison job.
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-DEC-1993   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:EXM_STATISTICS.INC/LIST'
C
      LOGICAL ZTR_EXM_HCOMPARE
C
      INTEGER IER
      INTEGER MIN_NEVENTS
      INTEGER PREV_NEVS,PREV_RUN
      INTEGER RUN,RUNNO
      INTEGER LL
      INTEGER ISTAT,LIB$SPAWN,LIBERA,LSPAWN,LIBREP
C
      CHARACTER*100 COMMAND
      CHARACTER*10 CRUN
C
      LOGICAL FIRST
      LOGICAL COMPARE
C
      DATA MIN_NEVENTS/100/
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      ZTR_EXM_HCOMPARE =  .TRUE.
      IF (FIRST) THEN
        CALL EZPICK('CD_EXM_HISTOS_RCP')
        CALL EZGET('MIN_NEVENTS',MIN_NEVENTS,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      COMPARE = .FALSE.
C
      IF (NEVS.GT.MIN_NEVENTS) THEN
C
        RUN = RUNNO()
        IF (RUN.NE.PREV_RUN.AND.RUN.NE.0) THEN
          COMPARE = .TRUE.
          PREV_RUN = RUN
          PREV_NEVS = NEVS
        ELSE IF (RUN.EQ.PREV_RUN.AND.RUN.NE.0) THEN
          IF (NEVS.GT.PREV_NEVS) THEN
            COMPARE = .TRUE.
            PREV_NEVS = NEVS
          END IF
        END IF
C
      END IF
C
      IF (COMPARE) THEN
C
        IF (RUN.LT.10000.AND.RUN.GE.1000) THEN
          WRITE(CRUN,'(I4)') RUN
        ELSE IF (RUN.LT.100000.AND.RUN.GE.10000) THEN
          WRITE(CRUN,'(I5)') RUN
        ELSE IF (RUN.LT.1000000.AND.RUN.GE.100000) THEN
          WRITE(CRUN,'(I6)') RUN
        ELSE IF (RUN.LT.10000000.AND.RUN.GE.1000000) THEN
          WRITE(CRUN,'(I7)') RUN
        ELSE IF (RUN.LT.100000000.AND.RUN.GE.10000000) THEN
          WRITE(CRUN,'(I8)') RUN
        END IF
C
        LL = INDEX(CRUN,' ')
C
        COMMAND =
     &      'SUB_CD_HCOMPARE/PARAM=(USR$OUT:RUN'//CRUN(1:LL-1)//'.HST)'
        IF (ASTFLG) CALL DISABL       !Disable unsolicited input before spawn
        CALL BROADC(.FALSE.)     !Turn off broadcast trapping
        ISTAT=LIB$SPAWN(COMMAND)
        CALL BROADC(.TRUE.)      !Turn on broadcast trapping again
        IF (MOD(ISTAT,2).EQ.0) THEN
          CALL MSGSCR(ISTAT,'SPAWN FAILED-->')
          CALL PFWAIT
        ELSE IF (FULSCR) THEN
          ISTAT=LIBREP()
          CALL INTMSG(' Histogram comparison job submitted.')
        ELSE
          CALL INTMSG(' Histogram comparison job submitted.')
        ENDIF
C
C        END IF
C
      END IF
C
  999 RETURN
      END
