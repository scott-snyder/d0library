      FUNCTION ZTRAKS_EXM_HPRINT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-JUN-1992   Susan K. Blessing
C-   Updated   5-FEB-1993   Susan K. Blessing  Remove PRINT_HISTOS logical
C-    since this routine is a separate .PBD now.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:EXM_STATISTICS.INC/LIST'
C
      LOGICAL ZTRAKS_EXM_HPRINT
C
      INTEGER ISTAT,LIB$SPAWN,LIBERA,LSPAWN,LIBREP
      INTEGER RUN,RUNNO,LL,L1,IER
      INTEGER PREV_RUN
      INTEGER PREV_NEVS
C
      CHARACTER*3 ANS
      CHARACTER*100 COMMAND
      CHARACTER*10 CRUN
      CHARACTER*6 CNEVS
C
      LOGICAL PRINT
C
      DATA RUN/0/
      DATA PREV_RUN/-1/
C----------------------------------------------------------------------
C
      PRINT = .FALSE.
C
      IF (NEVS.GT.0) THEN
C
        RUN = RUNNO()
        IF (RUN.NE.PREV_RUN.AND.RUN.NE.0) THEN
          PRINT = .TRUE.
          PREV_RUN = RUN
          PREV_NEVS = NEVS
        ELSE IF (RUN.EQ.PREV_RUN.AND.RUN.NE.0) THEN
          IF (NEVS.GT.PREV_NEVS) THEN
            PRINT = .TRUE.
            PREV_NEVS = NEVS
          END IF
        END IF
C
      END IF
C
      IF (PRINT) THEN
C
C        CALL LIB$GET_INPUT(ANS,
C     &    'Do you want to print the histograms? [Y] ',LL)
C
C        IF (ANS(1:1).NE.'N'.AND.ANS(1:1).NE.'n') THEN
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
          IF (NEVS.LT.10.AND.NEVS.GE.1) THEN
            WRITE(CNEVS,'(I1)') NEVS
          ELSE IF (NEVS.LT.100.AND.NEVS.GE.10) THEN
            WRITE(CNEVS,'(I2)') NEVS
          ELSE IF (NEVS.LT.1000.AND.NEVS.GE.100) THEN
            WRITE(CNEVS,'(I3)') NEVS
          ELSE IF (NEVS.LT.10000.AND.NEVS.GE.1000) THEN
            WRITE(CNEVS,'(I4)') NEVS
          ELSE IF (NEVS.LT.100000.AND.NEVS.GE.10000) THEN
            WRITE(CNEVS,'(I5)') NEVS
          ELSE IF (NEVS.LT.1000000.AND.NEVS.GE.100000) THEN
            WRITE(CNEVS,'(I6)') NEVS
          END IF
C
          LL = INDEX(CRUN,' ')
          L1 = INDEX(CNEVS,' ')
C
          COMMAND =
     &      'SUB_CD_HISTS/PARAM=(USR$OUT:RUN'
     &      //CRUN(1:LL-1)//'.HST,'//CNEVS(1:L1-1)//')'
          IF (ASTFLG) CALL DISABL       !Disable unsolicited input before spawn
          CALL BROADC(.FALSE.)     !Turn off broadcast trapping
          ISTAT=LIB$SPAWN(COMMAND)
          CALL BROADC(.TRUE.)      !Turn on broadcast trapping again
          IF (MOD(ISTAT,2).EQ.0) THEN
            CALL MSGSCR(ISTAT,'SPAWN FAILED-->')
            CALL PFWAIT
          ELSE IF (FULSCR) THEN
            ISTAT=LIBREP()
            CALL INTMSG(' Histogram print job submitted.')
            CALL INTMSG
     &        (' Please put the output histograms in the folder.')
          ELSE
            CALL INTMSG(' Histogram print job submitted.')
            CALL INTMSG
     &        (' Please put the output histograms in the folder.')
          ENDIF
C
C        END IF
C
      END IF
      ZTRAKS_EXM_HPRINT = .TRUE.
C
  999 RETURN
      END
