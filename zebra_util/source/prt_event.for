      FUNCTION PRT_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to make a printout of the content of the
C_                         TRGR bank which includes level 1.5
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: PRT_EVENT.RCP (read PRTRGR routine for the available options) 
C-
C-   Created  30-DEC-1993   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IER,STATUS,IUSER,LUN,LTRGR,NTRGR,LENGTH,IFL
      CHARACTER*(8) CFL
      LOGICAL PRT_EVENT, FIRST, AHA
      DATA FIRST /.TRUE./
      DATA IUSER /109/
C
C----------------------------------------------------------------------
C
      PRT_EVENT = .TRUE.
      IF (FIRST) THEN
         FIRST = .FALSE.
C
C       open output file
C
         CALL INRCP ('PRT_EVENT_RCP',IER)
         CALL EZPICK ('PRT_EVENT_RCP')
         CALL EZGET ('LTRGR',LTRGR,IER)
         CALL EZGET ('NTRGR',NTRGR,IER)
         CALL EZGETS ('CHR_FLG', 1, CFL, LENGTH, IER)
         CALL EZGET ('INT_FLG',IFL,IER)
         CALL EZRSET
  10     CALL GTUNIT(IUSER,LUN,IER)
         IF (IER.NE.0) THEN
           IF (IUSER.EQ.1000)
     &        CALL ERRMSG('PRT_EVENT','PRT_INIT',
     &        'Cannot get LUN for output','F')
              IUSER = IUSER + 1
              GOTO 10
         ENDIF
         CALL D0OPEN(LUN,'PRTRGR_L15.OUT','OF',AHA)
         IF (.NOT.AHA) THEN
             LUN = 77
             CALL ERRMSG('PRTRGR_L15','PRT_INIT',
     &       'Cannot open new PRTRGR_L15.OUT','I')
         ENDIF
      ENDIF
      CALL PRTRGR(LUN,LTRGR,NTRGR,CFL,IFL)
C----------------------------------------------------------------------
      END
