      SUBROUTINE BKCRCA(NCELL,LCRCA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book CRCA bank for NCELL calorimeter cells.
C-                         Note this is a linear bank.
C-
C-   Inputs  : NCELL is number of calorimeter cells intercepted by our
C-             ideal track
C-   Outputs : LCRCA is link to created bank.
C-   Controls:
C-
C-   Created  17-SEP-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCRCA.LINK'
      LOGICAL FIRST
      INTEGER NCELL,LCRCA,LSUP,IOH,NSIZ,GZFRES,NDATA
      INTEGER LCRCA2
      SAVE IOH
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      NDATA = 2*NCELL + 4               ! Need address and energy word
C                                       ! for each cell.
      IF ( FIRST ) THEN
        CALL MZFORM('CRCA','3I1F/1I1F',IOH)
        FIRST = .FALSE.
      END IF
C
      LCRCA = 0
      LSUP = GZFRES()
      IF (LSUP .LE. 0) CALL BKFRES(LSUP)
      IF (LSUP .LE. 0) RETURN           ! Couldnt do it. Dont know why?
C
      LCRCA = LQ(LSUP - IZCRCA)
      IF (LCRCA .GT. 0) THEN
   10   CONTINUE
        IF (LQ(LCRCA) .NE. 0) THEN
          LCRCA = LQ(LCRCA)
          GOTO 10
        ELSE
          LCRCA2 = LCRCA
          CALL MZBOOK(IXMAIN,LCRCA,LCRCA2,0,'CRCA',0,0,NDATA,IOH,0)
          IQ(LCRCA+1) = NCELL               ! # of cells
        END IF
      ELSE
C---Book it for the first time
        CALL MZBOOK(IXMAIN,LCRCA,LSUP,-IZCRCA,'CRCA',0,0,NDATA,IOH,0)
        IQ(LCRCA+1) = NCELL               ! # of cells
      END IF
  999 RETURN
      END
