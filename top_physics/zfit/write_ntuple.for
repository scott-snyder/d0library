      SUBROUTINE WRITE_NTUPLE(STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Write parameters into NTUPLE
C-      Upon first call book ntuple.
C-
C-   Inputs  : NTUP_NPAR     [I]     Ntuple dimension
C-             PIN(*)   [R]     Ntuple array
C-             STATUS   [I]
C-   Outputs : None
C-   Controls: None
C-
C-   Created  26-FEB-1993   Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:FIT_TWO.INC'
      CHARACTER*(*) RCPBANK
      INTEGER STATUS
C----------------------------------------------------------------------
      INTEGER IER
C----------------------------------------------------------------------
      REAL   XX(MAXPAR)
      CHARACTER*32 TOPDIR
      SAVE TOPDIR
C----------------------------------------------------------------------
C
C ****  FILL NTUPLE
C
      CALL UCOPY(PIN(1),XX(1),NTUP_NPAR)
C
      CALL NTUPLE_SAVE_DIRECTORY
      CALL DHDIR(' ','//PAWC',STATUS,' ')
      CALL NTUPLE_FILL(TOPDIR,NTUPLE_ID,XX,STATUS)
      CALL NTUPLE_RESTORE_DIRECTORY
      RETURN
C
      ENTRY OPEN_NTUPLE(RCPBANK,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open Ntuple file.
C-
C-   Inputs  : RCPBANK    [C*]  Name of RCP bank
C-
C-   Outputs : NTUP_NPAR       [I]   NTUPLE ARRAY DIMENSION
C-             STATUS     [I]   0 --- OK
C-   Controls:
C-
C-   Created  26-FEB-1993     Pushpa Bhat
C-
C----------------------------------------------------------------------
      CALL BOOK_NTUPLE
     &  (RCPBANK(1:LEN(RCPBANK)),TOPDIR,NTUP_NPAR,STATUS)
      RETURN
C
      ENTRY CLOSE_NTUPLE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write out last buffer of ntuple, delete
C-   the ntuple and close ntuple file.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-SEP-1991   Harrison B. Prosper
C-   Updated  27-FEB-1993   Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      CALL NTUPLE_SAVE_DIRECTORY
      CALL DHDIR(' ','//PAWC',IER,' ')
      CALL NTUPLE_CLOSE(TOPDIR,IER)
      CALL HDELET(NTUPLE_ID)
      CALL NTUPLE_RESTORE_DIRECTORY
  999 RETURN
      END
