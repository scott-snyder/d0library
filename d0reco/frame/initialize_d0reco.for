      SUBROUTINE INITIALIZE_D0RECO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Initialize reconstruction program
C-
C-
C-   Created   6-SEP-1989   Serban D. Protopopescu
C-   Updated   8-MAR-1993   Hailin Li and Kirill Denisenko
C-                          Flag PARALLEL added for paralle RECO
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      CALL PBDINI                   ! Program Builder initialization
      CALL MZEBRA(0)                ! initialize ZEBRA
      CALL INZCOM(2)                ! initialize ZEBCOM
      CALL INZSTP                   ! initialize ZEBSTP
      CALL INZLNK                   ! initialize ZLINKA
      CALL INPAWC                   ! initialize HBOOK4
      CALL DMPINI                   ! initialize dump facility
C
C       book needed flags
      CALL FLGBK('WRITE_E_FILE',1)
      CALL FLGBK('VERIFY',1)
      CALL FLGBK('REMOTE_STA',1)
      CALL FLGBK('PARALLEL',1)
      CALL FLGBK('DBL3SERVER',1)

  999 RETURN
      END
