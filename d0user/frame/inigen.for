      SUBROUTINE INIGEN
C---------------------------------------------------------------------
C-                                                                   -
C-    Call Zebra and Menu intialization                              -
C-
C-    ENTRY INCLUD (dummy entry point to force loading of
C-                  user hooks)
C-                                                                   -
C-                       SDP Apr.,1987                               -
C-   Updated  27-FEB-1994   Meenakshi Narain  add czlini call 
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL USRINI,FLGVAL
      CHARACTER*72 V,VD0USER,VGENERAL

C
      CALL PBDINI                 ! Program Builder initialization
      CALL MZEBRA(0)              ! initialize ZEBRA
      CALL INZCOM(2)              ! initialize ZEBCOM
      CALL INZLNK                 ! initialize ZLINKA
      CALL INPAWC                 ! initialize HBOOK4
      CALL D0HINI                 ! initialize D0HPLT for HBOOK4
      CALL INIMEN                 ! initialize menus
      CALL INIFLG                 ! initialize flags
      CALL CZLINI                 ! initialize ZLINKC
C
C         identify program version
      V=VD0USER()
      CALL INTMSG(V)
      V=VGENERAL()
      CALL INTMSG(V)
C
C user initialization
      IF(.NOT.USRINI()) CALL D0_ABORT(' Failed at initialization')
      IF(FLGVAL('READ_FROM_DAQ')) CALL SETINT(.TRUE.)
      RETURN
C
      ENTRY INCLUD
      CALL DMPUDF   ! this user hook is called by the event dump facility
      CALL DMPUSR   ! this user hook is called by the event dump facility
      END
