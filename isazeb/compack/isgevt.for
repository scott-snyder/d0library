      LOGICAL FUNCTION ISGEVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      user hook for ISAJET event generation
C-
C-    ENTRY ISGZNC to zero event counter
C-
C-   Created   8-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:IDRUN.INC'
      INCLUDE 'D0$INC:PARTCL.INC'
      LOGICAL ISGZNC,FLGVAL,OK,DONE,USE_CONE_LIM
      REAL    XYZ(3),PXYZ(3)
      INTEGER NCALLS,ID,USNVRN
      SAVE NCALLS
C----------------------------------------------------------------------
C
        DATA NCALLS/0/
        NCALLS=NCALLS+1
        CALL EVTSET(NCALLS)
C
      IF ( FLGVAL('ONE_TRACK') ) THEN   ! events with one track only
        NPTCL=0
        IEVT=NCALLS
        CALL IS1TRK(ID,XYZ,PXYZ)        ! user supplied tracks
        CALL ISGTRK(ID,XYZ,PXYZ)        ! pass track information to ISAJET
        OK=.TRUE.
C
      ELSEIF (FLGVAL('PARTONS')) THEN   ! read a file with partons
        CALL ISA_PARTONS(OK,DONE,USE_CONE_LIM)
C
      ELSE
        CALL ISAEVT(NCALLS,OK,DONE)   ! generate event (ISAJET subroutine)
      ENDIF
C
      ISGEVT=OK
      IF(OK) CALL ISAEFL        ! fill event record
      IF(DONE) THEN
        NCALLS=USNVRN()+1
        CALL EVTSET(NCALLS)
      ENDIF
  999 RETURN
C
      ENTRY ISGZNC()
      NCALLS=0
      RETURN
      END
