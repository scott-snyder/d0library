      LOGICAL FUNCTION ISARCP_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      user hook for ISAJET event generation
C-
C-   Updated  11-NOV-1989   Rajendran Raja   (BASED ON ISGEVT)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:IDRUN.INC'
      INCLUDE 'D0$INC:PARTCL.INC'
      INCLUDE 'D0$INC:NODCAY.INC'
      REAL    XYZ(3),PXYZ(3)
      INTEGER NCALLS,ID,IER,NTRY
      LOGICAL FIRST,DO_ONE_TRACK,OK,DONE
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        NCALLS = 0
        CALL EZGET('DO_ONE_TRACK_EVENTS',DO_ONE_TRACK,IER)
      ENDIF
        NCALLS=NCALLS+1
C
      IF ( DO_ONE_TRACK ) THEN   ! events with one track only
        NPTCL=0
        IEVT=NCALLS
        CALL IS1TRK_RCP(ID,XYZ,PXYZ)        ! user supplied tracks
        CALL ISGTRK(ID,XYZ,PXYZ)        ! pass track information to ISAJET
        NOEVOL=.FALSE.
      ELSE
C
        NTRY = 0
   10   CALL ISAEVT(NCALLS,OK,DONE)   ! generate event (ISAJET subroutine)
        IF(NTRY.LT.10 .AND. .NOT.OK) THEN
          NTRY = NTRY + 1
          GOTO 10
        END IF
      ENDIF
C
      IF(.NOT.NOEVOL) THEN
        CALL ISAEFL        ! fill event record
        ISARCP_EVT=.TRUE.
      ELSE
        ISARCP_EVT=.FALSE.
      ENDIF
  999 RETURN
C
      END
