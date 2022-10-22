      SUBROUTINE PFDCRZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action Routine to draw R-Z view of FDC delay 
C-                         line hits, tracks, and Isajet tracks. 
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  24-JUL-1989   Jeffrey Bantly
C-   Updated   5-NOV-1990   Jeffrey Bantly  add phi limits 
C-   Updated  22-JAN-1991   Jeffrey Bantly  generalize utilities called, 
C-                                          add check for FDC Hits banks
C-   Updated  26-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK 
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack 
C-   Updated   9-AUG-1991   Robert E. Avery  Remove unnecessary printout.
C-   Updated  25-JAN-1992   Robert E. Avery  Delete "no tracks" message,
C-    Add call to FDCISA, so that isajet tracks in FDC can be drawn.
C-   Updated  25-MAY-1992   Robert E. Avery  Move call to draw vertex
C-      to end, so that it doesn't get covered up by isajet.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LKFDCH,GZFDCH
      INTEGER LISAE, GZISAE
      INTEGER LFTRH, GZFTRH
      INTEGER DRAISA,IVIEW,IER
      REAL     PHI1,PHI2,PHI3,PHI4
      LOGICAL  FDONLY,DRAW_ALL_ISA,EZERROR
      CHARACTER*35 TEXT
      SAVE IVIEW
      DATA IVIEW/3/                     ! IVIEW=3 is RZ view
C----------------------------------------------------------------------
C
C ****  Pick PIXIE RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFDCRZ','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PFGPHI(PHI1,PHI2,PHI3,PHI4)  ! Get phi limits for plot
C
      CALL PUGET_l('FDC ONLY        ', FDONLY)
      CALL PUGET_i('FDC DRAW ISATRK ', DRAISA)
      CALL PUGET_l('FDC DRAW ALL ISA', DRAW_ALL_ISA)
      CALL EZRSET
C
      CALL PFGETD                            ! Find delay line hits
C
      CALL PUOPEN
      CALL PFDCDR(IVIEW)                     ! Draw FDC main volumes
C
      LKFDCH=GZFDCH()
      IF(LKFDCH.GT.0) THEN
        CALL PFDHDR(PHI1,PHI2,PHI3,PHI4,IVIEW)  ! Draw delay line hits
      ENDIF
C
      LFTRH=GZFTRH()
      IF(LFTRH.GT.0) THEN
        CALL PFTKDR(PHI1,PHI2,PHI3,PHI4,IVIEW) ! Draw FDC tracks
      ENDIF
C
      LISAE = GZISAE()
      IF ( (DRAISA.GE.1) .AND. (LISAE .GT. 0) ) THEN
        IF(DRAW_ALL_ISA) THEN                ! Draw all Isajet tracks 
          CALL PFISDR_ALL(PHI1,PHI2,PHI3,PHI4,IVIEW)
        ELSE                      ! Draw Isajet tracks if thru FDC
          CALL FDCISA
          CALL PFISDR_FDC(PHI1,PHI2,PHI3,PHI4,IVIEW)
        ENDIF
      ENDIF
C                               !  Draw Vertex (after isajet, so you can see it)
      IF (FDONLY) THEN
        CALL PFDVTX
      ENDIF
      CALL JRCLOS
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
