      SUBROUTINE PBDINI
C-
C-
C-    Purposes and Methods:
C-    --------------------
C-    This routine must be called by the FrameWork to initialize
C-    the run time switches associated with the packages
C-    Created by the PROGRAM BUILDER Release V1.10
C-    13-OCT-1989 08:52
C-
C-
C-
      LOGICAL WRNGOK
      INTEGER LOGUNT,MAXLOG,MAXWRN
      CHARACTER*32 STRGLG
      LOGUNT = 0
      WRNGOK = .TRUE.
      CALL ERRINI(LOGUNT,WRNGOK)
      MAXLOG = 1
      MAXWRN = 1
      STRGLG = ' '
      CALL ERRMAX(STRGLG,MAXLOG,MAXWRN)
C-
      CALL FLGBK('PBD_CALOR',1)
      CALL FLGSET('PBD_CALOR',.TRUE.)
      CALL FLGBK('PBD_CAHITS',1)
      CALL FLGSET('PBD_CAHITS',.TRUE.)
      CALL FLGBK('PBD_CAJETS',1)
      CALL FLGSET('PBD_CAJETS',.TRUE.)
      CALL FLGBK('PBD_CAPHEL',1)
      CALL FLGSET('PBD_CAPHEL',.TRUE.)
      RETURN
      END
