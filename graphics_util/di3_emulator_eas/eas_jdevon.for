      SUBROUTINE JDEVON(DEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
CD   The purpose of this routine is to set the structure of the emulator
CD   and to set a dummy that just sets a flag to
CD   indicate that the device is turned on. The parameter DEV is
CD   a dummy variable used for conformity.
C-
C-
C-   Created  10-AUG-1988   SHAHRIAR ABACHI, A. VIRGO
C-   Modified 12-JUN-1989   SHAHRIAR ABACHI  (new structure put in)
C-   Modified 08-MAR-1990   SHAHRIAR ABACHI  (Surface/solid rendering added)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:DEVSTS.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      INTEGER DEV, IALIS
      REAL TVUP(3), TEYEP(3), TUPV(3), MAT(4,4)
      DATA TVUP, TEYEP, TUPV /1.,1.,1., 0.,0.,0., 0.,1.,0./
      DATA MAT /1.,0.,0.,0., 0.,1.,0.,0., 0.,0.,1.,0., 0.,0.,0.,0./
      EXTERNAL ERRHND
C
      IF (DINIT) THEN
        IF (DEVON) THEN
          IF (SEGOPN) THEN
            CALL ERROR ('A SEGMENT IS OPEN')
          ELSE
            CALL ERROR('DSPDEV IS ALREADY SELECTED')
          ENDIF
        ELSE
          CALL PSNFIX(0, 1, 'TURNONDISPLAY', ERRHND)
          CALL PSNFIX(2, 7, 'SHADINGENVIRONMENT', ERRHND)
          CALL PSNBOO(.TRUE., 9, 'SHADINGENVIRONMENT', ERRHND)
          CALL PSNBOO(.TRUE., 15, 'SHADINGENVIRONMENT', ERRHND)
          IALIS = 1
          CALL PSNFIX(IALIS, 5, 'SHADINGENVIRONMENT', ERRHND)
          EMDISP = 'FRAME"'
          DISPL  = 'FRAMER"'
          TDISP  = 'FRAMET"'
          TEMDIS = 'TDISPL"'
          CALL PDISP(DISPL, ERRHND)
          CALL PINST(EMDISP, '"', ERRHND)
CC          CALL PDISP(TEMDIS, ERRHND)
          CALL PDISP(TDISP, ERRHND)
          CALL PINST(TDISP, '"', ERRHND)
C--------------------
CC          CALL PBEGS(TEMDIS, ERRHND)
CC          IF(IREND .GT. 0) CALL PSURRE('SURRN"', '"', ERRHND)       ! TESTING
CC          CALL PINST('A"', TDISP, ERRHND)
CC          CALL PENDS(ERRHND)
C--------------------
          IF(.NOT. NUDI3) THEN
            CALL PBEGS(DISPL, ERRHND)
            IF(IREND .GT. 0) CALL PSURRE('SURRN"', '"', ERRHND)       ! TESTING
            CALL PSEDCL('ICLP"', .FALSE., '"', ERRHND)
            CALL PINST('B"', EMDISP, ERRHND)
            CALL PENDS(ERRHND)
          ELSE
C--------------------
            CALL PRSVST(100000,ERRHND)
            CALL PPURGE(ERRHND)
            CALL PBEGS(DISPL, ERRHND)
            CALL PSEDCL('ICLP"', .FALSE., '"', ERRHND)
            CALL PMAT44('IM44"', MAT, '"', ERRHND)
            CALL PLOOKA('ILKA"', TVUP, TEYEP, TUPV, '"', ERRHND)
            CALL PVIEWP('IVWP"', -.9, .9, -.9, .9, .5, 1., '"',ERRHND)
            CALL KIMGTR
            IF(IREND .GT. 0) CALL PSURRE('SURRN"', '"', ERRHND)       ! TESTING
            CALL PINST('C"', EMDISP, ERRHND)
            CALL PENDS(ERRHND)
C
            IF(IREND .GT. 0) THEN
              CALL PFN('CONS"', 'CONSTANT', ERRHND)
              CALL PCONN(DISPL//'.SURRN"', 1, 1, 'CONS"', ERRHND)
              CALL PCONN('CONS"', 1, 1, 'TURNONDISPLAY', ERRHND)
              CALL PSNFIX(0, 2, 'CONS"', ERRHND)
            ENDIF
          ENDIF
C--------------------
          DEVON = .TRUE.
        ENDIF
      ELSE
        CALL ERROR('DSPDEV IS NOT INITIALIZED')
      ENDIF
C
      RETURN
      END
