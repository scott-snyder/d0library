      SUBROUTINE ECONNT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To setup segment transformation network. Dials
C-                          are connected to the transformations as follows:
C-                                                                Label
C-                           Dial 1 ----> Rotate about X.          Rot X
C-                           Dial 2 ----> Rotate about Y.          Rot Y
C-                           Dial 3 ----> Rotate about Z.          Rot Z
C-                           Dial 4 ----> Scale.                   Scale
C-                           Dial 5 ----> Translate along X        Tran X
C-                           Dial 6 ----> Translate along Y        Tran Y
C-                           Dial 7 ----> Translate along Z.       Tran Z
C-                           Dial 8 ----> View angle               Angle
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   8-FEB-1989   SHAHRIAR ABACHI
C-   UPDATED   11-AUG-1992   SHAHRIAR ABACHI   fine scale added to knob 8
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*4 TRNSF(50)
      COMMON /ESCAPC/ TRNSF
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      EXTERNAL ERRHND
C
      CALL PSNST('ROT X', 1, 'DLABEL1', ERRHND)
      CALL PSNST('ROT Y', 1, 'DLABEL2', ERRHND)
      CALL PSNST('ROT Z', 1, 'DLABEL3', ERRHND)
      CALL PSNST('SCAL F', 1, 'DLABEL4', ERRHND)
      CALL PSNST('TRAN X', 1, 'DLABEL5', ERRHND)
      CALL PSNST('TRAN Y', 1, 'DLABEL6', ERRHND)
      CALL PSNST('TRAN Z', 1, 'DLABEL7', ERRHND)
      CALL PSNST('SCAL C', 1, 'DLABEL8', ERRHND) !NEW
CC      CALL PSNST('ANGLE', 1, 'DLABEL8', ERRHND)
C
      TRNSF(1) = 'XROT"'
      TRNSF(2) = 'YROT"'
      TRNSF(3) = 'ZROT"'
      TRNSF(4) = 'SCAL"'
      TRNSF(5) = 'FOVU"'                 ! Instanced in KFUN FOR NUDI3
      TRNSF(6) = 'LKAT"'                 ! Instanced in KFUN FOR NUDI3
      TRNSF(7) = 'ZTRN"'
      TRNSF(8) = 'XTRN"'
      TRNSF(9) = 'YTRN"'
C
      IF(NUDI3) THEN
        CALL KFUN
        CALL KFUNC
      ELSE
        CALL PFN(TRNSF(5), 'FOV', ERRHND)
        CALL PFN(TRNSF(6), 'LOOKAT', ERRHND)
      ENDIF
C
      CALL PFN(TRNSF(1), 'DXROTATE', ERRHND)
      CALL PFN(TRNSF(2), 'DYROTATE', ERRHND)
      CALL PFN(TRNSF(3), 'DZROTATE', ERRHND)
      CALL PFN(TRNSF(4), 'DSCALE', ERRHND)
      CALL PFN(TRNSF(7), 'ZVECTOR', ERRHND)
      CALL PFN(TRNSF(8), 'XVECTOR', ERRHND)
      CALL PFN(TRNSF(9), 'YVECTOR', ERRHND)
C
      CALL PFN('MULP1', 'MULC', ERRHND)
      CALL PFN('MULP2', 'MULC', ERRHND)
      CALL PFN('MULP3', 'MULC', ERRHND)
      CALL PFN('MULP4', 'MULC', ERRHND)
      CALL PFN('ACCM1', 'ACCUMULATE', ERRHND)
      CALL PFN('ACCM2', 'ACCUMULATE', ERRHND)
      CALL PFN('ACCM3', 'ACCUMULATE', ERRHND)
      CALL PFN('ACCM4', 'ACCUMULATE', ERRHND)
      CALL PFN('ACCM5', 'ACCUMULATE', ERRHND)
C
      CALL PCONN('DIALS', 1, 1, TRNSF(1), ERRHND)
      CALL PCONN('DIALS', 2, 1, TRNSF(2), ERRHND)
      CALL PCONN('DIALS', 3, 1, TRNSF(3), ERRHND)
      CALL PCONN('DIALS', 4, 1, TRNSF(4), ERRHND)
C
      CALL PCONN('DIALS', 5, 1, TRNSF(8), ERRHND)
C
      CALL PCONN('DIALS', 6, 1, TRNSF(9), ERRHND)
C
CC      CALL PCONN('DIALS', 8, 1, 'MULP3', ERRHND)
c      CALL PCONN('MULP3', 1, 1, 'ACCM1', ERRHND)
CC      CALL PCONN('ACCM1', 1, 2, TRNSF(5), ERRHND)
CC      CALL PCONN('DIALS', 8, 1, TRNSF(5), ERRHND)  !NEW
      CALL PCONN('DIALS', 8, 1, 'MULP3', ERRHND)
      CALL PCONN('MULP3', 1, 1, TRNSF(4), ERRHND)  !NEW
C
      CALL PCONN('DIALS', 7, 1, TRNSF(7), ERRHND)
C
      CALL PCONN(TRNSF(7), 1, 1, 'ACCM5', ERRHND)
      CALL PCONN(TRNSF(8), 1, 1, 'ACCM5', ERRHND)
      CALL PCONN(TRNSF(9), 1, 1, 'ACCM5', ERRHND)
C
      CALL PPURGE(ERRHND)
  999 RETURN
      END
