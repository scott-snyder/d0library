      SUBROUTINE CALCYL(IETA, ILAYER, DELX, RINV, XCEN, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gives the parameters necessary for the 
C-                         survey transformation.  These parameters
C-                         are merely lifted from the appropriate CLIN 
C-                         banks
C-
C-   Inputs  :     IETA    Eta
C-                 ILAYER  layer  (Eta and layer are used to determine
C-                           the appropriate module)
C-   Outputs :     DELX    Survey transformation vector parameters
C-                 RINV    Survey rotation matrix.  The matrix is
C-                           stored as its inverse (ie. its transpose)
C-                 XCEN    Position around which the rotation is made.
C-                           should be the module center if all corners
C-                           were measured.
C-                 IERR    Error code  -- 0    OK
C-                                        1    not in acceptable module
C-                                             (currently only CC)
C-                                        2    CLGI bank not found
C-                                        3    CLIN bank not found
C-   Controls: 
C-
C-   Created  15-OCT-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLIN.PARAMS'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCLGI.LINK'
      INTEGER IETA, ILAYER, IERR, IDENT, LZFIND, I
      REAL    DELX(3), RINV(3, 3), XCEN(3)
C
      IERR = 0
C
      IF( ABS(IETA) .GT. 12) THEN           ! only CC is currently
                                        ! available
        IERR = 1
        RETURN
      END IF
C
      IF( ILAYER .GE. MNLYEM .AND. ILAYER .LE. MXLYEM) THEN   
                                        ! CC/EM module
        IDENT = ICCEMI
      ELSE IF (ILAYER .GE. MNLYFH .AND. ILAYER .LE. MXLYFH .AND. 
     +    ABS(IETA) .LE. 21-ILAYER) THEN    ! CC/FH module
        IDENT = ICCFHI 
      ELSE IF (ILAYER .EQ. MNLYCH .AND. ABS(IETA) .LE. 21-ILAYER) THEN
        IDENT = ICCCHI
      END IF
C
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-IZCREG),ICCAL,IGREGN)
      LQCLGI = LZFIND(IDVSTP,LC(LQCREG-IZCLGI),IDENT,IGIDEN)
      IF( LQCLGI .EQ. 0) THEN
        IERR = 2
        RETURN
      END IF
C
      LQCLIN = LC(LQCLGI-IXCLIN)
      IF( LQCLIN .EQ. 0) THEN
        IERR = 3
        RETURN
      END IF
C
      DO 100 I = 1, 3
        XCEN(I) = C(LQCLIN + IGMDLX + I -1)       ! copy module coord
        DELX(I) = C(LQCLIN + IGDTX + I - 1)       ! copy displ vector
  100 CONTINUE
C
      DO 200 I = 1, 9
  200 RINV(I, 1) = C(LQCLIN + IGR11 + I -1)       ! copy inverse
                                        ! rotation matrix
C----------------------------------------------------------------------
  999 RETURN
      END
