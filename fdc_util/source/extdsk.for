      SUBROUTINE EXTDSK ( VIN, VOUT, RNEW, ZNEW, IREJ )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Intersection of a track with a disk defined by
C-                         its radius and its z position.
C-                         The disk axis is along Oz, with its
C-                         plane parallel to the plane xOy.
C-
C-   Inputs  :  VIN(6) = X, Y, Z, DX/DS, DY/DS, DZ/DS for the track
C-              RNEW   = Radius of the disk
C-              ZNEW   = Z position of the disk 
C-   Outputs :  VOUT(6)= new components of the track, loaded for IREJ<2
C-              IREJ   =  0 if the calculation is OK 
C-                        1 if track passes outside radius of disk 
C-                        2 if track never passes plane of disk 
C-
C-   Created  16-FEB-1989   Jeffrey Bantly   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IREJ
      REAL VIN(6), VOUT(6), RNEW, ZNEW
      DOUBLE  PRECISION A, B
C----------------------------------------------------------------------
      IREJ = 0
      CALL UCOPY ( VIN(4), VOUT(4), 3)
C
      A = VIN(6)*VIN(6)
      IF ( A .EQ. 0. ) THEN
C
C ****  The track cannot reach the disk in Z.
C
        IREJ = 2
        GO TO 999
      ENDIF
      VOUT(1) = VIN(1) + (VIN(4) / VIN(6)) * (ZNEW - VIN(3))
      VOUT(2) = VIN(2) + (VIN(5) / VIN(6)) * (ZNEW - VIN(3))
      VOUT(3) = ZNEW
      B = VOUT(1)*VOUT(1) + VOUT(2)*VOUT(2) - RNEW*RNEW
      IF ( B .GT. 0. ) THEN
C
C ****  The track passes outside radius of disk.
C
        IREJ = 1
        GO TO 999
      ENDIF
      IREJ = 0
C-----------------------------------------------------------
  999 CONTINUE
      RETURN
      END
