      SUBROUTINE DSCHIT(ITRA,VECT,IXSET,CHARGE,INWVOL,DESTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given that a step is within a sensitive volume,
C-                         determine which detector it's in, and call the
C-                         appropriate subroutine to calculate the readout cell
C-                         indices. Does much the same thing as GSCHIT.
C-
C-   Inputs  : ITRA   = Current track number
C-             VECT   = X,Y,Z of current step
C-             IXSET  = Current detector SET (no longer used)
C-             CHARGE = Charge of current track
C-             INWVOL = Involume Flag
C-             DESTEP = Energy lost on this step
C-   Outputs :
C-   Controls:
C-
C-   Created  23-JAN-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C INPUT VARIABLES
      INTEGER ITRA,IXSET,INWVOL
      REAL VECT(3),DESTEP,CHARGE
C
      INCLUDE 'D0$INC:GCSETS.INC'
C
      REAL X,Y,Z
C
C ****  Local variables
      INTEGER KDET,LAYER,SUBLAY
      REAL    TILEL
C
C----------------------------------------------------------------------
C
C ****  Unpack IDTYPE
      KDET = IDTYPE/1000
      LAYER = MOD(IDTYPE,1000)/10
      SUBLAY = MOD(IDTYPE,10)
C
C  FIRST CALCULATE REQUIRED COORDINATES OF POINT.
C
      X = VECT(1)
      Y = VECT(2)
      Z = VECT(3)
C
      IF ( LAYER.EQ.9 ) THEN            ! ICD
        CALL FLICDH(INWVOL,DESTEP,TILEL)
        IF ( TILEL.NE.0 ) THEN
          CALL DSHIT(ITRA,TILEL)
        ENDIF
C
      ELSEIF ( LAYER.LE.17 ) THEN       ! Live layers
C
        IF ( KDET.EQ.3 ) THEN           ! CC
          CALL DUHIT(ITRA,X,Y,Z,LAYER,SUBLAY,DESTEP)
C
        ELSEIF ( KDET.EQ.4 ) THEN       ! EC
          CALL DEHIT(ITRA,X,Y,Z,LAYER,SUBLAY,DESTEP)
        ENDIF
      ELSE                              ! Dead Material
        CALL DCHIT(ITRA,X,Y,Z,LAYER,DESTEP)
      ENDIF
C
  999 RETURN
      END
