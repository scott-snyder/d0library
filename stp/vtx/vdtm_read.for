      SUBROUTINE VDTM_READ(LUN,ASCII,ITEMS,NWIRE,SIZE,WIRE_POS,
     &                      POINTERS,MAP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read VTX distance time map from either an ascii file
C-                         or a binary file.
C-
C-   Inputs  : LUN: input unit number
C-             ASCII: .TRUE. if file is ascii
C-   Outputs : Data from the file
C-             ITEMS, NWIRE, SIZE: # of maps, wires, size of maps
C-             WIRE_POS: wire position array
C-             POINTERS: array containing pointers to maps for each wire
C-             MAP: the actual DTM maps
C-
C-   Created  27-JUN-1992   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER MXITEM, MXWIRE, MAXMAP
      PARAMETER ( MXITEM = 5 )
      PARAMETER ( MXWIRE = 8 )
      PARAMETER ( MAXMAP = 3000 ) 
      REAL WIRE_POS(0:15), MAP(MAXMAP)
      INTEGER ITEMS, NWIRE, SIZE, LUN, POINTERS(4*MXITEM*MXWIRE)
      LOGICAL ASCII
C
      INTEGER I, NUM
C----------------------------------------------------------------------
      IF ( ASCII ) THEN
        READ(LUN,*) ITEMS, NWIRE, SIZE
        READ(LUN,*) (WIRE_POS(I), I = 0, 15)
        NUM = 4 * ITEMS * NWIRE
        READ(LUN,*) (POINTERS(I), I = 1, NUM)
        NUM = 2 * SIZE
        READ(LUN,*) (MAP(I), I = 1, NUM)
      ELSE
        READ(LUN) ITEMS, NWIRE, SIZE
        READ(LUN) (WIRE_POS(I), I = 0, 15)
        NUM = 4 * ITEMS * NWIRE
        READ(LUN) (POINTERS(I), I = 1, NUM)
        NUM = 2 * SIZE
        READ(LUN) (MAP(I), I = 1, NUM)
      ENDIF
  999 RETURN
      END
