      SUBROUTINE OBJECT_PEVENT(IDX,NMAX,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object PEVENT.
C-   (Contents of ISAE bank).
C-
C-   Inputs  : IDX      [I]   Object Number (Unused)
C-             NMAX     [I]   Maximum number of quantities to return
C-   Outputs : ARRAY(*) [R]   Array of quantities
C-
C             +1     I       event id(1)
C              2     I       event id(2)
C              3     I       event number
C              4     I       reaction type
C              5     I       number of primary jet banks
C              6     I         "    of stable parton banks (final+initial)
C              7     I         "    of PJET banks
C              8     I         "    of particle banks
C              9     I         "    of vertex banks
C             10     I         "    of lepton banks
C             11     F       cross section in microbarns
C             12     F       weight
C             13     F       effective q**2
C             14     F       hard scattering invariant s
C             15     F        "      "          "      t
C             16     F        "      "          "      u
C             17     D        Seed (part 1)
C             18     D        Seed (part 2)
C-
C-   Controls:
C-
C-   Created  21-APR-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER MAXBUF
      PARAMETER( MAXBUF = 50 )
      INTEGER IBUFFER(MAXBUF), NN, GZISAE, LBANK
      REAL    BUFFER(MAXBUF)
      EQUIVALENCE (IBUFFER(1),BUFFER(1))
C----------------------------------------------------------------------
      INTEGER NTOTAL
      PARAMETER( NTOTAL = 18 )
C----------------------------------------------------------------------
C
C ****  OBJECT: PEVENT
C
      NN = MIN(NMAX,NTOTAL)
      CALL VZERO(BUFFER,NN)
      LBANK = GZISAE()
      IF ( LBANK .GT. 0 ) THEN
        CALL UCOPY(Q(LBANK+1),BUFFER(1),18)
        CALL UCOPY(BUFFER,ARRAY,NN)
      ENDIF
      RETURN
C
      ENTRY NOBJ_PEVENTS(NOBJS,NSIZE)
C
      NSIZE = NTOTAL
      LBANK = GZISAE()
      IF ( LBANK .GT. 0 ) THEN
        NOBJS = 1
      ELSE
        NOBJS = 0
      ENDIF
C
      RETURN
      END
