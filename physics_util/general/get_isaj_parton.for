      SUBROUTINE GET_ISAJ_PARTON(PARTID,ISAJID,P,NP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the 8-vectors of all partons with the
C-   specified parton identifier from the ISAJ bank.
C-
C-   Inputs  : PARTID     [I]   ISAJET particle ID of parton
C-             ISAJID(*)  [I]   ISAJID(1) = 0 - to return ALL partons
C-                              ISAJID(1) > 0 - to return the data from
C-                                              the specified ISAJ bank
C-
C-   Outputs : ISAJID(*) [I]    ISAJ Bank Ids
C-             P(8,*)    [R]    4-vectors of partons (px,py,pz,p)
C-                              + mass, phi, theta, eta
C-             NP        [I]    Number of partons
C-             IER       [I]     0  --  OK
C-                              -1  --  No ISAJ bank
C-                              -2  --  No partons found
C-   Controls:
C-
C-   Created  28-APR-1992   Stan M. Krzywdzinski, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARTID
      INTEGER ISAJID(*)
      REAL    P(8,*)
      INTEGER NP,IER
C----------------------------------------------------------------------
      INTEGER  GZISAJ
      EXTERNAL GZISAJ
C----------------------------------------------------------------------
C
      CALL GET_ISAJET_PARTON (GZISAJ, PARTID, ISAJID, P, NP, IER)
C
  999 RETURN
      END
