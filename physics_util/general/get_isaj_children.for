      SUBROUTINE GET_ISAJ_CHILDREN(PARENTID,PISAJID,ID,ISAJID,P,NP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-   Return IDs and 8-vectors of children within ISAJ banks for a given
C-   parent parton within the ISAJ banks.
C-
C-   Call
C-   the routine GET_ISAJ_PARTON to get the PISAJID which can then be
C-   used along with the PARENTID to obtain the children (decay products).
C-
C-   Inputs  : PARENTID  [I]     ISAJET particle ID of parent
C-             PISAJID   [I]     ISAJ bank identifier of parent
C-
C-   Outputs : ID(*)     [I]     ISAJET id's of children
C-             ISAJID(*) [I]     ISAJ bank identifiers of children
C-             P(8,*)    [R]     8-vectors of children :
C                                (px, py, pz, p, mass, phi, theta, eta)
C-             NP        [I]     Number of children
C-             IER       [I]      0 -- OK
C-                               -1 -- No ISAJ bank 
C-                               -2 -- No ISAJ bank with specified parent 
C-                                     parton
C-                               -5 -- No children found
C-   Controls:
C-
C-   Created 28-APR-1992   Harrison B. Prosper and Stan M. Krzywdzinski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARENTID, PISAJID
      INTEGER ID(*), ISAJID(*)
      REAL    P(8,*)
      INTEGER NP,IER
C----------------------------------------------------------------------
      INTEGER  GZISAJ
      EXTERNAL GZISAJ
C----------------------------------------------------------------------
C
      CALL GET_ISAJET_CHILDREN (GZISAJ, GZISAJ, 1, PARENTID, PISAJID,
     &                          ID, ISAJID, P, NP, IER)
C
  999 RETURN
      END
