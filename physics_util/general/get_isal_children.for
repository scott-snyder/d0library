      SUBROUTINE GET_ISAL_CHILDREN(PARENTID,PISAQID,ID,ISALID,P,NP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-   Return IDs and 8-vectors of children within ISAL banks for a given
C-   parent parton within the ISAQ banks.
C-
C-   Call
C-   the routine GET_ISAQ_PARTON to get the PISAQID which can then be
C-   used along with the PARENTID to obtain the children (decay products).
C-
C-   Inputs  : PARENTID  [I]     ISAJET particle ID of parent
C-             PISAQID   [I]     ISAQ bank identifier of parent
C-
C-   Outputs : ID(*)     [I]     ISAJET id's of children
C-             ISALID(*) [I]     ISAL bank identifiers of children
C-             P(8,*)    [R]     8-vectors of children :
C                                (px, py, pz, p, mass, phi, theta, eta)
C-             NP        [I]     Number of children
C-             IER       [I]      0 -- OK
C-                               -1 -- No ISAQ bank 
C-                               -2 -- No ISAQ bank with specified parent 
C-                                     parton
C-                               -3 -- No ISAL bank
C-                               -5 -- No children found
C-   Controls:
C-
C-   Created 28-APR-1992   Harrison B. Prosper and Stan M. Krzywdzinski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARENTID, PISAQID
      INTEGER ID(*), ISALID(*)
      REAL    P(8,*)
      INTEGER NP,IER
C----------------------------------------------------------------------
      INTEGER  GZISAQ, GZISAL
      EXTERNAL GZISAQ, GZISAL      
C----------------------------------------------------------------------
C
      CALL GET_ISAJET_CHILDREN (GZISAQ, GZISAL, 2, PARENTID, PISAQID,
     &                          ID, ISALID, P, NP, IER)
C
  999 RETURN
      END
