      SUBROUTINE GET_ISAL_PARTON(PARTID,ISALID,P,NP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the 8-vectors of all partons with the
C-   specified parton identifier from the ISAL bank.
C-
C-   Inputs  : PARTID     [I]   ISAJET particle ID of parton
C-             ISALID(*)  [I]   ISALID(1) = 0 - to return ALL partons
C-                              ISALID(1) > 0 - to return the data from
C-                                              the specified ISAL bank
C-
C-   Outputs : ISALID(*) [I]    ISAL Bank Ids
C-             P(8,*)    [R]    4-vectors of partons (px,py,pz,p)
C-                              + mass, phi, theta, eta
C-             NP        [I]    Number of partons
C-             IER       [I]     0  --  OK
C-                              -1  --  No ISAL bank
C-                              -2  --  No partons found
C-   Controls:
C-
C-   Created  28-APR-1992   Stan M. Krzywdzinski, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARTID
      INTEGER ISALID(*)
      REAL    P(8,*)
      INTEGER NP,IER
C----------------------------------------------------------------------
      INTEGER  GZISAL
      EXTERNAL GZISAL
C----------------------------------------------------------------------
C
      CALL GET_ISAJET_PARTON (GZISAL, PARTID, ISALID, P, NP, IER)
C
  999 RETURN
      END
