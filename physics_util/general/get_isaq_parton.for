      SUBROUTINE GET_ISAQ_PARTON(PARTID,ISAQID,P,NP,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the 8-vectors of all partons with the
C-   specified parton identifier from the ISAQ bank.
C-
C-   Inputs  : PARTID     [I]   ISAJET particle ID of parton
C-             ISAQID(*)  [I]   ISAQID(1) = 0 - to return ALL partons
C-                              ISAQID(1) > 0 - to return the data from
C-                                              the specified ISAQ bank
C-
C-   Outputs : ISAQID(*) [I]    ISAQ Bank Ids
C-             P(8,*)    [R]    4-vectors of partons (px,py,pz,p)
C-                              + mass, phi, theta, eta
C-             NP        [I]    Number of partons
C-             IER       [I]     0  --  OK
C-                              -1  --  No ISAQ bank
C-                              -2  --  No partons found
C-   Controls:
C-
C-   Created  28-APR-1992   Stan M. Krzywdzinski, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARTID
      INTEGER ISAQID(*)
      REAL    P(8,*)
      INTEGER NP,IER
C----------------------------------------------------------------------
      INTEGER  GZISAQ
      EXTERNAL GZISAQ
C----------------------------------------------------------------------
C
      CALL GET_ISAJET_PARTON (GZISAQ, PARTID, ISAQID, P, NP, IER)
C
  999 RETURN
      END
