      SUBROUTINE BLDTMP(LAYER,PLDTMW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build DTMP bank which contains Time-Space 
C-                         relation parameters for the sense wires in
C-                         the non-uniform drift region near anode
C-   Inputs  : data file containing parameters for the non-uniform 
C-             drift region near anode   
C-             LAYER: layer number
C-             LDTMW: address of the support bank
C-   Outputs : DTMP bank is filled
C-
C-   Created  12-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LOWRUN, HIGRUN, MPDTMP(5)
      INTEGER NWORDS, NSWIRE
      PARAMETER( NWORDS = 3 )
      PARAMETER( NSWIRE = 7 )
      INTEGER LDTMP(0:3), PLDTMW, PLDTMP, LAYER, IW, IP, JPOINT
      REAL    PARAM(3,7)
      DATA    MPDTMP/ 4HDTMP, 0, 0, 0, 0 /
      DATA    PARAM/ 0.120E-5, 0.333E-2,  0.2E-1,
     &               0.805E-6, 0.354E-2, -0.2E-1,
     &               0.103E-5, 0.343E-2,  0.2E-1,
     &               0.866E-6, 0.351E-2, -0.2E-1,
     &               0.103E-5, 0.343E-2,  0.2E-1,
     &               0.805E-6, 0.354E-2, -0.2E-1,
     &               0.120E-5, 0.333E-2,  0.2E-1/
C----------------------------------------------------------------------
      CALL MZFORM( 'DTMP', '5I -F', MPDTMP(5) )
      MPDTMP(4) = 5 + NSWIRE * NWORDS
      CALL MZLIFT( IDVSTP, LDTMP(LAYER), PLDTMW, -1, MPDTMP, -1 )
      PLDTMP = LDTMP(LAYER)
      IC( PLDTMP-5 ) = LAYER 
      IC( PLDTMP+1 ) = 0 
      IC( PLDTMP+2 ) = LOWRUN
      IC( PLDTMP+3 ) = HIGRUN
      IC( PLDTMP+4 ) = NWORDS 
      IC( PLDTMP+5 ) = NSWIRE 
C
      DO 100 IW = 0, NSWIRE-1
        JPOINT = PLDTMP + IW * NWORDS + 5
        DO 200 IP = 1, 3
          IC(JPOINT+IP) = PARAM(IP,IW+1)
  200   CONTINUE
  100 CONTINUE
C
  999 RETURN
      END
