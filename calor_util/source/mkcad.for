C DEC/CMS REPLACEMENT HISTORY, Element MKCAD.FOR
C *1    11-SEP-1991 14:19:47 LINNEMANN "for remaking cad from caep "
C DEC/CMS REPLACEMENT HISTORY, Element MKCAD.FOR
      SUBROUTINE MKCAD(ECUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  BUILDS THE FAKE RAW CALORIMETER DATA BANKS 
C-                          CAD1,CAD2 INSIDE D0GEANT. 
C-                          uses CAEP bank built by D0GEANT
C-   Inputs  : ECUT     [R]     Minimum energy to record in CAD
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   7-APR-1989   A.P. White 
C-
C----------------------------------------------------------------------
C-
      IMPLICIT NONE
      REAL ECUT
C-
C----------------------------------------------------------------------
C
C--- This does the build from the GEANT CAEP bank
      CALL PATHST('GEAN')
      CALL FLCAD(ECUT)
   99 RETURN
      END
