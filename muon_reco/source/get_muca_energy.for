      SUBROUTINE GET_MUCA_ENERGY(LPMUO,E_CONE_6,E_CONE_7,E_CONE_8,
     & ET_CONE_6,ET_CONE_7,ET_CONE_8)
C----------------------------------------------------------------------
C-   Purpose: To sum the total muon energy and Et from cone sizes of
C-            .1 to 1.
C-   
C-   Input:  LPMUO 
C-   Output: E_CONE_X  - E within DR = .6,.7,.8 around the muon
C-           ET_CONE_X - Et within DR = .6,.7,.8 around the muon
C-
C-   Author:  Gene Álvarez
C-   Created: 30 Oct 1993
C-   Revised:  3 Dec 1993 Gene Álvarez - conformed to D0 standards.
C-   Updated  17-DEC-1993   Ian Adam  - fix VZERO call  
C-   Updated: 26 Mar 1994 Gene Álvarez - protect against no MUON link
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'

      INTEGER I,J,ICALL
      INTEGER LPMUO,LMUON,NS
      REAL    ETA,PHI,ET,HIT_CELLS,MU_ETOT
      REAL    E_CONE_6,E_CONE_7,E_CONE_8,ET_CONE_6,ET_CONE_7,ET_CONE_8
      REAL    MU_ET,ENERGY(10,2),MU_EEM_ARRAY(10,2),MU_HAD_ARRAY(10,2)
      DATA    ICALL/0/
C----------------------------------------------------------------------

      IF (ICALL .EQ. 0) ICALL = 1

      MU_ETOT = 0.
      MU_ET   = 0.
      CALL VZERO(ENERGY,20)

        IF (LPMUO .LE. 0) RETURN
        NS    = IQ(LPMUO - 2)
        LMUON = LQ(LPMUO - NS - 2)
        IF (LMUON .LE. 0) RETURN

        CALL ET_MUCA(LMUON,ET)

        ETA = Q(LPMUO + 16)
        PHI = Q(LPMUO + 17)
        HIT_CELLS = Q(LPMUO + 34)

        CALL UNPACK_CATD(ETA,PHI,'EM',MU_EEM_ARRAY)
        CALL UNPACK_CATD(ETA,PHI,'HA',MU_HAD_ARRAY)
        DO 1 J = 1, 10
          MU_ETOT = MU_ETOT + MU_EEM_ARRAY(J,1) + MU_HAD_ARRAY(J,1)
          MU_ET   = MU_ET + MU_EEM_ARRAY(J,2) + MU_HAD_ARRAY(J,2)
          ENERGY(J,1) = MU_ETOT - HIT_CELLS
          ENERGY(J,2) = MU_ET - ET
    1   CONTINUE

        E_CONE_6  = ENERGY(6,1)
        ET_CONE_6 = ENERGY(6,2)
        E_CONE_7  = ENERGY(7,1)
        ET_CONE_7 = ENERGY(7,2)
        E_CONE_8  = ENERGY(8,1)
        ET_CONE_8 = ENERGY(8,2)

      RETURN
      END

