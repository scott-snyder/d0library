      SUBROUTINE CLANDA ( DGAS, PLHT)
C======================================================================
C
C   Purpose and Methods : Generate the integrated pulse height
C                         PARAMETERS : IDRAY=1 ILOSS=1 
C
C   Inputs  :    DGAS : Track length in the gas in the sense wire cell
C   Outputs :    PLHT : Integrated pulse height
C
C   Created  30-MAR-1987   K. Nishikawa
C   Updated  13-DEC-1989   Qizhong Li-Demarteau   add a check on BETHE 
C   Updated  17-JAN-1992   Srini Rajagopalan      completely rewritten
C   Updated  30-MAR-1992   K. Wyatt Merritt   Add a saturation cut on PLHT
C======================================================================
      IMPLICIT NONE

      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
C     
      INTEGER NPRIM,NSCND             ! # of primaries/secondaries
      INTEGER NPION(4),NPEXC(4)
      INTEGER MAX_PRIM                ! Maximum # of primaries allowed
      INTEGER I,NCOLL
      PARAMETER (MAX_PRIM=1000)
C
      REAL ELKE(MAX_PRIM)             ! Kinetic energy of primary electrons
C
      REAL DGAS                       ! Input path length
      REAL PLHT                       ! Output pulse height
C
      LOGICAL STATUS
C
C======================================================================
C
      PLHT=0.
C
      IF ( VECT(7) .EQ. 0. ) GO TO 999
      IF ( GETOT .LT. 0.0005 ) GO TO 999
C
C  Calculate the number of collisions due to each atomic shell
C
      CALL GTPRIM(DGAS,NPION,NPEXC,STATUS)
      IF (.NOT.STATUS) GO TO 999
C
C  Determine the number of primary electrons and their Kinetic energies
C
      CALL GTELKE(NPION,NPEXC,NPRIM,ELKE)
C
C  Calculate the number of secondary electrons generated
C
      CALL GTSCND(NPRIM,ELKE,NSCND)
C
C  PLHT is the sum of primaries and secondaries 
C  The fatctor 5.75 is to normalize the PLHT with the old CLANDA output.
C
      PLHT = 5.75*(FLOAT(NPRIM+NSCND))
      PLHT = AMIN1(PLHT,5000.) ! Place a sensible maximum on pulse height
C                                to avoid GSAHIT errors that may mask 
C                                more serious problems.
C
  999 RETURN
      END
