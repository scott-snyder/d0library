      SUBROUTINE GTCAID_PACKED_WORD ( PACKEDWORD, IHIT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a CAEP/CAID style packed word, determine if
C-   there is a CAID
C-
C-   Inputs  : PACKEDWORD - [I] the packed address of the cell, in CAEP or CAID
C-                              format. NOTE THAT CAEP and CAID differ in the
C-                              meaning of one of the 4 bytes; only the 3
C-                              similar bytes are used.
C-
C-   Outputs : IHIT        - [I] the number of the hit in the CAID bank
C-                               corresponding to this cell.
C-                                = 0 if there is no such hit
C-                                = -1 if there are NO hits in the CAID bank
C-                                = -2 if there is no CAID bank (i.e. the AIDA
C-                                  algorithm has not been run on this event)
C-   Controls: none
C-
C-   Created  27-MAY-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER  PACKADDR, PACKEDWORD,IHIT,PACKWD
C----------------------------------------------------------------------
      INTEGER  LCAID, GZCAID, NREP, NAID, I, PTR
      EXTERNAL GZCAID
      BYTE     BB(4), CC(4)
      EQUIVALENCE (PACKADDR, BB(1))
      EQUIVALENCE (PACKWD, CC(1))
C----------------------------------------------------------------------
      PACKWD=PACKEDWORD
      LCAID = GZCAID()

      IF (LCAID .LE. 0 ) THEN
        IHIT = -2
        RETURN
      ENDIF

      NAID = IQ(LCAID + 4)
      IF ( NAID .EQ. 0 ) THEN
        IHIT = -1
        RETURN
      ENDIF

      IHIT = 0                          ! default is "no such hit found"
      NREP = IQ(LCAID + 2)

      DO I = 1, NAID
        PTR = LCAID + (I-1)*NREP
        PACKADDR = IQ(PTR + 8)
        IF ( BB(BYTE2) .EQ. CC(BYTE2) .AND.
     &       BB(BYTE3) .EQ. CC(BYTE3) .AND.
     &       BB(BYTE4) .EQ. CC(BYTE4) ) THEN
          IHIT = I
          RETURN                        ! found the matching hit, so return
        ENDIF                           ! if bb(byte2) .eq. ...
      ENDDO                             ! i = 1, naid

      RETURN
      END
