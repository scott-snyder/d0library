      SUBROUTINE FLGCAH(ITRA,LGCAH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill GCAH with bank number and data obtained from
C-                              JKINE bank via CALL GKINE(....)
C-
C-   Inputs  : ITRA  = GEANT track number
C-             LGCAH = pointer to GCAH bank
C-   Outputs : GCAH data ->
C-                 LQ(LGCAH -   1)  = Reference link to ISP1 or previous GCAH 
C-                  Q(LGCAH +  4-6) = Source Vertex position
C-                  Q(  "   +  7-10)= ISAJET track 4 momentum
C-                 IQ(  "   + 11)   = Geant particle type
C-                 IQ(  "   + 12)   = Geant origin Vertex number
C-                 IQ(  "   + 13)   = Geant end Vertex number
C-                 IQ(  "   + 14)   = Geant track number of parent
C-                 IQ(  "   + 15)   = Geant source reason
C-                      = 1   -> ISAJET track                - -
C-                      = 11  -> decay product              'DCAY'
C-                      = 12  -> hadronic interaction       'HADR'
C-                      = 13  -> Muon-nuclear interaction   'MUNU'
C-                      = 14  -> Photo fission              'PFIS'
C-                      = 15  -> Pair production            'PAIR'
C-                      = 16  -> Compton scattering         'COMP'
C-                      = 17  -> Photo production           'PHOT'
C-                      = 18  -> Annihilation               'ANNI'
C-                      = 21  -> Punchthrough                - -
C-                      = 22  -> Bremstrahlung              'BREM'
C-                      = 23  -> Delta ray electron         'DRAY'
C-                      = 999 -> Stopping end vertex         - -
C-                 IQ(  "   + 16)   = ISAJET Vertex number
C-                 IQ(  "   + 17)   = ISAJET track number
C-   Controls: None
C-
C-   Created  23-JAN-1987   Alan M. Jonckheere
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRA,LGCAH
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      REAL VERT(3),PVERT(4),UBUF(10)
      INTEGER IPART,NVERT,NUBUF
      INTEGER I
C----------------------------------------------------------------------
C
      IF (ITRA.NE.0) THEN
        CALL GFKINE(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NUBUF)
        DO 10 I = 1,3
   10   Q(LGCAH+I+3) = VERT(I)
        DO 20 I = 1,4
   20   Q(LGCAH+6+I) = PVERT(I)
        IQ(LGCAH+11) = IPART
        IQ(LGCAH+12) = NVERT
        IF (NUBUF.GT.0) THEN
          DO 30 I = 1,5
   30     IQ(LGCAH+12+I) = UBUF(I)
        ENDIF
      ENDIF
  999 RETURN
      END
