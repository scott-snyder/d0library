      SUBROUTINE FLGMUH(ITRA,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill GMUH with bank number and data obtained from
C-                              JKINE bank via CALL GKINE(....)
C-
C-   Inputs  : ITRA  = GEANT track number
C-             LBANK = pointer to GMUH bank
C-   Outputs : GMUH data ->
C-                 LQ(LBANK -   1)  = Reference link to ISP1 or previous GMUH
C-                  Q(LBANK +  4-6) = GEANT Vertex position
C-                  Q(  "   +  7-10)= GEANT track 4 momentum
C-                 IQ(  "   + 11)   = GEANT particle type
C-                 IQ(  "   + 12)   = ISAJET particle momentum
C-                 IQ(  "   + 13)   = ISAJET Vertex Z
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
C-   Created  08-APR-1993   Jasbir Singh, Chip Stewart 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRA,LBANK
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      REAL VERT(3),PVERT(4),UBUF(10)
      INTEGER IPART,NVERT,NUBUF,ISAJET_TRK,ISAJET_VTX,PARENT_TRK,NTRAK
      INTEGER I
      CHARACTER MSG*80
C----------------------------------------------------------------------
C
      IF (ITRA.LE.0)  GOTO 999
      CALL GFKINE(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NUBUF)
      IQ(LBANK+11) = IPART         ! GEANT ID
      DO 10 I = 1,3
   10 Q(LBANK+I+3) = VERT(I)       ! GEANT VERTEX XYZ
      DO 20 I = 1,4
   20 Q(LBANK+6+I) = PVERT(I)      ! GEANT TRACK MOMENTUM
      IF (NUBUF.GT.0) THEN
        IQ(LBANK+14) = UBUF(2)     ! GEANT PARENT TRK & SOURCE REASON
        IQ(LBANK+15) = UBUF(3)
      ENDIF

C
C ****  look for ISAJET track up geant track stack 
C
      ISAJET_VTX = UBUF(4)
      ISAJET_TRK = UBUF(5)
      PARENT_TRK = UBUF(2)
      NTRAK = ITRA
      IF (ISAJET_VTX*ISAJET_TRK.GT. 0) THEN
        GOTO 5
      ELSE IF( PARENT_TRK .GT.0) THEN
        DO WHILE (PARENT_TRK.GT.0)
          CALL GFKINE(PARENT_TRK,VERT,PVERT,IPART,NVERT,UBUF,NUBUF)
          PARENT_TRK = UBUF(2)
          IF(UBUF(5).GT.0.0)GO TO 2
        END DO
 2      CONTINUE
        ISAJET_VTX = UBUF(4)
        ISAJET_TRK = UBUF(5)
        IF (ISAJET_VTX*ISAJET_TRK.GT. 0) GOTO 5
        WRITE(MSG,'(A,2I5)')
     &      'UNEXPECTED UBUF FROM GFKINE ' ,NINT(UBUF(2)),NINT(UBUF(5))
        CALL ERRMSG('GMUH','FLGMUH',MSG,'W')
        GOTO 999
      ELSE
        WRITE(MSG,'(A,2I5)')
     &      'UNEXPECTED UBUF FROM GFKINE ' ,NINT(UBUF(2)),NINT(UBUF(5))
        CALL ERRMSG('GMUH','FLGMUH',MSG,'W')
        GOTO 999
      END IF
    5 CONTINUE
      Q(LBANK+12)=SQRT(PVERT(1)**2+PVERT(2)**2+PVERT(3)**2) !ISAJET TRK MOMENTUM
      Q(LBANK+13)= VERT(3)       ! ISAJET VERTEX Z
      IF (NUBUF.GT.0) THEN
        IQ(LBANK+16) = UBUF(4)   !ISAJET VTX & TRK NUMBERS
        IQ(LBANK+17) = UBUF(5)   !ISAJET VTX & TRK NUMBERS
      ENDIF
  999 RETURN
      END
