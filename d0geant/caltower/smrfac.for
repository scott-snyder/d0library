      REAL FUNCTION SMRFAC(LNEW,P,IDTRK,IETA,IP,IL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds a smearing factor by which to multiply
C-   the energy in cells given by D0GEANT.
C-
C-   The smearing factor is based on measured testbeam electron and hadron data
C-   and includes the following effects:
C-      varying plate thicknesses (smearing goes like sqrt(t))
C-      varying angles of incidence (smearing goes like 1/sqrt(costheta))
C-
C-   Returned value  : SMRFAC, smeared energy = SMRFAC * unsmeared energy
C-   Inputs  : LNEW  logical  .TRUE. if this is a new track
C-                            .FALSE. if same track as last call to SMRFAC
C-             P(9)  real     px,py,pz,p of track, m of track,P(6:8) unused,
C-                              E deposited in cell
C-             IDTRK integer  ISAJET track ID
C-             IETA,IP,IL     Cell indices
C-
C-   Created   18-NOV-1986  Alan M. Jonckheere
C-   Updated   9-OCT-1989   John Womersley   added # of gaps dependence
C-   Updated   20-NOV-1990  Natalie Roe  Re-tuned, made smearing a function of
C-                            energy deposited in cell
C-   Updated   2-MAR-1992   K. Wyatt Merritt   Put in J. Womersley's new
C-                            number for effective hadron resolution for
C-                            GHEISHA 8 (GEANT 3.14)
C-
C----------------------------------------------------------------------
C
C INPUT VARIABLES
      LOGICAL LNEW
      REAL P(9)
      INTEGER IDTRK,IETA,IP,IL
C LOCAL VARIABLES
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IE
      REAL PX,PY,PZ,PPP,PP,PT,COSTHN,PLATE,ECELL
      REAL ETASQ,BS,R1,R2
      REAL    RANGAM
C
C----------------------------------------------------------------------
C
C ****  announce smrfac version is new
      IF(FIRST)THEN
        WRITE(6,*) ' Using SMRFAC March 1992 - retuned with GHEISHA 8'
        FIRST=.FALSE.
      ENDIF
C
C ****  Get out as soon as possible
      IF ( IL.GT.17 ) THEN              ! Dead Material
        SMRFAC = 1.
        GOTO 999
      ENDIF
C
C ****  Do this part once per track
      IF ( LNEW ) THEN
        PX = P(1)
        PY = P(2)
        PZ = P(3)
        PP = P(4)
        PT = SQRT(PX*PX+PY*PY)
C
C ****  DETERMINE SMEARING BASED ON EC RESOLUTION; CORRECT FOR PLATE
C ****  THICKNESSES LATER 
        IF ((IABS(IDTRK).EQ.12).OR.(IDTRK.EQ.10)) THEN
          BS = 0.15**2/4.                      ! ELECTRONS
        ELSE
          BS = 0.35**2/6.                ! HADRONS
        ENDIF
      ENDIF  ! NEW TRACK
C
C ****  FIND PARTICULAR DETECTOR - FIND PLATE AND ANGLE FOR EACH
      IE = IABS(IETA)
      IF ( IL.LE.7 ) THEN               ! EM
        IF ( IE.GE.14 ) THEN
          PLATE = 4.                    ! EC EM
          COSTHN = ABS(PZ)/PP
        ELSE
          PLATE = 3.                    ! CC EM
          COSTHN = PT/PP
        ENDIF
      ELSEIF ( IL.LE.10 ) THEN          ! ICD & MSG
        SMRFAC = 1.
        GOTO 999
      ELSEIF ( IL.LE.14 ) THEN          ! FH
        PLATE = 6.
        IF ( IE.GE.11 ) THEN
          COSTHN = ABS(PZ)/PP           ! EC IFH + MFH
        ELSE
          COSTHN = PT/PP                ! CC FH
        ENDIF
      ELSEIF ( IL.EQ.15 ) THEN
        IF ( IE.LE.7 ) THEN
          COSTHN = PT/PP                ! CC CH
          PLATE = 46.48*(.32/1.43)      ! Cu plates
        ELSEIF ( IE.LE.12 ) THEN
          COSTHN = PT/PP                ! ECOH
          PLATE = 46.48*(.32/1.76)      ! Fe plates
        ELSE
          COSTHN = PT/PP                ! EC ICH + MCH
          PLATE = 46.48*(.32/1.76)      ! Fe plates
        ENDIF
      ELSEIF ( IL.GT.15 ) THEN          ! ECOH
        PLATE = 46.48*(.32/1.76)        ! Fe plates
        COSTHN = PT/PP
      ENDIF
C
      IF ( COSTHN.LT.0.5 ) COSTHN = 0.5 ! PROBABLY WIDE ANGLE SCATTER
C
C ****  determine SMRFAC
      ETASQ = BS*PLATE/COSTHN
      ECELL = P(9)
      SMRFAC = (ETASQ/ECELL)*RANGAM(ECELL/ETASQ)
C
  999 RETURN
      END
