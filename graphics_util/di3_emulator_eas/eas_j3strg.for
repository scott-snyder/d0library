        SUBROUTINE J3STRG(STR)
C
C    Purpose:
CD   This module displays the character string passed to it with the
CD   preset parameters.  The parameter passed is:
CD      STR --> which is a character string.
CD   
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 08-Nov-1988
CH   History:
CH      16-MAR-89  SA   Translation performed after rotation.
CH      15-MAR-89  SA   Order of rotations corrected.
CH      14-MAR-89  SA   The erroneous routine KANGS was replaced by
CH                      a newly written routine KCANGS to correctly
CH                      perform string rotations.
CH      08-NOV-88  ATV  Add right handed stuff.
CH      10-OCT-88  ATV  Added the " char to string name.
CH      03-OCT-88  ATV  Segment restructure mods.
CH      26-AUG-88  ATV  Add call to KPRIM for primitive name generation.
CH      17-Aug-88  ATV  Add alternate entry points JHSTRG & JFSTRG.
CH      15-Aug-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-R, SEGINF-R, PRIMVR-R, LINATT-R, TEXATT-R
C
C    Calls:
CC      ERROR, KPRIM, PBEGS, PCHSCA, PSECOL, KANGS, PROTX, PROTY,
CC      PROTZ, PTRANS, PCHS, PENDS, PINCL
C
      EXTERNAL ERRHAN
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      CHARACTER*(*) STR 
C
C    Then local declarations of variables (non-common variables).
C
      CHARACTER*4 STRNAM
      REAL X, Y, Z, TRANS(3), ROT1, ROT2, ROT3, STEPX, STEPY
      REAL PHI, PSI, TET
      INTEGER NC, NCW, NCH
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
C
C    Then executable code follows
C
      ENTRY JHSTRG(STR)
      ENTRY JFSTRG(STR)
      ENTRY J2STRG(STR)
      ENTRY J1STRG(STR)
      IF (.NOT. SEGOPN) THEN
         CALL ERROR('JxSTRG: NO SEGMENT IS OPEN')
      ENDIF
      CALL KPRIM(STRNAM)
      NC = LEN(STR)
      GOTO (1000, 2000, 3000, 4000), CPATH
C
C    Path Left to Right
C
 1000 CONTINUE
      NCW = NC
      NCH = 1
      STEPX = 1.0 + CGAP
      STEPY = 0.0
      GOTO 90000
C
C    Path Top to bottom
C
 2000 CONTINUE
      NCW = 1
      NCH = -NC
      STEPX = 0.0
      STEPY = -1.0 - CGAP
      GOTO 90000
C
C    Path Right to Left
C
 3000 CONTINUE
      NCW = -NC
      NCH = 1
      STEPX = -1.0 - CGAP
      STEPY = 0.0
      GOTO 90000
C
C    Path Bottom to Top
C
 4000 CONTINUE
      NCW = 1
      NCH = NC
      STEPX = 0.0
      STEPY = 1.0 + CGAP
90000 CONTINUE
C
C    Determine and use justification.
C
      IF (CHJUST .EQ. 1) THEN
         X = CPX
      ELSE IF (CHJUST .EQ. 2) THEN
         X = CPX - NCW * CXSIZE / 2.0
      ELSE
         X = CPX - NCW * CXSIZE - CXSIZE
      ENDIF
      IF (CVJUST .EQ. 1) THEN
         Y = CPY
      ELSE IF (CVJUST .EQ. 2) THEN
         Y = CPY - NCH * CYSIZE / 2.0
      ELSE
         Y = CPY - NCH * CYSIZE - CYSIZE
      ENDIF
C
      CALL PBEGS(STRNAM, ERRHAN)
      CALL PCHSCA('"', CXSIZE, CYSIZE, '"', ERRHAN)
      CALL PSECOL('"', HUECOL(CURCOL + 1, 1),
     +            SATCOL(CURCOL + 1, 1), '"', ERRHAN)
      TRANS(1) = X
      TRANS(2) = Y
      TRANS(3) = CPZ
      CALL PTRANS('"', TRANS, '"', ERRHAN)
      CALL KCANGS(PHI, TET, PSI)
         CALL PROTZ('"', -PHI, '"', ERRHAN)
         CALL PROTX('"', -TET, '"', ERRHAN)
         CALL PROTZ('"', -PSI, '"', ERRHAN)
      CALL PCHS('"', 0.0, 0.0, 0.0, STEPX, STEPY, STR, ERRHAN)
      CALL PENDS(ERRHAN)
C
      CALL PINCL(STRNAM//'"', INST, ERRHAN)
      RETURN
      END
