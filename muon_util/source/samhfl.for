      SUBROUTINE SAMHFL (NSTA,NSEC,IADD,IADC,DIST,ITYPE,SPL,TUBE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine fills one hit in bank SAMH
C-
C-   Inputs  : NSTA    - Station number
C-             NSEC    - Section number
C-             IADD    - Module/cell address
C-             IADC    - Raw ADC time
C-             DIST    - Drift distance
C-             ITYPE   - Tube type
C-             SPL     - Split distance
C-             TUBE(6) - Position and direction cosines of tube
C-   Outputs : none
C-   Controls: none
C-
C-   Created  12-MAR-1991   O. Eroshin 
C-   Updated  11-SEP-1991   Daria Zieminska:  new structure:
C-                          assume that the MUD1 data have been unpacked
C-                          by a call to Mike Fortner's upgraded MUSRT1
C-                          and the pointers to SAMUS stations are
C-                          stored in SAHH add a logical argument OK. 
C-   Updated   6-NOV-1991   Daria Zieminska: eliminate the use of QSORT
C-   Updated  23-DEC-1991   Daria Zieminska: check the presence of data
C-                          for a given station
C    DH 5/92 12 bit module word count
C-   Updated  14-MAY-1992   Daria Zieminska: cut 12 bits: check if MC data 
C-   Updated  27-MAY-1992   Vladimir Glebov: update for real SAMUS data 
C-                          and for D0 standards      
C-   Updated  16-NOV-1992   Alexander Efimov : SAGTUB get tubes
C-                          coordinates in the D0 global system.
C-   Repaired 25-Feb-1993   HTD DF Put switch for L2 cuz no SELC bank there.
C-   Updated  06-MAY-1993   Denisov - add protection against wrong SAADR output
C-   Replaced 27-Sep-1993   M. Fortner - remove loop, match 1B unpacking
C-   Replaced 19-Feb-1994   M. Fortner - add call to fill MUHM bank
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE   'D0$INC:ZEBCOM.INC/LIST'
      INTEGER N_PLANES, N_SECTIONS
      PARAMETER (N_PLANES=3, N_SECTIONS=6)
      INTEGER NSTA, NSEC, IADD, IADC, ITYPE, IND, IHIT, NHIT, NMOD
      INTEGER LSAHH, GZSAHH, LSAMH, NDUM(4)
      EXTERNAL GZSAHH
      REAL DIST, SPL, TUBE(6)
      INTEGER KPLN(N_SECTIONS)
      DATA    KPLN /1, 3, 2, 1, 3, 2/
C
C  Get bank pointer
C
      LSAHH = GZSAHH()
      IND = N_PLANES * (NSTA - 1) + KPLN(NSEC)
      NHIT = IQ(LSAHH+IND)
      LSAMH = LQ(LSAHH-IND) + 15*NHIT
C
C  Load SAMH information
C
      IQ(LSAMH+2)  = IADD
      IQ(LSAMH+3)  = 0
      Q (LSAMH+4)  = SPL
      IQ(LSAMH+5)  = ITYPE
      Q (LSAMH+6)  = TUBE(1)
      Q (LSAMH+7)  = TUBE(2)
      Q (LSAMH+8)  = TUBE(3)
      Q (LSAMH+9)  = TUBE(4)
      Q (LSAMH+10) = TUBE(5)
      Q (LSAMH+11) = TUBE(6)
      Q (LSAMH+12) = IADC
      Q (LSAMH+13) = 1.
      Q (LSAMH+14) = DIST
      Q (LSAMH+15) = 1.
C
C  Update SAHH and MUHM/MUOF/MUHT banks
C
      NHIT = NHIT + 1
      IQ(LSAHH+IND) = NHIT
      NMOD = IADD/256
      CALL MUHMFL(3,NMOD,IHIT,NHIT,NDUM)
      CALL MUOFFL(3,NMOD,IHIT,NHIT)
C
      RETURN
      END
