C+
      SUBROUTINE SAMSRT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine does the initial sorting
C-                         on the muon drift cells and fills bank
C-                         'SAHS' with pointers to the location in the
C-                         raw data for the initial hit in each
C-                         SAMUS module.
C-
C-     SAHS -- SAMUS FLAG BANK
C-
C-       There are 18 entries for counts, 6 for pointers.
C-            1 -- no. hit tubes in station 1 plane 1
C-            2 -- no. hit tubes in station 1 plane 2
C-            3 -- no. hit tubes in station 1 plane 3
C-          4-6 -- no. hit tubes in station 2 planes 1-3
C-          7-9 -- no. hit tubes in station 3 planes 1-3
C-        10-12 -- no. hit tubes in station 4 planes 1-3
C-        13-15 -- no. hit tubes in station 5 planes 1-3
C-        16-18 -- no. hit tubes in station 6 planes 1-3
C-        19-24 -- number of the 3-hits in station 1 - 6
C-        25-30 -- pointers in MUD1 to station 1 - 6
C-
C-   Inputs  : HIT3MX - maximum number of the 3-hits in station.
C-   Outputs : none.
C-   Controls: none.
C-
C-   Created  12-AUG-1992   Alexander Efimov
C-   Updated  12-NOV-1992   Alexander Efimov   
C-   Updated  10-NOV-1993   Vladimir Podstavkov  
C-                          Rewritten to eliminate the direct
C-                          access to MUD1 bank and to switch to MF routine  
C-   Updated   8-FEB-1994   Alexander Efimov  use GZ... routines 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LSAHS, GZSAHS, LD, CRID, NHEADW, NDATAW
      INTEGER ISTA, INDA, IND, NMAC, IMAC, MAC, LOC, NWORD, NHITS
      INTEGER N_PLANES, N_STATION, N_SECTION
      PARAMETER (N_PLANES=3, N_STATION=6, N_SECTION=6)
      INTEGER TABL(6)
      LOGICAL FIRST
      INTEGER JHIT, IHEAD, ISEC
      INTEGER NMOD
      INTEGER IERR, NTRG1
      SAVE FIRST, NTRG1
      DATA TABL /1, 3, 2, 1, 3, 2/
      DATA FIRST /.TRUE./
C
C ****  initializing
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('NTRG1', NTRG1, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      LSAHS = GZSAHS ()
C
C ****  Loop over SAMUS stations
C
      CALL MUDMOD(0, NHITS, JHIT, IHEAD)              ! VP
      DO ISTA = 1, N_STATION                    ! Station loop
C
C ****  The following statement is changed because it violates
C       the new concept of MF and fortunately there are no references to 
C       these locations. VP
C
C       IQ(LSAHS+25+ISTA-1) = LOC - LMUD1         ! pointer to data
        IQ(LSAHS+25+ISTA-1) = 0                         ! VP
        IQ(LSAHS+19+ISTA-1) = NTRG1               ! number of the 3-hits
        INDA = LSAHS + 3*(ISTA-1)
C
C ****  Loop over SAMUS sections
C
        DO ISEC = 1, N_SECTION                  ! Section (MAC) loop
          CALL SATOPM(ISTA, ISEC, NMOD)               ! VP
          CALL MUDMOD(NMOD, NHITS, JHIT, IHEAD)       ! VP
          IND = INDA + TABL(ISEC)
          IQ(IND) = IQ(IND) + NHITS         ! increment number of hits in SAHS
        END DO
      END DO
C
  999 CONTINUE
      RETURN
      END
