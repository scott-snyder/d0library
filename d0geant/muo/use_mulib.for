C VAX/DEC CMS REPLACEMENT HISTORY, Element USE_MULIB.FOR
C *1     5-AUG-1993 15:40:37 STEWART "muon gmuh fill code"
C VAX/DEC CMS REPLACEMENT HISTORY, Element USE_MULIB.FOR
      SUBROUTINE USE_MULIB(PS,KTRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :GIVEN MULB ARRAY, Takes the GMUH info
C-   from there and dumps it into Geant.
C-
C-   Inputs  :PS(4) = 4 VECTOR OF ISAJET TRACK
C-            KTRA = TRACK NUMBER
C-   Outputs :
C-   Controls:
C-
C-   Created : 10-Jun-1993  Jasbir Singh and Chip Stewart
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:MULDAT.INC'
C
      INTEGER IPART
      INTEGER PT_GMUH
C
      INTEGER ITRA,KTRA
      INTEGER I,NSKIP,ISET,IDET,NUMBV(2),IHIT
      INTEGER IETRK,IPTRK,IESTO,IPSTO,INDEX
      INTEGER IDTRK,NUM_HITS
      REAL PS(4),PP(9),HITSM(9),RSET,RDET,RTRA,RNUMBV(2)
      EQUIVALENCE (ISET,RSET),(IDET,RDET),(ITRA,RTRA)
      EQUIVALENCE (NUMBV(1),RNUMBV(1))
C
C----------------------------------------------------------------------
C

      DO I =  1, 4
        PP(I) = PS(I)                   ! Track 4 momentum
        PP(4+I) = 0                     ! Unused
      ENDDO
      IESTO = MULIB(10)                   ! ETA OF STORED TRACK
      IPSTO = MULIB(11)                   ! PHI OF STORED TRACK
C
      IETRK = IETAM_PRIMARY
      IPTRK = IPHIM_PRIMARY             ! ETA AND PHI OF CURRENT TRACK
C
      PT_GMUH = MULIB(2)                 ! POINTER TO 1ST GMUH AREA IN SHLB
C
C ****  Calculate energy scaling from library track to event track
C
      IPART = MULIB(5)
C
 1000 NUM_HITS=MULIB(PT_GMUH+6)
C
      IPART = MULIB(PT_GMUH + 5)         ! GEANT ID OF GMUH SEGMENT
C
      CALL GEAISA(IPART,IDTRK)
C
C ****  Loop over hits and store them
C
      IF (NUM_HITS.GE.1)THEN
        NSKIP = PT_GMUH + 6
        DO I = 1,NUM_HITS
C********  HIT SUMMARY
        RSET = MULIB(NSKIP+1)
        RDET = MULIB(NSKIP+2)
        RTRA = MULIB(NSKIP+3)
        RNUMBV(1) = MULIB(NSKIP+4)
        RNUMBV(2) = MULIB(NSKIP+5)
        CALL UCOPY(MULIB(NSKIP+6),HITSM(1),9)
        NSKIP = NSKIP + 15
        CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITSM,IHIT)
        ENDDO
C
  550   CONTINUE
      ENDIF
C
      PT_GMUH = mulib(PT_GMUH)
      IF(PT_GMUH.NE.0)GO TO 1000        ! MORE GMUH STUFF
C
 9999 CONTINUE
      KTRA = ITRA
  999 RETURN
      END
