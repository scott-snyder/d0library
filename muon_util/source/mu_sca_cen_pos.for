      SUBROUTINE MU_SCA_CEN_POS(ICENT,NMUOH_CENT,MUOH_CENT,IPADHIT,
     1  			IBEST,X,Y,Z,DX,DY,DZ,LOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : ICENT, SCALER CENTROID NUMBER
C-             NMUOH_CENT, NUMBER OF HITS ASSOCIATED W/ THE SCALER CENTROID
C-	       MUOH_CENT(20,16), ARRAY OF MUOH POINTERS FOR HITS
C-   Outputs : X,Y,Z,DX,DY,DZ, the position and error of the associated hits.
C-             LOK, logical .TRUE. if ICENT position found.
C-   Controls:
C-
C-   Created  23-APR-1993   Tom Diehl
C-   Updated  28-APR-1993   DF. Change format of input hits; correct logic
C-   Modified 14-NOV-1993   HTD. Uses TDIV1 or TDIV2
C-   Modified 19-NOV-1993   J. de Mello HTD. Use pads by MU_CENT_PAD_SOLNS.
C-   Modified 09-DEC-1993   HTD. New way to use pads. More like MUFTNB of DH.
C-   Modified 07-FEB-1994   HTD. LT->LE in eval. pad solu. Thanks to S.Snyder
C-   Modified 20-DEC-1994   HTD. Protect from PDTs with > 32 centroids. 
C-   Modified 04-APR-1995   MF Add test for 2nd time
C----------------------------------------------------------------------------
      IMPLICIT NONE
C<<
      INTEGER ICENT,NMUOH_CENT,MUOH_CENT(20,16)
      REAL X,Y,Z,DX,DY,DZ
      LOGICAL LOK,DTOK,POK
C<<
C-    Input (IMOD) and output from MUADD
      INTEGER IMOD,IPLN,IWIR,IERR
C<<
C-    Input (IHIT)  and output from GTMUOH
      INTEGER IHIT,IWADD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR,NHIT
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,DDIS2
      REAL TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR
C<<
C-    Chamber geometry variables.
      REAL VECT(3),WLEN,VOFF,VECTNB
      INTEGER IORI
C<<
C-    Calculable Parameters.
      REAL XSUM,YSUM,ZSUM,XHIT,YHIT,ZHIT
      REAL SQRTRJ,RJ
      REAL TDIV
C<<
C-    Loop indexes
      INTEGER J, IWAVE, IGOODDT, IGOODPAD
C<<
C-    How many pad lengths the delta t average contains
      INTEGER NWAVE
C<<
C-    Auxiliary variables to sort the best pad solution
      REAL POSNB, SLOPENB, PADSUM, PROJ1, PROJ2, PPOS(16,2)
      REAL GUESS1,GUESS2,XPOS(16),YPOS(16),ZPOS(16)
      REAL CLOSEST(16),SOLNW(16)
      INTEGER IBEST, IPADHIT, SOLNP(16)
C-
      REAL DNB
      REAL BIGT
      REAL VERNIR
C<<
      REAL SQRT12
      DATA SQRT12 /3.464102/                  ! Roughly
      DATA BIGT /9999./
      DATA VERNIR /60.96/
C----------------------------------------------------------------------
      XSUM = 0.
      YSUM = 0.
      ZSUM = 0.
      IGOODDT = 0
      IGOODPAD = 0
      IPADHIT = 0
      IBEST = 0
      DO J = 1,16
        CLOSEST(J) = 9999.
      ENDDO
C-
      IF(NMUOH_CENT.GE.25) THEN
        CALL ERRMSG('MU_ASTUB','MU_SCA_CEN_POS',
     $   'Number of centroids gt 24','W')
        LOK = .FALSE.
        GOTO 999
      ENDIF
C-
      DO J = 1,NMUOH_CENT	! LOOP OVER # OF HITS IN CENTROID
        IHIT = MUOH_CENT(ICENT,J)
        POK = .TRUE.
        DTOK = .TRUE.                       ! Look into MUOH bank.
        CALL GTMUOH(IHIT,IWADD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR,
     $            CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,DDIS2,
     $            TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
        CALL MUADD(IWADD,IMOD,IPLN,IWIR,IERR)
C-
C- This part contains the DT hit evaluation.
C-
        IF (IERR.LT.0) THEN               ! Not a valid module number.
          DTOK = .FALSE.
          POK = .FALSE.
        ENDIF
        IF (ABS(TDIV1) .GT. BIGT) THEN   ! This cut has not been studied.
          IF (ABS(TDIV2) .GT. BIGT .OR. NHWIR .EQ. 1) THEN
            DTOK = .FALSE.
          ELSE
            TDIV = TDIV2
          ENDIF
        ELSE
          TDIV = TDIV1
        ENDIF
C-
C- Here I turn the position of the center of the wire into a hit
C- position by correcting for the delta-t information.
C- The units are cm in global coordinates.
C-
        IF (DTOK) THEN
          IGOODDT = IGOODDT + 1  !  Add the hit to the "good" delta t list.
          IORIEN = IABS(IORIEN)
          IF (IORIEN.EQ.1 .OR. IORIEN.EQ.3) THEN
            XHIT = XCWIR
            YHIT = YCWIR + TDIV
            ZHIT = ZCWIR
          ELSE
            XHIT = XCWIR + TDIV
            YHIT = YCWIR
            ZHIT = ZCWIR
          ENDIF
C<<
          XSUM = XSUM + XHIT
          YSUM = YSUM + YHIT
          ZSUM = ZSUM + ZHIT
        ENDIF
C-
C- Pad hit usage determined here.
C- If any of the 3 bits of IFW1 are set we don't want this pad:
C-   (see MUOH.ZEB)   bit 0 - No pad latch
C-                    bit 1 - No pad pulse
C-                    bit 2 - Pad pulse overflow
C-
        IF (IAND(IFWH1,7).NE.0) THEN
          POK = .FALSE.
        ELSE
          IPADHIT = IBSET(IPADHIT,J-1) ! Identifier for which pad hits are
          IGOODPAD = IGOODPAD + 1      ! used in padfit.
          PPOS(IGOODPAD,1) = VERD1
          PPOS(IGOODPAD,2) = VERD2
          XPOS(IGOODPAD) = XCWIR
          YPOS(IGOODPAD) = YCWIR
          ZPOS(IGOODPAD) = ZCWIR
        ENDIF
C<<
      ENDDO                      ! DO J looped over HITS.
C-
C- If no good DT hits, get out - set flag
C-
      IF(IGOODDT.EQ.0) THEN
        LOK = .FALSE.
        GOTO 999
      ELSE
	LOK = .TRUE.
      ENDIF
C-
C- Calculate the delta t average of all the good hits.
C-
      RJ = FLOAT(IGOODDT)
      SQRTRJ = SQRT(RJ)                        ! Faster to take sqrts once.
      X = XSUM/RJ
      Y = YSUM/RJ
      Z = ZSUM/RJ
C-
C- Positions using DT information and a crude estimate for the uncertainties.
C- Setup some of the pad calculation.
C- Calculate the NB slope of a line from x=y=z=0 DT point.
C- Warning: I use pdt number to determine if its in central. This is
C- incorrect for central B and C layer chambers.
C-
      IF(IMOD.LE.36) THEN
        IF(IORIEN.EQ.1.OR.IORIEN.EQ.3) THEN
          DX = (5./SQRT12)/SQRTRJ
          DY = 35./SQRTRJ                      ! Non-Drift View.
          DZ = (10./SQRT12)/SQRTRJ             ! Drift View.
          POSNB = Y
          DNB = DY
          SLOPENB = POSNB/X
        ELSE
          DX = 35./SQRTRJ                      ! Non-Drift View.
          DY = (5./SQRT12)/SQRTRJ
          DZ = (10./SQRT12)/SQRTRJ             ! Drift View.
          POSNB = X
          DNB = DX
          SLOPENB = X/Y
        ENDIF
      ELSE
        IF(IORIEN.EQ.1.OR.IORIEN.EQ.3) THEN
          DX = (10./SQRT12)/SQRTRJ             ! Drift View.
          DY = 35./SQRTRJ                      ! Non-Drift View.
          DZ = (5./SQRT12)/SQRTRJ
          POSNB = Y
          DNB = DY
          SLOPENB = Y/Z
        ELSE
          DX = 35./SQRTRJ                      ! Non-Drift View.
          DY = (10./SQRT12)/SQRTRJ             ! Drift View.
          DZ = (5./SQRT12)/SQRTRJ
          POSNB = X
          DNB = DX
          SLOPENB = X/Z
        ENDIF
      ENDIF
C-
C- If no good pad hits we are done. Bail.
C-
      IF(IGOODPAD.EQ.0) THEN
        GOTO 999
      ENDIF
C-
C- Determine the chamber length WLEN and nonbend view center VECTNB of the PDT.
C-
      IPLN = 1
      IWIR = 1
      CALL MUGEOM(IMOD,IPLN,IWIR,VECT,WLEN,VOFF,IORI)
      IF(IORIEN.EQ.1.OR.IORIEN.EQ.3) THEN
        VECTNB = VECT(2)      ! NON-BEND PDT CENTER
      ELSE
        VECTNB = VECT(1)      ! NON-BEND PDT CENTER
      ENDIF
C-
C- Calculate the PAD wavelength of the DT solution.
C- NWAVE is number of wavelengths of DT hit from the nonbend view center of
C- the PDT.
C-
      NWAVE = (POSNB-VECTNB)/VERNIR
C-
C- Select the pad solution closest to the delta time position.
C- Project the NB track to each padhit assuming vertex at x=y=z=0.
C- Loop over possible wavelengths.
C-
      DO IWAVE = NWAVE-2, NWAVE+2
        DO J = 1, IGOODPAD
          GUESS1 = FLOAT(IWAVE)*VERNIR + PPOS(J,1) + VECTNB + WLEN/2.
          GUESS2 = FLOAT(IWAVE)*VERNIR + PPOS(J,2) + VECTNB + WLEN/2.
          IF(IMOD.LE.36) THEN
            IF(IORIEN.EQ.1.OR.IORIEN.EQ.3) THEN
              PROJ1 = SLOPENB*XPOS(J) - GUESS1
              PROJ2 = SLOPENB*XPOS(J) - GUESS2
            ELSE
              PROJ1 = SLOPENB*YPOS(J) - GUESS1
              PROJ2 = SLOPENB*YPOS(J) - GUESS2
            ENDIF
          ELSE
            PROJ1 = SLOPENB*ZPOS(J) - GUESS1
            PROJ2 = SLOPENB*ZPOS(J) - GUESS2
          ENDIF
          IF( ABS(PROJ1).LE.ABS(PROJ2).AND.
     $                      ABS(PROJ1).LT.CLOSEST(J) ) THEN
            SOLNP(J) = 1
            SOLNW(J) = IWAVE
            CLOSEST(J) = ABS(PROJ1)
            IBEST = IBCLR(IBEST,J-1)
          ELSEIF( ABS(PROJ2).LT.ABS(PROJ1).AND.
     $                          ABS(PROJ2).LT.CLOSEST(J) ) THEN
            SOLNP(J) = 2
            SOLNW(J) = IWAVE
            CLOSEST(J) = ABS(PROJ2)
            IBEST = IBSET(IBEST,J-1)
          ENDIF
        ENDDO
      ENDDO
C-
C- Put together those padhits identified as being closest to the
C- Delta-time solution into one average position.
C- Find the position and uncertainty in the NB position. This is pretty
C- crude and could be improved using better per hit estimates of the
C- uncertainty. I have assumed 5 cm per padhit.
C-
      PADSUM = 0.
      DO J = 1,IGOODPAD
        PADSUM = PADSUM + SOLNW(J)*VERNIR + PPOS(J,SOLNP(J)) + WLEN/2.
     $                  + VECTNB
      ENDDO
C<<
      IF(IORIEN.EQ.1.OR.IORIEN.EQ.3) THEN
        Y = PADSUM/FLOAT(IGOODPAD)
        DY = 12./SQRT(FLOAT(IGOODPAD))
      ELSE
        X = PADSUM/FLOAT(IGOODPAD)
        DX = 12./SQRT(FLOAT(IGOODPAD))
      ENDIF
C<<
  999 RETURN
      END
