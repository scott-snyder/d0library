      SUBROUTINE MTC_MUCALFEN(VTX,DVTX,ETA,PHI)
C----------------------------------------------------------------------
C- MTC_MUCALFEN: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : - FAST VERSION - Fill the block /MTC_ETOWERS/
C-      with cell energies in the 3x3 tower at the input ETA,PHI 
C-      projected from the VTX
C-
C-   Inputs  : VTX(3)  - vertex coordinates
C-             DVTX(3) - uncertainty in vertex
C-             ETA,PHI
C-   Outputs : fills /MTC_ETOWERS/ and /MTC_EHTOWERS/
C-
C-   Created   9-DEC-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- Input vertex, eta, and phi ...
      REAL VTX(3),DVTX(3),ETA, PHI
C- block for energies, chi squareds ...
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
      INCLUDE 'D0$INC:MTC_AETOWERS.INC'
C- arguments for CLINPH ...
      INTEGER NCLMAX
      PARAMETER (NCLMAX=20)
      INTEGER NCELL, IETAC(NCLMAX), IPHIC(NCLMAX),LAYERC(NCLMAX), ARGSOK
      INTEGER NCL, ICL
C- for calling GTCAEP_ADDR ...
      REAL    energy
      INTEGER ier_caep
ccC- for calling GTCAEH_ADDR ...
cc      REAL EX,EY,EZ,ENERGY,ET_CAEH,SEX,SEY,CWEIGHT
cc      INTEGER STATUS,IER_CAEH
C- direction cosines of input eta,phi
      REAL DIRCIN(3)
C- do loop and cal location ...
      INTEGER ICAL, IHERE, ISUB
      INTEGER IETA, IPHI, JPHI, ILYR
      INTEGER IE2, IP2, IETA2, IPHI2, IHERE2, ISUB2
      INTEGER IXYZ
C- functns ...
      INTEGER MTC_IWHERE, MTC_IWHLAYER,MTC_ISUBETA
C----------------------------------------------------------------------
C- /MTC_CALSECTS/ contains arrays specifying all cal sections, sublayers and
C- eta ranges -- acaltype(10), isublayer(10), islnum(10,5), isleta(10,5)
      INCLUDE 'D0$INC:MTC_CALSECTS.INC'
      INCLUDE 'D0$INC:MTC_ACALSECTS.INC'
C----------------------------------------------------------------------
C- put vertex position into /MTC_ETOWERS/
      DO 10 IXYZ=1,3
        VTXTWR(IXYZ) = VTX(IXYZ)
        DVTXTWR(IXYZ) = DVTX(IXYZ)
   10 CONTINUE
C----------------------------------------------------------------------
C- zero tower energy ...
      DO 212 ILYR=1,18
        IETATOWER(ILYR) = -50
        IPHITOWER(ILYR) = -50
        ESUM3(ILYR) = -2.             ! energy sum in each layer
C- init tower energy, ident arrays ...
        DO 36 IE2=ITLO,ITHI
          DO 37 IP2=ITLO,ITHI
            ENTOWER(IE2,IP2,ILYR)  = -2.
            ATOWER(IE2,IP2,ILYR)   = '--------'
            IF(ILYR.EQ.18) THEN
              ATOWER(IE2,IP2,ILYR)   = 'TOTAL-->'
            END IF
   37     CONTINUE
   36   CONTINUE
  212 CONTINUE

C- Return initialized values if zvtx=-5000. (no caep bank)
      IF(vtx(3).EQ.-5000.) return

C- Get the direction cosines DIRCIN(3) for the current track ETA,PHI
      CALL MTC_ETAPHICOS(ETA, PHI, DIRCIN)

C- Find all calor cells hit by a line ...
      CALL CLINPH(VTXTWR,DIRCIN,NCLMAX,NCELL,IETAC,IPHIC,LAYERC,ARGSOK)
      IF(ARGSOK.NE.0)
     &  WRITE(6,*) ' MTC_MUCALFEN: clinph failure flag !!!',ARGSOK,NCELL

C- Look for multiple cells hit in same layer, if there are 2 then do nothing,
C- if there are 3 then set the first occurance to the average eta and phi.
C- if there are 4 layers, then print out the results (4 should be impossible?)
      IF(NCELL.GE.2) THEN
        DO 210 NCL=1,NCELL-1
          IF(LAYERC(NCL).EQ.LAYERC(NCL+1)) THEN
            IF(LAYERC(NCL).EQ.LAYERC(NCL+2) .AND.
     &           (NCL+2).LE.NCELL ) THEN
              IF(LAYERC(NCL).EQ.LAYERC(NCL+3) .AND.
     &           (NCL+3).LE.NCELL ) THEN
                WRITE(6,*) ' MTC_MUCALFEN: Too many cells in layer !!!'
                WRITE(6,*) ' CLINPH out: cal cells hit by current line'
                WRITE(6,*) '    layer #  -  ietac  -  iphic  '
                DO 33 ICL=1,NCELL
                  WRITE(6,900) LAYERC(ICL), IETAC(ICL), IPHIC(ICL)
   33           CONTINUE
  900           FORMAT(1X,I11,I10,I10)
              ELSE
                IETAC(NCL) = IETAC(NCL+1)
                IPHIC(NCL) = IPHIC(NCL+1)
                LAYERC(NCL)= LAYERC(NCL+1)
              END IF
            END IF
          END IF
  210   CONTINUE
      END IF

      DO 110 ILYR = 1,17
C- skip last 3 EM3 sublayers (included when ilyr = 3)
        IF(ILYR.GE.4 .AND. ILYR.LE.6) GO TO 110

C- look for the current layer number in the list of cells hit by the line ...
        DO 34 NCL=1,NCELL
          IF(ILYR.EQ.LAYERC(NCL)) GO TO 35
          IF(ILYR.EQ.3 .AND.
     &        (LAYERC(NCL).GE.4 .AND. LAYERC(NCL).LE.6) ) GO TO 35
   34   CONTINUE
C- Current layer not found ... go to next layer ...
        GO TO 110

   35   CONTINUE              ! NCL indicates index of ietac,iphic
        IETA = IETAC(NCL)
        IPHI = IPHIC(NCL)
        IETATOWER(ILYR) = IETA
        IPHITOWER(ILYR) = IPHI

C- determine which calorimeter section I am in ...
        IHERE = MTC_IWHERE(IETA,IPHI,ILYR)
C- EM3 includes EM4-EM6 so reset IHERE if neccessary
        IF(ABS(IETA).LE.26 .AND. ILYR.EQ.3) THEN
          IF(IHERE.EQ.0 .AND.
     &        ABS(IETA).NE.12 .AND. ABS(IETA).NE.14) GO TO 114
          IF(ABS(IETA).EQ.14) IHERE = 2       ! ECEM
          IF(ABS(IETA).EQ.12) IHERE = 1       ! CCEM
        END IF

        IF(IHERE.EQ.0) GO TO 114

C- Get the sublayer number within this calorimeter section ...
        ISUB    = MTC_IWHLAYER(IHERE,ILYR)

        ESUM3(ILYR) = 0.
C- look at the cells in and around the central tower ...
        DO 112 IE2=ITLO,ITHI
          IETA2 = IETA + IE2
          IF(IETA2.EQ.0) IETA2 = IETA2 + IE2
          IF(ABS(IETA2).GE.38) GO TO 112

          DO 113 IP2=ITLO,ITHI
            IPHI2 = IPHI + IP2
Cccc            IF(ie2.eq.0 .AND. ip2.eq.0) go to 113  ! skip central bin
            JPHI = IPHI2
            IF (IPHI2.GE.65) JPHI = MOD(IPHI2,64)
            IF (IPHI2.LE.0)  JPHI = 64 + IPHI2
            IPHI2 = JPHI

C- determine which calorimeter section I am in ...
            IHERE2 = MTC_IWHERE(IETA2,IPHI2,ILYR)

C- If we are in the .05x.05 section of EM3,
C- sum energy over 4 pads (get ENERGY, reset IHERE if necessary) ...
            ENERGY = 0.
            IF(ABS(IETA2).LE.26 .AND. ILYR.EQ.3) THEN
              IF(IHERE2.EQ.0 .AND.
     &            ABS(IETA2).NE.12 .AND. ABS(IETA2).NE.14) GO TO 113
              CALL MTC_EN_EM3(IETA2,IPHI2,ENERGY,IER_CAEP)
              IF(ENERGY.LT.0.) ENERGY = 0.
              IF(ABS(IETA2).EQ.14) IHERE2 = 2         ! ECEM
              IF(ABS(IETA2).EQ.12) IHERE2 = 1         ! CCEM
            ELSE
C- else (not in EM3) get energy for this .1x.1 pad ...
              IF(IHERE2.NE.0)
     &            CALL GTCAEP_ADDR(IETA2,IPHI2,ILYR,ENERGY,IER_CAEP)
cc     &            CALL GTCAEH_ADDR(IETA2,IPHI2,ILYR,EX,EY,EZ,ENERGY,
cc     &            ET_CAEH,SEX,SEY,CWEIGHT,STATUS,IER_CAEH)
              IF(ENERGY.LT.0.) ENERGY = 0.
            END IF

            IF(IHERE2.EQ.0 .OR. IHERE2.NE.IHERE) GO TO 113
C- Get the sublayer number within this calorimeter section ...
            ISUB2  = MTC_IWHLAYER(IHERE2,ILYR)
            WRITE(ATOWER(IE2,IP2,ILYR),201)
     &          ACALTYPE(IHERE2),ISUB2,IETA2
  201       FORMAT(A4,I1,I3)

C- if ier_caep =0 --> we have a pad energy, ier_caep=-5 --> pad energy 0.
            IF(energy.EQ.0.) ier_caep = -5
            IF(IER_CAEP.EQ.0) THEN
              ENTOWER(IE2,IP2,ILYR) = ENERGY
            ELSE
              ENERGY = 0.
              IF(IER_CAEP.EQ.-5) THEN
                ENTOWER(IE2,IP2,ILYR) = 0.
              ELSE
                ENTOWER(IE2,IP2,ILYR) = -1.
                GO TO 113
              END IF
            END IF

            ESUM3(ILYR) = ESUM3(ILYR)  + ENERGY
  113     CONTINUE                    ! end of loop over ip2
  112   CONTINUE                      ! end of loop over ie2
  114   CONTINUE  ! go here if no cell (or zero energy cell) in this layer
  110 CONTINUE                        ! loop over 17 layer numbers ...

C- sum energy and chi squareds in towers ..............................
      DO 310 ILYR=1,17
C- If the energy sum for this layer is gt zero, then there are energies
C- and chi squareds in this layer ...
        IF(ESUM3(ILYR).GT.0.) THEN
C- initialize the energy ...
          IF(ESUM3(18).LT.0.) THEN
            ESUM3(18) = 0.
            DO 312 IE2=ITLO,ITHI
              DO 312 IP2=ITLO,ITHI
                ENTOWER(IE2,IP2,18)  = 0.
  312       CONTINUE
          END IF
C- Now sum the energies and chi squareds ...
          ESUM3(18) = ESUM3(18) + ESUM3(ILYR)
          DO 313 IE2=ITLO,ITHI
            DO 313 IP2=ITLO,ITHI
              IF(ENTOWER(IE2,IP2,ILYR).GE.0.) ENTOWER(IE2,IP2,18) =
     &            ENTOWER(IE2,IP2,18) + ENTOWER(IE2,IP2,ILYR)
  313     CONTINUE                    ! loop over 3x3 cells about tower
        END IF
  310 CONTINUE                        ! loop over layers
C----------------------------------------------------------------------
  999 RETURN
      END
