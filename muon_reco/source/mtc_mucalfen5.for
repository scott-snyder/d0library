      SUBROUTINE MTC_MUCALFEN5()
C----------------------------------------------------------------------
C- MTC_MUCALFEN5: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : - FAST VERSION - Fill the 2nd nearest neighbor
C-      components of the energy arrays in blocks /MTC_ETOWERS/ and
C-      /MTC_E5TOWERS/.  These are the energies of the OUTERMOST cells
C-      in the 5x5 tower about the input ETA,PHI projected from the VTX.
C-      MTC_MUCALFEN must be called before calling this routine.
C-
C-   Outputs : fills IETA=+-2 or IPHI=+-2 elements of energy
C-      arrays in /MTC_ETOWERS/.
C-
C-   Created   17-DEC-1993      Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- block for energies, chi squareds ...
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
      INCLUDE 'D0$INC:MTC_AETOWERS.INC'
      INCLUDE 'D0$INC:MTC_E5TOWERS.INC'
C- for calling GTCAEP_ADDR ...
      REAL    ENERGY
      INTEGER IER_CAEP
ccC- for calling GTCAEH_ADDR ...
cc      REAL EX,EY,EZ,ENERGY,ET_CAEH,SEX,SEY,CWEIGHT
cc      INTEGER STATUS,IER_CAEH
C- do loop and cal location ...
      INTEGER ICAL, IHERE, ISUB
      INTEGER IETA, IPHI, JPHI, ILYR
      INTEGER IE2, IP2, IETA2, IPHI2, IHERE2, ISUB2
C- functns ...
      INTEGER MTC_IWHERE, MTC_IWHLAYER,MTC_ISUBETA
C----------------------------------------------------------------------
C- /MTC_CALSECTS/ contains arrays specifying all cal sections, sublayers and
C- eta ranges -- acaltype(10), isublayer(10), islnum(10,5), isleta(10,5)
      INCLUDE 'D0$INC:MTC_CALSECTS.INC'
      INCLUDE 'D0$INC:MTC_ACALSECTS.INC'
C----------------------------------------------------------------------
C- zero tower energy ...
      DO 212 ILYR=1,18
        ESUM5(ILYR) = -2.             ! energy sum in each layer
C- init tower energy, ident arrays ...
        DO 36 IE2=ITTLO,ITTHI
          DO 37 IP2=ITTLO,ITTHI
            IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 37
            ENTOWER(IE2,IP2,ILYR)  = -2.
            ATOWER(IE2,IP2,ILYR)   = '--------'
            IF(ILYR.EQ.18) THEN
              ATOWER(IE2,IP2,ILYR)   = 'TOTAL-->'
            END IF
   37     CONTINUE
   36   CONTINUE
  212 CONTINUE

C- Return initialized values if zvtx in /MTC_ETOWERS/ is -5000. (no caep bank)
      IF(vtxtwr(3).EQ.-5000.) return

      DO 110 ILYR = 1,17
        IETA = IETATOWER(ILYR)
        IPHI = IPHITOWER(ILYR)
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

        ESUM5(ILYR) = 0.
C- look at the cells in and around the central tower ...
        DO 112 IE2=ITTLO,ITTHI
          IETA2 = IETA + IE2
          IF(IETA2.EQ.0) IETA2 = IETA2 + IE2
          IF(ABS(IETA2).GE.38) GO TO 112

          DO 113 IP2=ITTLO,ITTHI
            IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 113
            IPHI2 = IPHI + IP2
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
              IF(ABS(IETA2).EQ.14) IHERE2 = 2         ! ECEM
              IF(ABS(IETA2).EQ.12) IHERE2 = 1         ! CCEM
            ELSE
C- else (not in EM3) get energy for this .1x.1 pad ...
              IF(IHERE2.NE.0)
     &            CALL GTCAEP_ADDR(IETA2,IPHI2,ILYR,ENERGY,IER_CAEP)
cc     &            CALL GTCAEH_ADDR(IETA2,IPHI2,ILYR,EX,EY,EZ,ENERGY,
cc     &            ET_CAEH,SEX,SEY,CWEIGHT,STATUS,IER_CAEH)
            END IF
            IF(ENERGY.LT.0.) ENERGY = 0.
C-
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

            ESUM5(ILYR) = ESUM5(ILYR)  + ENERGY
  113     CONTINUE                    ! end of loop over ip2
  112   CONTINUE                      ! end of loop over ie2

C- Find chi sq for energy sum around the central tower in this layer ...
        IF(ENTOWER(0,0,ILYR).GE.0.) THEN
          ESUM5(ILYR) = ESUM5(ILYR) + ESUM3(ILYR)
        END IF

  114   CONTINUE  ! go here if no cell (or zero energy cell) in this layer
  110 CONTINUE                        ! loop over 17 layer numbers ...

C- sum energy in towers ..............................
      DO 310 ILYR=1,17
C- If the energy sum for this layer is gt zero, then there are non0 energies
C- in this layer ...
        IF(ESUM5(ILYR).GT.0.) THEN
C- initialize the energy ...
          IF(ESUM5(18).LT.0.) THEN
            ESUM5(18) = 0.

            DO 312 IE2=ITTLO,ITTHI
              DO 322 IP2=ITTLO,ITTHI
                IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 322
                ENTOWER(IE2,IP2,18)  = 0.
  322         CONTINUE
  312       CONTINUE
          END IF
C- Now sum the energies and chi squareds ...
          ESUM5(18) = ESUM5(18) + ESUM5(ILYR)
          DO 313 IE2=ITTLO,ITTHI
            DO 323 IP2=ITTLO,ITTHI
              IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 323
              IF(ENTOWER(IE2,IP2,ILYR).GE.0.) THEN
                ENTOWER(IE2,IP2,18) =
     &            ENTOWER(IE2,IP2,18) + ENTOWER(IE2,IP2,ILYR)
              END IF
  323       CONTINUE                    ! loop over ip2
  313     CONTINUE                    ! loop over ie2
        END IF
  310 CONTINUE                        ! loop over layers
C----------------------------------------------------------------------
  999 RETURN
      END
