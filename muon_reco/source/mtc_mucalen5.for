      SUBROUTINE MTC_MUCALEN5()
C----------------------------------------------------------------------
C- MTC_MUCALEN5: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Fill the 2nd nearest neighbor
C-      components of the energy and chisquared arrays in
C-      blocks /MTC_ETOWERS/ and /MTC_EHTOWERS/.
C-      These are the energies and chi squareds of the OUTERMOST cells
C-      in the 5x5 tower about the input ETA,PHI projected from the VTX.
C-      You must call MTC_MUCALEN before calling this routine.
C-
C-   Outputs : fills IETA=+-2 or IPHI=+-2 elements of energy and
C-      chi squared arrays in /MTC_ETOWERS/ and /MTC_EHTOWERS/
C-
C-   Created   17-DEC-1993      Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

C- block for energies, chi squareds ...
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
      INCLUDE 'D0$INC:MTC_AETOWERS.INC'
      INCLUDE 'D0$INC:MTC_EHTOWERS.INC'
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
      INTEGER ISETA,ICOUNT, ISETA2,ICOUNT2
      CHARACTER*4 ATUBE
      INTEGER IXYZ
C- functns ...
      INTEGER MTC_IWHERE, MTC_IWHLAYER,MTC_ISUBETA
C----------------------------------------------------------------------
C- /MTC_CALSECTS/ contains arrays specifying all cal sections, sublayers and
C- eta ranges -- acaltype(10), isublayer(10), islnum(10,5), isleta(10,5)
      INCLUDE 'D0$INC:MTC_CALSECTS.INC'
      INCLUDE 'D0$INC:MTC_ACALSECTS.INC'
C- /MTC_MPVCOM/ contains most prob value in adc counts, and in GeV
      INCLUDE 'D0$INC:MTC_MPVCOM.INC'
C----------------------------------------------------------------------
C- initialize the # of total possible layers, # of nonzero cal en layers
      ESUMT5   = -2.
      ESUMTH5  = -2.
      ITOTCHI5 = ITOTCHI3
      ICNTCHI5 = 0
      FCNTCHI5 = 0.
C- hadronic stuff
      ESUMH5  = -2.
      CHITH5  = -2.
      ITOTCH5 = ITOTCH3
      ICNTCH5 = 0
      FCNTCH5 = 0.
C- zero tower chisquared ...
      DO 212 ILYR=1,18
        ESUM5(ILYR) = -2.             ! energy sum in each layer
        CHIT5(ILYR) = -2.
C- init tower energy,chisq,ident arrays ...
        DO 36 IE2=ITTLO,ITTHI
          DO 37 IP2=ITTLO,ITTHI
            IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 37
            ENTOWER(IE2,IP2,ILYR)  = -2.
            CHITOWER(IE2,IP2,ILYR) = -2.
            EXPE(IE2,IP2,ILYR) = -2.
            ATOWER(IE2,IP2,ILYR)   = '--------'
            IF(ILYR.EQ.18) THEN
              ATOWER(IE2,IP2,ILYR)   = 'TOTAL-->'
              ITOTCHI(IE2,IP2) = 0
              ICNTCHI(IE2,IP2) = 0
              FCNTCHI(IE2,IP2) = 0.
C- hadronic stuff
              ENTH(IE2,IP2)    = -2.
              CHITH(IE2,IP2)   = -2.
              EXPEH(IE2,IP2)   = -2.
              FCNTCH(IE2,IP2)  = 0.
              ICNTCH(IE2,IP2)  = 0
              ITOTCH(IE2,IP2)  = 0
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
C- Get the sublayer eta index ...
        ISETA   = MTC_ISUBETA(IHERE,ISUB,IETA)
        IF(IHERE.NE.4) THEN
          ICOUNT = -1000
        ELSE
          CALL MTC_ICD_TUBE(IETA,IPHI,ICOUNT,ATUBE)
        END IF

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
            ITOTCHI(IE2,IP2) = ITOTCHI(IE2,IP2) + 1
            IF(ILYR.GE.11) ITOTCH(IE2,IP2) = ITOTCH(IE2,IP2) + 1
C- Get the sublayer number within this calorimeter section ...
            ISUB2  = MTC_IWHLAYER(IHERE2,ILYR)
C- Get the sublayer eta index ...
            ISETA2 = MTC_ISUBETA(IHERE2,ISUB2,IETA2)
            WRITE(ATOWER(IE2,IP2,ILYR),201)
     &          ACALTYPE(IHERE2),ISUB2,IETA2
  201       FORMAT(A4,I1,I3)
C- Get the expected energies ...
            IF(IHERE2.NE.4) THEN
              EXPE(IE2,IP2,ILYR) = GEVMPV(IHERE2,ISUB2,ISETA2)
            ELSE
              CALL MTC_ICD_TUBE(IETA2,IPHI2,ICOUNT2,ATUBE)
              EXPE(IE2,IP2,ILYR) = GEV_MPV_ICD(ICOUNT2)
            END IF
C- if ier_caep =0 --> we have a pad energy, ier_caep=-5 --> pad energy 0.
            IF(energy.eq.0.) ier_caep = -5
            IF(IER_CAEP.EQ.0) THEN
              IF(ENERGY.GT.0.) THEN
                ICNTCHI(IE2,IP2) = ICNTCHI(IE2,IP2) + 1
                IF(ILYR.GE.11) ICNTCH(IE2,IP2) = ICNTCH(IE2,IP2) + 1
              END IF
              ENTOWER(IE2,IP2,ILYR) = ENERGY
C- Get the chi squared
              IF(IHERE2.NE.4) THEN
                CHITOWER(IE2,IP2,ILYR) = ( ENTOWER(IE2,IP2,ILYR) -
     &              GEVMPV(IHERE2,ISUB2,ISETA2) )**2 /
     &              GEVMPV_W(IHERE2,ISUB2,ISETA2)
              ELSE
                CHITOWER(IE2,IP2,ILYR) = ( ENTOWER(IE2,IP2,ILYR) -
     &              GEV_MPV_ICD(ICOUNT2) )**2 / GEV_MPV_ICD_W(ICOUNT2)
              END IF
            ELSE
              ENERGY = 0.
              IF(IER_CAEP.EQ.-5) THEN
                ENTOWER(IE2,IP2,ILYR) = 0.
                CHITOWER(IE2,IP2,ILYR) = -1.
              ELSE
                ENTOWER(IE2,IP2,ILYR) = -1.
                CHITOWER(IE2,IP2,ILYR) = -1.
                IF(IER_CAEP.EQ.-1) WRITE(6,*) 'Reached end of CAEP bank'
                IF(IER_CAEP.EQ.-2) WRITE(6,*) 'No CAEP bank address'
                IF(IER_CAEP.EQ.-3) WRITE(6,*) 'IETA,IPHI out of bounds '
                IF(IER_CAEP.EQ.-4) WRITE(6,*) 'ILYR 4DEAD ENER notcoded'
                GO TO 113
              END IF
            END IF

            ESUM5(ILYR) = ESUM5(ILYR)  + ENERGY
  113     CONTINUE                    ! end of loop over ip2
  112   CONTINUE                      ! end of loop over ie2

C- Find chi sq for energy sum around the central tower in this layer ...
        IF(ENTOWER(0,0,ILYR).GE.0.) THEN
          ESUM5(ILYR) = ESUM5(ILYR) + ESUM3(ILYR)
          IF(ESUM5(ILYR).EQ.0.) THEN
            CHIT5(ILYR) = -1.
          ELSE
            ICNTCHI5 = ICNTCHI5 + 1
            IF(ILYR.GE.11) ICNTCH5 = ICNTCH5 + 1
            IF(IHERE.NE.4) THEN
              CHIT5(ILYR) =
     &          ( ESUM5(ILYR) - GEVMPV3(IHERE,ISUB,ISETA) )**2 /
     &          GEVMPV_W3(IHERE,ISUB,ISETA)
            ELSE
              CHIT5(ILYR) =
     &          ( ESUM5(ILYR) - GEV_MPV_ICD3(ICOUNT) )**2 /
     &          GEV_MPV_ICD_W3(ICOUNT)
            END IF
          END IF
        END IF

  114   CONTINUE  ! go here if no cell (or zero energy cell) in this layer
  110 CONTINUE                        ! loop over 17 layer numbers ...

C- sum expected energy in towers ......................................
C- in .1x.1 towers ...
      DO 308 IE2=ITTLO,ITTHI
        DO 328 IP2=ITTLO,ITTHI
          IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 328
          EXPE(IE2,IP2,18) = 0.
          EXPEH(IE2,IP2) = 0.
  328   CONTINUE
  308 CONTINUE
      DO 306 ILYR=1,17
        DO 307 IE2=ITTLO,ITTHI
          DO 327 IP2=ITTLO,ITTHI
            IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 327
            IF(EXPE(IE2,IP2,ILYR).GE.0.) THEN
              EXPE(IE2,IP2,18) = EXPE(IE2,IP2,18) + EXPE(IE2,IP2,ILYR)
              IF(ILYR.GT.11)
     &            EXPEH(IE2,IP2) = EXPEH(IE2,IP2) + EXPE(IE2,IP2,ILYR)
            END IF
  327     CONTINUE
  307   CONTINUE
  306 CONTINUE

C- sum energy and chi squareds in towers ..............................
      DO 310 ILYR=1,17
C- If the energy sum for this layer is gt zero, then there are energies
C- and chi squareds in this layer ...
        IF(ESUM5(ILYR).GT.0.) THEN
C- initialize the energy and chisquared sums ...
          IF(ESUM5(18).LT.0.) THEN
            ESUM5(18) = 0.

            IF(ESUMT5.LT.0.) ESUMT5 = 0.
            IF(ILYR.GE.11 .AND. ESUMTH5.LT.0.) ESUMTH5 = 0.

            IF(CHIT5(18).LT.0.) CHIT5(18) = 0.
            IF(ESUMH5.LT.0.) ESUMH5 = 0.
            IF(CHITH5.LT.0.) CHITH5 = 0.
            DO 312 IE2=ITTLO,ITTHI
              DO 322 IP2=ITTLO,ITTHI
                IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 322
                ENTOWER(IE2,IP2,18)  = 0.
                CHITOWER(IE2,IP2,18) = 0.
                ENTH(IE2,IP2)  = 0.
                CHITH(IE2,IP2) = 0.
  322         CONTINUE
  312       CONTINUE
          END IF
C- Now sum the energies and chi squareds ...
          ESUM5(18) = ESUM5(18) + ESUM5(ILYR)
          CHIT5(18) = CHIT5(18) + CHIT5(ILYR)
          IF(ILYR.GE.11) THEN
            ESUMH5 = ESUMH5 + ESUM5(ILYR)
            CHITH5 = CHITH5 + CHIT5(ILYR)
          END IF
          DO 313 IE2=ITTLO,ITTHI
            DO 323 IP2=ITTLO,ITTHI
              IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 323
              IF(ENTOWER(IE2,IP2,ILYR).GE.0.) THEN
                ENTOWER(IE2,IP2,18) =
     &            ENTOWER(IE2,IP2,18) + ENTOWER(IE2,IP2,ILYR)
                ESUMT5 = ESUMT5 + ENTOWER(IE2,IP2,ILYR)
                IF(ILYR.GE.11) ESUMTH5 = ESUMTH5 + ENTOWER(IE2,IP2,ILYR)
              END IF
              IF(CHITOWER(IE2,IP2,ILYR).GE.0.) THEN
                CHITOWER(IE2,IP2,18) =
     &              CHITOWER(IE2,IP2,18) + CHITOWER(IE2,IP2,ILYR)
                IF(ILYR.GE.11) THEN
                  CHITH(IE2,IP2) =
     &              CHITH(IE2,IP2) + CHITOWER(IE2,IP2,ILYR)
                  ENTH(IE2,IP2) =
     &              ENTH(IE2,IP2) + ENTOWER(IE2,IP2,ILYR)
                END IF
              END IF
  323       CONTINUE                    ! loop over ip2
  313     CONTINUE                    ! loop over ie2
        END IF
  310 CONTINUE                        ! loop over layers

C- Divide the total chisquareds by the number of layers hit ...
C- and calculate the fraction of layers hit ...
C- 1x1
      DO 314 IE2=ITTLO,ITTHI
        DO 324 IP2=ITTLO,ITTHI
          IF(ABS(IE2).NE.2 .AND. ABS(IP2).NE.2) GO TO 324
          IF(ICNTCHI(IE2,IP2).GT.0) THEN
            CHITOWER(IE2,IP2,18) =
     &        CHITOWER(IE2,IP2,18) / FLOAT(ICNTCHI(IE2,IP2))
            FCNTCHI(IE2,IP2) =
     &        FLOAT(ICNTCHI(IE2,IP2)) / FLOAT(ITOTCHI(IE2,IP2))
            IF(ICNTCH(IE2,IP2).GT.0) CHITH(IE2,IP2) =
     &        CHITH(IE2,IP2) / FLOAT(ICNTCH(IE2,IP2))
            IF(ITOTCH(IE2,IP2).GT.0) FCNTCH(IE2,IP2) =
     &        FLOAT(ICNTCH(IE2,IP2)) / FLOAT(ITOTCH(IE2,IP2))
          ELSE
            CHITOWER(IE2,IP2,18) = -1.
            FCNTCHI(IE2,IP2) = 0.
            CHITH(IE2,IP2)   = -1.
            FCNTCH(IE2,IP2)  = 0.
          END IF
  324   CONTINUE
  314 CONTINUE
C- 5x5
      IF(ICNTCHI5.GT.0) THEN
        CHIT5(18) = CHIT5(18) / FLOAT(ICNTCHI5)
        FCNTCHI5  = FLOAT(ICNTCHI5) / ITOTCHI5
        IF(ICNTCH5.NE.0) THEN
          CHITH5    = CHITH5 / FLOAT(ICNTCH5)
          FCNTCH5   = FLOAT(ICNTCH5) / ITOTCH5
        ELSE
          CHITH5    = -1.
          FCNTCH5   = 0.
        END IF
      ELSE
        CHIT5(18) = -1.
        FCNTCHI5  = 0.
        CHITH5    = -1.
        FCNTCH5   = 0.
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
