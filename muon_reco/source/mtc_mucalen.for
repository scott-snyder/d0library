      SUBROUTINE MTC_MUCALEN(VTX,DVTX,ETA,PHI)
C----------------------------------------------------------------------
C- MTC_MUCALEN: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Fill the blocks /mtc_etowers/ and /mtc_ehtowers/
C-      which contain the energies and chi squareds of the cells in the
C-      3x3 tower at the input ETA,PHI projected from the VTX
C-
C-   Inputs  : VTX(3)  - vertex coordinates
C-             DVTX(3) - uncertainty in vertex
C-             ETA,PHI
C-   Outputs : fills /MTC_ETOWERS/ and /MTC_EHTOWERS/
C-
C-   Created   9-DEC-1993   Elizabeth Gallas
C-   Modified 30-OCT-1995   EG - allow adjacent cells in the same
C-        layer to have a different module type than the module type
C-        of the central tower cell in that layer.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- Input vertex, eta, and phi ...
      REAL VTX(3),DVTX(3),ETA, PHI
C- block for energies, chi squareds ...
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
      INCLUDE 'D0$INC:MTC_AETOWERS.INC'
      INCLUDE 'D0$INC:MTC_EHTOWERS.INC'
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
      INTEGER IHERE, ISUB
      INTEGER IETA, IPHI, JPHI, ILYR
      INTEGER IE2, IP2, IETA2, IPHI2, IHERE2, ISUB2
      INTEGER ISETA,ICOUNT, ISETA2,ICOUNT2
      CHARACTER*4 ATUBE
      INTEGER IXYZ
C- functns ...
      INTEGER MTC_IWHERE, MTC_IWHLAYER,MTC_ISUBETA
C- need some constants ...
      REAL PI, TWOPI, HALFPI
      PARAMETER (PI=3.141593,TWOPI=6.283185,HALFPI=1.570796)
C----------------------------------------------------------------------
C- /MTC_CALSECTS/ contains arrays specifying all cal sections, sublayers and
C- eta ranges -- acaltype(10), isublayer(10), islnum(10,5), isleta(10,5)
      INCLUDE 'D0$INC:MTC_CALSECTS.INC'
      INCLUDE 'D0$INC:MTC_ACALSECTS.INC'
C- /MTC_MPVCOM/ contains most prob value in adc counts, and in GeV
      INCLUDE 'D0$INC:MTC_MPVCOM.INC'
C----------------------------------------------------------------------
C- put vertex position into /MTC_ETOWERS/
      DO 10 IXYZ=1,3
        VTXTWR(IXYZ) = VTX(IXYZ)
        DVTXTWR(IXYZ) = DVTX(IXYZ)
   10 CONTINUE
C----------------------------------------------------------------------
C- initialize the # of total possible layers, # of nonzero cal en layers
      ITOTCHI3 = 0
      ICNTCHI3 = 0
      FCNTCHI3 = 0.
C- hadronic stuff
      ESUMH3  = -2.
      EXPEH3  = -2.
      CHITH3  = -2.
      FCNTCH3 = 0.
      ICNTCH3 = 0
      ITOTCH3 = 0
C- zero tower chisquared ...
      DO 212 ILYR=1,18
        IETATOWER(ILYR) = -50
        IPHITOWER(ILYR) = -50
        ESUM3(ILYR) = -2.             ! energy sum in each layer
        CHIT3(ILYR) = -2.
        EXPE3(ILYR) = -2.
C- init tower energy,chisq,ident arrays ...
        DO 36 IE2=ITLO,ITHI
          DO 37 IP2=ITLO,ITHI
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

C- Return initialized values if zvtx=-5000. (no caep bank)
      IF(vtx(3).EQ.-5000.) return

C- Get the direction cosines DIRCIN(3) for the current track ETA,PHI
      CALL MTC_ETAPHICOS(ETA, PHI, DIRCIN)

C- Find all calor cells hit by a line ...
      CALL CLINPH(VTXTWR,DIRCIN,NCLMAX,NCELL,IETAC,IPHIC,LAYERC,ARGSOK)
      IF(ARGSOK.NE.0)
     &  WRITE(6,*) ' MTC_MUCALEN:  clinph failure flag !!!',ARGSOK,NCELL
      IF(NCELL.EQ.0) RETURN

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
                WRITE(6,*) ' MTC_MUCALEN: Too many cells in layer !!!'
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
        ITOTCHI3 = ITOTCHI3 + 1
        IF(ILYR.GE.11) ITOTCH3 = ITOTCH3 + 1

C- Get the sublayer number within this calorimeter section ...
        ISUB    = MTC_IWHLAYER(IHERE,ILYR)
C- Get the sublayer eta index ...
        ISETA   = MTC_ISUBETA(IHERE,ISUB,IETA)
C- Find the expected .3x.3 energy deposited ...
        IF(IHERE.NE.4) THEN
          EXPE3(ILYR) = GEVMPV3(IHERE,ISUB,ISETA)
        ELSE
          CALL MTC_ICD_TUBE(IETA,IPHI,ICOUNT,ATUBE)
          EXPE3(ILYR) = GEV_MPV_ICD3(ICOUNT)
        END IF

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

C- 12-APR-95            IF(IHERE2.EQ.0 .OR. IHERE2.NE.IHERE) GO TO 113
            IF(IHERE2.EQ.0) GO TO 113
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
            IF(energy.EQ.0.) ier_caep = -5
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

            ESUM3(ILYR) = ESUM3(ILYR)  + ENERGY
  113     CONTINUE                    ! end of loop over ip2
  112   CONTINUE                      ! end of loop over ie2

C- Find chi sq for energy sum around the central tower in this layer ...
        IF(ENTOWER(0,0,ILYR).GE.0.) THEN
          IF(ESUM3(ILYR).EQ.0.) THEN
            CHIT3(ILYR) = -1.
          ELSE
            ICNTCHI3 = ICNTCHI3 + 1
            IF(ILYR.GE.11) ICNTCH3 = ICNTCH3 + 1
            IF(IHERE.NE.4) THEN
              CHIT3(ILYR) =
     &          ( ESUM3(ILYR) - GEVMPV3(IHERE,ISUB,ISETA) )**2 /
     &          GEVMPV_W3(IHERE,ISUB,ISETA)
            ELSE
              CHIT3(ILYR) =
     &          ( ESUM3(ILYR) - GEV_MPV_ICD3(ICOUNT) )**2 /
     &          GEV_MPV_ICD_W3(ICOUNT)
            END IF
          END IF
        END IF

  114   CONTINUE  ! go here if no cell (or zero energy cell) in this layer
  110 CONTINUE                        ! loop over 17 layer numbers ...

C- sum expected energy in towers ......................................
C- in .3x.3 towers ...
      IF(EXPEH3.LT.0.) EXPEH3 = 0.
      IF(EXPE3(18).LT.0.) EXPE3(18) = 0.
      DO 309 ILYR=1,17
        IF(EXPE3(ILYR).GE.0.) THEN
          EXPE3(18) = EXPE3(18) + EXPE3(ILYR)
          IF(ILYR.GE.11) EXPEH3 = EXPEH3 + EXPE3(ILYR)
        END IF
  309 CONTINUE
C- in .1x.1 towers ...
      DO 308 IE2=ITLO,ITHI
        DO 308 IP2=ITLO,ITHI
          EXPE(IE2,IP2,18) = 0.
          EXPEH(IE2,IP2) = 0.
  308 CONTINUE
      DO 306 ILYR=1,17
        DO 307 IE2=ITLO,ITHI
          DO 307 IP2=ITLO,ITHI
            IF(EXPE(IE2,IP2,ILYR).GE.0.) THEN
              EXPE(IE2,IP2,18) = EXPE(IE2,IP2,18) + EXPE(IE2,IP2,ILYR)
              IF(ILYR.GT.11)
     &            EXPEH(IE2,IP2) = EXPEH(IE2,IP2) + EXPE(IE2,IP2,ILYR)
            END IF
  307   CONTINUE
  306 CONTINUE

C- sum energy and chi squareds in towers ..............................
      DO 310 ILYR=1,17
C- If the energy sum for this layer is gt zero, then there are energies
C- and chi squareds in this layer ...
        IF(ESUM3(ILYR).GT.0.) THEN
C- initialize the energy and chisquared sums ...
          IF(ESUM3(18).LT.0.) THEN
            ESUM3(18) = 0.
            IF(CHIT3(18).LT.0.) CHIT3(18) = 0.
            IF(ESUMH3.LT.0.) ESUMH3 = 0.
            IF(CHITH3.LT.0.) CHITH3 = 0.
            DO 312 IE2=ITLO,ITHI
              DO 312 IP2=ITLO,ITHI
                ENTOWER(IE2,IP2,18)  = 0.
                CHITOWER(IE2,IP2,18) = 0.
                ENTH(IE2,IP2)  = 0.
                CHITH(IE2,IP2) = 0.
  312       CONTINUE
          END IF
C- Now sum the energies and chi squareds ...
          ESUM3(18) = ESUM3(18) + ESUM3(ILYR)
          CHIT3(18) = CHIT3(18) + CHIT3(ILYR)
          IF(ILYR.GE.11) THEN
            ESUMH3 = ESUMH3 + ESUM3(ILYR)
            CHITH3 = CHITH3 + CHIT3(ILYR)
          END IF
          DO 313 IE2=ITLO,ITHI
            DO 313 IP2=ITLO,ITHI
              IF(ENTOWER(IE2,IP2,ILYR).GE.0.) ENTOWER(IE2,IP2,18) =
     &            ENTOWER(IE2,IP2,18) + ENTOWER(IE2,IP2,ILYR)
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
  313     CONTINUE                    ! loop over 3x3 cells about tower
        END IF
  310 CONTINUE                        ! loop over layers

C- Divide the total chisquareds by the number of layers hit ...
C- and calculate the fraction of layers hit ...
C- 1x1
      DO 314 IE2=ITLO,ITHI
        DO 314 IP2=ITLO,ITHI
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
  314 CONTINUE
C- 3x3
      IF(ICNTCHI3.GT.0) THEN
        CHIT3(18) = CHIT3(18) / FLOAT(ICNTCHI3)
        FCNTCHI3  = FLOAT(ICNTCHI3) / ITOTCHI3
        IF(ICNTCH3.NE.0) THEN
          CHITH3    = CHITH3 / FLOAT(ICNTCH3)
          FCNTCH3   = FLOAT(ICNTCH3) / ITOTCH3
        ELSE
          CHITH3    = -1.
          FCNTCH3   = 0.
        END IF
      ELSE
        CHIT3(18) = -1.
        FCNTCHI3  = 0.
        CHITH3    = -1.
        FCNTCH3   = 0.
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
