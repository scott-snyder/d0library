      INTEGER FUNCTION JDEPTH(I)
C-------------------------------------------------------------------------
C
C     THIS FUNCTION GIVES THE LAYER REGION CODE AS A FUNCTION OF DEPTH
C     NUMBER.  IT IS DEPENDENT ON THE GEOMETRY TYPE BEING CONSIDERED
C     (STANDARD D0 OR TEST BEAM).  IF I IS NEGATIVE THE TOTAL NUMBER OF
C     DEPTHS IS RETURNED.  NOTE THAT IT SENSES WHETHER IT IS 
C     IN THE CENTRAL OR END REGIONS BY WHATEVER REGION BANK WAS LAST
C     REFERRED TO (IE THAT WHICH IS POINTED TO BY LQCREG).  IT SENSES
C     THE ETA NUMBER BY WHATEVER LQCTOW IS CURRENTLY POINTING TO.
C     *** IN THIS VERSION THE DEPTH NUMBERS CORRESPOND TO "PHYSICS 
C         VARIABLES" ***
C
C     INPUT:    I      .GE. 0  => DEPTH NUMBER 
C                      .EQ. -1 => TOTAL NUMBER OF DEPTHS
C                      .EQ. -2 => NUMBER OF EM DEPTHS
C                      .EQ. -3 => NUMBER OF MG DEPTHS
C                      .EQ. -4 => NUMBER OF FH DEPTHS
C                      .EQ. -5 => NUMBER OF CH DEPTHS
C                      .EQ. -6 => NUMBER OF CC DEPTHS
C                      .EQ. -7 => NUMBER OF EC DEPTHS
C               LQCETA
C
C     AUTHOR:   S KAHN      2 JULY 1987
C     REVISIONS:   S KAHN  29 NOV 1988   COMPATIBILITY WITH NEW TOWER SCHEME
C                                        ADDRESSED BY PHYSICS VARIABLES
C
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CGHEAD.DEF'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER I, IETA
C------------------------------------------------------------------------
C     NOTE THAT:
C                  IETA = ETA(PHYSICS) 
C                  I    = DEPTH(PHYSICS)
C------------------------------------------------------------------------
      IETA = IC(LQCETA + IGIETA)
      JDEPTH = 0        ! initialize to zero for illegal cells
C
C     TEST BEAM CASE
C
      IF(IC(LCGEH+IGSTAT) .EQ. IHTSBM) THEN   ! type = test beam
              WRITE (6, *) ' JDEPTH: TEST BEAM CASE NOT CODED'
              STOP 921
      ELSE
C      
C     D0 STANDARD CASE  -- CENTRAL CALORIMETER
C
CS fix   IF(IC(LCGEH+IGSTAT) .EQ. IHSTRD ) THEN         ! D0 standard 
C
          IF( I.EQ.MNLYEM .OR. I.EQ.MNLYEM+1 ) THEN        ! D1, D2
            IF(IETA .LE. 12) THEN              ! CC-EM
              JDEPTH = ICCEML + (I-1)*ICLINC
            ELSE IF( IETA .GE. 15) THEN        ! EC-EM
              JDEPTH = ICEC1L + (I-1)*IELINC
            END IF
          ELSE IF (I.GE.LYEM3A .AND. I.LE.LYEM3D) THEN ! D3,..,D6 third
C                                        ! layer
            IF(IETA .LE. 12) THEN              ! CC-EM
              JDEPTH = ICCEML + 2*ICLINC
              IF(IETA .EQ. 12 .AND. (I .EQ. LYEM3C .OR. I .EQ. LYEM3D))
     &          JDEPTH = 0                     ! special case for last CC
            ELSE IF (IETA .GE. 14 .AND. IETA .LE. 35) THEN        ! EC-EM
              JDEPTH = ICEC3L
              IF(IETA .EQ. 14 .AND. (I .EQ. LYEM3A .OR. I .EQ. LYEM3B))
     +          JDEPTH = 0                     ! special case for 1st EC
              IF( IETA .GE. 27 .AND. I .NE. LYEM3A ) JDEPTH = 0 ! high
C                                        ! eta -- change of scale
            END IF
          ELSE IF (I .EQ. MXLYEM) THEN              ! D7 fourth EM layer
            IF (IETA .LE. 11) THEN
              JDEPTH = ICCEML + 3*ICLINC       ! in CC
            ELSE IF (IETA. GE. 14) THEN
              JDEPTH = ICEC4L                  ! in EC
            END IF
          ELSE IF (I.EQ.LYICD) THEN            ! ICD
            IF (IETA .GE. 9 .AND. IETA .LE. 14) THEN
              JDEPTH = ICSCIN + (IETA-9)*ICSINC
            ELSE
              JDEPTH = 0
            END IF
          ELSE IF (I .EQ. MNLYMG) THEN         ! CC_MG
            IF (IETA .GE. 8 .AND. IETA .LE. 12) THEN
              JDEPTH = ICCMG
            ELSE
              JDEPTH = 0
            END IF
          ELSE IF ( I.EQ.MXLYMG) THEN   ! EC massless gaps 
            IF (IETA .GE. 11 .AND. IETA .LE. 13 ) THEN
              JDEPTH = ICMHMG
            ELSE IF (IETA .GE. 8 .AND. IETA .LE. 10 ) THEN
              JDEPTH = ICOHMG
            ELSE
              JDEPTH = 0
            END IF
          ELSE IF (I.GE.MNLYFH .AND. I.LE.MXLYFH) THEN ! FH layers
            IF (IETA .LE. 21-I ) THEN
              JDEPTH = ICCFHL + (I-11)*ICLINC   ! in CC
            ELSE IF (I .LE. MXLYFH-2 .AND. IETA .GE. I .AND. IETA .LE.
     &        I+5) THEN                  ! 1st, 2nd, R.O. in MFH
              JDEPTH = ICMFHL + 2*(I-MNLYFH) * ICLINC
            ELSE IF (I .GE. MXLYFH-1 .AND. IETA .GE.13 .AND. IETA .LE. 
     &        I+4)   THEN                       ! 3rd, 4th R.O. in MFH
              JDEPTH = ICMFHL + 2*(I-MNLYFH) * ICLINC
            ELSE IF (IETA .GE. I+6 .AND. IETA .LE. 37) THEN
              JDEPTH = ICIFHL + 2*(I-MNLYFH)*ICLINC    ! IFH
            ELSE
              JDEPTH = 0                       ! non defined
            END IF
          ELSE IF (I .GE. MNLYCH) THEN             ! CH layers
            IF (IETA .LE. 7 .AND. I.EQ.MNLYCH) THEN
              JDEPTH = ICCCHL                  ! in CC
            ELSE IF (I.EQ.MNLYCH .AND. IETA.GE.8 .AND. IETA.LE.12) THEN
              JDEPTH = ICOCHL                  ! 1st R.O. in OCH
            ELSE IF(I.EQ.MNLYCH+1 .AND. IETA.GE.9 .AND. IETA.LE.13)THEN
              JDEPTH = ICOCHL + 4*ICLINC       ! 2nd R.O. in OCH
            ELSE IF(I.EQ.MNLYCH+2 .AND. IETA.GE.11 .AND. IETA.LE.15)THEN
C                      note that IETA=15 is included here.  This cell 
C                      will later be ganged to the MH cell.
              JDEPTH = ICOCHL + 8*ICLINC       ! 3rd R.O. in OCH
            ELSE IF(I.EQ.MNLYCH .AND. IETA.GE.14 .AND. IETA.LE.20) THEN
              JDEPTH = ICMCHL                  ! MCH
            ELSE IF(I.EQ.MNLYCH .AND. IETA.GE.21 .AND. IETA.LE.37) THEN
              JDEPTH = ICICHL                  ! ICH
            ELSE
              JDEPTH = 0                       ! not defined
            END IF
          ELSE IF (I .EQ. -1) THEN             ! total number of depths
              JDEPTH = 16
          ELSE IF (I .EQ. -2) THEN             ! number of EM depths
            IF (IETA .LE.11) THEN
              JDEPTH = 7
            ELSE IF (IETA .EQ. 12) THEN
              JDEPTH = 4
            ELSE IF (IETA .EQ. 14) THEN
              JDEPTH = 3
            ELSE IF (IETA .GE. 15 .AND. IETA .LE. 26) THEN
              JDEPTH = 7
            ELSE IF (IETA .GE. 27 .AND. IETA .LE. 35) THEN
              JDEPTH = 4
            ELSE
              JDEPTH = 0
            END IF
          ELSE IF (I .EQ. -3) THEN             ! number of MG depths
              JDEPTH = 0
          ELSE IF (I .EQ. -4) THEN             ! number of FH depths
            IF (IETA .LE. 8) THEN
              JDEPTH = 3
            ELSE IF (IETA.EQ.9 .OR. IETA.EQ.12) THEN
              JDEPTH = 2
            ELSE IF (IETA.EQ.11 .OR. IETA.EQ.10) THEN
              JDEPTH = 1
            ELSE IF (IETA.GE.13 .AND. IETA .LE. 37 .AND. IETA .NE.18
     +        .AND. IETA .NE. 17) THEN
              JDEPTH =4
            ELSE IF (IETA.EQ. 17 .OR. IETA .EQ. 18) THEN
              JDEPTH = 3
            ELSE 
              JDEPTH = 0
            ENDIF
          ELSE IF (I .EQ. -5) THEN             ! number of CH depths
            IF ((IETA .LE. 8 .AND. IETA .NE.7) .OR. IETA.GE.15) THEN
              JDEPTH = 1
            ELSE IF (IETA .EQ. 9 .OR. IETA.EQ.10) THEN
              JDEPTH = 2
            ELSE IF (IETA .GE. 13 .OR. IETA .LE. 4) THEN
              JDEPTH = 2
            ELSE IF (IETA .EQ.11 .OR. IETA.EQ.12) THEN
              JDEPTH = 3
            ELSE
              JDEPTH = 0
            END IF
          ELSE IF (I.EQ.-6) THEN              ! number of CC depths
            IF (IETA.LE.6) THEN
              JDEPTH = 11
            ELSE IF (IETA .EQ. 7) THEN
              JDEPTH = 10
            ELSE IF (IETA .GE. 8 .AND. IETA .LE.11) THEN
              JDEPTH = 18 - IETA
            ELSE IF (IETA .EQ.12) THEN
              JDEPTH = 4
            ELSE 
              JDEPTH = 0
            END IF
          ELSE IF (I.EQ.-7) THEN              ! number of EC depths
            IF (IETA .LE. 7) THEN
              JDEPTH = 0
            ELSE IF (IETA .EQ. 8) THEN
              JDEPTH = 1
            ELSE IF (IETA .EQ. 9 .OR. IETA .EQ. 10) THEN
              JDEPTH = 2
            ELSE IF (IETA .GE. 11 .AND. IETA .LE. 13) THEN
              JDEPTH = IETA - 7
            ELSE IF (IETA .EQ. 14) THEN
              JDEPTH = 9
            ELSE IF (IETA .GE. 15 .AND. IETA .LE. 26 .AND. IETA.NE.18
     &        .AND. IETA.NE.19) THEN
              JDEPTH = 12
            ELSE IF (IETA .GE. 27 .AND. IETA .LE. 35) THEN
              JDEPTH = 9
            ELSE IF (IETA .EQ. 36 .OR. IETA .EQ. 37) THEN
              JDEPTH = 5
            ELSE 
              JDEPTH = 0
            END IF
          ELSE
              WRITE(6,*) ' JDEPTH ERROR ', I
              STOP 922
          END IF
      END IF
C
      RETURN
      END

