      SUBROUTINE ISAEFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     fill all ISAJET event banks
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-   Updated  22-AUG-1991   Marc Paterno  Save PJSET into PJSET_AUX, and back
C-                                        again at the end, in order to
C-                                        stop failure of multiple
C-                                        evolution/fragmentation in ISAJET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:IDRUN.INC'
      INCLUDE 'D0$INC:FINAL.INC'
      INCLUDE 'D0$INC:JETPAR.INC'
      INCLUDE 'D0$INC:JETSET.INC'
      INCLUDE 'D0$INC:KEYS.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INCLUDE 'D0$INC:ISABNK.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:SEED.INC'
C
      INTEGER  QREF(MXJSET),PQREF(5)
      INTEGER  I,K,LISAE,IOHEAD,IOISAE,IUH,LISV1,GZISV1
      INTEGER  NPART,NVERTX,NQS,NQF,NLEP,NJT,NREAC,NPJET
      INTEGER  LISAJ,LISAQ,GZISAQ,GZISAJ,LPJHD,GZPJHD
      INTEGER  ISA_RUNNO
      LOGICAL  QPART,QCAL,QLEP
      LOGICAL  FIRST
      REAL*8   DSEED
      REAL     SSEED(2)
      EQUIVALENCE (DSEED,SSEED)
      LOGICAL  EDIT,YES
      REAL    CROSS_SECTION,WEIGHT
      LOGICAL DOIT
      INTEGER NJ_AUX
      REAL     PJSET_AUX(5,MXJSET)
      SAVE NJ_AUX,PJSET_AUX
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      CALL MZWIPE(0)      ! make sure division is wiped before starting
 
      IF(.NOT.EDIT(I)) GOTO 999   ! quit if don't wish to write event
 
      IF(FIRST) THEN              ! only for first event
        CALL MZFORM('ISAE','10I 6F 2D',IOISAE)
        FIRST=.FALSE.
      ENDIF
C
      DO I = 1, 10
        IF(KEYS(I)) NREAC=I
      ENDDO                             ! I = 1, 10
C
C ****  Create HEAD bank; identify this as an isajet event record
C
      CALL BKHEAD                       ! book HEAD bank
 
      IQ(LHEAD+1)=1005
      CALL UCTOH('ISAJ',IUH,4,4)
      IQ(LHEAD+2)=IUH
      CALL UCTOH('EVNT',IUH,4,4)
      IQ(LHEAD+3)=IUH
      IQ(LHEAD+6)=ISA_RUNNO()
      IQ(LHEAD+9)=IEVT
      IQ(LHEAD+10)=NREAC
      IQ(LHEAD+14)=1
      READ(XSEED,'(E24.15)') DSEED
C
C ****  Create ZEBRA bank ISAE (main supporting bank)
C
      CALL MZBOOK(IXMAIN,LISAE,LHEAD,-IZISAE,
     $            'ISAE',9,9,18,IOISAE,-1)
      Q(LISAE+11)=SIGF*1000.        ! cross section in microbarns
      Q(LISAE+12)=Q(LISAE+11)/NEVENT  ! weight
      Q(LISAE+13)=QSQ               ! effective q**2
      Q(LISAE+14)=SHAT              ! hard scattering invariant s
      Q(LISAE+15)=THAT              !  "      "          "      t
      Q(LISAE+16)=UHAT              !  "      "          "      u
      Q(LISAE+17)=SSEED(1)          !  part 1 of SEED
      Q(LISAE+18)=SSEED(2)          !  part 2 of SEED
      IQ(LISAE+1)=IDG(1)            ! event id
      IQ(LISAE+2)=IDG(2)            !   "
      IQ(LISAE+3)=IEVT              ! event number
      IQ(LISAE+4)=NREAC             ! reaction type
      NPART=0
      NVERTX=0
      NLEP=0
      NPJET=0
C
C ****  Copy the PJSET array into PJSET_AUX
C
      NJ_AUX=NJSET
      CALL UCOPY (PJSET, PJSET_AUX, 5*NJSET)
C
C ****  Recalculate jet and parton momenta starting from particles
C
      CALL QRECAL
C
C ****  Create ZEBRA banks for primary partons
C
      CALL ISAJFL
C
C ****  Create ZEBRA banks ISAQ (initial and final partons)
C
      CALL ISAQFL
C
C ****  Create ISP1 ZEBRA banks (stable particles) and
C ****  ISV1 ZEBRA banks (short lived vertices)
C
      CALL ISAPFL(NPART,NVERTX)
C
C ****  Create ISAC ZEBRA bank (pseudocalorimeter) if requested
C
      CALL ISLBNK(QPART,QCAL,QLEP)    ! find banks requested
      IF(QCAL) CALL ISACFL
C
C ****  Create ISAL ZEBRA banks (leptons) if requested
C
      IF(QLEP) CALL ISALFL(NLEP)
C
      IF(.NOT.QPART) THEN        ! drop particle and vertex banks
        LISV1=GZISV1()
        CALL MZDROP(IXCOM,LISV1,'L')
        NPART=0
        NVERTX=0
      ENDIF
      CALL PJETFL
      LPJHD=GZPJHD()
      NPJET=IQ(LPJHD+3)
C
C ****  Fill rest of ISAE
C
      CALL ISNUMQ(NJT,NQS)      ! find number of primary and secondary partons
C
      LISAE=LQ(LHEAD-IZISAE)
      IQ(LISAE+5)=NJT               ! number of primary parton banks
      IQ(LISAE+6)=NQS               !  "     of stable parton banks
      IQ(LISAE+7)=NPJET             !  "     of PJET banks
      IQ(LISAE+8)=NPART             !  "     of particle banks
      IQ(LISAE+9)=NVERTX            !  "     of vertex banks
      IQ(LISAE+10)=NLEP             !  "     of lepton banks
      CALL ISAE_ESUM                ! fill ESUM bank
C
C       get weight and cross section directly if external to ISAJET
      CALL ISA_PARTONS_WEIGHT(CROSS_SECTION,WEIGHT,DOIT)
      IF ( DOIT ) THEN
        LISAE=LQ(LHEAD-IZISAE)
        Q(LISAE+11)=CROSS_SECTION
        Q(LISAE+12)=WEIGHT
      ENDIF
C
C ****  Copy the PJSET_AUX array back to PJSET
C
      NJSET=NJ_AUX
      CALL UCOPY (PJSET_AUX, PJSET, 5*NJSET)
 
  999 RETURN
      END
