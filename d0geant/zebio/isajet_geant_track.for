      SUBROUTINE ISAJET_GEANT_TRACK(LISP1,NVTX,NTRAK,NTRAK1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PUT ISAJET TRACK ON GEANT STACK.
C-
C-   Inputs  : LISP1 = ISP1 LINK . Should be protected by caller.
C-             NVTX = Geant VTX Number
C-             NTRAK = TOTAL TRACK COUNT
C-             NTRAK1 = TRACK COUNT FOR THIS VERTEX
C-   Outputs :
C-   Controls:
C-
C-   Created  27-SEP-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBIO.INC'
C
      INTEGER LISP1,NVTX,NT,KPART
      INTEGER LISAQ,LISAJ,NPARTN,NJET
      REAL    UBUF(10),PP(4)
      INTEGER KPART2,NWBUF2,ITRTY2
      CHARACTER*20 NPARTN2
      REAL    AMASS2,CHARG2,TLIFE2,UBUF2(10)
      INTEGER I
      INTEGER NTRAK,NTRAK1
C
C----------------------------------------------------------------------
C  --limit maximum number of particles for each isajet vertex for debug...
C       (ZBIO 8=n)
      IF ( IZBOPT(6).LE.0 .OR. NTRAK1.LE.IZBOPT(6) ) THEN
        UBUF(1) = 0.                       ! end vertex
        UBUF(2) = 0.                       ! geant track number of
C                                          ! parent
        UBUF(3) = 1.                       ! vertex id
        UBUF(4) = FLOAT(NVTX)              ! vertex number
        UBUF(5) = FLOAT(NTRAK)             ! track number
        LISAQ = LQ(LISP1-2)                ! parent parton bank
        IF ( LISAQ.GT.0 ) THEN
          NPARTN = IQ(LISAQ-5)            ! parent parton number
        ELSE
          NPARTN = 0
        ENDIF
        LISAJ = LQ(LISP1-3)
        IF ( LISAJ.GT.0 ) THEN
          NJET = IQ(LISAJ-5)
        ELSE
          NJET = 0
        ENDIF
        UBUF(6) = NPARTN
        UBUF(7) = NJET
C
        PP(1) = Q(LISP1+2)                 ! px
        PP(2) = Q(LISP1+3)                 ! py
        PP(3) = Q(LISP1+4)                 ! pz
        PP(4) = SQRT(PP(1)**2+PP(2)**2+PP(3)**2+Q(LISP1+6)**2)   !
C                                        ! energy
C  convert ISAJET particle code to Geant code...
        CALL ISAGEA(IQ(LISP1+1),KPART)   ! convert particle code
C  --replace particle codes for debugging...
C       (ZBIO 7=n)
        IF ( IZBOPT(5).GT.0 ) THEN
          KPART = IZBOPT(5)
          CALL GEAISA(KPART,IQ(LISP1+1))         ! Change particle type
C          --recalculate energy...
          NWBUF2 = 0
          CALL GFPART(KPART2,NPARTN2,ITRTY2,AMASS2,CHARG2,TLIFE2,
     &       UBUF2,NWBUF2)
          Q(LISP1+6) = AMASS2          ! Change particle mass
          PP(4) = SQRT(PP(1)**2+PP(2)**2+PP(3)**2+AMASS2**2)
          Q(LISP1+5) = PP(4)           ! Change particle energy
        ENDIF
C  --reset momentum accoding to option 9 in ZBIO key card...
C       (ZBIO 9=n)
C               >0 -> momentum, <0 -> transverse momentum
        IF ( IZBOPT(7).NE.0 ) THEN
          IF ( IZBOPT(7).GT.0 ) THEN
            PP(4) = FLOAT(IZBOPT(7))/SQRT(PP(1)**2+PP(2)**2+PP(3)**2)
          ELSE
            PP(4) = FLOAT(IABS(IZBOPT(7)))/SQRT(PP(1)**2+PP(2)**2)
          ENDIF
          PP(1) = PP(1)*PP(4)
          PP(2) = PP(2)*PP(4)
          PP(3) = PP(3)*PP(4)
          NWBUF2 = 0
          CALL GFPART(KPART2,NPARTN2,ITRTY2,AMASS2,CHARG2,TLIFE2,
     &       UBUF2,NWBUF2)
          PP(4) = SQRT(PP(1)**2+PP(2)**2+PP(3)**2+AMASS2**2)
          Q(LISP1+2) = PP(1)            ! Reset momentum and energy
          Q(LISP1+3) = PP(2)
          Q(LISP1+4) = PP(3)
          Q(LISP1+5) = PP(4)
        ENDIF
C
        CALL GSKINE(PP,KPART,NVTX,UBUF,7,NT)
      ENDIF
      IQ(LISP1-5) = NTRAK                ! Renumber to Geant #'S
  999 RETURN
      END
