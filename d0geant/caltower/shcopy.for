      SUBROUTINE SHCOPY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieves shower information from RZ shower
C-   library into /ZEBSHL/, chooses one shower, scales, smears hits, and
C-   calls DHSTOR to copy into calorimeter working banks
C-
C    Called by STPCAL if there is a hit in the calorimeter
C    and SHWG=2 i.e. keyed access file is in use
C
C-   23-FEB-1989   John Womersley
C-   Updated  11-OCT-1989   John Womersley  now use track parameters at
C-                              calorimeter entry point 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER I

      INCLUDE 'D0$INC:pi.def/list'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCKING.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSHL.INC/LIST'            ! shower library zebcom
      INCLUDE 'D0$LINKS:IZSHHD.LINK/LIST'         ! links
      INCLUDE 'D0$LINKS:IZSHLB.LINK/LIST'         ! links
      INCLUDE 'D0$INC:SHLCON.INC/LIST'            ! shower library constants
      INCLUDE 'D0$INC:QUEST.INC/LIST'             ! QUEST vector for RZ
C
      LOGICAL EM,HAD
      LOGICAL LNEW
C
C&IF SIUNIX,IBMAIX
C&      REAL RAN
C&ENDIF
      INTEGER IETRK,IPTRK,IESTO,IPSTO,INDEX
      INTEGER NHITS,IP,IE,IL,IDTRK
      REAL THETA,ETA,PHI,NVTX,XT,PT
      REAL PS(8)
      DATA PS/8*0./
      REAL SMRFAC,SMF
      REAL ECELL,ESCALE,EMISS,ETOT,ESCIN
C
      INTEGER NB,NBW,LW                           ! Zebra stuff
      EXTERNAL NZBANK
      INTEGER NZBANK
C
      IF(SHWG.NE.2) RETURN                        ! Shouldn't be called!
C
      EM=.FALSE.
      HAD=.FALSE.

      XT=SQRT(VECT(1)**2 + VECT(2)**2)
      PT=SQRT(VECT(4)**2 + VECT(5)**2)
      IF (PT.EQ.0.)PT=0.000001
      NVTX=VECT(3)-XT*VECT(6)/PT

      CALL GETBIN(vect(1),vect(2),vect(3),vect(7),nvtx,IPART,KEY)

      IF(KEY(1).EQ.0)THEN
        RETURN
      ELSEIF(KEY(1).LT.0)THEN
        EM=.TRUE.
      ELSEIF(KEY(1).GT.0)THEN
        HAD=.TRUE.
      ENDIF

      CALL GETCEL(VECT(1),VECT(2),VECT(3),IETRK,IPTRK)
C
C *** Read in the shower library file corresponding to the requested key
C
  700 CONTINUE

      LSHHD=0

      CALL RZIN(ISHDIV,LSHHD,2,KEY,9999,' ')
      IF(IQUEST(1).NE.0)WRITE(LOUT,*)
     +    'RZIN ERROR: IQUEST(1),KEY = ',IQUEST(1),KEY
C
      LW=LS(LSHHD-IZSHLB)                ! 1st bank in the linear structure
      NB=NZBANK(IXSHLB,LW)

      IF(NB.LE.1) THEN                  ! No entries in requested bin
        IF(ABS(KEY(1)).EQ.1)THEN        ! If lowest momentum bin... dump
C                                       ! all energy
          EMISS=VECT(7)
          GOTO 9800
        ENDIF

        IF(KEY(1).GT.0)THEN
          KEY(1)=KEY(1)-1
        ELSE
          KEY(1)=KEY(1)+1
        ENDIF
        GOTO 700                         ! Else try again with lower momentum

      ENDIF
C
C *** CHOOSE ONE LIBRARY ENTRY AT RANDOM
C
      NBW=1+INT(FLOAT(NB-1)*RAN(ISEED))
C
      DO 500 I=1,NBW
        LW=LS(LW)         ! Get the next bank address
  500 CONTINUE
C
C *** LW is now the address of the desired track bank
C
      NHITS=IS(LW+12)
C
C ****  Calculate energy scaling from library track to event track
C
      ESCALE=VECT(7)/S(LW+4)
C
      CALL GETCEL(S(LW+1),S(LW+2),S(LW+3),IESTO,IPSTO)
C
C ****  calculate quantities needed for smearing
C
      PS(1)=VECT(4)*VECT(7)
      PS(2)=VECT(5)*VECT(7)
      PS(3)=VECT(6)*VECT(7)
      PS(4)=VECT(7)
      CALL GEAISA(IPART,IDTRK)
C
      LNEW=.TRUE.
C
C ****  Loop over hits and store them
C
      IF (NHITS.GE.1)THEN
        DO 550 I=1,NHITS                ! LOOP OVER THE NON ZERO HITS
C
C ****  Get packed index of hit
C
          INDEX=IS(LW+12+2*I)
          ECELL=S(LW+11+2*I)
C
C ****  Unpack the index to IETAC, IPHI, ILYR coordinates
C
          IE=ISIGN(IABS(INDEX)/10000,INDEX)
          IP=MOD(IABS(INDEX)/100,100)
          IL=MOD(IABS(INDEX),100)
C
C ****  Rotate hit in phi to match event track
C
          IP=IP-IPSTO+IPTRK
          IF(IP.GT.64)IP=IP-64
          IF(IP.LT.1)IP=IP+64
          IF(ABS(IE).GE.33.AND.MOD(IP,2).EQ.0)IP=IP-1
C
C ****  Flip hit in +- z if necessary
C
          IF(IESTO*IETRK.LT.0)THEN
            IE=-1*IE
C
C ****  Rearrange EM3 layers where division is 0.05 x 0.05
C
            IF(ABS(IE).LE.26)THEN
              IF(IL.EQ.3)THEN
                IL=5
              ELSEIF(IL.EQ.5)THEN
                IL=3
              ELSEIF(IL.EQ.4)THEN
                IL=6
              ELSEIF(IL.EQ.6)THEN
                IL=4
              ENDIF
            ENDIF
          ENDIF
C
          ECELL=ECELL*ESCALE*SMRFAC(LNEW,PS,IDTRK,IE,IP,IL)
          LNEW=.FALSE.
C
C ****  Store the energies
C
          CALL DHSTOR(ITRA,IE,IP,IL,ECELL)  ! STORE HITS
C
  550   CONTINUE
      ENDIF

C **** STORE DEAD ENERGY

      IL=18
      IF(ABS(IETRK).GE.14)IL=19
      CALL DHSTOR(ITRA,IETRK,IPTRK,IL,ESCALE*S(LW+10))
C
C ****  Store MG energies not explicitly accounted for
C
      CALL DHSTOR(ITRA,IETRK,IPTRK,8,ESCALE*S(LW+7))
      CALL DHSTOR(ITRA,IETRK,IPTRK,9,ESCALE*S(LW+8))
      CALL DHSTOR(ITRA,IETRK,IPTRK,10,ESCALE*S(LW+9))
C
C ****  Store Calorimeter energy not explicitly accounted for
C
      EMISS=ESCALE*S(LW+6)

 9800 CONTINUE                          ! also get here if we couldn't
C                                       ! find a library entry
      IF(EM)THEN
        IL=1
        IF(ABS(IETRK).EQ.13)IL=11
        IF(ABS(IETRK).EQ.14)IL=7
        IF(ABS(IETRK).GE.36)IL=13
      ELSEIF(HAD)THEN
        IL=11
        IF(ABS(IETRK).EQ.37)IL=13
      ENDIF

      CALL DHSTOR(ITRA,IETRK,IPTRK,IL,EMISS)  ! STORE ENERGY
C
 9999 CONTINUE
      CALL MZWIPE(ISHDIV)      ! Clear the Zebra division
      RETURN
      END
