      INTEGER FUNCTION GZMCON(MOD, TYPE, TREE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get the ZEBRA address of a MUON constant file.
C-                         For data banks call appropriate GZxxxx routine.
C-                         For header banks get directly.   
C-
C-   Returned value  : ZEBRA address.  if = 0, no such bank found
C-   Inputs  : MOD                      ! module number
C-             TYPE                     ! MPED, MGAN, etc
C-             TREE                     ! STPC/ STPO/ STPN
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-APR-1989   J.Green
C-             8-Jun-89     J.Green   added ability to get header banks
C-             MAY 90       D. HEDIN  add MBHD,MBAD
C-             SEPT 91      D.HEDIN   add MDFH,MDFT
C-             Jul 93       J.Green   add MSTH,MSTC
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMDFH.LINK'
      INCLUDE 'D0$LINKS:IZMPDH.LINK'
      INCLUDE 'D0$LINKS:IZMGNH.LINK'
      INCLUDE 'D0$LINKS:IZMTMH.LINK'
      INCLUDE 'D0$LINKS:IZMDTH.LINK'
      INCLUDE 'D0$LINKS:IZMGEH.LINK'
      INCLUDE 'D0$LINKS:IZMMAH.LINK'
      INCLUDE 'D0$LINKS:IZMSRH.LINK'
      INCLUDE 'D0$LINKS:IZMBHD.LINK'
      INCLUDE 'D0$LINKS:IZMSTH.LINK'
      INTEGER      MOD
      CHARACTER*4  TYPE
      CHARACTER*4  TREE
      CHARACTER*4  INTYPE               ! TYPE for internal consumption
      INTEGER      GZMPED, GZMPED_N, GZMPED_R
      INTEGER      GZMGAN, GZMGAN_N, GZMGAN_R
      INTEGER      GZMTIM, GZMTIM_N, GZMTIM_R
      INTEGER      GZMGEO, GZMGEO_N, GZMGEO_R
      INTEGER      GZMMAP, GZMMAP_N, GZMMAP_R
      INTEGER      GZMMAG, GZMMAG_N, GZMMAG_R
      INTEGER      GZMSAU, GZMSAU_N, GZMSAU_R
      INTEGER      GZMSOP, GZMSOP_N, GZMSOP_R
      INTEGER      GZMDTM, GZMDTM_N, GZMDTM_R
      INTEGER      GZMBAD, GZMBAD_N, GZMBAD_R
      INTEGER      GZMDFT, GZMDFT_N, GZMDFT_R
      INTEGER      GZMSTC, GZMSTC_N, GZMSTC_R
      INTEGER      LCON                 ! hold the output value
      INTEGER      KSUP,   KSMUO
C----------------------------------------------------------------------
C
C                                       ! allow use of PEDS etc as TYPE
      LCON = 0
C                                       ! find if Header or data
      IF (TYPE(4:4) .EQ. 'H'.OR.TYPE.EQ.'MBHD') THEN      ! header
        IF     (TREE .EQ. 'STPO') THEN
          KSUP = LC(LSTPH-IZSTPO)
        ELSEIF (TREE .EQ. 'STPC') THEN
          KSUP = LC(LSTPH-IZSTPC)
        ELSEIF (TREE .EQ. 'STPN') THEN
          KSUP = LC(LSTPH-IZSTPN)
        ENDIF
        IF (KSUP .NE. 0) THEN
          KSMUO = LC(KSUP-IZSMUO)
          IF (KSMUO .NE. 0) THEN
            IF     (TYPE .EQ. 'MPDH') THEN
              LCON = LC(KSMUO-IZMPDH)
            ELSEIF (TYPE .EQ. 'MGNH') THEN
              LCON = LC(KSMUO-IZMGNH)
            ELSEIF (TYPE .EQ. 'MTMH') THEN
              LCON = LC(KSMUO-IZMTMH)
            ELSEIF (TYPE .EQ. 'MDFH') THEN
              LCON = LC(KSMUO-IZMDFH)
            ELSEIF (TYPE .EQ. 'MDTH') THEN
              LCON = LC(KSMUO-IZMDTH)
            ELSEIF (TYPE .EQ. 'MGEH') THEN
              LCON = LC(KSMUO-IZMGEH)
            ELSEIF (TYPE .EQ. 'MMAH') THEN
              LCON = LC(KSMUO-IZMMAH)
            ELSEIF (TYPE .EQ. 'MSRH') THEN
              LCON = LC(KSMUO-IZMSRH)
            ELSEIF (TYPE .EQ. 'MBHD') THEN
              LCON = LC(KSMUO-IZMBHD)
            ELSEIF (TYPE .EQ. 'MSTH') THEN
              LCON = LC(KSMUO-IZMSTH)
            ENDIF
          ENDIF                         ! KSMUO
        ENDIF                           ! KSUP 
      ELSE                              ! data
        IF     (TYPE .EQ. 'PEDS') THEN
          INTYPE = 'MPED'
        ELSEIF (TYPE .EQ. 'GAIN') THEN
          INTYPE = 'MGAN'
        ELSEIF (TYPE .EQ. 'TIME') THEN
          INTYPE = 'MTIM'
        ELSEIF (TYPE .EQ. 'DTIM') THEN
          INTYPE = 'MDTM'
        ELSEIF (TYPE .EQ. 'SCNT') THEN
          INTYPE = 'MSCT'
        ELSE
          INTYPE = TYPE
        ENDIF
C
        IF     (TREE .EQ. 'STPO') THEN
          IF (INTYPE.EQ.'MPED')     LCON = GZMPED_R(MOD)
          IF (INTYPE.EQ.'MGAN')     LCON = GZMGAN_R(MOD)
          IF (INTYPE.EQ.'MTIM')     LCON = GZMTIM_R(MOD)
          IF (INTYPE.EQ.'MDTM')     LCON = GZMDTM_R(MOD)
          IF (INTYPE.EQ.'MDFT')     LCON = GZMDFT_R(MOD)
          IF (INTYPE.EQ.'MGEO')     LCON = GZMGEO_R(MOD)
          IF (INTYPE.EQ.'MMAP')     LCON = GZMMAP_R(MOD)
          IF (INTYPE.EQ.'MMAG')     LCON = GZMMAG_R(MOD)
          IF (INTYPE.EQ.'MSAU')     LCON = GZMSAU_R(MOD)
          IF (INTYPE.EQ.'MSOP')     LCON = GZMSOP_R(MOD)
          IF (INTYPE.EQ.'MBAD')     LCON = GZMBAD_R(MOD)
          IF (INTYPE.EQ.'MSCT')     LCON = GZMSTC_R(MOD)
        ELSEIF (TREE .EQ. 'STPC') THEN
          IF (INTYPE.EQ.'MPED')     LCON = GZMPED(MOD)
          IF (INTYPE.EQ.'MGAN')     LCON = GZMGAN(MOD)
          IF (INTYPE.EQ.'MTIM')     LCON = GZMTIM(MOD)
          IF (INTYPE.EQ.'MDTM')     LCON = GZMDTM(MOD)
          IF (INTYPE.EQ.'MDFT')     LCON = GZMDFT(MOD)
          IF (INTYPE.EQ.'MGEO')     LCON = GZMGEO(MOD)
          IF (INTYPE.EQ.'MMAP')     LCON = GZMMAP(MOD)
          IF (INTYPE.EQ.'MMAG')     LCON = GZMMAG(MOD)
          IF (INTYPE.EQ.'MSAU')     LCON = GZMSAU(MOD)
          IF (INTYPE.EQ.'MSOP')     LCON = GZMSOP(MOD)
          IF (INTYPE.EQ.'MBAD')     LCON = GZMBAD(MOD)
          IF (INTYPE.EQ.'MSCT')     LCON = GZMSTC(MOD)
        ELSEIF (TREE .EQ. 'STPN') THEN
          IF (INTYPE.EQ.'MPED')     LCON = GZMPED_N(MOD)
          IF (INTYPE.EQ.'MGAN')     LCON = GZMGAN_N(MOD)
          IF (INTYPE.EQ.'MTIM')     LCON = GZMTIM_N(MOD)
          IF (INTYPE.EQ.'MDTM')     LCON = GZMDTM_N(MOD)
          IF (INTYPE.EQ.'MDFT')     LCON = GZMDFT_N(MOD)
          IF (INTYPE.EQ.'MGEO')     LCON = GZMGEO_N(MOD)
          IF (INTYPE.EQ.'MMAP')     LCON = GZMMAP_N(MOD)
          IF (INTYPE.EQ.'MMAG')     LCON = GZMMAG_N(MOD)
          IF (INTYPE.EQ.'MSAU')     LCON = GZMSAU_N(MOD)
          IF (INTYPE.EQ.'MSOP')     LCON = GZMSOP_N(MOD)
          IF (INTYPE.EQ.'MBAD')     LCON = GZMBAD_N(MOD)
          IF (INTYPE.EQ.'MSCT')     LCON = GZMSTC_N(MOD)
        ENDIF
      ENDIF                             ! header or data
C
      GZMCON = LCON
C
  999 RETURN
      END
