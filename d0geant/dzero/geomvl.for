      SUBROUTINE GEOMVL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up "Standard" Mother volumes
C-
C-   Inputs  : Logical flags
C-   Outputs : Positioned volumes
C-
C-   Created  ??-???-????   R.Raja
C-   Updated  17-APR-1989   Chip Stewart  Create MCAL if LV0 defined
C-   Updated  24-JUN-1991   K. Wyatt Merritt  Change GSVOLU arguments
C-                          for 3.14 compatibility 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:SRCPR.INC'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER IVOLU,FLVOL,NMCAL,NMVOL,NMCEN,IER
C
      CHARACTER*4 CNAME,CSHAPE
C----------------------------------------------------------------------
C
C  NOW DEFINE OVERALL MOTHER VOLUME
C
      CALL EZPICK('GNWSTP_RCP')
C--->
C       Code between the arrows should be replaced by a call to VOLPOS,
C       after altering the STP file so that OVERALL_MOTHER_VOLUME is given
C       the position code 'NONE' instead of 'POS'.
C
      CALL EZGET('OVERALL_MOTHER_VOLUME',SRCPAR,IER)
      CALL UHTOC(NAME,4,CNAME,4)
      CALL UHTOC(SHAPE,4,CSHAPE,4)
      CALL GSVOLU(CNAME,CSHAPE,NUMED,PAR,NPAR,IVOLU)      ! NO POSITIONING
C--->
      FLVOL = NAME                      ! FLANGE MOTHER VOLUME
C
      CALL VOLPOS('TEVATRON_BEAM_PIPE')
      CALL VOLPOS('VACUUM_BEAM_PIPE')  ! PROVIDES VACUUM IN BEAM PIPE!
C
      IF ( (DCAL.NE.0) .OR. (DCEN.NE.0) .OR. (DLV0.NE.0) ) THEN
        CALL VOLPOS('CALORIMETER_MOTHER_VOLUME')
        FLVOL = NAME                      ! FLANGE MOTHER VOLUME
        IF ( DCEN.NE.0 .OR. DLV0.NE.0 ) THEN
          CALL VOLPOS('CENTRAL_DETECTOR_MOTHER_VOLUME')
          FLVOL = NAME                      ! FLANGE MOTHER VOLUME
        ENDIF
      ENDIF
      IF ( DMUO.NE.0.OR.DSAM.NE.0 ) THEN
        CALL VOLPOS('MUON_MOTHER_VOLUME')
      ENDIF
C
C
C ****  NOW WORK OUT FLANGE MOTHER VOLUME
C
C
      CALL EZGSET('BEAM_FLANGE+Z(4:4)',FLVOL,-1)  ! MOTHER VOLUME RESET
      CALL EZGSET('BEAM_FLANGE-Z(4:4)',FLVOL,-1)  !  MOTHER VOLUME RESET
C
      CALL VOLPOS('BEAM_FLANGE+Z')
      CALL VOLPOS('BEAM_FLANGE-Z')
C
      CALL EZRSET                       ! RESET TO PREVIOUS BANK
  999 RETURN
      END
