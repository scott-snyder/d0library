      LOGICAL FUNCTION em_fits_mask ( iwant, bank, mask )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine is intended for use with GTPELC and
C-   GTTPHO, to allow selection of only those EM clusters which satisfy the
C-   requirements imposed by MASK, as implemented in CLEANEM
C-
C-   Returned value  : .TRUE. if the cluster passes, .FALSE. if it fails or does
C-                     not exist.
C-   Inputs  :  IWANT   [I] the index of the bank to be tested
C-              BANKS   [C*(4)] 'PELC' or 'PPHO' (any other value returns FALSE)
C-              MASK    [I] the mask defining the cuts required by the user.
C-                          See CLEANEM for the definition of this mask.
C-
C-
C-   EXAMPLE OF USE:
C-
C-      usermask = 128                 ! or whatever is required for your use
C-      CALL GTPELC_TOTAL (npelc, ier)
C-      IF (ier .NE. 0) THEN
C-        DO i = 1, npelc
C-          IF (EM_FITS_MASK (i, 'PELC', usermask))
C-            CALL GTPELC (i, eel, etel, sigel, theta, eta, phi, cone_energy,
C-                         dist, num_tracks, ier)
C-            IF (ier .ne. 0) THEN
C-             .
C-             .
C-             {use the values read by GTPELC here}
C-             .
C-             .
C-            ENDIF
C-          ENDIF
C-        ENDDO
C-      ENDIF
C-
C-
C-
C-   Outputs : none
C-   Controls: none
C-
C-   Created   8-OCT-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER  iwant, mask
      CHARACTER*4 bank
C----------------------------------------------------------------------
      CHARACTER*4 localbank
      INTEGER     lbank, gzpelc, gzppho, jbank
      LOGICAL     ok
C----------------------------------------------------------------------
      CALL upcase (bank(1:4), localbank)
      em_fits_mask = .false.
      IF (localbank .EQ. 'PELC') THEN
        jbank = 1
        lbank = gzpelc()
        IF (lbank .GT. 0) THEN
          DO WHILE (lbank .GT. 0)
            IF (iwant .EQ. jbank) THEN  ! check the bank
              CALL check_em_quality (lbank, mask, ok)
              em_fits_mask = ok
              RETURN
            ELSE
              lbank = lq(lbank)
              jbank = jbank + 1
            ENDIF                       ! if (iwant .eq. jbank)
          ENDDO                         ! whilte (lbank .gt. 0)
        ENDIF                           ! if lbank .gt. 0
      ELSE IF (localbank .EQ. 'PPHO') THEN
        jbank = 1
        lbank = gzppho()
        IF (lbank .GT. 0) THEN
          DO WHILE (lbank .GT. 0)
            IF (iwant .EQ. jbank) THEN  ! check the bank
              CALL check_em_quality (lbank, mask, ok)
              em_fits_mask = ok
              RETURN
            ELSE
              lbank = lq(lbank)
              jbank = jbank + 1
            ENDIF                       ! if (iwant .eq. jbank)
          ENDDO                         ! whilte (lbank .gt. 0)
        ENDIF                           ! if lbank .gt. 0
      ENDIF
      RETURN
      END
