      SUBROUTINE CGL1HXR (L1INX,RINGN,NTOWRS,LIST,ITSOK) ! Generate L-1 HeX Ring
C----------------------------------------------------------------------
C-
C-   CGL1HXR = (Calorimeter) Generate L-1 HeX Ring list.
C-
C-   Purpose and Methods : This routine builds the list of hex addresses
C-                         corrosponding to the ring of L-1 trigger towers
C-                         surrounding a tower. The list sent back is a
C-                         string of hex addresses, increasing numericaly,
C-                         that contain all of the good addresses for the
C-                         readout towers. There are as many addresses as
C-                         towers (1-1). If the center of the ring lies
C-                         close to an eta edge then those towers that lie
C-                         over the edge are not included in the list. As
C-                         that the number of elements in a ring is a well
C-                         known function (if r=0 n=1, else n=8r, where r
C-                         is RINGN) if NTOWRS<n then clearly the ring is
C-                         outside the bounds of the calorimeter.
C-
C-   Inputs  : L1INX gives the L-1 data-block index; RINGN gives the number
C-             of rings worked with; and common block L1OL is employed.
C-
C-   Outputs : NTOWRS = the # of address pairs in the string; 
C-             vector LIST gives NTOWRS addresses.
C-             ITSOK returns false if the ring is so big it overlaps
C-             itself in the phi-direction.
C-
C-   Controls: None.
C-
C-   Created  18-MAY-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
C     Passesd Variables:
*
         INTEGER  L1INX    ! L-1 INdeX (into the L-1 trigger block).
         INTEGER  RINGN    ! RING-N; which ring surrounding the central tower.
         INTEGER  NTOWRS   ! The Number of TOWeRS in the ring.
         INTEGER  LIST(100,2)  ! The list of those addresses.
         LOGICAL  ITSOK        ! Signals if somthing went awry.
*
C     Local Variables:
*
         INTEGER  ETALFT   ! "Left" most tower in ring.
         INTEGER  ETART    ! "Right" most tower in ring. 
         INTEGER  L1ETAC,L1PHIC ! L-1 eta,phi indicies.
         INTEGER  L1DBI    ! L-1 DataBlock Index.
         INTEGER  META,MPHI     ! local loop variables for eta,phi.
         INTEGER  N             ! general index; becomes NTOWRS.
         INTEGER  PHI1,PHI2     ! 'Order' index phi's.
         INTEGER  PHIUP    ! "Up" most tower in ring.
         INTEGER  PHIDWN   ! "Down" most tower in ring.
         LOGICAL  UPBND,DWNBND  ! If true then the ring the 'UP' ('DOWN')
                                ! BouNDdarry. Remeber, the up (down) 
                                ! boundary is at netal1 (0).
*
         INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
         INCLUDE 'D0$INC:L1OL.INC'
*
*
C     ---And now for the next ExCiTiNg chapter of...The Code:
*
*
      RINGN = ABS(RINGN)  !  RINGN must be a positive number.
      CALL CDBITT(L1INX,L1ETAC,L1PHIC,ITSOK) ! Find out where i am in L1
C                                            ! eta,phi indicies.
      IF (L1ETAC+NETAL1 .GT. RINGN) THEN  ! This block determines the 
            ETALFT = L1ETAC-RINGN         ! the limits of etac over
         ELSE                             ! which the addresses can
            ETALFT = -NETAL1              ! be generated. ETALFT and
      ENDIF                               ! ETART are the min. and max.,
      IF (NETAL1-L1ETAC .GT. RINGN) THEN  ! respectively, of etac. "right"
            ETART  = L1ETAC+RINGN         ! are "left" are meaningfull
         ELSE                             ! when looking at the cartesian
            ETART  = NETAL1               ! eta,phi map.
      ENDIF                               !
      IF (L1ETAC.LE.RINGN.AND.L1ETAC.GE.1) ETALFT = ETALFT-1 ! This takes
      IF (L1ETAC.GE.-RINGN.AND.L1ETAC.LE.-1) ETART  = ETART +1 ! care of
C                                         ! the case where the radius
C                                         ! crosses the (imaginary) eta = 0
C                                         ! boundary 
C
      IF (L1PHIC+RINGN .GT. NPHIL1) THEN  ! This section determines the
            PHIUP  = RINGN-NPHIL1+L1PHIC  ! limits of phic. PHIUP and
            UPBND  = .FALSE.              ! PHIDWN are min. and max.,
         ELSE                             ! repectively of etac. As above, 
            PHIUP  = L1PHIC+RINGN         ! "up" and "down" are meaningful
            UPBND  = .TRUE.               ! when looking at the
      ENDIF                               ! cartesian eta, phi map.
      IF (L1PHIC-RINGN .LT. 1) THEN       ! UPBND flags (false) if the
            PHIDWN = L1PHIC-RINGN+NPHIL1  ! ring crosses the l1phic=nphil1
            DWNBND = .FALSE.              ! boundary; DWNBND flags (false)
         ELSE                             ! if the ring crosses the l1phic
            PHIDWN = L1PHIC-RINGN         ! =1 boundary. 
            DWNBND = .TRUE.
      ENDIF                               !
*
      IF (.NOT.DWNBND .AND. .NOT.UPBND) THEN ! If it's not ok then
         ITSOK = .FALSE.                ! the ring crosses both the top
         GOTO 999                  ! and bottom boundaries. In this case
      ENDIF                   ! clearly the ring overlaps itself and is
      ITSOK = .TRUE.    ! thus disqualified.
C
C     ---Now we have the limits of the ring. With this I'll build the
C     ---the address list that comprises the ring.
C
      N = 0
      IF (DWNBND .AND. UPBND) THEN  ! The case where both the top and bottom
            DO MPHI = PHIDWN,PHIUP  ! of the ring cross neither the phi-max
               CALL CTTDBI(ETALFT,MPHI,L1DBI)  ! nor the phi-min=1 boundary.
               IF (MOD(L1DBI,NPHIL1*NETAL1).LE.FINDBI) THEN ! Does the tower
                  N=N+1                                ! exist?
                  LIST(N,1) = L1OL(L1DBI,3)  !  get CRATE
                  LIST(N,2) = L1OL(L1DBI,4)  !  get IADDR
               ENDIF
            END DO
         ELSE ! Either (.NOT.dwnbnd .AND. upbnd).or.(dwnbnd .AND. .NOT.upbnd)
            DO MPHI = 1,PHIUP
               CALL CTTDBI(ETALFT,MPHI,L1DBI)
               IF (MOD(L1DBI,NPHIL1*NETAL1).LE.FINDBI) THEN
                  N=N+1
                  LIST(N,1) = L1OL(L1DBI,3)  !  get CRATE
                  LIST(N,2) = L1OL(L1DBI,4)  !  get IADDR
               ENDIF
            END DO
            DO MPHI = PHIDWN,NPHIL1
               CALL CTTDBI(ETALFT,MPHI,L1DBI)
               IF (MOD(L1DBI,NPHIL1*NETAL1).LE.FINDBI) THEN
                  N=N+1
                  LIST(N,1) = L1OL(L1DBI,3)  !  get CRATE
                  LIST(N,2) = L1OL(L1DBI,4)  !  get IADDR
               ENDIF
            END DO
      ENDIF
      IF (RINGN .GT. 0) THEN  ! If not 0 then there's cookie's
C                             ! center; get it.
            DO META = ETALFT+1,ETART-1
               IF (PHIDWN .LT. PHIUP) THEN
                     PHI1 = PHIDWN
                     PHI2 = PHIUP
                  ELSE
                     PHI1 = PHIUP
                     PHI2 = PHIDWN
               ENDIF
               CALL CTTDBI(META,PHI1,L1DBI)
               IF (MOD(L1DBI,NPHIL1*NETAL1).LE.FINDBI) THEN
                  N=N+1
                  LIST(N,1) = L1OL(L1DBI,3)  !  get CRATE
                  LIST(N,2) = L1OL(L1DBI,4)  !  get IADDR
               ENDIF
               CALL CTTDBI(META,PHI2,L1DBI)
               IF (MOD(L1DBI,NPHIL1*NETAL1).LE.FINDBI) THEN
                  N=N+1
                  LIST(N,1) = L1OL(L1DBI,3)  !  get CRATE
                  LIST(N,2) = L1OL(L1DBI,4)  !  get IADDR
               ENDIF
            END DO
            IF (DWNBND .AND. UPBND) THEN
                  DO MPHI = PHIDWN,PHIUP
                     CALL CTTDBI(ETART,MPHI,L1DBI)
                     IF (MOD(L1DBI,NPHIL1*NETAL1).LE.FINDBI) THEN
                        N=N+1
                       LIST(N,1) = L1OL(L1DBI,3)  !  get CRATE
                       LIST(N,2) = L1OL(L1DBI,4)  !  get IADDR
                    ENDIF
                  END DO
               ELSE  ! Either (.NOT.dwnbnd .AND. upbnd).or.
                  DO MPHI = 1,PHIUP  ! (dwnbnd .AND. .NOT.upbnd)
                     CALL CTTDBI(ETART,MPHI,L1DBI)
                     IF (MOD(L1DBI,NPHIL1*NETAL1).LE.FINDBI) THEN
                        N=N+1
                        LIST(N,1) = L1OL(L1DBI,3)  !  get CRATE
                        LIST(N,2) = L1OL(L1DBI,4)  !  get IADDR
                     ENDIF
                  END DO
                  DO MPHI = PHIDWN,NPHIL1
                     CALL CTTDBI(ETART,MPHI,L1DBI)
                     IF (MOD(L1DBI,NPHIL1*NETAL1).LE.FINDBI) THEN
                        N=N+1
                        LIST(N,1) = L1OL(L1DBI,3)  !  get CRATE
                        LIST(N,2) = L1OL(L1DBI,4)  !  get IADDR
                     ENDIF
                  END DO
            ENDIF
      ENDIF
      NTOWRS = N
  999 RETURN
      END
