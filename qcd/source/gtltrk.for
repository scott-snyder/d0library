      SUBROUTINE GTLTRK(ITRK,X0,Y0,Z0,THETA,PHI,N1,CHI1,N2,CHI2,
     &                  STAT,IONIZ,ITYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns microSTA packed tracking info:
C-                         X0,Y0,Z0,theta,phi,etc...
C-
C-   Inputs  :             ITRK = number of track
C-   Outputs :             ITYP = type of track 
C-                                   1 => CDC 
C-                                   2 => FDC 
C-                                  -1 => end of bank 
C-                                 -99 => no LTRK bank 
C-   Controls: 
C-
C-   Created  24-AUG-1993   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LLTRK, GZLTRK, ITRK, N1, N2, STAT, ITYP, IIONIZ
      INTEGER ICHI1, ICHI2, ITHT, IPHI, NCDC, NFDC, IPNT, IWORD
      REAL X0, Y0, Z0, THETA, PHI, CHI1, CHI2, IONIZ
C----------------------------------------------------------------------
      LLTRK = GZLTRK()
      IF (LLTRK .EQ. 0 .OR. ITRK.LE.0) THEN
        ITYP = -99    ! no bank or invalid track number
        RETURN 
      ENDIF
      NCDC = IQ(LLTRK+3)
      NFDC = IQ(LLTRK+4)
      IF (ITRK.LE.NCDC) THEN
        ITYP = 1      ! CDC Track
      ELSEIF (ITRK.LE.NCDC+NFDC) THEN
        ITYP = 2      ! FDC Track
      ELSE
        ITYP = -1
        RETURN        ! end of bank
      ENDIF
      IPNT = (ITRK-1)*6 + 4
      X0 =  Q(LLTRK+IPNT+1)      ! X, Y for track
      Y0 =  Q(LLTRK+IPNT+2)
      ITHT = 0
      IPHI = 0
      N1 = 0
      N2 = 0
      ICHI1 = 0
      ICHI2 = 0
      IF (ITYP.EQ.1) THEN    ! Unpack CDC track
        Z0  =  Q(LLTRK+IPNT+3)
        IWORD = IQ(LLTRK+IPNT+4)
        CALL MVBITS(IWORD,0,16,ITHT,0)
        CALL MVBITS(IWORD,16,16,IPHI,0)
        THETA =  FLOAT(ITHT)/10000. 
        PHI =  FLOAT(IPHI)/10000. 
        IWORD = IQ(LLTRK+IPNT+5)
        CALL MVBITS(IWORD,0,8,N1,0)       ! NHIT, number of xy wire hits 
        CALL MVBITS(IWORD,8,8,ICHI1,0)    ! chi square on xy fit
        CALL MVBITS(IWORD,16,8,N2,0)      ! NZ, number of rz wire hits
        CALL MVBITS(IWORD,24,8,ICHI2,0)   ! chi square on rz fit
        CHI1 = FLOAT(ICHI1)
        CHI2 = FLOAT(ICHI2)
        STAT = 0
        IWORD = IQ(LLTRK+IPNT+6)
        CALL MVBITS(IWORD,0,24,IIONIZ,0)  ! Track ionization in MIPS
        IONIZ = FLOAT(IIONIZ)/1.0E4
      ELSE                                ! Unpack FDC track
        IONIZ = Q(LLTRK+IPNT+3)           ! Track ionization in MIPS
        IWORD = IQ(LLTRK+IPNT+4)
        CALL MVBITS(IWORD,0,16,ITHT,0)    
        CALL MVBITS(IWORD,16,16,IPHI,0)
        THETA =  FLOAT(ITHT)/10000. 
        PHI =  FLOAT(IPHI)/10000.
        IWORD = IQ(LLTRK+IPNT+5)
        CALL MVBITS(IWORD,0,8,N1,0)       ! NFIT
        CALL MVBITS(IWORD,8,8,ICHI1,0)    ! normalized chi square
        CALL MVBITS(IWORD,16,8,N2,0)      ! NHIT
        CHI1 = FLOAT(ICHI1-128)/10.
        CHI2 = 0.0
        STAT = IQ(LLTRK+IPNT+6)           ! FDC status word
        Z0  =  105. * (2*IBITS(STAT,0,1)-1)   ! - for north, + for south
      ENDIF
  999 RETURN
      END
