      SUBROUTINE ECPAD(LCLAY, TITLE, LYR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO FILL PAD INFORMATION  FROM SRCP INTO 
C-       CLAY BANK.
C-
C-   Inputs  :     LCLAY     pointer to CLAY bank
C-                 TITLE     identifier of SRCP array
C-                 LAYER     sub-identifier for the specific "sub-floor"
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Altered:    CLAY
C-
C-   Created   4-FEB-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:SCPARR.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      CHARACTER TITLE*(*)
      CHARACTER*4 lyr
      INTEGER LAYER, INDX, INDX1, INDX2, INDX3, N32, N64, N128
      INTEGER LCLAY, JCLAY, NPHI, NETA, I, IERR, LUNIT
      REAL SGN, FG
      DATA LUNIT / 6 /
C
      call uctoh(lyr,layer,4,4)
      IF(TITLE(1:3) .EQ. 'EM3') THEN
        FG = 2.0             ! fudge factor for granularity of floor 3
      ELSE
        FG = 1.0
      END IF
C
      CALL EZGET(TITLE, IVAL, IERR)      ! get SRCP array
      IF(IERR .NE. 0) THEN
        WRITE( LUNIT, 45) TITLE
   45   FORMAT(' ECPAD -- CANT FIND ARRAY: ',A)
        STOP 45
      END IF
C
C ... OBTAIN INDEX TO SUB-LAYER BLOCK
C
      INDX = 4
      DO 50 I = 1, IVAL(1)
      INDX1 = INDX + 3
      INDX2 = INDX1 + IVAL(INDX1) + 1
      INDX3 = INDX2 + IVAL(INDX2) + 1
      IF ( IVAL(INDX) .EQ. LAYER) GO TO 60
      INDX = INDX3 + IVAL(INDX3) + 1
   50 CONTINUE
      WRITE(LUNIT,55) TITLE, LAYER
   55 FORMAT(' ECPAD -- CANT FIND: ',A,A4)
      STOP 50
   60 CONTINUE
C
C ... DETERMINE ETA-PHI ZONES
C
C ... MH or OH regions ... fill against increasing rapidity (by convention)
      IF ( IVAL(INDX3) .LE.0) THEN
C     modify IVAL(INDX2) => IVAL(INDX2)-1 to correct for added IDWORD
        NETA = IVAL(INDX2)-1      ! number of etas
        IC(LCLAY + ILNETA) = NETA
        IC(LCLAY + ILNPHI) = 64
        IC(LCLAY + ILETPH) = 1  ! one eta-phi zone
        C(LCLAY + ILDETA) = (RVAL(INDX2+2) - RVAL(INDX2+1))/10.   ! eta
C                                        ! increment
        C(LCLAY + ILETA0) = 0.5*(RVAL(INDX2+1) + RVAL(INDX2+2))*
     +    ABS(C(LCLAY + ILDETA))*FG      ! nominal value of first pad
        C(LCLAY + ILDPHI) = TWOPI/IC(LCLAY + ILNPHI)      ! phi increment
        C(LCLAY + ILPHI0) = C(LCLAY + ILPHIC) + 0.5*C(LCLAY + ILDPHI) 
C                                        ! nominal value of first pad
        CALL UCOPY(RVAL(INDX1+1),C(LCLAY+ILRPD0), IVAL(INDX1))   ! fill
C                                        ! r division positions
C ... IH or EC/EM regions ... fill with increasing rapidity (by convention)
      ELSE
        N32 = 0
        N64 = 0
        N128 = 0
C
        DO 80 I = 1, IVAL(INDX3)
        IF( IVAL( INDX3 + I) .EQ. 32) N32 = N32 + 1
        IF( IVAL( INDX3 + I) .EQ. 64) N64 = N64 + 1
        IF( IVAL( INDX3 + I) .EQ. 128) N128 = N128 + 1
   80   CONTINUE
C
        IF (N128 .EQ. 0 .AND. N32 .EQ. 0) THEN    ! one eta-phi zone
C     modify IVAL(INDX2) => IVAL(INDX2)-1 to correct for added IDWORD
          NETA = IVAL(INDX2)-1      ! number of etas
          IC(LCLAY + ILNETA) = NETA
          IC(LCLAY + ILNPHI) = 64
          IC(LCLAY + ILETPH) = 1  ! one eta-phi zone
          C(LCLAY + ILDETA) = (RVAL(INDX2+NETA-1) - RVAL(INDX2+NETA))
     &      /10.                  ! eta increment
          C(LCLAY + ILETA0) = 0.5*(RVAL(INDX2+NETA-1) + 
     +      RVAL(INDX2+NETA)-2.)*ABS(C(LCLAY+ILDETA))*FG  ! nominal val 
C                                        ! of 1st pad
          C(LCLAY + ILDPHI) = TWOPI/IC(LCLAY + ILNPHI)      ! phi increment
          C(LCLAY + ILPHI0) = C(LCLAY + ILPHIC) + 0.5*C(LCLAY + ILDPHI) 
C                                        ! nominal value of first pad
          DO 100 I = 1, IVAL(INDX1)
  100     C(LCLAY + ILRPD0 + I - 1) = RVAL(INDX1 + IVAL(INDX1) - I+1)
     &                            ! fill r divisions backwards
        ELSE IF(N32 .NE. 0 .AND. N64 .NE. 0 .AND. N128 .EQ. 0) THEN     
C                                        ! two eta-phi zones
          IC(LCLAY + ILETPH) = 2  ! two eta-phi zone
C     modify IVAL(INDX2) => IVAL(INDX2)-1 to correct for added IDWORD
          NETA = IVAL(INDX2)-1
          IC(LCLAY + ILNETA) = N64
          IC(LCLAY + ILNPHI) = IVAL(INDX3+NETA-1)+0.001
          C(LCLAY + ILDETA) = (RVAL(INDX2+NETA-1) - RVAL(INDX2+NETA))
     &      /10.                  ! eta increment
          C(LCLAY + ILETA0) = 0.5*(RVAL(INDX2+NETA-1) + 
     +      RVAL(INDX2+NETA)-2.)*ABS(C(LCLAY+ILDETA))*FG  ! nominal val 
C                                        ! of 1st pad
          C(LCLAY + ILDPHI) = TWOPI/IC(LCLAY + ILNPHI)      ! phi increment
          C(LCLAY + ILPHI0) = C(LCLAY + ILPHIC) + 0.5*C(LCLAY + ILDPHI) 
C                                        ! nominal value of first pad
          JCLAY = LCLAY + NWZONE
C     modify IVAL(INDX2) => IVAL(INDX2)-1 to correct for added IDWORD
          NETA = IVAL(INDX2)-1 - N64
          IC(JCLAY + ILNETA) = N32
          IC(JCLAY + ILNPHI) = IVAL(INDX3+NETA-1)+0.001
          C(JCLAY + ILDETA) = (RVAL(INDX2+NETA-1) - RVAL(INDX2+NETA))
     &      /10.                  ! eta increment
          C(JCLAY + ILETA0) = 0.5*(RVAL(INDX2+NETA-1) + 
     +      RVAL(INDX2+NETA)-2.)*ABS(C(JCLAY+ILDETA))*FG  ! nominal val 
C                                        ! of 1st pad
          C(JCLAY + ILDPHI) = TWOPI/IC(JCLAY + ILNPHI)      ! phi increment
          C(JCLAY + ILPHI0) = C(LCLAY + ILPHIC) + 0.5*C(JCLAY + ILDPHI) 
C                                        ! nominal value of first pad
          DO 120 I = 1, IVAL(INDX1) 
  120     C(JCLAY + ILRPD0 + I - 1) = RVAL(INDX1 + IVAL(INDX1) - I+1)
     &                            ! fill r divisions backwards
        ELSE IF(N32 .EQ. 0 .AND. N64 .NE. 0 .AND. N128 .NE. 0) THEN     
C                                        ! two eta-phi zones
          IC(LCLAY + ILETPH) = 2  ! two eta-phi zone
C     modify IVAL(INDX2) => IVAL(INDX2)-1 to correct for added IDWORD
          NETA = IVAL(INDX2)-1
          IC(LCLAY + ILNETA) = N128
          IC(LCLAY + ILNPHI) = IVAL(INDX3+NETA-1)+0.001
          C(LCLAY + ILDETA) = (RVAL(INDX2+NETA-1) - RVAL(INDX2+NETA))
     &      /10.                  ! eta increment
          C(LCLAY + ILETA0) = 0.5*(RVAL(INDX2+NETA-1) + 
     +      RVAL(INDX2+NETA)-2.)*ABS(C(LCLAY+ILDETA))*FG  ! nominal val 
C                                        ! of 1st pad
          C(LCLAY + ILDPHI) = TWOPI/IC(LCLAY + ILNPHI)      ! phi increment
          C(LCLAY + ILPHI0) = C(LCLAY + ILPHIC) + 0.5*C(LCLAY + ILDPHI) 
C                                        ! nominal value of first pad
          JCLAY = LCLAY + NWZONE
C     modify IVAL(INDX2) => IVAL(INDX2)-1 to correct for added IDWORD
          NETA = IVAL(INDX2)-1 - N128
          IC(JCLAY + ILNETA) = N64
          IC(JCLAY + ILNPHI) = IVAL(INDX3+NETA-1)+0.001
          C(JCLAY + ILDETA) = (RVAL(INDX2+NETA-1) - RVAL(INDX2+NETA))
     &      /10.                  ! eta increment
          C(JCLAY + ILETA0) = 0.5*(RVAL(INDX2+NETA-1) + 
     +      RVAL(INDX2+NETA)-2.)*ABS(C(JCLAY+ILDETA))*FG  ! nominal val 
C                                        ! of 1st pad
          C(JCLAY + ILDPHI) = TWOPI/IC(JCLAY + ILNPHI)      ! phi increment
          C(JCLAY + ILPHI0) = C(LCLAY + ILPHIC) + 0.5*C(JCLAY + ILDPHI) 
C                                        ! nominal value of first pad
          DO 140 I = 1, IVAL(INDX1) 
  140     C(JCLAY + ILRPD0 + I - 1) = RVAL(INDX1 + IVAL(INDX1) - I+1)
     &                            ! fill r divisions backwards
        ELSE IF(N32 .NE. 0 .AND. N64 .NE. 0 .AND. N128 .NE. 0) THEN     
C                                        ! two eta-phi zones
          IC(LCLAY + ILETPH) = 3  ! three eta-phi zone -- EC/EM 3
C     modify IVAL(INDX2) => IVAL(INDX2)-1 to correct for added IDWORD
          NETA = IVAL(INDX2)-1
          IC(LCLAY + ILNETA) = N128
          IC(LCLAY + ILNPHI) = IVAL(INDX3+NETA-1)+0.001
          C(LCLAY + ILDETA) = (RVAL(INDX2+NETA-1) - RVAL(INDX2+NETA))
     &      /10.                  ! eta increment
          C(LCLAY + ILETA0) = 0.5*(RVAL(INDX2+NETA-1) + 
     +      RVAL(INDX2+NETA)-2.)*ABS(C(LCLAY+ILDETA))*FG  ! nominal val 
C                                        ! of 1st pad
          C(LCLAY + ILDPHI) = TWOPI/IC(LCLAY + ILNPHI)      ! phi increment
          C(LCLAY + ILPHI0) = C(LCLAY + ILPHIC) + 0.5*C(LCLAY + ILDPHI) 
C                                        ! nominal value of first pad
          JCLAY = LCLAY + NWZONE
C     modify IVAL(INDX2) => IVAL(INDX2)-1 to correct for added IDWORD
          NETA = IVAL(INDX2)-1 - N128
          IC(JCLAY + ILNETA) = N64
          IC(JCLAY + ILNPHI) = IVAL(INDX3+NETA-1)+0.001
          C(JCLAY + ILDETA) = (RVAL(INDX2+NETA-1) - RVAL(INDX2+NETA))
     &      /10.                  ! eta increment
          C(JCLAY + ILETA0) = 0.5*(RVAL(INDX2+NETA-1) + 
     +      RVAL(INDX2+NETA)-2.)*ABS(C(JCLAY+ILDETA))  ! nominal value 
C                                        ! of 1st pad
          C(JCLAY + ILDPHI) = TWOPI/IC(JCLAY + ILNPHI)      ! phi increment
          C(JCLAY + ILPHI0) = C(LCLAY + ILPHIC) + 0.5*C(JCLAY + ILDPHI) 
C                                        ! nominal value of first pad
          JCLAY = JCLAY + NWZONE
C     modify IVAL(INDX2) => IVAL(INDX2)-1 to correct for added IDWORD
          NETA = IVAL(INDX2)-1 - N128 - N64
          IC(JCLAY + ILNETA) = N32
          IC(JCLAY + ILNPHI) = IVAL(INDX3+NETA-1)+0.001
          C(JCLAY + ILDETA) = (RVAL(INDX2+NETA-1) - RVAL(INDX2+NETA))
     &      /10.                  ! eta increment
          C(JCLAY + ILETA0) = 0.5*(RVAL(INDX2+NETA-1) + 
     +      RVAL(INDX2+NETA)-2.)*ABS(C(JCLAY+ILDETA))  ! nominal value 
C                                        ! of 1st pad
          C(JCLAY + ILDPHI) = TWOPI/IC(JCLAY + ILNPHI)      ! phi increment
          C(JCLAY + ILPHI0) = C(LCLAY + ILPHIC) + 0.5*C(JCLAY + ILDPHI) 
C                                        ! nominal value of first pad
          DO 160 I = 1, IVAL(INDX1) 
  160     C(JCLAY + ILRPD0 + I - 1) = RVAL(INDX1 + IVAL(INDX1) - I+1)
     &                            ! fill r divisions backwards
        END IF
      END IF  
C
      C(JCLAY + ILWDTH) = TWOPI          ! active azi width
C----------------------------------------------------------------------
  999 RETURN
      END
