      SUBROUTINE MNPMAD( SCADD, MMOD, NPM, IWIR, IPLN, IPM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return P.M. connected information
C-                          
C-
C-   Inputs  : SCADD  : Scintillator address
C-                      #Scint*1000 + #Muon_module
C-   Outputs : MMOD   : Muon moudle ID
C-             NPM    : Number of P.M. in this scintillator
C-                      if 0 then error
C-             IWIR   : Wire number, array
C-             IPLN   : Plan number, array
C-             IPM    : Array, Lower part - 1, upper part - 2 in MUD1 bank 
C-   Controls: NONE
C-
C-   Created   13-MAY-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER  SCADD
      INTEGER  MMOD, NPM, IWIR(2), IPLN(2), IPM(2)
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER  NSCI, I, LMSGE, GZMSGE
      INTEGER  CCS, IUP
C----------------------------------------------------------------------
C get MSGE pointer
      MMOD = MOD( SCADD, 1000 )     ! #muon_module
      NSCI = (SCADD - MMOD)/1000    ! #scintilltor
      LMSGE = GZMSGE(MMOD,NSCI)     ! 1st scintillator pointer
      I = NSCI - 1
      DO I=1,NSCI-1               ! get NSCI-th pointer from linear structure
        LMSGE = LC(LMSGE)
      END DO
C
      IF ( LMSGE.EQ.0 ) THEN      
        NPM = 0
        GOTO 999
      END IF
C
      NPM = IC(LMSGE+9)       ! number of P.M.
C
      DO I=1,NPM
        CCS = IC(LMSGE+19+(I-1)*4)      ! P.M. connected channel at SCIBO
        IWIR(I) = (CCS+1)/2             ! wire number
        IUP = MOD(CCS,2)                ! P.M. lower or upper
        IF ( IUP.EQ.1 ) THEN
          IPM(I) = 1
        ELSE
          IPM(I) = 2
        END IF
        IPLN(I) = 4                     ! plane 
      END DO
C
  999 RETURN
      END
