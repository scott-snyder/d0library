      SUBROUTINE PRPARH(PRUNIT,LPARHI,NPARHI,CFL,IFL)
C***********************************************************************
C-
C-  Print out for PARH (particle header) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LPARHI= bank address 
C-  NPARHI= bank number
C-  CFL   = flag to control printout
C-        = 'HED' header only
C-  IFL   = flag to control printout (not used)
C-                                                 Serban Protopopescu 
C-                                                        Jan. 13, 1987 
C-   Updated  10-NOV-1990   Daria Zieminska  print out # of particle banks 
C-   Updated  20-FEB-1991   Daria Zieminska  print out roads 
C-   Updated   2-DEC-1991   Daria Zieminska  add PDIL (dileptons) 
C-   Updated  13-MAR-1992   Qizhong Li-Demarteau  if it is an old format 
C-                      PARH bank don't try to print outside of the bank
C-   Updated  20-AUG-1992   sss - compile on ibm
C***********************************************************************

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LPARH,NPARH,IFL,LPARHI,NPARHI,GZPARH
      INTEGER K,NFULL,NEMPTY,NROADS,KROAD,IROAD,I
      CHARACTER CFL*(*)
      INTEGER NBANKS
      PARAMETER (NBANKS=7)
      CHARACTER*4 BLIST(NBANKS),EMPTY(NBANKS),FULL(NBANKS)
      DATA BLIST/'PMUO','PELC','PPHO','PNUT','PVEE','PTAU','PDIL'/ 
C--------------------------------------------------------------------
C
C--   check if bank does exist
C
      LPARH=LPARHI
      NPARH=NPARHI
      IF(LPARH.EQ.0)THEN
        LPARH=GZPARH()
        IF(LPARH.LE.0) GOTO 99
      ENDIF
C
C--   Print header
C
      WRITE(PRUNIT,102) IQ(LPARH+1) 
C
      IF(CFL.EQ.'HED') GOTO 98
C
C     Give list of empty banks and banks with data
C
      NEMPTY=0
      NFULL=0
      DO 1 K=1,NBANKS
        IF(LQ(LPARH-K).EQ.0) THEN
          NEMPTY=NEMPTY+1
          EMPTY(NEMPTY)=BLIST(K)
        ELSE
          NFULL=NFULL+1
          FULL(NFULL)=BLIST(K)
        ENDIF
  1   CONTINUE  
C      
      IF(NEMPTY.GT.0) WRITE(PRUNIT,103) (EMPTY(K),K=1,NEMPTY)
      IF(NFULL.GT.0)  WRITE(PRUNIT,104) (FULL(K),K=1,NFULL)
C
      IF (IQ(LPARH-1) .LT. 10) THEN
        WRITE(PRUNIT,109) 
        GOTO 98
      ENDIF
C
      WRITE(PRUNIT,106) (IQ(LPARH+K),K=2,8)
C
      NROADS=IQ(LPARH+10)
      IF (NROADS.EQ.0) GO TO 98
      WRITE (PRUNIT,107)  
      DO 200 IROAD=1,NROADS
        KROAD=LPARH+10+5*(IROAD-1)
        WRITE (PRUNIT,108)  IROAD,(Q(KROAD+I), I=1,5)
 200  CONTINUE
C
  98  RETURN
  99  WRITE(PRUNIT,100)
      WRITE(PRUNIT,101) LPARH,NPARH
      RETURN
C
C
 100  FORMAT(//' ',57('*')/)
 101  FORMAT(/,' Wrong Address for a PARH bank: LPARH =',I8
     +,' NPARH =',I8/)
 102  FORMAT(/,
     +' ========================================================='/
     +'      PARH: Particle header bank     Version #',I3,/
     +' ========================================================='/)
 103  FORMAT('  Bank ',A4,' is EMPTY')
 104  FORMAT('  Bank ',A4,' has DATA')
 106  FORMAT(/,'   # PMUO  # PELC  # PPHO  # PNUT  # PVES  # PTAU',
     X'  # PDIL',/,7I8 )
C
 107  FORMAT('  ROAD      PHIMIN   PHIMAX   THEMIN   THEMAX     PT')
 108  FORMAT(1X,I4,3X,5F9.2)
 109  FORMAT(1X,
     & 'Old PARH bank has no # of particles and roads information')
      END
