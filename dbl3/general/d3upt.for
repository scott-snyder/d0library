C----------------------------------------------------------------------
      LOGICAL FUNCTION D3UPT (VTIM,KTIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will convert a VMS system time to a DBL3
C-    packed time
C-
C-   Returned value  : .true.   it did what you asked for
C-                     .false.  somthing went wrong
C-
C-   Inputs  : VTIM  (2*I*4) VMS system time (64 bits)
C-   Outputs : KTIM  (I*4)   DBL3 packed time
C-   Controls: 
C-
C-   Created  23-OCT-1992   Lars Rasmussen
C-   Mod      24-AUG-1994   Will save last convertions, l0r
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL SYS$NUMTIM,SYS$BINTIM
      LOGICAL D3UUT
C
      INTEGER *4 VTIM(2),KTIM
      INTEGER *4 VTIM_S(2),KTIM_S
      INTEGER *4 YE,MO,DA,HO,MI,SE,IDAT,ITIM
      INTEGER *2 NTIM(7)
      CHARACTER*20 STIM
      CHARACTER*3 MONT(12)
      DATA MONT/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &  'SEP','OCT','NOV','DEC'/
      DATA VTIM_S /0,0/
      DATA KTIM_S /0/
C----------------------------------------------------------------------
      D3UPT = .FALSE.
      IF ( VTIM(1) .EQ. 0 ) RETURN
      IF ( VTIM(1) .EQ. VTIM_S(1) .AND. VTIM(2) .EQ. VTIM_S(2) ) THEN
        D3UPT = .TRUE.
        KTIM = KTIM_S
        RETURN
      END IF
C
      IF ( .NOT. SYS$NUMTIM( NTIM,VTIM ) ) RETURN
      YE = ((NTIM(1) - (NTIM(1)/100)*100))
      MO = NTIM(2)
      DA = NTIM(3)
      HO = NTIM(4)
      MI = NTIM(5)
      SE = NTIM(6)
      IDAT = YE*10000 + MO*100 + DA
      ITIM = HO*10000 + MI*100 + SE
      CALL D0DBL3_DBPKTS(IDAT,ITIM,KTIM)
      D3UPT = .TRUE.
C
      GOTO 991
C----------------------------------------------------------------------
      ENTRY D3UUT (VTIM,KTIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will convert a DBL3 packed time to a VMS 
C-    system time
C-
C-   Returned value  : .true.   it did what you asked for
C-                     .false.  somthing went wrong
C-
C-   Inputs  :  KTIM  (I*4)    DBL3 packed time
C-   Outputs :  VTIM  (2*I*4)  VMS system time
C-   Controls: 
C-
C-   Created  23-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      D3UUT = .FALSE.
      IF ( KTIM .EQ. 0 ) RETURN
      IF ( KTIM .EQ. KTIM_S ) THEN
         D3UUT = .TRUE.
         VTIM(1) = VTIM_S(1)
         VTIM(2) = VTIM_S(2)
         RETURN
      END IF
C
      CALL D0DBL3_DBUPTS(IDAT,ITIM,KTIM)
      YE = IDAT/10000
      MO = IDAT/100 - YE*100
      DA = IDAT - YE*10000 - MO*100
      HO = ITIM/10000
      MI = ITIM/100 - HO*100
      SE = ITIM - HO*10000 - MI*100
      YE = YE + 1900
C
      WRITE (STIM,801,ERR=999) DA,MONT(MO),YE,HO,MI,SE
801   FORMAT (I2.2,'-',A3,'-',I4.4,' ',I2.2,':',I2.2,':',I2.2)
      IF (.NOT. SYS$BINTIM(STIM,VTIM)) RETURN
      D3UUT = .TRUE.
C
991   VTIM_S(1) = VTIM(1)
      VTIM_S(2) = VTIM(2)
      KTIM_S = KTIM
C
999   RETURN
      END
