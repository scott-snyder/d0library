      SUBROUTINE GTCMAP (ICLUST,ICLASS,INEXT,LADDR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return information from the bank CMAP
C-   (Cluster MAP). See CMAP.ZEB for a full description.
C-
C-   Inputs  : ICLUST   [I]     Pre-cluster number (1...NCLUST)
C-   Outputs : ICLASS   [I]     CLASS number
C-             INEXT    [I]     NEXT number
C-             LADDR    [I]     Pre-cluster bank address
C-             IER      [I]     0 -- OK
C-                             -1 -- CMAP bank not found
C-                             -2 -- Cluster number out of range
C-   Controls:
C-
C-   Created   9-JAN-1990   Harrison B. Prosper
C-
C-    ENTRY GTCMAP_TOTAL(IVERS,NCLUST,IREP,ICLASS,INEXT,IER)
C-   Outputs : IVERS    [I]     Bank Version number
C-             NCACL    [I]     Number of pre-clusters
C-             IREPEAT  [I]     Repetition number
C-             ICLASS   [I]     CLASS offset
C-             INEXT    [I]     NEXT  offset
C-             IER      [I]     0 -- OK
C-                             -1 -- CMAP bank not found
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCACL,ICLUST,IVERS,NCLUST
      INTEGER IREPEAT,IREP,ICLASS,INEXT,LADDR,IER
C
      INTEGER GZCMAP
      INTEGER JPOINT,JCLASS,JNEXT,NS
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      IER = 0
C
C ****  Get address of bank CMAP
C
      IF ( LCMAP .LE. 0 ) THEN
        LCMAP = GZCMAP()
      ENDIF
      IF ( LCMAP .LE. 0 ) THEN
        IER =-1
        GOTO 999
      ENDIF
C
      NCLUST= IQ(LCMAP+2)               ! Number of pre-clusters
C
      IF ( (ICLUST .LT. 1 ) .OR. (ICLUST .GT. NCLUST) ) THEN
        IER =-2                         ! Invalid cluster number
      ELSE
        IREP   = IQ(LCMAP+3)               ! Repetition number
        JCLASS = IQ(LCMAP+4)               ! CLASS offset
        JNEXT  = IQ(LCMAP+5)               ! NEXT offset
        JPOINT = IREP*(ICLUST-1)
        NS     = IQ(LCMAP-2)          ! Number of structural links
        ICLASS = IQ(LCMAP+JPOINT+JCLASS)      ! CLASS number
        INEXT  = IQ(LCMAP+JPOINT+JNEXT)       ! NEXT number
        LADDR  = LQ(LCMAP-NS-ICLUST)  ! Address of cluster bank
      ENDIF
      RETURN
C
      ENTRY GTCMAP_TOTAL (IVERS,NCACL,IREPEAT,ICLASS,INEXT,IER)
C
      IER = 0
C
C ****  Get address of bank CMAP
C
      IF ( LCMAP .LE. 0 ) THEN
        LCMAP = GZCMAP()
      ENDIF
      IF ( LCMAP .LE. 0 ) THEN
        IER =-1
        GOTO 999
      ENDIF
C
      IVERS = IQ(LCMAP+1)               ! Version number
      NCACL = IQ(LCMAP+2)               ! Number of pre-clusters
      IREPEAT = IQ(LCMAP+3)               ! Repetition number
      ICLASS= IQ(LCMAP+4)               ! CLASS offset
      INEXT = IQ(LCMAP+5)               ! NEXT offset
  999 RETURN
      END
