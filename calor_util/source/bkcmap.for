      SUBROUTINE BKCMAP(NCLUST,LCMAP1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CMAP.
C-   This bank describes the mapping of calorimeter pre-clusters into 
C-   super-clusters. It is designed to be used with the Saul Youssef
C-   algorithm.
C-   
C-
C-   Inputs  : NCLUST   [I]     Number of pre-clusters
C-   Outputs : LCMAP1   [I]     Address of Booked CMAP Bank
C-   Controls: None
C-
C-   Created   9-JAN-1990  Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NCLUST
      INTEGER LCMAP1
C
      INTEGER NL,NS,ND,IXIO,NZERO
      INTEGER IVERS,IBASE,IREP,ICLASS,INEXT
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
C ****  Calorimeter Link area (contains LCMAP)
C
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST,IVERS,IBASE,IREP,ICLASS,INEXT
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCMAP = 0
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('CMAP','-I',IXIO)   ! Describe Bank format
C
        IVERS = 1                       ! Version number
        IREP  = 2                       ! Repetition number
        IBASE = 5                       ! Fixed number of words
        ICLASS= IBASE + 1               ! CLASS offset
        INEXT = IBASE + 2               ! NEXT offset
      ENDIF
C
C ****  Build a stand-alone bank
C
      NS  =  1                          ! Number of structural links
      NL  =  NCLUST + NS                ! Total number of links
      ND  =  IBASE + NCLUST*IREP        ! Bank length
      NZERO = 0                         ! Preset words to zero
      CALL MZBOOK (IXMAIN,LCMAP,0,2,'CMAP',NL,NS,ND,IXIO,NZERO)
      LCMAP1 = LCMAP
C
C ****  Fill first IBASE words
C
      IQ(LCMAP+1) = IVERS               ! Version number
      IQ(LCMAP+2) = NCLUST              ! Number of pre-clusters
      IQ(LCMAP+3) = IREP                ! Repetition number
      IQ(LCMAP+4) = ICLASS              ! CLASS offset
      IQ(LCMAP+5) = INEXT               ! NEXT offset
C
  999 RETURN
      END
