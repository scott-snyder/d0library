      SUBROUTINE CEMCLS(LCASH,ENDPTH,ETOT,EMAX,ETAMX,PHIMX,ILR3MX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Work out the longitudinal and partial
C-                         transverse profile of EM showers
C-
C-   Inputs  : LCASH     = Pointer to the calorimeter cluster cash bank
C-   Outputs : ENDPTH(4) = Energy in each depth
C-             ETOT      = Total Energy
C-             EMAX(4)   = Highest energy hit in each layer
C-             ETAMX(4)  = eta of highest energy hit in each layer
C-             PHIMX(4)  = phi of highest energy hit in each layer
C-             ILR3MX    = Layer number for highest energy cell in EM3
C-   Controls: None
C-
C-   Created  15-MAY-1989   Rajendran Raja
C-   Modified 28-Mar-1990   N.A. Graf 
C-                          Now returns amount of energy in cluster outside
C-                          central tower
C-   Updated  23-OCT-1992   Meenakshi Narain   optionally use CASH banks
C-   Updated  27-OCT-1992   Meenakshi Narain/Natalie Roe 
C-                          add ETAMX ==> eta of maximum energy hit in each
C-                          layer
C-   Updated  19-APR-1995   Gregory L. Landsberg, modified from original 
C-                          CEMENR routine
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ETAMX(4),PHIMX(4), ILR3MX, LCASH
      REAL    ENDPTH(4),EMAX(4),ETOT
      REAL    ECELL
      INTEGER INUM,POINTER,ILYR,IDEPTH
      INTEGER NCELLS,ETA,PHI,PACKED_WORD,NCELL
      Common /Cells/ NCELL(4)
C----------------------------------------------------------------------
      ETOT = 0.
      CALL UZERO(ENDPTH,1,4)
      CALL UZERO(EMAX,1,4)
      CALL vZERO_i(ETAMX,4)
      CALL vZERO_i(PHIMX,4)
      CALL vZERO_i(NCELL,4)
      ILR3MX = 0
C
C ****  GET POINTERS AND ETA OF CENTRAL TOWER
C
      NCELLS = IQ(LCASH+2)
      DO 20 INUM = 1,NCELLS
        POINTER = LCASH + 2*(INUM-1)
        PACKED_WORD = IQ(POINTER+3)
        ECELL = Q(POINTER+4)
        CALL CAEP_INDICES(PACKED_WORD,ETA,PHI,ILYR)
C
        IF (ILYR.LT.LYEM3A) Then
          IDEPTH = ILYR
        Else If(ILYR.GE.LYEM3A.AND.ILYR.LE.LYEM3D) Then
          IDEPTH = 3
        Else IF(ILYR.LE.MXLYEM) Then
          IDEPTH = 4
        Else
          Go To 20
        End If
        NCELL(IDEPTH) = NCELL(IDEPTH)+1
C
        ENDPTH(IDEPTH) = ENDPTH(IDEPTH) + ECELL ! Layer totals
        IF(ECELL.GT.EMAX(IDEPTH))THEN    !  New maximum energy
          EMAX(IDEPTH)=ECELL
          ETAMX(IDEPTH)=ETA
          PHIMX(IDEPTH)=PHI
          If (IDEPTH .eq. 3) ILR3MX = ILYR
        ENDIF
        ETOT = ETOT + ECELL             ! Sum total
   20 CONTINUE
C
  999 RETURN
      END
