      SUBROUTINE CL2_RESET_BIG_CELL_LIST(ET_MEMBER)
C----------------------------------------------------------------------
C-
C-    THIS ROUTINE CONTAINS SEVERAL ENTRY POINTS
C-      together, they manage a list of potential hot cells
C-
C----------------------------------------------------------------------
C-  CL2_RESET_BIG_CELL_LIST(ET_MEMBER)
C-
C-  Purpose and Methods: re-initialize the list 
C-
C-   Inputs  : none
C-   Outputs : ET_MEMBER  minimum Et for consideration as a member
C-
C----------------------------------------------------------------------
C-  CL2_GET_N_BIG_CELL(NBIG_OUT)
C-
C-  Purpose and Methods:  return current list length
C-
C-   Inputs  : none
C-   Outputs : NBIG_OUT length of current list
C-
C----------------------------------------------------------------------
C-  CL2_GET_BIG_CELL(I,ET,IETA,IPHI,LYR,IN_CAEP,IN_PNUT)
C-
C-  Purpose and Methods:  return one member of list
C-
C-   Inputs  : I  which member of the list to fetch
C-   Outputs : ET    Et of member
C-             IETA of member        0 if no such member
C-             IPHI of member
C-             LYR of member
C-             IN_CAEP  .FALSE. if member already removed from CAEP bank
C-             IN_PNUT  .FALSE. if member already removed from PNUT bank
C-
C----------------------------------------------------------------------
C-  CL2_SET_BIG_CELL(I,ET,IETA,IPHI,LYR,IN_CAEP,IN_PNUT)
C-
C-  Purpose and Methods:  modify one member of list
C-
C-  Inputs   : same arguments as for GET_BIG, except ALL are inputs
C-
C----------------------------------------------------------------------
C-  CL2_ADD_BIG_CELL(ET,IETA,IPHI,LYR,ET_MEMBER)

C-
C-  Purpose and Methods:  add new member to list, if it passes minimum Et cut
C-
C-    If the list is not full, simply add the new member at the end
C-    If the list is full, processing is slower:
C-        find the lowest Et current member
C-        raise the Et membership criterion to the Et of that member
C-        replace the previous lowest with the new member, IF it has higher Et
C-                
C-  Inputs  : ET    Et of potential member
C-            IETA, IPHI, LYR  coordinates of potential member
C-  Outputs : ET_MEMBER   current estimate of minimum Et for membership
C-
C-   Created  23-OCT-1992   James T. Linnemann
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CL2_BIG_CELLS.INC'
      REAL    ET,ET_MEMBER
      INTEGER NBIG_OUT,I,J,IN
      LOGICAL IN_CAEP,IN_PNUT
      INTEGER IETA,IPHI,LYR             ! offline indices after unpacking
C----------------------------------------------------------------------
C     ENTRY CL2_RESET_BIG_CELL_LIST(ET_MEMBER)
      NBIG = 0
      ETMIN_CURRENT = ETMIN_DEFAULT
      ET_MEMBER = ETMIN_DEFAULT
      RETURN
C----------------------------------------------------------------------
      ENTRY CL2_GET_N_BIG_CELL(NBIG_OUT)
      NBIG_OUT = NBIG             !might want to sort the list at this point
      RETURN
C----------------------------------------------------------------------
      ENTRY CL2_GET_BIG_CELL(I,ET,IETA,IPHI,LYR,IN_CAEP,IN_PNUT)
      IF (I.GE.1.AND.I.LE.NBIG) THEN
        ET = ET_BIG(I)
        IETA = IETA_BIG(I)
        IPHI = IPHI_BIG(I)
        LYR = LYR_BIG(I)
        IN_CAEP = IN_CAEP_BIG(I)
        IN_PNUT = IN_PNUT_BIG(I)
      ELSE
        IETA = 0                      !flag for bad or abandoned member
      ENDIF
      RETURN
C----------------------------------------------------------------------
      ENTRY CL2_SET_BIG_CELL(I,ET,IETA,IPHI,LYR,IN_CAEP,IN_PNUT)
      IF (I.GE.1.AND.I.LE.NBIG) THEN    !else do nothing
        ET_BIG(I) = ET
        IETA_BIG(I) = IETA
        IPHI_BIG(I) = IPHI
        LYR_BIG(I) = LYR
        IN_CAEP_BIG(I) = IN_CAEP
        IN_PNUT_BIG(I) = IN_PNUT
      ENDIF
      RETURN
C----------------------------------------------------------------------
      ENTRY CL2_ADD_BIG_CELL(ET,IETA,IPHI,LYR,ET_MEMBER)
C
C...find a place on the list
      IF (NBIG.LT.MAXBIG) THEN
        NBIG = NBIG + 1
        IN = NBIG
      ELSE
C
C...full list; locate lowest energy member as candidate insertion point (SLOW)
        IN = 1
c$$$        DO J = 2,MAXBIG ! avoid g77 warning; this is a null loop
c$$$          IF (ET_BIG(J).LT.ET_BIG(IN)) IN = J
c$$$        ENDDO
        ETMIN_CURRENT = ET_BIG(IN) !raise membership level to lowest existing Et
      ENDIF
C
C...if it qualifies for membership, insert new member
      ET_MEMBER = ETMIN_CURRENT
      IF (ET.GT.ETMIN_CURRENT) THEN
        ET_BIG(IN) = ET
        IETA_BIG(IN) = IETA
        IPHI_BIG(IN) = IPHI
        LYR_BIG(IN) = LYR
        IN_CAEP_BIG(IN) = .TRUE.
        IN_PNUT_BIG(IN) = .TRUE.
      ENDIF
      RETURN
      END
