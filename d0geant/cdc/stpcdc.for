      LOGICAL FUNCTION STPCDC
C----------------------------------------------------------------------
C
C   Purpose and Methods : Handles stepping through the central drift
C                         cells as defined by DETCDC.
C                         Stores the hits
C
C   Inputs  :
C   Outputs :
C-            HITSV(1)   X-GLOBAL incoming track
C-            HITSV(2)   Y-GLOBAL
C-            HITSV(3)   Z-GLOBAL
C-            HITSV(4)   X-GLOBAL outgoing track
C-            HITSV(5)   Y-GLOBAL
C-            HITSV(6)   Z-GLOBAL
C-            HITSV(7)   PULSE HEIGHT ( Integrated charge)
C-            HITSV(8)   Track length in the cell ( dx**2 + dy**2 + dz**2)
C-            HITSV(9)   Track id.=2**11*Secondary track #+Primary track#
C-            HITSV(10)  Empty
C
C   Created   FEB- 4-1986  K. Ng  ( Some statements are extracted
C                          from T. Trippe)
C             JAN-22-1987  K. Nishikawa: Changed to give "wire data".
C             MAR- 4-1987  M. Aufderheide: Simple determination of
C                          track lengths, drift times.
C             MAR-30-1987  G. Rahal-Callot: Change of drift times to
C                          local coordinate X, add of HITSV(6,7,8), and
C                          other changes.
C             MAI-04-1987  G. Rahal : new Landau from K. Nishikawa
C-   Updated  22-FEB-1988   Ghita Rahal-Callot  : Store only the global
C-                          coordinates , the pulse height, the track
C-                          length, and the track number.
C-   Updated  17-JUL-1989   Harrison B. Prosper  
C-   Made into a pbd interface function 
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC'
C
C
      REAL  HITSV(10)
      REAL  TRKLEG, PULPH
      REAL  VIN(7), VMID(6)
      INTEGER IHIT, I
C
C ****  Secondary track number bits = 2**11
C
      INTEGER  NSECON
C
      DATA VIN    /7*0.0/
      DATA NSECON/2048/
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER CDC
C----------------------------------------------------------------------
      STPCDC = .TRUE.
      IF ( DCDC .LT. 2 ) GOTO 999
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH('CDC ',CDC,4,4)
      ENDIF
C
      IF ( .NOT. (DCDC .GT. 1 .AND. IHSET .EQ. CDC) ) GOTO 999
C
C ****  No chamber hit if no charge
C
      IF(CHARGE.EQ.0) RETURN
C
C ****  Incident point in the sense wire CELL
C
      IF (INWVOL.EQ.1) THEN
        DO 10 I=1,7
          VIN(I)=VECT(I)
   10   CONTINUE
        TRKLEG =  SLENG
      END IF
C
C ****  Exit point : we can look at the DRIFT Distance
C
      IF ( INWVOL .NE. 2 ) GO TO 999
C
C ****  Calculate midpoint of the track in the cell
C
      CALL UCOPY ( VECT, VMID, 6 )
C
C ****  Calculate the track length
C
      TRKLEG = SLENG - TRKLEG
C
C ****  Landau distribution
C
      CALL CLANDA  (TRKLEG, PULPH)
C
C ****  Fill the hit banks
C
      CALL UCOPY ( VIN, HITSV, 3 )
      CALL UCOPY ( VMID, HITSV(4), 3)
      HITSV(7)= PULPH
      HITSV(8)= TRKLEG
C
C ****  Fill track id.=2**11*secondary track # + primary track #
C
      IF ( ISTAK .LE. 15 .AND. ITRA .LE. NSECON )  THEN
        HITSV(9) = ISTAK * NSECON + ITRA
      ELSE
        HITSV(9) =   16  * NSECON + ITRA
      ENDIF
C
C
      CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITSV,IHIT)
      IF ( IHIT .LE. 0 ) THEN
        WRITE ( LOUT, * ) '***** STPCDC: Problem in the ',
     &                    'storage of a hit'
      ENDIF
C
  999 CONTINUE
      RETURN
      END
