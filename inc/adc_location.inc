      INTEGER NCALADC
      PARAMETER (NCALADC = 12)          ! number of calorimeter ADC's
C-
      INTEGER ADC_POINT,CRATE_POINT,CRATE_CAD
      INTEGER CL2CAD_EVT           
C-
      COMMON /ADC_LOCATION/ADC_POINT(0:(NADCC-1),NCALADC),
     & CRATE_POINT(NCALADC),CRATE_CAD(NCALADC),CL2CAD_EVT
C-
C-              /ADC_LOCATION/ADC_POINT(I,J),CRATE_LOCATION(J) where
C-                      I = 0...11      ADC card number
C-                      J = 1...12      CL2 ADC crate number
C-                                      (see /CL2CRATE/)
C- The pointer points to the 0th data word of the object
C- CRATE_CAD = 0 if crate never found, 1 if on CAD1, 2 if on CAD2
C-              
C-              CL2CAD_EVT is the event for which these pointers are valid
C-
