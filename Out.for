      SUBROUTINE OUTEXF(LUSTOT,IATM,IEXFCT,LURSLT,EXFCSS,EXFCSG,EXFCGG,
     1                  EXFCGS,ACTZON,ACTVZN,NZONES,NVZONE,MXZN,MXVZN)
C
C  Routine to output the values of the exchange factors held in the EXF arrays
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER IATM,IEXFCT,LURSLT,MXZN,MXVZN,ACTZON(MXZN),ACTVZN(MXVZN),
     1        NZONES,NVZONE,LUSTOT
      REAL EXFCSS(MXZN,MXZN),EXFCSG(MXZN,MXVZN),EXFCGG(MXVZN,MXVZN),
     1     EXFCGS(MXVZN,MXZN)
      INTEGER I,J,IZNFRM
      CHARACTER EXTYPE*21
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ACTVZN  Array of actual volume zone numbers
C  ACTZON  Array of actual surface zone numbers
C  EXFCGG  Array of gas to gas exchange factors
C  EXFCGS  Array of gas to surface exchange factors
C  EXFCSG  Array of surface to gas exchange factors
C  EXFCSS  Array of surface to surface exchange factors
C  IATM    Number of the atmosphere
C  IEXFCT  Exchange factor option - 1 for view factors; 2 for dea; 3 for tea
C  LURSLT  Logical unit for results
C  LUSTOT  Logical unit number for standard output
C  MXVZN   Maximum number of volume zones allowed 
C  MXZN    Maximum number of surface zones allowed
C  NVZONE  Number of actual volume zones
C  NZONES  Number of actual surface zones (walls and obstacles)
C
C**** Arguments - output
C  NONE
C
C**** Local variables
C  EXTYPE  Character variable showing which type of exchange factor
C  I       Loop counter on sending zone
C  J       Loop counter on receiving zone
C  IZNFRM  Loop counter on sending wall surface zone number
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C**** Set exchange factor type
      IF (IEXFCT.EQ.1) THEN
         EXTYPE='VIEW FACTORS'
      ELSE IF (IEXFCT.EQ.2) THEN
         EXTYPE='DIRECT EXCHANGE AREAS'
      ELSE
         EXTYPE='TOTAL EXCHANGE AREAS'
      END IF

C**** Display message that exchange factors are being written to the file
      WRITE (LUSTOT,10) EXTYPE
10    FORMAT (/1X,A21,' being written to results file'/)

C**** First output the surface to surface exchange factors
      WRITE (LURSLT,90) IATM,EXTYPE
90    FORMAT (' ATMOSPHERE ',I2,' SURFACE TO SURFACE ',A21)
      DO 120 I=1,NZONES
         IZNFRM=ACTZON(I)
         WRITE (LURSLT,100) IZNFRM
100      FORMAT (' SENDING SURFACE ZONE ',I3)
         WRITE (LURSLT,110) (EXFCSS(IZNFRM,ACTZON(J)),J=1,NZONES)
110      FORMAT (6(1X,E11.6,1X))
c110      FORMAT (6(1X,G11.4,1X))
120   CONTINUE

C**** Second output the gas to surface exchange factors
C**** For atmosphere 1 all gas (to or from) exchange factors are zero
C**** so do not bother outputting any values
      IF (IATM.EQ.1) GOTO 250
      WRITE (LURSLT,130) IATM,EXTYPE
130   FORMAT (' ATMOSPHERE ',I2,' SURFACE TO GAS ',A21)
      DO 160 I=1,NZONES
         IZNFRM=ACTZON(I)
         WRITE (LURSLT,140) IZNFRM
140      FORMAT (' SENDING SURFACE ZONE ',I3)
         WRITE (LURSLT,150) (EXFCSG(IZNFRM,ACTVZN(J)),J=1,NVZONE)
150      FORMAT (6(1X,E11.6,1X))
c150      FORMAT (6(1X,G11.4,1X))
160   CONTINUE

C**** Third output the gas to gas exchange factors
      WRITE (LURSLT,170) IATM,EXTYPE
170   FORMAT (' ATMOSPHERE ',I2,' GAS TO GAS ',A21)
      DO 200 I=1,NVZONE
         IZNFRM=ACTVZN(I)
         WRITE (LURSLT,180) IZNFRM
180      FORMAT (' SENDING GAS ZONE ',I3)
         WRITE (LURSLT,190) (EXFCGG(IZNFRM,ACTVZN(J)),J=1,NVZONE)
190      FORMAT (6(1X,E11.6,1X))
c190      FORMAT (6(1X,G11.4,1X))
200   CONTINUE

C**** Finally output the gas to surface exchange factors
      WRITE (LURSLT,210) IATM,EXTYPE
210   FORMAT (' ATMOSPHERE ',I2,' GAS TO SURFACE ',A21)
      DO 240 I=1,NVZONE
         IZNFRM=ACTVZN(I)
         WRITE (LURSLT,220) IZNFRM
220      FORMAT (' SENDING GAS ZONE ',I3)
         WRITE (LURSLT,230) (EXFCGS(IZNFRM,ACTZON(J)),J=1,NZONES)
230      FORMAT (6(1X,E11.6,1X))
c230      FORMAT (6(1X,G11.4,1X))
240   CONTINUE

C**** Jump down to here if the gas is clear (ie atmosphere 1)
250   CONTINUE

      RETURN
      END

