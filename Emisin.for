      SUBROUTINE EMISIN(INDATA,LUSTIN,LUSTOT,LU,LUSAVE,NCGCX,NCGCY,
     1                  NCGCZ,NSURF,ZNAREA,MXZN,CGX,CGY,CGZ,GX,GY,GZ,
     2                  NVMAX,NZMAX,EMISS,FILE1,FILE2)

C  Routine to determine the surface zone emissivities
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**04/04/91
C
C
      INTEGER INDATA,LUSTIN,LUSTOT,LU,LUSAVE,NCGCX,NCGCY,NCGCZ,
     1        NSURF,MXZN,NVMAX,NZMAX,CGX(NVMAX),CGY(NVMAX),CGZ(NVMAX)
      REAL ZNAREA(MXZN),GX(NVMAX),GY(NVMAX),GZ(NZMAX),EMISS(MXZN)
      CHARACTER*30 FILE1,FILE2,FILE
      INTEGER INOPT,NEWOLD,LUFL,IEMOPT,LUIN
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array giving fine x grid line numbers of the coarse x grid lines
C  CGY     Array giving fine y grid line numbers of the coarse y grid lines
C  CGZ     Array giving fine z grid line numbers of the coarse z grid lines
C  EMISS   Array to hold the surface emissivities
C  FILE1   Name of file from which emissivity data is read
C  GX      Array of fine x grid line co-ordinates
C  GY      Array of fine y grid line co-ordinates
C  GZ      Array of fine z grid line co-ordinates
C  INDATA  Integer flag showing mode of data entry (1=interactive,
C          2=quasi-interactive, 3=non-interactive)
C  LU      Logical unit for input data file
C  LUSAVE  Logical unit for created input data file
C  LUSTIN  Logical unit for standard input
C  LUSTOT  Logical unit for standard output
C  MXZN    Maximum number of surface zones permitted
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NSURF   Number of surface zones
C  NVMAX   Maximum number of vertices permitted
C  NZMAX   Maximum number of slices permitted
C  ZNAREA  Array of surface zone areas
C
C**** Arguments - output
C  EMISS   Array of surface zone emissivities
C  FILE1   Set to 'NOT USED' if data is from the terminal
C  FILE2   Filename for created emissivity data file 
C
C**** Local variables
C  FILE    Name of the file actually used in the input process
C          it may be read from or written to
C  IEMOPT  Emissivity input option (1-uniform,2-5 values,3-zone by zone,
C          4-start with option 2 then change individual values)
C  INOPT   Input option (1=terminal, 2=file)
C  LUFL    Actual logical unit number for FILE
C  LUIN    Logical unit from which this subroutine reads
C  NEWOLD  Integer flag showing if the file is being read from (0=old)
C          or being written to (1=new)
C  
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  CLEAR   Clear the screen
C  EMOPT1  Set uniform emissivity values
C  EMOPT2  Set emissivites using 5 basic values
C  EMOPT3  Set emissivities zone by zone
C  EMOPT4  Set emissivities using 5 values then change individual
C          zone values
C  PRSRTC  Display message 'Press return to continue'
C
C******************************************************************************

C**** Give introductory message
      CALL CLEAR(LUSTOT)
      WRITE (LUSTOT,10)
10    FORMAT (' SURFACE EMISSIVITY DEFINITION PHASE'/
     1        ' ==================================='//)

C**** Determine whether input is from the terminal or a file
      IF (INDATA.EQ.1) THEN
C****    Data is from the terminal
         NEWOLD=1
         LUIN=LUSTIN
         LUFL=LUSAVE
      ELSE IF (INDATA.EQ.2) THEN
C****    Data may be from the terminal or a file find out which
20       WRITE (LUSTOT,30)
30       FORMAT (/' Emissivity data may come from: '/
     1            '      1.  The terminal'/
     2            '      2.  A file'/
     3            ' Enter option number (1 or 2) >> ',$)
         READ (LUSTIN,*) INOPT
         IF (INOPT.EQ.1) THEN
C****       Data is from the terminal
            NEWOLD=1
            LUIN=LUSTIN
            LUFL=LUSAVE
         ELSE IF (INOPT.EQ.2) THEN
C****       Data is from a file
            NEWOLD=0
            LUIN=LU
            LUFL=LU
         ELSE
C****       Illegal input option
            WRITE (LUSTOT,40)
40          FORMAT (/' INVALID RESPONSE'/' Answer must be 1 or 2'/)
            GOTO 20
         END IF
      ELSE
C****    Data is from a file
         NEWOLD=0
         LUIN=LU
         LUFL=LU
      END IF

C**** Open the appropriate file
      CALL OPENFL(LUSTIN,LUSTOT,LUFL,FILE,3,NEWOLD)

C**** Determine the data input option
50    IF (NEWOLD.EQ.1) WRITE (LUSTOT,60)
60    FORMAT (/' There are three basic emissivity options'/
     1         '      1. All emissivities the same'/
     2         '      2. Emissivities specified by 5 values, one each'/
     3         '         for hearth, walls, roof, load and source'/
     4         '      3. Emissivities entered on a zone-by-zone basis'/
     5         ' There is a further option available'/
     6         '      4. Begin with option 2 then change nominated',
     7                 ' individual values'/
     8         ' Enter option number (1-4) >> ',$)
      READ (LUIN,*) IEMOPT

C**** Process the requested input option
      IF ((IEMOPT.LT.1).OR.(IEMOPT.GT.4)) THEN
C****    Invalid input option
         IF (NEWOLD.EQ.1) THEN
            WRITE (LUSTOT,70)
70          FORMAT (/' INVALID RESPONSE'/
     1               ' Answer must be in range 1-4'/)
            GOTO 50
         ELSE
            WRITE (LUSTOT,80)
80          FORMAT (/' ILLEGAL EMISSIVITY INPUT OPTION REQUESTED'/
     1               ' RUN ABORTED'/)
            STOP
         END IF
      ELSE IF (IEMOPT.EQ.1) THEN
C****    Uniform emissivity
         CALL EMOPT1(LUSTIN,LUSTOT,LUFL,NEWOLD,NSURF,ZNAREA,MXZN,EMISS)
      ELSE IF (IEMOPT.EQ.2) THEN
C****    5 basic values
         CALL EMOPT2(LUSTIN,LUSTOT,LUFL,NEWOLD,NCGCX,NCGCY,NCGCZ,NSURF,
     1               ZNAREA,MXZN,EMISS)
      ELSE IF (IEMOPT.EQ.3) THEN
C****    Zone by zone values
         CALL EMOPT3(LUSTIN,LUSTOT,LUFL,NEWOLD,NCGCX,NCGCY,NCGCZ,NSURF,
     1               ZNAREA,MXZN,CGX,CGY,CGZ,GX,GY,GZ,NVMAX,NZMAX,EMISS)
      ELSE IF (IEMOPT.EQ.4) THEN
C****    Option 2 followed by changing individual zone values
         CALL EMOPT4(LUSTIN,LUSTOT,LUFL,NEWOLD,NCGCX,NCGCY,NCGCZ,NSURF,
     1               ZNAREA,MXZN,EMISS)
      END IF

C**** Close the file that has been used with the input
      CLOSE(LUFL)

C**** Set the file names
      IF (NEWOLD.EQ.1) THEN
         FILE1='NOT USED'
         FILE2=FILE
      ELSE
         FILE1=FILE
         FILE2='NOT USED'
      END IF 

C**** Surface emissivity data entry is complete - give message
      WRITE (LUSTOT,90)
90    FORMAT (//' End of surface emissivity defintion phase'//)
      CALL PRSRTC(LUSTIN,LUSTOT)

      RETURN
      END

      SUBROUTINE EMOPT1(LUSTIN,LUSTOT,LUFL,NEWOLD,NSURF,ZNAREA,MXZN,
     1                  EMISS)

C  Routine to set surface zones emissivities to a single value
C
C*****VERSION 1.0********D.A.LAWSON, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUSTIN,LUSTOT,LUFL,NEWOLD,NSURF,MXZN
      REAL ZNAREA(MXZN),EMISS(MXZN)
      INTEGER ISURF,LUIN
      REAL EMISSG
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  EMISS   Array to hold the zone emissivities
C  LUFL    Logical unit of the file used durig input
C  LUSTIN  Logical unit for standard input
C  LUSTOT  Logical unit for standard output
C  MXZN    Maximum number of surface zones permitted
C  NEWOLD  Integer flag showing if input is new (1-ie from terminal)
C          or old (0-ie from a file)
C  NSURF   Number of surface zones
C  ZNAREA  Array of surface zone areas
C
C**** Arguments - output
C  EMISS   Array of zone emissivities
C
C**** Local variables
C  EMISSG  Value of the global emissivity
C  ISURF   Loop counter on the surface zones
C  LUIN    Logical unit number for input
C
C**** Local Arrays
C  NONE
C 
C**** Functions and Subroutines
C  NONE
C
C*****************************************************************************

C**** Determine the unit number for input
      IF (NEWOLD.EQ.1) THEN
         LUIN=LUSTIN
         WRITE (LUSTOT,10)
10       FORMAT (/' Enter the global emissivity value >> ',$)
      ELSE
         LUIN=LUFL
      END IF

C**** Read the global value
      READ (LUIN,*) EMISSG

C**** If input is from the terminal save it
      IF (NEWOLD.EQ.1) THEN
         WRITE (LUFL,20)
20       FORMAT (1X,'1',38X,'EMISSIVITY INPUT OPTION')
         WRITE (LUFL,30) EMISSG
30       FORMAT (1X,G13.6,26X,'GLOBAL EMISSIVITY')
      END IF

C**** Set the emissivity array
      DO 40 ISURF=1,NSURF
         IF (ZNAREA(ISURF).GT.0) EMISS(ISURF)=EMISSG
40    CONTINUE

      RETURN
      END

      SUBROUTINE EMOPT2(LUSTIN,LUSTOT,LUFL,NEWOLD,NCGCX,NCGCY,NCGCZ,
     1                  NSURF,ZNAREA,MXZN,EMISS)

C  Routine to set surface zones emissivities using 5 values
C
C*****VERSION 1.0********D.A.LAWSON, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUSTIN,LUSTOT,LUFL,NEWOLD,NSURF,MXZN,NCGCX,NCGCY,NCGCZ
      REAL ZNAREA(MXZN),EMISS(MXZN)
      INTEGER ISURF,LUIN,ICGX,ICGY,ICGZ,IZONE,NENDZN,NENCZN,NOBSZN
      REAL EMISSH,EMISSW,EMISSR,EMISSL,EMISSS
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  EMISS   Array to hold the zone emissivities
C  LUFL    Logical unit of the file used durig input
C  LUSTIN  Logical unit for standard input
C  LUSTOT  Logical unit for standard output
C  MXZN    Maximum number of surface zones permitted
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NEWOLD  Integer flag showing if input is old (0 - ie from a file)
C          or new (ie from a file - value 1 or 2 - 2 when called from EMOPT4)
C  NSURF   Number of surface zones
C  ZNAREA  Array of surface zone areas
C
C**** Arguments - output
C  EMISS   Array of zone emissivities
C
C**** Local variables
C  EMISSH  Value of the hearth emissivity
C  EMISSL  Value of the load emissivity
C  EMISSR  Value of the roof emissivity
C  EMISSS  Value of the source emissivity
C  EMISSW  Value of the wall emissivity
C  ICGX    Loop counter on coarse grid x cells
C  ICGY    Loop counter on coarse grid y cells
C  ICGZ    Loop counter on coarse grid z cells
C  ISURF   Loop counter on the surface zones
C  IZONE   Loop counter on surface zones
C  LUIN    Logical unit number for input
C  NENCZN  Number of zones on the enclosure boundary
C  NENDZN  Number of zones on an end wall (ie x=0 or x=xmax)
C  NOBSZN  Number of obstacle zones
C
C**** Local Arrays
C  NONE
C 
C**** Functions and Subroutines
C  NONE
C
C*****************************************************************************

C**** Determine the unit number for input
      IF (NEWOLD.GE.1) THEN
         LUIN=LUSTIN
      ELSE
         LUIN=LUFL
      END IF

C**** Read the 5 basic values
      IF (NEWOLD.GE.1) WRITE (LUSTOT,10)
10    FORMAT (/' Enter the hearth emissivity >> ',$)
      READ (LUIN,*) EMISSH
      IF (NEWOLD.GE.1) WRITE (LUSTOT,20)
20    FORMAT (/' Enter the wall emissivity >> ',$)
      READ (LUIN,*) EMISSW
      IF (NEWOLD.GE.1) WRITE (LUSTOT,30)
30    FORMAT (/' Enter the roof emissivity >> ',$)
      READ (LUIN,*) EMISSR
      IF (NEWOLD.GE.1) WRITE (LUSTOT,40)
40    FORMAT (/' Enter the load emissivity >> ',$)
      READ (LUIN,*) EMISSL
      IF (NEWOLD.GE.1) WRITE (LUSTOT,50)
50    FORMAT (/' Enter the source emissivity >> ',$)
      READ (LUIN,*) EMISSS

C**** If input is from the terminal save it
      IF (NEWOLD.EQ.1) WRITE (LUFL,60)
60    FORMAT (1X,'2',38X,'EMISSIVITY INPUT OPTION')
      IF (NEWOLD.GE.1) THEN
         WRITE (LUFL,70) EMISSH
70       FORMAT (1X,G13.6,26X,'HEARTH EMISSIVITY')
         WRITE (LUFL,80) EMISSW
80       FORMAT (1X,G13.6,26X,'WALL EMISSIVITY')
         WRITE (LUFL,90) EMISSR
90       FORMAT (1X,G13.6,26X,'ROOF EMISSIVITY')
         WRITE (LUFL,100) EMISSL
100      FORMAT (1X,G13.6,26X,'LOAD EMISSIVITY')
         WRITE (LUFL,110) EMISSS
110      FORMAT (1X,G13.6,26X,'SOURCE EMISSIVITY')
      END IF

C**** Set the emissivity array
C**** Start with the wall x=0
      NENDZN=NCGCY*NCGCZ
      DO 120 ISURF=1,NENDZN
         IF (ZNAREA(ISURF).GT.0.) EMISS(ISURF)=EMISSW
120   CONTINUE
C**** Now do front and rear walls and hearth and roof
      ISURF=NENDZN
      DO 160 ICGX=1,NCGCX
         DO 130 ICGY=1,NCGCY
C****       Front wall (z=0)
            ISURF=ISURF+1
            IF (ZNAREA(ISURF).GT.0.) EMISS(ISURF)=EMISSW
C****       Rear wall (z=zmax)
            ISURF=ISURF+1
            IF (ZNAREA(ISURF).GT.0.) EMISS(ISURF)=EMISSW
130      CONTINUE
         DO 140 ICGZ=1,NCGCZ
C****       Hearth
            ISURF=ISURF+1
            IF (ZNAREA(ISURF).GT.0.) EMISS(ISURF)=EMISSH
140      CONTINUE
         DO 150 ICGZ=1,NCGCZ
C****       Roof
            ISURF=ISURF+1
            IF (ZNAREA(ISURF).GT.0.) EMISS(ISURF)=EMISSR
150      CONTINUE
160   CONTINUE
C**** Now do end wall x=xmax
      DO 170 IZONE=1,NENDZN
         ISURF=ISURF+1
         IF (ZNAREA(ISURF).GT.0.) EMISS(ISURF)=EMISSW
170   CONTINUE
C**** Now do obstacles
      NENCZN=2*(NCGCX*NCGCY+NCGCY*NCGCZ+NCGCZ*NCGCX)
      IF (ISURF.NE.NENCZN) THEN
         WRITE (LUSTOT,180)
180      FORMAT (/' COUNTER IS OUT OF STEP IN EMOPT2'/
     1            ' RUN ABORTED'/)
         STOP
      END IF
      NOBSZN=2*NCGCX*NCGCY*NCGCZ
      DO 190 IZONE=1,NOBSZN/2
C****    Loads
         ISURF=ISURF+1
         IF (ZNAREA(ISURF).GT.0.) EMISS(ISURF)=EMISSL
C****    Sources
         ISURF=ISURF+1
         IF (ZNAREA(ISURF).GT.0.) EMISS(ISURF)=EMISSS
190   CONTINUE

      RETURN
      END

      SUBROUTINE EMOPT3(LUSTIN,LUSTOT,LUFL,NEWOLD,NCGCX,NCGCY,NCGCZ,
     1                  NSURF,ZNAREA,MXZN,CGX,CGY,CGZ,GX,GY,GZ,NVMAX,
     2                  NZMAX,EMISS)

C  Routine to set surface zones emissivities on a zone by zone basis
C
C*****VERSION 1.0********D.A.LAWSON, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUSTIN,LUSTOT,LUFL,NEWOLD,NSURF,MXZN,NCGCX,NCGCY,NCGCZ,
     1        NVMAX,NZMAX,CGX(NVMAX),CGY(NVMAX),CGZ(NZMAX)
      REAL ZNAREA(MXZN),EMISS(MXZN),GX(NVMAX),GY(NVMAX),GZ(NZMAX)
      INTEGER ISURF,ICGX,ICGY,ICGZ,NENDZN,NENCZN,ISURF1,NOBST
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX     Array giving fine x grid line numbers of the coarse x grid lines
C  CGY     Array giving fine y grid line numbers of the coarse y grid lines
C  CGZ     Array giving fine z grid line numbers of the coarse z grid lines
C  EMISS   Array to hold the zone emissivities
C  GX      Array of fine x grid line co-ordinates
C  GY      Array of fine y grid line co-ordinates
C  GZ      Array of fine z grid line co-ordinates
C  LUFL    Logical unit of the file used during input
C  LUSTIN  Logical unit for standard input
C  LUSTOT  Logical unit for standard output
C  MXZN    Maximum number of surface zones permitted
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NEWOLD  Integer flag showing if input is new (1-ie from terminal)
C          or old (0-ie from a file)
C  NSURF   Number of surface zones
C  NVMAX   Maximum number of vertices allowed
C  NZMAX   Maximum number of z slices allowed
C  ZNAREA  Array of surface zone areas
C
C**** Arguments - output
C  EMISS   Array of zone emissivities
C
C**** Local variables
C  ICGX    Loop counter on coarse grid x cells
C  ICGY    Loop counter on coarse grid y cells
C  ICGZ    Loop counter on coarse grid z cells
C  ISURF   Current surface zone number
C  ISURF1  ISURF+1 (ie next surface zone number)
C  NENCZN  Number of zones on the enclosure boundary
C  NENDZN  Number of zones on an end wall (ie x=0 or x=xmax)
C
C**** Local Arrays
C  NONE
C 
C**** Functions and Subroutines
C  NONE
C
C*****************************************************************************

      IF (NEWOLD.EQ.1) THEN

C****    Data is from the terminal
         NENDZN=NCGCY*NCGCZ
         NENCZN=2*(NCGCX*NCGCY+NCGCY*NCGCZ+NCGCZ*NCGCX)
C****    Start with wall x=0
         WRITE (LUSTOT,10)
10       FORMAT (/' SET EMISSIVITIES FOR LEFT HAND WALL (ie x=0)'/)
         DO 60 ICGY=1,NCGCY
            DO 50 ICGZ=1,NCGCZ
               ISURF=(ICGY-1)*NCGCZ+ICGZ
               IF (ZNAREA(ISURF).LE.0.0) GOTO 50
               WRITE (LUSTOT,20) GY(CGY(ICGY)),GY(CGY(ICGY+1)),
     1                           GZ(CGZ(ICGZ)),GZ(CGZ(ICGZ+1))
20             FORMAT (' Wall between y= ',G11.4,' and y= ',G11.4/
     1                 '  and between z= ',G11.4,' and z= ',G11.4/
     2         ' Enter emissivity for this part of the wall >> ',$)
               READ (LUSTIN,*) EMISS(ISURF)
50          CONTINUE
60       CONTINUE
C****    Now do wall z=0
         WRITE (LUSTOT,70)
70       FORMAT (/' SET EMISSIVITIES FOR WALL z=0')
         DO 100 ICGX=1,NCGCX
            DO 90 ICGY=1,NCGCY
               ISURF=NENDZN+(ICGX-1)*2*(NCGCY+NCGCZ)+2*(ICGY-1)+1
               IF (ZNAREA(ISURF).LE.0.0) GOTO 90
               WRITE (LUSTOT,80) GX(CGX(ICGX)),GX(CGX(ICGX+1)),
     1                           GY(CGY(ICGY)),GY(CGY(ICGY+1))
80             FORMAT (' Wall between x= ',G11.4,' and x= ',G11.4/
     1                 '  and between y= ',G11.4,' and y= ',G11.4/
     2         ' Enter emissivity for this part of the wall >> ',$)
               READ (LUSTIN,*) EMISS(ISURF)
90          CONTINUE
100      CONTINUE
C****    Now do wall z=zmax
         WRITE (LUSTOT,110) GZ(CGZ(NCGCZ+1))
110      FORMAT (/' SET EMISSIVITIES FOR WALL z=',G11.4)
         DO 140 ICGX=1,NCGCX
            DO 130 ICGY=1,NCGCY
               ISURF=NENDZN+(ICGX-1)*2*(NCGCY+NCGCZ)+2*ICGY
               IF (ZNAREA(ISURF).LE.0.0) GOTO 130
               WRITE (LUSTOT,120) GX(CGX(ICGX)),GX(CGX(ICGX+1)),
     1                            GY(CGY(ICGY)),GY(CGY(ICGY+1))
120            FORMAT (' Wall between x= ',G11.4,' and x= ',G11.4/
     1                 '  and between y= ',G11.4,' and y= ',G11.4/
     2         ' Enter emissivity for this part of the wall >> ',$)
               READ (LUSTIN,*) EMISS(ISURF)
130         CONTINUE
140      CONTINUE
C****    Now do hearth
         WRITE (LUSTOT,150)
150      FORMAT (/' SET HEARTH EMISSIVITIES')
         DO 180 ICGX=1,NCGCX
            DO 170 ICGZ=1,NCGCZ
               ISURF=NENDZN+(ICGX-1)*2*(NCGCY+NCGCZ)+2*NCGCY+ICGZ
               IF (ZNAREA(ISURF).LE.0.0) GOTO 170
               WRITE (LUSTOT,160) GX(CGX(ICGX)),GX(CGX(ICGX+1)),
     1                            GZ(CGZ(ICGZ)),GZ(CGZ(ICGZ+1))
160            FORMAT (' Wall between x= ',G11.4,' and x= ',G11.4/
     1                 '  and between z= ',G11.4,' and z= ',G11.4/
     2         ' Enter emissivity for this part of the wall >> ',$)
               READ (LUSTIN,*) EMISS(ISURF)
170         CONTINUE
180      CONTINUE
C****    Now do roof
         WRITE (LUSTOT,190)
190      FORMAT (/' SET ROOF EMISSIVITIES')
         DO 220 ICGX=1,NCGCX
            DO 210 ICGZ=1,NCGCZ
               ISURF=NENDZN+(ICGX-1)*2*(NCGCY+NCGCZ)+2*NCGCY+NCGCZ
     1               +ICGZ
               IF (ZNAREA(ISURF).LE.0.0) GOTO 210
               WRITE (LUSTOT,200) GX(CGX(ICGX)),GX(CGX(ICGX+1)),
     1                            GZ(CGZ(ICGZ)),GZ(CGZ(ICGZ+1))
200            FORMAT (' Wall between x= ',G11.4,' and x= ',G11.4/
     1                 '  and between z= ',G11.4,' and z= ',G11.4/
     2         ' Enter emissivity for this part of the wall >> ',$)
               READ (LUSTIN,*) EMISS(ISURF)
210         CONTINUE
220      CONTINUE
C****    Now do right hand wall x=xmax
         WRITE (LUSTOT,230) GX(CGX(NCGCX+1))
230      FORMAT (/' SET EMISSIVITIES FOR RIGHT HAND WALL x=',G11.4)
         DO 260 ICGY=1,NCGCY
            DO 250 ICGZ=1,NCGCZ
               ISURF=NENCZN-NENDZN+(ICGY-1)*NCGCZ+ICGZ
               IF (ZNAREA(ISURF).LE.0.0) GOTO 250
               WRITE (LUSTOT,240) GY(CGY(ICGY)),GY(CGY(ICGY+1)),
     1                            GZ(CGZ(ICGZ)),GZ(CGZ(ICGZ+1))
240            FORMAT (' Wall between y= ',G11.4,' and y= ',G11.4/
     1                 '  and between z= ',G11.4,' and z= ',G11.4/
     2         ' Enter emissivity for this part of the wall >> ',$)
               READ (LUSTIN,*) EMISS(ISURF)
250         CONTINUE
260      CONTINUE
C****    Now do obstacles
         WRITE (LUSTOT,270)
270      FORMAT (' Set emissivities for the obstacles')
         NOBST=0
         DO 330 ICGX=1,NCGCX
            DO 320 ICGY=1,NCGCY
                DO 310 ICGZ=1,NCGCZ
                  ISURF=NENCZN+2*(ICGX-1)*NENDZN+2*(ICGY-1)*NCGCZ
     1                  +2*(ICGZ-1)+1
                  ISURF1=ISURF+1
                  IF ((ZNAREA(ISURF).LE.0.0)
     1                .AND.(ZNAREA(ISURF1).LE.0.0)) GOTO 310
                  WRITE (LUSTOT,280) GX(CGX(ICGX)),GX(CGX(ICGX+1)),
     1                               GY(CGY(ICGY)),GY(CGY(ICGY+1)),
     2                               GZ(CGZ(ICGZ)),GZ(CGZ(ICGZ+1))
280               FORMAT (' Obstacles in cell from '/
     1                    ' x= ',G11.4,' TO x= ',G11.4/
     2                    ' y= ',G11.4,' TO y= ',G11.4/
     3                    ' z= ',G11.4,' TO z= ',G11.4)
                  IF (ZNAREA(ISURF).GT.0.0) THEN
                     WRITE (LUSTOT,290)
290                  FORMAT (' Enter load emissivity for this cell',
     1                       ' >> ',$)
                     READ (LUSTIN,*) EMISS(ISURF)
                     NOBST=NOBST+1
                  END IF
                  IF (ZNAREA(ISURF1).GT.0.0) THEN
                     WRITE (LUSTOT,300)
300                  FORMAT (' Enter source emissivity for this cell',
     1                       ' >> ',$)
                     READ (LUSTIN,*) EMISS(ISURF1)
                     NOBST=NOBST+1
                  END IF
310            CONTINUE
320         CONTINUE
330      CONTINUE
         IF ((NOBST.EQ.0).AND.(NEWOLD.EQ.1)) WRITE (LUSTOT,340)
340	 FORMAT (' NO OBSTACLES')

      ELSE

C****    Input is from a file
         READ (LUFL,*) NSURFL
         IF (NSURFL.NE.NSURF) THEN
            WRITE (LUSTOT,350) NSURF,NSURFL
350         FORMAT (/' ERROR IN EMISSIVITY FILE'/
     1               ' The geometry has ',I3,' surface zones'/
     2               ' The input file has data for ',I3,
     3               ' surface zones'/' RUN ABORTED'/)
            STOP
         END IF
         DO 360 ISURF=1,NSURF
            READ (LUFL,*) EMISS(ISURF)
360      CONTINUE

      END IF

C**** If input is from the terminal save it
      IF (NEWOLD.EQ.1) THEN
         WRITE (LUFL,370)
370      FORMAT (1X,'3',38X,'EMISSIVITY INPUT OPTION')
         WRITE (LUFL,380) NSURF
380      FORMAT (1X,I4,35X,'NO. OF SURFACE ZONES')
         DO 400 ISURF=1,NSURF
            WRITE (LUFL,390) EMISS(ISURF),ISURF
390         FORMAT (1X,G13.6,26X,'EMISSIVITY OF ZONE ',I3)
400      CONTINUE
      END IF

      RETURN
      END

      SUBROUTINE EMOPT4(LUSTIN,LUSTOT,LUFL,NEWOLD,NCGCX,NCGCY,NCGCZ,
     1                  NSURF,ZNAREA,MXZN,EMISS)

C  Routine to set surface zones emissivities firstly using 5 values
C  and then giving the option to change individual zone values
C
C*****VERSION 1.0********D.A.LAWSON, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      INTEGER LUSTIN,LUSTOT,LUFL,NEWOLD,NSURF,MXZN,NCGCX,NCGCY,NCGCZ
      REAL ZNAREA(MXZN),EMISS(MXZN)
      INTEGER ICHANG,LUIN,NWOLD2
      CHARACTER*1 YORN
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  EMISS   Array to hold the zone emissivities
C  LUFL    Logical unit of the file used durig input
C  LUSTIN  Logical unit for standard input
C  LUSTOT  Logical unit for standard output
C  MXZN    Maximum number of surface zones permitted
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NEWOLD  Integer flag showing if input is new (1-ie from terminal)
C          or old (0-ie from a file)
C  NSURF   Number of surface zones
C  ZNAREA  Array of surface zone areas
C
C**** Arguments - output
C  EMISS   Array of zone emissivities
C
C**** Local variables
C  ICHANG  Number of zone whose emissivity is to be changed
C  LUIN    Logical unit number for input
C  NWOLD2  Integer flag passed to EMOPT2 indicating if input is
C          new (2 - ie from the terminal) or old (0 - ie from a file)
C  YORN    Character variable with value Y(yes) or N(no)
C
C**** Local Arrays
C  NONE
C 
C**** Functions and Subroutines
C  EMOPT2  Set emissivities using 5 basic values
C
C*****************************************************************************

C**** If input is from terminal record tha option 4 is used
      IF (NEWOLD.EQ.1) WRITE (LUFL,10)
10    FORMAT (1X,'4',38X,'EMISSIVITY INPUT OPTION')

C**** Determine the unit number for input and NEWOLD flag for passing 
C**** to EMOPT2
      IF (NEWOLD.EQ.1) THEN
         LUIN=LUSTIN
         NWOLD2=2
      ELSE
         LUIN=LUFL
         NWOLD2=0
      END IF

C**** Firstly set emissivities using 5 values option
      CALL EMOPT2(LUSTIN,LUSTOT,LUFL,NWOLD2,NCGCX,NCGCY,NCGCZ,NSURF,
     1            ZNAREA,MXZN,EMISS)

C**** Now change individual surfaces as requested
20    IF (NEWOLD.EQ.1) WRITE (LUSTOT,30)
30    FORMAT (/' Enter number of zone whose emissivity is to be',
     1        ' changed >> ',$)
      READ (LUIN,*) ICHANG

C**** Check that this is a valid surface number
      IF ((ICHANG.LT.1).OR.(ICHANG.GT.NSURF)) THEN
         IF (NEWOLD.EQ.1) THEN
            WRITE (LUSTOT,40) NSURF
40          FORMAT (/' INVALID RESPONSE'/
     1               ' Answer must be in range 1 to ',I4/)
               GO TO 20
         ELSE
            WRITE (LUSTOT,50) NSURF
50          FORMAT (/' In using emissivity option 4 there is an',
     1               ' attempt to change a zone with a'/
     2               ' number outside the valid range (1-',i4,')'/
     3               ' RUN ABORTED'/)
         END IF
      END IF

C**** Check that this is an actual surface (ie has non-zero area)
      IF (ZNAREA(ICHANG).LE.0.0) THEN
         IF (NEWOLD.EQ.1) THEN
            WRITE (LUSTOT,60)
60          FORMAT (/' This surface does not have non-zero area'/)
            GOTO 20
         ELSE 
            WRITE (LUSTOT,65) ICHANG
65          FORMAT (/' Using emissivity option 4 surface zone ',I3,
     1               ' has been selected for change'/
     2               ' This zone does not have non-zero area'/
     3               ' RUN ABORTED'/)
            STOP
         END IF
      END IF

C**** Determine the new emiisivity for zone ICHANG
      IF (NEWOLD.EQ.1) WRITE (LUSTOT,70) EMISS(ICHANG)
70    FORMAT (' Old emissivity is ',G11.4/
     1        ' Enter new emissivity >> ',$)
      READ (LUIN,*) EMISS(ICHANG)

C**** Record change in data file 
      IF (NEWOLD.EQ.1) WRITE (LUFL,80) ICHANG,EMISS(ICHANG)
80    FORMAT (1X,I4,35X,'ZONE EMISSIVITY TO BE CHANGED'/
     1        1X,G13.6,26X,'NEW EMISSIVITY')

C**** Determine if changes are complete
90    IF (NEWOLD.EQ.1) WRITE (LUSTOT,100)
100   FORMAT (' Change any more emissivities (Y/N)? >> ',$)
      READ (LUIN,110) YORN
110   FORMAT (A1)
C**** Record answer in data file
      IF (NEWOLD.EQ.1) WRITE (LUFL,120) YORN
120   FORMAT (1X,A1,38X,'MORE CHANGES (Y/N)')

C**** Take appropriate action ie finish or go back and process next change
      IF ((YORN.EQ.'Y').OR.(YORN.EQ.'y')) GOTO 20      

      RETURN
      END

