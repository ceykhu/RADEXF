      SUBROUTINE FINGRD(LUSTOT,FINVOL,NVMX,NZMX)
C
C  Routine to determine all the fine grid details
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**17/04/91
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,NVMX,NZMX
      REAL FINVOL(NVMAX,NVMAX,NZMAX)
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTOT  Logical unit number for standard output
C  NVMX    Maximum number of vertices allowed
C  NZMX    Maximum number of z-slices allowed
C
C**** Arguments - output
C  FINVOL  Array of fine grid cell volumes
C
C**** Common variables - input
C  NONE
C
C**** Common variables - output
C  NONE
C
C**** Functions and Subroutines
C  AREACL  Calculate the areas of the fine grid cell faces
C  AREAMD  Modify the cell faces areas where an obstacle is present
C  ELMENT  Record the surface and gas element information
C  GRID    Calculate the co-ordinates of the x and y fine grid lines
C  INOUT   Determine which fine grid cells are inside and which outside
C  OBSLOC  Determine which fine grid cells the obstacles lie in
C  VOLCAL  Calculate the volumes of the fine grid cells
C  VOLMOD  Modify the fine grid cell volumes where obstacles are present
C  WALCHK  Check that the boundary of the cross-section does not cross itself
C  WALSEG  Divide the boundary of the cross-section into wall segments
C  
C******************************************************************************

C**** Determine the x and y grid line co-ordinates
      CALL GRID

C**** Divide the wall (enclosure boundary) into segments
C**** and check it does not cross itself
      CALL WALSEG(LUSTOT)
      CALL WALCHK(LUSTOT)

C**** Determine which fine grid cells the obstacles are in
      CALL OBSLOC(LUSTOT)

C**** Determine which fine grid cells are inside & which outside the enclosure
      CALL INOUT(LUSTOT)

C**** Calculate cell face areas and cell volumes ignoring obstacles      
      CALL AREACL(LUSTOT)
      CALL VOLCAL(NGX,NGY,NGZ,GZ,NZMAX,AREA,NVMAX,FINVOL)

C**** Modify cell face areas and cell volumes to allow for obstacles      
      CALL AREAMD(LUSTOT)
      CALL VOLMOD(NOB,NOBMAX,TYPE,OBSTXS,OBSTXE,OBSTYS,OBSTYE,OBSTZS,
     1            OBSTZE,NVMAX,NZMAX,GX,GY,GZ,GLO,GHI,PI,FINVOL)

C**** Record the surface and volume element information
      CALL ELMENT(LUSTOT,FINVOL,NVMAX,NZMAX)

      RETURN
      END
      
      SUBROUTINE GRID
C
C  Routine to determine the number and location of the x and y grid lines
C  and also to record the x and y grid line numbers of each vertex
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**25/05/90
C
C
      include 'comvar.for'
C
      INTEGER I,J,ORDER(NVMAX),TEMP
      REAL DIFF
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  NONE
C
C**** Arguments - output
C  NONE
C 
C**** Common variables - input
C  COINC   Largest value by which two co-ordinates may differ but still be
C          deemed to coincide
C  NV      Number of vertices
C  NVMAX   Maximum number of vertices permitted plus one - needed for 
C          dimensioning arrays - parameter set in comvar.for
C  VX      Array of vertex x co-ordinates
C  VY      Array of vertex y co-ordinates
C
C**** Common variables - output
C  GX      Array giving the x co-ordinates of the grid lines
C  GY      Array giving the y co-ordinates of the grid lines
C  NGX     Number of x grid lines
C  NGY     Number of y grid lines
C  VXGRID  Array giving the x grid line number of each vertex
C  VYGRID  Array giving the y grid line number of each vertex
C
C**** Local variables
C  DIFF    Difference between co-ordinate of next vertex and current grid
C          line co-ordinate
C  I       Loop counter
C  J       Loop counter
C  TEMP    Temporary integer storage when two values need to be interchanged
C
C**** Local Arrays
C  ORDER   Array used in the sorting procedure - after the sort ORDER(I) gives
C          the position of the ith smallest element of the array being sorted
C
C**** Functions and Subroutines
C  NONE
c
C******************************************************************************

C     Sort vertex x co-ords (array VX) into ascending order
      DO 100 I=1,NV
         ORDER(I)=I
100   CONTINUE
      DO 300 I=1,NV-1
         DO 200 J=I+1,NV
	    IF (VX(ORDER(J)).LT.VX(ORDER(I))) THEN
	       TEMP=ORDER(I)
	       ORDER(I)=ORDER(J)
	       ORDER(J)=TEMP
	    END IF
200      CONTINUE
300   CONTINUE

C     Set x grid line co-ords (array GX), count x grid lines (NGX)
C     & record x grid line number of each vertex (array VXGRID)
      GX(1)=VX(ORDER(1))
      NGX=1
      VXGRID(ORDER(1))=NGX
      DO 400 I=2,NV
         DIFF=VX(ORDER(I))-GX(NGX)
C        See if next vertex lies on the current grid line or not
	 IF (DIFF.GT.COINC) THEN
C           Next vertex is not on current grid line so record a new one
	    NGX=NGX+1
	    GX(NGX)=VX(ORDER(I))
	 END IF
	 VXGRID(ORDER(I))=NGX
400   CONTINUE
C     For computational purposes record data for fictitious vertex NV+1
C     which coincides with the first vertex
      GX(NV+1)=GX(1)
      VXGRID(NV+1)=VXGRID(1)

C     Sort vertex y co-ords (array VY) into ascending order
      DO 500 I=1,NV
         ORDER(I)=I
500   CONTINUE
      DO 700 I=1,NV-1
         DO 600 J=I+1,NV
	    IF (VY(ORDER(J)).LT.VY(ORDER(I))) THEN
	       TEMP=ORDER(I)
	       ORDER(I)=ORDER(J)
	       ORDER(J)=TEMP
	    END IF
600      CONTINUE
700   CONTINUE
C     Sort completed

C     Set y grid line co-ords (array GY), count y grid lines (NGY)
C     & record y grid line number of each vertex (array VYGRID)
      GY(1)=VY(ORDER(1))
      NGY=1
      VYGRID(ORDER(1))=NGY
      DO 800 I=2,NV
         DIFF= VY(ORDER(I))-GY(NGY)
C        See if next vertex lies on the current grid line or not
	 IF (DIFF.GT.COINC) THEN
C           Next vertex is not on current grid line so record a new one
	    NGY=NGY+1
	    GY(NGY)=VY(ORDER(I))
	 END IF
	 VYGRID(ORDER(I))=NGY
800   CONTINUE
C     For computational purposes record data for fictitious vertex NV+1
C     which coincides with the first vertex
      GY(NV+1)=GY(1)
      VYGRID(NV+1)=VYGRID(1)

      RETURN
      END

      SUBROUTINE WALSEG(LUSTOT)
C
C  Routine to divide the perimeter of the enclosure into segments
C  The perimeter is made up of a number of a walls.  A wall is that
C  part of the perimeter between two consecutive vertices.  A segment
C  is a portion of wall that crosses only one cell
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**25/05/90
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,I,IE,DVXGRD,DVYGRD
      REAL GRAD
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTOT  Logical unit number for standard output - to give error message
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  NV      Number of vertices
C  VX      Array of vertex x co-ordinates
C  VY      Array of vertex y co-ordinates
C  VXGRID  Array giving x grid line numbers of the vertices
C  VYGRID  Array giving y grid line numbers of the vertices
C
C**** Common variables - output
C  NSEG    Number of wall segments
C  VXWSEG  Array giving number of the wall segment at the start of the wall
C          beginning at each vertex
C  WSEGXS  Array giving x co-ordinate of the start of each wall segment
C  WSEGYS  Array giving y co-ordinate of the start of each wall segment
C
C**** Local variables
C  DVXGRD  Difference in the x grid line numbers at start and end of wall
C  DVYGRD  Difference in the y grid line numbers at start and end of wall
C  GRAD    Gradient of the wall
C  I       Vertex number at the start of the current wall
C  IE      Vertex number at the end of the current wall
C  
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  SEGXLN  Divide into segments a wall that is along an x grid line
C  SEGYLN  Divide into segments a wall that is along a y grid line
C  SEGX1   Divide into segments a wall that crosses only 1 column of x cells
C  SEGY1   Divide into segments a wall that crosses only 1 row of y cells
C  SEGSKW  Divide into segments a wall skewed across the grid ie that crosses
C          more than 1 column of x cells and more than 1 row of y cells
C
C******************************************************************************

C     Initialise number of wall segments
      NSEG=0
C     Loop over the walls
      DO 200 I=1,NV
C        Set vertex number of end of wall
	 IE=I+1
C        Record the wall segment number at the start of this wall
         VXWSEG(I)=NSEG+1
C        Calculate the changes in x and y grid line numbers
	 DVXGRD=VXGRID(IE)-VXGRID(I)
	 DVYGRD=VYGRID(IE)-VYGRID(I)
	 IF ((DVXGRD.EQ.0).AND.(DVYGRD.EQ.0)) THEN
C           Vertices I and IE are at the same point
	    WRITE (LUSTOT,100) I,IE
100         FORMAT (' VERTICES ',I2,' AND ',I2,' ARE COINCIDENT'/
     1              ' THIS IS NOT ALLOWED - RUN ABORTED')
            STOP
	 END IF
	 IF (IABS(DVXGRD).EQ.0) THEN
C           Wall is along an x grid line
            CALL SEGXLN(I,DVYGRD,LUSTOT)
	    GOTO 200
	 END IF
	 IF (IABS(DVYGRD).EQ.0) THEN
C           Wall is along a y grid line
            CALL SEGYLN(I,DVXGRD,LUSTOT)
	    GOTO 200
	 END IF
C        Wall is neither vertical nor horizontal
	 GRAD=(VY(IE)-VY(I))/(VX(IE)-VX(I))
	 IF (IABS(DVXGRD).EQ.1) THEN
C           Wall crosses only 1 column of x cells
            CALL SEGX1(I,DVXGRD,DVYGRD,GRAD,LUSTOT)
	    GOTO 200
	 END IF
	 IF (IABS(DVYGRD).EQ.1) THEN
C           Wall crosses only 1 row of y cells
	    CALL SEGY1(I,DVXGRD,DVYGRD,GRAD,LUSTOT)
	    GOTO 200
	 END IF
C        Wall crosses more than 1 column of x and more than 1 row of y cells
         CALL SEGSKW(I,DVXGRD,DVYGRD,GRAD,LUSTOT)
200   CONTINUE

C     The end of each segment is found by looking at the start of the next
C     So we must record the start of segment NSEG+1 although such a segment
C     does not really exist
      WSEGXS(NSEG+1)=WSEGXS(1)
      WSEGYS(NSEG+1)=WSEGYS(1)

      RETURN
      END
      	 	    

      SUBROUTINE SEGXLN(I,DVYGRD,LUSTOT)
C
C  Routine to divide into segments a wall which lies along an x grid line
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**25/05/90
C
C
      include 'comvar.for'
C
      INTEGER DVYGRD,I,INCY,J,LUSTOT,NCELLX,NCELLY
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  DVYGRD  Difference in y grid line numbers between start and end of wall
C  I       Number of the wall being divided into segments
C  LUSTOT  Logical unit number for standard output passed down to RECWS
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  GX      Array of x grid line co-ordinates
C  GY      Array of y grid line co-ordinates
C  NSEG    Number of wall segments before this wall
C  VXGRID  Array of vertex x grid line numbers
C  VYGRID  Array of vertex y grid line numbers
C
C**** Common variables - output
C  NSEG     Number of wall segments including this wall
C  WSEGXS   Array of x co-ordinate at start of each segment
C  WSEGYS   Array of y co-ordinate at start of each segment
C
C**** Local variables
C  INCY    Increment to y grid line and y cell numbers
C  J       Loop counter
C  NCELLX  Current x cell number
C  NCELLY  Current y cell number
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  RECWS   Record the wall segments of this wall in appropriate cells
C******************************************************************************

C     NB Walls along x grid lines are taken to be in the cell which is on
C     the inside of the wall in question

C     Determine direction in y, the y increment and the starting cell number
      IF (DVYGRD.GT.0) THEN
C        y increase along the wall
         INCY=1
	 NCELLY=VYGRID(I)
	 NCELLX=VXGRID(I)-1
      ELSE
C        y decreases along the wall
         INCY=-1
	 NCELLY=VYGRID(I)-1
	 NCELLX=VXGRID(I)
      END IF

C     Process each of the DVYGRD segments
      DO 100 J=1,IABS(DVYGRD)
C        Update segment number
         NSEG=NSEG+1
C        Set co-ordinates of start of segment
	 WSEGXS(NSEG)=GX(VXGRID(I))
	 WSEGYS(NSEG)=GY(VYGRID(I)+(J-1)*INCY)
C        Record this segment in this cell
	 CALL RECWS(NCELLX,NCELLY,NSEG,2,LUSTOT)
C        Update the cell number for next segment
	 NCELLY=NCELLY+INCY
100   CONTINUE

      RETURN
      END
      	       	 

      SUBROUTINE SEGYLN(I,DVXGRD,LUSTOT)
C
C  Routine to divide into segments a wall which lies along a y grid line
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**25/05/90
C
C
      include 'comvar.for'
C
      INTEGER DVXGRD,I,INCX,J,LUSTOT,NCELLX,NCELLY
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  DVXGRD  Difference in x grid line numbers between start and end of wall
C  I       Number of wall being divided into segments
C  LUSTOT  Logical unit number for standard output passed down to RECWS
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  GX      Array of x grid line co-ordinates
C  GY      Array of y grid line co-ordinates
C  NSEG    Number of wall segments before this wall
C  VXGRID  Array of vertex x grid line numbers
C  VYGRID  Array of vertex y grid line numbers
C
C**** Common variables - output
C  NSEG    Number of wall segments including this wall
C  WSEGXS  Array of x co-ordinate at start of each segment
C  WSEGYS  Array of y co-ordinate at start of each segment
C
C**** Local variables
C  INCX    Increment to x grid line and x cell numbers
C  J       Loop counter
C  NCELLX  Current x cell number
C  NCELLY  Current y cell number
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  RECWS   Record the wall segments of this wall in appropriate cells
C******************************************************************************

C     NB Walls along y grid lines are taken to be in the cell which is on
C     the inside of the wall in question
C     Determine direction in x, the x increment and the starting cell number
      IF (DVXGRD.GT.0) THEN
C        x increases along the wall
         INCX=1
	 NCELLX=VXGRID(I)
	 NCELLY=VYGRID(I)
      ELSE
C        x decreases along the wall
         INCX=-1
	 NCELLX=VXGRID(I)-1
	 NCELLY=VYGRID(I)-1
      END IF

C     Process each of the DVXGRD segments
      DO 100 J=1,IABS(DVXGRD)
C        Update the segment number
         NSEG=NSEG+1
C        Set co-ordinates at start of segment
	 WSEGXS(NSEG)=GX(VXGRID(I)+(J-1)*INCX)
	 WSEGYS(NSEG)=GY(VYGRID(I))
C        Record this segment in this cell
	 CALL RECWS(NCELLX,NCELLY,NSEG,2,LUSTOT)
C        Update the cell number for next segment
	 NCELLX=NCELLX+INCX
100   CONTINUE

      RETURN
      END


      SUBROUTINE SEGX1(I,DVXGRD,DVYGRD,GRAD,LUSTOT)
C
C  Routine to divide into segments a wall which crosses a single column of
C  cells
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**04/06/90
C
C
      include 'comvar.for'
C
      INTEGER I,DVXGRD,DVYGRD,LUSTOT
      REAL GRAD
      INTEGER INCX,INCY,NCELLX,NCELLY,J
      REAL DX
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  DVXGRD  Difference in x grid line numbers between start and end of wall
C  DVYGRD  Difference in y grid line numbers between start and end of wall
C  GRAD    Gradient of wall (ie change in y divided by change in x)
C  I       Number of wall being divided into segments
C  LUSTOT  Logical unit number for standard output passed down to RECWS
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  GX      Array of x grid line co-ordinates
C  GY      Array of y grid line co-ordinates
C  NSEG    Number of wall segments before this wall
C  VXGRID  Array of vertex x grid line numbers
C  VYGRID  Array of vertex y grid line numbers
C
C**** Common variables - output
C  NSEG     Number of wall segments including this wall
C  WSEGXS   Array of x co-ordinate at start of each segment
C  WSEGYS   Array of y co-ordinate at start of each segment
C
C**** Local variables
C  DX      Change in x from start of wall to current point
C  INCX    Increment to x grid line and cell numbers
C  INCY    Increment to y grid line and cell numbers
C  J       Loop counter
C  NCELLX  Current x cell number
C  NCELLY  Current y cell number
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  RECWS   Record the wall segments of this wall in appropriate cells
C
C******************************************************************************

C     Determine direction in y and starting y cell number
      IF (DVYGRD.GT.0) THEN
         INCY=1
	 NCELLY=VYGRID(I)
      ELSE
         INCY=-1
	 NCELLY=VYGRID(I)-1
      END IF

C     Determine direction in x and starting x cell number
      IF (DVXGRD.GT.0) THEN
         INCX=1
	 NCELLX=VXGRID(I)
      ELSE
         INCX=-1
	 NCELLX=VXGRID(I)-1
      END IF

C     There are ABS(DVYGRD) wall segments - the x cell number for each of 
C     these is the same - the y cell numbers change by INCY
      DO 100 J=1,IABS(DVYGRD)
         NSEG=NSEG+1
	 DX=ABS((GY(VYGRID(I)+(J-1)*INCY)-GY(VYGRID(I)))/GRAD)
	 WSEGXS(NSEG)=GX(VXGRID(I))+DX*INCX
	 WSEGYS(NSEG)=GY(VYGRID(I)+(J-1)*INCY)
	 CALL RECWS(NCELLX,NCELLY,NSEG,1,LUSTOT)
	 NCELLY=NCELLY+INCY
100   CONTINUE	       	       
      
      RETURN
      END


      SUBROUTINE SEGY1(I,DVXGRD,DVYGRD,GRAD,LUSTOT)
C
C  Routine to divide into segments a wall which crosses only one
C  row of cells
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**04/06/90
C
C
      include 'comvar.for'
C
      INTEGER I,DVXGRD,DVYGRD,LUSTOT
      REAL GRAD
      INTEGER INCX,INCY,NCELLX,NCELLY,J
      REAL DY
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  DVXGRD  Difference in x grid line numbers between start and end of wall
C  DVYGRD  Difference in y grid line numbers between start and end of wall
C  GRAD    Gradient of wall (ie change in y divided by change in x)
C  I       Number of the wall being divided into segments
C  LUSTOT  Logical unit number for standard output
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  GX      Array of x grid line co-ordinates
C  GY      Array of y grid line co-ordinates
C  NSEG    Number of wall segments before this wall
C  VXGRID  Array of vertex x grid line numbers
C  VYGRID  Array of vertex y grid line numbers
C
C**** Common variables - output
C  NSEG    Number of wall segments including this wall
C  WSEGXS  Array of x co-ordinates at start of each segment
C  WSEGYS  Array of y co-ordinates at start of each segment
C
C**** Local variables
C  DY      Change in y from start of wall to current position
C  INCX    Increment to x cell number
C  INCY    Increment to y cell number
C  J       Loop counter
C  NCELLX  Current x cell number
C  NCELLY  Current y cell number
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  RECWS   Record the wall segments of this wall in appropriate cells
C******************************************************************************

C     Determine direction in y and starting y cell number
      IF (DVYGRD.GT.0) THEN
         INCY=1
	 NCELLY=VYGRID(I)
      ELSE
         INCY=-1
	 NCELLY=VYGRID(I)-1
      END IF

C     Determine direction in x and starting x cell number
      IF (DVXGRD.GT.0) THEN
         INCX=1
	 NCELLX=VXGRID(I)
      ELSE
         INCX=-1
	 NCELLX=VXGRID(I)-1
      END IF

C     There are ABS(DVXGRD) wall segments - each has the same y cell number
C     The x cell number changes by INCX each time
      DO 100 J=1,IABS(DVXGRD)
         NSEG=NSEG+1
	 DY=ABS((GX(VXGRID(I)+(J-1)*INCX)-GX(VXGRID(I)))*GRAD)
	 WSEGXS(NSEG)=GX(VXGRID(I)+(J-1)*INCX)
	 WSEGYS(NSEG)=GY(VYGRID(I))+DY*INCY
	 CALL RECWS(NCELLX,NCELLY,NSEG,1,LUSTOT)
	 NCELLX=NCELLX+INCX
100   CONTINUE	       	       
      
      RETURN
      END


      SUBROUTINE SEGSKW(I,DVXGRD,DVYGRD,GRAD,LUSTOT)
C
C  Routine to divide into segments a wall which crosses more than one
C  row and more than one column of cells
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**04/06/90
C
C
      include 'comvar.for'
C
      INTEGER I,DVXGRD,DVYGRD,LUSTOT
      REAL GRAD
      INTEGER IE,INCX,INCY,INCCLX,INCCLY,NCELLX,NCELLY,LASTX,LASTY
      REAL XIN,YIN,DX,DY,X,Y
      LOGICAL XCHANG,YCHANG
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  DVXGRD  Difference in x grid line numbers between start and end of wall
C  DVYGRD  Difference in y grid line numbers between start and end of wall
C  GRAD    Gradient of the wall (ie change in y divided by change in x)
C  I       Number of the wall being divided into segments
C  LUSTOT  Logical unit number for standard output
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  GX      Array of x grid line co-ordinates
C  GY      Array of y grid line co-ordinates
C  NSEG    Number of wall segments before this wall
C  VXGRID  Array of vertex x grid line numbers
C  VYGRID  Array of vertex y grid line numbers
C
C**** Common variables - output
C  NSEG     Number of wall segments including this wall
C  WSEGXS   Array of x co-ordinates at start of each segment
C  WSEGYS   Array of y co-ordinates at start of each segment
C
C**** Local variables
C  DX      Change in x along current wall
C  DY      Change in y along current wall
C  IE      Number of vertex at end of wall
C  INCCLX  x cell number increment
C  INCCLY  y cell number increment
C  INCX    x grid line number increment
C  INCY    y grid line number increment
C  LASTX   Last x cell number on wall
C  LASTY   Last y cell number on wall
C  NCELLX  Current x cell number
C  NCELLY  Current y cell number
C  X       x co-ordinate of wall on next y grid line
C  Y       y co-ordinate of wall on next x grid line
C  XCHANG  Logical variable indicating if x cell number is changed
C  YCHANG  Logical variable indiciating if y cell number is changed
C  XIN     x co-ordinate of entry point of wall to current cell
C  YIN     y co-ordinate of entry point of wall to current cell
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  RECWS   Record the wall segments of this wall in appropriate cells
C******************************************************************************

C     Find the vertex number at the end of the wall
      IE=I+1
      
C     Determine the direction in y and 1st and last y cell numbers
      IF (DVYGRD.GT.0) THEN
         INCY=1
	 INCCLY=1
	 NCELLY=VYGRID(I)
	 LASTY=VYGRID(IE)-1
      ELSE
         INCY=-1
	 INCCLY=0
	 NCELLY=VYGRID(I)-1
	 LASTY=VYGRID(IE)
      END IF

C     Determine direction in x and 1st and last x cell numbers
      IF (DVXGRD.GT.0) THEN
         INCX=1
	 INCCLX=1
	 NCELLX=VXGRID(I)
	 LASTX=VXGRID(IE)-1
      ELSE
         INCX=-1
	 INCCLX=0
	 NCELLX=VXGRID(I)-1
	 LASTX=VXGRID(IE)
      END IF

C     Set entry co-ordinates of 1st cell
      XIN=GX(VXGRID(I))
      YIN=GY(VYGRID(I))

C     Record wall segment
100   NSEG=NSEG+1
      WSEGXS(NSEG)=XIN
      WSEGYS(NSEG)=YIN       
      CALL RECWS(NCELLX,NCELLY,NSEG,1,LUSTOT)
C     Check if last segment has been reached
      IF ((NCELLX.EQ.LASTX).AND.(NCELLY.EQ.LASTY)) RETURN
C     Last segment has not been reached so find where wall leaves current cell
C     Exit point is either on next x grid line or next y grid line
C     So find where the wall reaches each of these lines
C     Calculate y co-ordinate on next x grid line
      DX=ABS(GX(NCELLX+INCCLX)-XIN)
      Y=YIN+DX*ABS(GRAD)*INCY
C     Calculate x co-ordinate on next y grid line
      DY=ABS(GY(NCELLY+INCCLY)-YIN)
      X=XIN+(DY/ABS(GRAD))*INCX
C     If exit point is on next y grid-line X-XIN<DX
C     If exit point is on next x grid line Y-YIN<DY
C     If exit point is at a vertex X-XIN=DX and Y-YIN=DY
      IF (DX.GE.ABS(X-XIN)) THEN
         YCHANG=.TRUE.
      ELSE
         YCHANG=.FALSE.
      END IF
      IF (DY.GE.ABS(Y-YIN)) THEN
         XCHANG=.TRUE.
      ELSE
         XCHANG=.FALSE.
      END IF

C     Exit point must be such that at least one of XCHANG or YCHANG is true
C     If this is not the case an error has occurred
      IF ((.NOT.XCHANG).AND.(.NOT.YCHANG)) THEN
         WRITE (LUSTOT,200) I,NSEG
200      FORMAT (' ERROR IN SEGSKW'/' FOR WALL FROM VERTEX ',I2/
     1           ' WHEN NSEG= ',I2/' RUN ABORTED')
	 STOP
      END IF
      
C     No error has occurred - set new cell numbers and co-ordinates of entry
C     point to next cell
      IF (YCHANG) THEN
         YIN=GY(NCELLY+INCCLY)
	 XIN=X
	 NCELLY=NCELLY+INCY
      END IF
      IF (XCHANG) THEN
         XIN=GX(NCELLX+INCCLX)
	 YIN=Y
	 NCELLX=NCELLX+INCX
      END IF

C     Process next wall segment
      GOTO 100	 	       	 

      END	 	                         
      

      SUBROUTINE RECWS(NX,NY,ISEG,IN,LUSTOT)
C
C  Routine to record segment ISEG in cell NX,NY and also to set CLINOT
C  to indicate if the cell is fully inside the enclosure (IN=2) or only 
C  partially inside (IN=1)
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**25/05/90
C
C
      include 'comvar.for'
C
      INTEGER IN,ISEG,IW,NX,NY,LUSTOT
C
C******* List of variables, arrays and subprograms used *******
C  
C**** Arguments - input
C  IN      Flag showing if segment ISEG makes current NX,NY only partially
C          inside the enclosure (IN=1) or as far as this segment is concerned
C          fully inside the enclosure
c  ISEG    Segment number
C  LUSTOT  Logical unit number for standard output - used for error messages
C  NX      x cell number
C  NY      y cell number
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  CELLWS  Array giving segment numbers of segments in each cell
C          CELLWS(I,J,K) is the segment number of the Kth segment of cell I,J
C  CLINOT  Array inidicating how much of each cell is in the enclosure
C          CLINOT(K,I,J) relates to cell I,J in plane K; K=1 is the plane z=0
C          and K=2 is the plane z=zmax;  CLINOT=1 means cell is only partially
C          inside the enclosure whilst CLINOT=2 means the cell is fully inside
C
C**** Common variables - output
C  CELLWS  As above
C  CLINOT  As above
C  MXWLCL  Maximum number of wall segments allowed in each cell
C  SEGCEL  Array giving the x and y cell numbers of each wall segment
C          SEGCEL(I,1) is the x cell number of the cell containing segment I
C          SEGCEL(I,2) is the y cell number of the cell containing segment I
C
C**** Local variables
C  IW      Loop counter
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************


C     If the current wall segment is not along a grid line we must check that 
C     this is the only sloping wall in this cell
      IF (IN.EQ.1) THEN
C        Current wall segment is sloping
         IF (CLINOT(1,NX,NY).EQ.1) THEN
C           Cell is already only partially inside hence it must already
C           contain a sloping wall
	    WRITE (LUSTOT,50) NX,NY
50          FORMAT (' CELL ',I2,',',I2,' HAS MORE THAN 1 SLOPING WALL'/
     1              ' THIS IS NOT ALLOWED'/
     2              ' PUT IN EXTRA VERTICES'/
     3              ' RUN ABORTED')
            STOP
	 END IF 
C        Cell has not yet been flagged as partially in so do so
         CLINOT(1,NX,NY)=IN
      ELSE
C        Current wall is along a grid line so if CLINOT has not been altered
C        from 0 set it to 2 but if CLINOT is already set to 1 leave it alone
         IF (CLINOT(1,NX,NY).EQ.0) CLINOT(1,NX,NY)=IN
      END IF	 

C     Decide which segment of the cell this one is
      DO 100 IW=1,MXWLCL
         IF (CELLWS(NX,NY,IW).GT.0) GOTO 100
C        Record the segment and cell information and return
	 CELLWS(NX,NY,IW)=ISEG
         SEGCEL(ISEG,1)=NX
	 SEGCEL(ISEG,2)=NY
	 RETURN
100   CONTINUE

C     If we make it to here then the cell already has its full quota (MXWLCL)
C     of segments so print an error message
      WRITE (LUSTOT,200) MXWLCL,NX,NY
200   FORMAT (' MORE THAN ',I2,' WALL SEGMENTS IN CELL ',I2,
     1        ',',I2/' THIS IS NOT ALLOWED - RUN ABORTED')
      STOP

      END



      SUBROUTINE WALCHK(LUSTOT)
C
C  Routine to check that the perimeter of the enclosure does not cross
C  itself anywhere.
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**04/06/90
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,I,J,K,NS,K1,K2,ISEG1,ISEG2,IFLAG
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTOT  Logical unit number for standard output
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  CELLWS  Array recording which segments are in each cell 
C          CELLWS(I,J,K) is the number of the Kth segment in cell (I,J)
C  MXWLCL  Maximum number of wall segments allowed per cell
C  NGX     Number of x grid lines
C  NGY     Number of y grid lines
C
C**** Common variables - output
C  NONE
C
C**** Local variables
C  I       Loop counter on x cell number
C  IFLAG   Flag (returned by INTSCT) indicating if a pair of segments do
C          intersect in the current cell
C  ISEG1   Number of first segment
C  ISEG2   Number of second segment
C  J       Loop counter on y cell number
C  K       Loop counter on segments in this cell
C  K1      Loop counter on first segment
C  K2      Loop counter on second segment
C  NS      Number of segments in current cell
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  INTSCT  Check if 2 particular segments intersect within current cell
C
C******************************************************************************

C     Cycle through the cells
      DO 900 I=1,NGX-1
         DO 800 J=1,NGY-1
C           Determine the number of segments in current cell
            DO 300 K=1,MXWLCL
	       IF (CELLWS(I,J,K).EQ.0) THEN
	          NS=K-1
		  GOTO 400
	       END IF
300         CONTINUE
C           If we reach here array CELLWS is full for cell (I,J) ie there
C           are MXWLCL segments in this cell
            NS=MXWLCL
C           If there are less than 2 segments in current cell the perimeter
C           cannot cross itslef in this cell
400         IF (NS.LT.2) GOTO 800
C           There are more than 2 segments - so take them in pairs to see
C           if any intersect within this cell
            DO 700 K1=1,NS-1
	       DO 600 K2=K1+1,NS
	          ISEG1=CELLWS(I,J,K1)
		  ISEG2=CELLWS(I,J,K2)
C                 If the 2 segments are consecutive they intersect at a vertex
C                 This is permissible.  Consecutive segments are n,n+1 or
C                 n+1,n or 1,nseg or nseg,1
		  IF (ABS(ISEG1-ISEG2).EQ.1) GOTO 600            
		  IF ((ISEG1.EQ.1).AND.(ISEG2.EQ.NSEG)) GOTO 600
		  IF ((ISEG1.EQ.NSEG).AND.(ISEG2.EQ.1)) GOTO 600
C                 The 2 segments may intersect - call INTSCT to find out
		  CALL INTSCT(ISEG1,ISEG2,IFLAG,LUSTOT)
C                 IFLAG shows if they intersect
		  IF (IFLAG.EQ.1) THEN
		     WRITE (LUSTOT,500) ISEG1,ISEG2,I,J
500                  FORMAT (' WALL SEGMENTS ',I2,' & ',I2,' INTERSECT'/
     1                       ' IN CELL ',I2,',',I2/
     2                       ' RUN ABORTED')
                     STOP
		  END IF
600            CONTINUE
700         CONTINUE
800      CONTINUE
900   CONTINUE

      RETURN
      END



      SUBROUTINE INTSCT(I1,I2,IFLAG,LUSTOT)
C
C  Routine to determine if segments I1 and I2 intersect within the cell
C  in which they lie
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**04/06/90
C
C
      include 'comvar.for'
C
      INTEGER I1,I2,IFLAG,LUSTOT
      REAL XS1,XE1,XS2,XE2,YS1,YE1,YS2,YE2,GRAD1,GRAD2,C1,C2,
     1     XINT,YINT,TEST
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  I1      Number of first segment
C  I2      Number of second segment
C  LUSTOT  Logical unit number of standard output
C
C**** Arguments - output
C  IFLAG   Flag indicating if segments I1 and I2 intersect in this cell
C          0 - no intersection; 1 - intersection
C
C**** Common variables - input
C  COINC   Largest value by which 2 values be differ and still be held 
C          to coincide
C  WSEGXS  Array of x co-ordinate at start of each segment
C  WSEGYS  Array of y co-ordinate at start of each segment
C
C**** Common variables - output
C  NONE
C
C**** Local variables
C  C1      y intercept of first segment
C  C2      y intercept of second segment
C  GRAD1   gradient of first segment
C  GRAD2   gradient of second segment
C  TEST    parameter used to determine if intersection co-ordinate lies
C          between extreme co-ordinates of this cell
C  XE1     x co-ordinate at end of first segment
C  XE2     x co-ordinate at end of second segment
C  XINT    x co-ordinate at point of intersection
C  XS1     x co-ordinate at start of first segment
C  XS2     x co-ordinate at start of second segment
C  YE1     y co-ordinate at end of first segment
C  YE2     y co-ordinate at end of second segment
C  YINT    y co-ordinate at point of intersection
C  YS1     y co-ordinate at start of first segment
C  YS2     y co-ordiante at start of second segment
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Set the start and end co-ordinates of both segments
      XS1=WSEGXS(I1)
      XE1=WSEGXS(I1+1)
      XS2=WSEGXS(I2)
      XE2=WSEGXS(I2+1)
      YS1=WSEGYS(I1)
      YE1=WSEGYS(I1+1)
      YS2=WSEGYS(I2)
      YE2=WSEGYS(I2+1)

C     Are both lines parallet to y axis?
      IF ((ABS(XE1-XS1).LE.COINC).AND.(ABS(XE2-XS2).LE.COINC)) THEN
C        Yes - Are they the same line?
         IF (ABS(XE1-XE2).LT.COINC) THEN
	    WRITE (LUSTOT,100) I1,I2
100         FORMAT (' SEGMENTS ',I2,' & ',I2,' COINCIDE'/
     1              ' RUN ABORTED')
            STOP
	 ELSE
C           Segments are both parallel to y axis but are not the same line
C           so they do not intersect
            IFLAG=0
	    RETURN
	 END IF
      END IF

C     Is first segment parallel to y axis
      IF (ABS(XE1-XS1).LE.COINC) THEN
C        Yes - find gradient & y intercept of second segment 
C        N.B. second segment is not parallel to y axis
         GRAD2=(YE2-YS2)/(XE2-XS2)
	 C2=(XE2*YS2-XS2*YE2)/(XE2-XS2)
C        Find y co-ordinate of point of intersection
	 YINT=GRAD2*XE1+C2
C        Determine if YINT is between YS2 and YE2?
	 TEST=(YINT-YS2)*(YINT-YE2)
	 IF (TEST.LE.0) THEN
	    IFLAG=1
	 ELSE
	    IFLAG=0
	 END IF
	 RETURN
      END IF   	 	    

C     Is second segment paralle to y axis?
      IF (ABS(XE2-XS2).LT.COINC) THEN
C        Yes - find gradient and y intercept of first segment
C        N.B. first segment is not parallel to y axis
         GRAD1=(YE1-YS1)/(XE1-XS1)
	 C1=(XE1*YS1-XS1*YE1)/(XE1-XS1)
C        Find y co-ordinate of point of intersection
	 YINT=GRAD1*XE2+C1
C        Determine if YINT is between YS1 and YE1
	 TEST=(YINT-YS1)*(YINT-YE1)
	 IF (TEST.LE.0) THEN
	    IFLAG=1
	 ELSE
	    IFLAG=0
	 END IF
	 RETURN
      END IF      

C     Neither segment is parallel to y axis so calculate both gradients 
C     and y intercepts
      GRAD1=(YE1-YS1)/(XE1-XS1)
      GRAD2=(YE2-YS2)/(XE2-XS2)
      C1=(XE1*YS1-XS1*YE1)/(XE1-XS1)
      C2=(XE2*YS2-XS2*YE2)/(XE2-XS2)
C     Are the segments parallel?
      IF (ABS(GRAD1-GRAD2).LT.COINC) THEN      
C        Yes - Are they the same line?
	 IF (ABS(C1-C2).LT.COINC) THEN
	    WRITE (LUSTOT,100) I1,I2
	    STOP
	 ELSE
            IFLAG=0
 	    RETURN
	 END IF
      END IF
C     The segments are not parallel and neither is vertical so find 
C     x co-ordinate of point of intersection
      XINT=(C2-C1)/(GRAD1-GRAD2)
C     Determine if XINT is between XS1 and XE1
      TEST=(XINT-XS1)*(XINT-XE1)
      IF (TEST.LE.0) THEN
         IFLAG=1
      ELSE
         IFLAG=0
      END IF

      RETURN
      END      



      SUBROUTINE OBSLOC(LUSTOT)
C
C  Routine to locate which cells each obstacle lies in and to
C  record this for each obstacle - also to record for each cell 
C  the number of the obstacle in that cell (if there is one)
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**05/06/90
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,I,J,IDIR,K,L
      REAL RAD
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTOT  Logical unit number for standard output
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  COINC   Largest amount by which two values may differ and still be deemed
C          to coincide
C  GHI     Array of high co-odinates of obstacles
C  GLO     Array of low co-ordinates of obstacles
C  GX      Array of x grid line co-ordinates
C  GY      Array of y grid line co-ordinates
C  GZ      Array of z grid line co-ordinates
C  NGX     Number of x grid lines
C  NGY     Number of y grid lines
C  NGZ     Number of z grid lines
C  NOB     Number of obstacles
C
C**** Common variables - output
C  CELLOB  Array of obstacle numbers in each cell CELLOB(I,J,K,L) is the
C          obstacle number of the Lth obstacle in cell (I,J,K)
C  NOBCMX  Maximum number of obstacles allowed in each cell
C  NOBST   Array giving the number of obstacles in each cell
C  OBSTXE  Array giving the ending x cell number of each obstacle
C  OBSTXS  Array giving the starting x cell number of each obstacle
C  OBSTYE  Array giving the ending y cell number of each obstacle
C  OBSTYS  Array giving the starting y cell number of each obstacle
C  OBSTZE  Array giving the ending z cell number of each obstacle
C  OBSTZS  Array giving the starting z cell number of each obstacle
C
C**** Local variables
C  I       Loop counter for obstacles
C  IDIR    Integer giving direction of cylinders
C  J       Loop counter for cells
C  K       Loop counter for cells
C  L       Loop counter for cells
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Process each obstacle in turn
      DO 5500 I=1,NOB

C        ***********************************************
C        *       Find the starting cell numbers        *
C        *     Same method for boxes and cylinders     *
C        ***********************************************
C        N.B. The starting cell for a cylinder means the cell
C        which contains the centre of the low face

C        * * * * * * * * * * * *
C        *    Starting x cell  *
C        * * * * * * * * * * * *
C        Check that low x co-ordinate is not too small
         IF (GLO(I,1).LT.GX(1)-COINC) THEN
	    WRITE (LUSTOT,50) I,GLO(I,1)
50	    FORMAT (' LOW X CO-ORD OF OBSTACLE ',I1,' IS ',F7.4/
     1              ' THIS IS BEFORE START OF ENCLOSURE'/
     2              ' RUN ABORTED')
            STOP
	 END IF
C        Low x co-ordinate is O.K. - find which cell it comes in
         DO 100 J=1,NGX-1
	    IF (GLO(I,1).LT.GX(J+1)-COINC) THEN
	       OBSTXS(I)=J
	       GOTO 300
	    END IF
100      CONTINUE	    
C        If we reach here the low x co-ordinate is too big
         WRITE (LUSTOT,200) I,GLO(I,1)
200      FORMAT (' X CO-ORD OF OBSTACLE ',I2,' BEGINS AT ',F7.4/
     1           ' THIS IS AT OR BEYOND END OF ENCLOSURE'/
     2           ' RUN ABORTED')
         STOP

C        * * * * * * * * * * * *
C        *    Starting y cell  *
C        * * * * * * * * * * * *
C        Check that low y co-ordinate is not too small
300      IF (GLO(I,2).LT.GY(1)-COINC) THEN
            WRITE (LUSTOT,350) I,GLO(I,2)
350	    FORMAT (' LOW Y CO-ORD OF OBSTACLE ',I1,' IS ',F7.4/
     1              ' THIS IS BEFORE START OF ENCLOSURE'/
     2              ' RUN ABORTED')
            STOP
	 END IF
C        Low y co-ordinate is O.K. - find which cell it comes in
         DO 400 J=1,NGY-1
            IF (GLO(I,2).LT.GY(J+1)-COINC) THEN
	       OBSTYS(I)=J
	       GOTO 600
	    END IF
400      CONTINUE
C        If we reach here low y co-ordinate is too big
         WRITE (LUSTOT,500) I ,GLO(I,2)
500      FORMAT (' Y CO-ORD OF OBSTACLE ',I2, ' BEGINS AT ',F7.4/
     1           ' THIS IS AT OR BEYOND END OF ENCLOSURE'/
     2           ' RUN ABORTED')
         STOP

C        * * * * * * * * * * * *
C        *    Starting z cell  *
C        * * * * * * * * * * * *
C        Check that low z co-ordinate is not too small
600      IF (GLO(I,3).LT.GZ(1)-COINC) THEN
            WRITE (LUSTOT,650) I,GLO(I,3)
650         FORMAT (' LOW Z CO-ORD OF OBSTACLE ',I1,' IS ',F7.4/
     1              ' THIS IS BEFORE START OF ENCLOSURE'/
     2              ' RUN ABORTED')
            STOP
	 END IF
C        Low z co-ordinate is O.K. - find which cell it comes in
         DO 800 J=1,NGZ-1
            IF (GLO(I,3).LT.GZ(J+1)-COINC) THEN
	       OBSTZS(I)=J
	       GOTO 1000
	    END IF
800      CONTINUE
C        If we reach here low z co-ordinate is too big
         PRINT 900, I,GLO(I,3)
900      FORMAT (' Z CO-ORD OF OBSTACLE ',I2,' BEGINS AT ',F7.4/
     1           ' THIS IS AT OR BEYOND THE END OF THE ENCLOSURE'/
     2           ' RUN ABORTED')
         STOP
	 

C        *****************************************************
C        *             Find ending cell numbers              *
C        *   Different method needed for boxes & cylinders   *
C        *****************************************************

1000     IF ((TYPE(I).EQ.'C').OR.(TYPE(I).EQ.'c')) GOTO 2000

C        !!!!!!!!!!!!!!!
C        !    BOXES    !
C        !!!!!!!!!!!!!!!

C        * * * * * * * * * * * *
C        *     Ending x cell   *
C        * * * * * * * * * * * *
C        Find the x cell where the box ends
         DO 1100 J=OBSTXS(I),NGX-1
            IF (GHI(I,1).LT.GX(J+1)+COINC) THEN
	       OBSTXE(I)=J
	       GOTO 1300
	    END IF
1100     CONTINUE
C        If we reach here the high x co-ordinate is too big
	 WRITE (LUSTOT,1200) I,GHI(I,1)
1200     FORMAT (' X CO-ORD OF OBSTACLE ',I2,' ENDS AT ',F7.4/
     1           ' THIS IS BEYOND END OF ENCLOSURE'/
     2           ' RUN ABORTED')
         STOP

C        * * * * * * * * * * * *
C        *     Ending y cell   *
C        * * * * * * * * * * * *
C        Find the y cell where the box ends
1300     DO 1400 J=OBSTYS(I),NGY-1
            IF (GHI(I,2).LT.GY(J+1)+COINC) THEN
	       OBSTYE(I)=J
	       GOTO 1600
	    END IF
1400     CONTINUE
C        If we reach here high y co-ordinate is too big
	 WRITE (LUSTOT,1500) I,GHI(I,2)
1500     FORMAT (' Y CO-ORD OF OBSTACLE ',I2,' ENDS AT ',F7.4/
     1           ' THIS IS BEYOND THE END OF THE ENCLOSURE'/
     2           ' RUN ABORTED')
         STOP

C        * * * * * * * * * * * *
C        *     Ending z cell   *
C        * * * * * * * * * * * *
C        Find the z cell where the box ends
1600     DO 1700 J=OBSTZS(I),NGZ-1
            IF (GHI(I,3).LT.GZ(J+1)+COINC) THEN
	       OBSTZE(I)=J
	       GOTO 1900
	    END IF
1700     CONTINUE
C        If we reach here high z co-ordinate is too big
	 WRITE (LUSTOT,1800) I,GHI(I,3)
1800     FORMAT (' Z CO-ORD OF OBSTACLE ',I2,' ENDS AT ',F7.4/
     1           ' THIS IS BEYOND THE END OF THE ENCLOSURE'/
     2           ' RUN ABORTED')
         STOP

C        All starts and ends of the box are found - go to recording section
1900     GOTO 5000

C        !!!!!!!!!!!!!!!!!!!
C        !    CYLINDERS    !
C        !!!!!!!!!!!!!!!!!!!
C        The cross-section of a cyliner must lie within a single cell
C        This is checked and if it is O.K. then the relevant OBST.Es are set
C        The other OBST.E is set from the high co-ordinate of the centre
2000     IDIR=NINT(GHI(I,2))
         RAD=GHI(I,1)
	 IF (IDIR.EQ.1) THEN 
C           ! ! ! Cylinder parallel to x-axis ! ! !
C           Check that cross-section lies within one cell
            IF ((GLO(I,2)-RAD).LT.(GY(OBSTYS(I))-COINC)) GOTO 2200
	    IF ((GLO(I,2)+RAD).GT.(GY(OBSTYS(I)+1)+COINC)) GOTO 2200
	    IF ((GLO(I,3)-RAD).LT.(GZ(OBSTZS(I))-COINC)) GOTO 2200
	    IF ((GLO(I,3)+RAD).GT.(GZ(OBSTZS(I)+1)+COINC)) GOTO 2200
C           Cross section is O.K. so set OBSTYE and OBSTZE
	    OBSTYE(I)=OBSTYS(I)
	    OBSTZE(I)=OBSTZS(I)
C           Find OBSTXE
	    DO 2100  J=OBSTXS(I),NGX-1
	       IF (GHI(I,3).LT.GX(J+1)+COINC) THEN
	          OBSTXE(I)=J
		  GOTO 2300
	       END IF
2100        CONTINUE	       
C           If we reach here high x co-ordinate is too large 
C           or cross-section spans more than 1 cell
2200        WRITE (LUSTOT,2250) I
2250        FORMAT (' PROBLEM WITH CYLINDER - OBSTACLE ',I2/
     1              ' EITHER CROSS-SECTION SPANS MORE THAN ONE CELL'/
     2              ' OR HIGH CO-ORDINATE BEYOND END OF ENCLOSURE'/
     3              ' RUN ABORTED')     
            STOP
2300	    CONTINUE
         ELSE IF (IDIR.EQ.2) THEN
C           ! ! ! Cylinder is parallel to y-axis ! ! !
C           Check that cross-section lies within one cell
            IF ((GLO(I,1)-RAD).LT.(GX(OBSTXS(I))-COINC)) GOTO 2500
	    IF ((GLO(I,1)+RAD).GT.(GX(OBSTXS(I)+1)+COINC)) GOTO 2500
	    IF ((GLO(I,3)-RAD).LT.(GZ(OBSTZS(I))-COINC)) GOTO 2500
	    IF ((GLO(I,3)+RAD).GT.(GZ(OBSTZS(I)+1)+COINC)) GOTO 2500
C           Cross section is O.K. so set OBSTXE and OBSTZE
	    OBSTXE(I)=OBSTXS(I)
	    OBSTZE(I)=OBSTZS(I)
C           Find OBSTYE
	    DO 2400 J=OBSTYS(I),NGY-1
	       IF (GHI(I,3).LT.GY(J+1)+COINC) THEN
	          OBSTYE(I)=J
		  GOTO 2600
	       END IF
2400        CONTINUE	       
C           If we reach here high y co-ordiante is too large 
C           or cross-section is too large
2500        WRITE (LUSTOT,2550) I
2550        FORMAT (' PROBLEM WITH CYLINDER - OBSTACLE ',I2/
     1              ' EITHER CROSS-SECTION SPANS MORE THAN ONE CELL'/
     2              ' OR HIGH CO-ORDINATE BEYOND END OF ENCLOSURE'/
     3              ' RUN ABORTED')     
            STOP
2600	    CONTINUE
         ELSE IF (IDIR.EQ.3) THEN
C           ! ! ! Cylinder is parallel to z-axis ! ! !
C           Check that cross-section lies within one cell
            IF ((GLO(I,2)-RAD).LT.(GY(OBSTYS(I))-COINC)) GOTO 2800
	    IF ((GLO(I,2)+RAD).GT.(GY(OBSTYS(I)+1)+COINC)) GOTO 2800
	    IF ((GLO(I,1)-RAD).LT.(GX(OBSTXS(I))-COINC)) GOTO 2800
	    IF ((GLO(I,1)+RAD).GT.(GX(OBSTXS(I)+1)+COINC)) GOTO 2800
C           Cross-section is O.K. so set OBSTYE and OBSTXE
	    OBSTYE(I)=OBSTYS(I)
	    OBSTXE(I)=OBSTXS(I)
C           Find OBSTZE
	    DO 2700  J=OBSTZS(I),NGZ-1
	       IF (GHI(I,3).LT.GZ(J+1)+COINC) THEN
	          OBSTZE(I)=J
		  GOTO 2900
	       END IF
2700        CONTINUE	       
C           If we reach here high z co-ordinate is too large
C           or cross-section is too large
2800        WRITE (LUSTOT,2850) I
2850        FORMAT (' PROBLEM WITH CYLINDER - OBSTACLE ',I2/
     1              ' EITHER CROSS-SECTION SPANS MORE THAN ONE CELL'/
     2              ' OR HIGH CO-ORDINATE BEYOND END OF ENCLOSURE'/
     3              ' RUN ABORTED')     
            STOP
2900	    CONTINUE
         END IF

C        *****************************
C        *     Recording section     *
C        *****************************
C        Set NOBST and CELLOB for all cells this obstacle lies in
5000     DO 5400 J=OBSTXS(I),OBSTXE(I)
            DO 5300 K=OBSTYS(I),OBSTYE(I)
	       DO 5200 L=OBSTZS(I),OBSTZE(I)
	          NOBST(J,K,L)=NOBST(J,K,L)+1
C                 Check that there are not too many obstacles in this cell
		  IF (NOBST(J,K,L).GT.NOBCMX) THEN
		     WRITE (LUSTOT,5100) J,K,L,NOBCMX
5100                 FORMAT (' NUMBER OF OBSTACLES IN CELL ',I2,I2,I2,
     1                       ' EXCEEDS MAXIMUM ALLOWED (',I2,')'/
     2                       ' RUN ABORTED')
                     STOP
		  END IF
		  CELLOB(J,K,L,NOBST(J,K,L))=I
5200           CONTINUE
5300        CONTINUE
5400     CONTINUE

5500  CONTINUE		  

      RETURN
      END     


      SUBROUTINE INOUT(LUSTOT)
C
C  Routine to set the array CLINOT which shows whether each cell face
C  in the uniform cross-section is fully inside (CLINOT=2), partially inside
C  (CLINOT=1) or fully outside (CLINOT=-1) the enclosure.  The entire CLINOT
C  array is initialized to zero and then those cells which have wall segments
C  in them have their entries in CLINOT set to 1 or 2 as appropriate in RECWS
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**05/06/90
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,I,J,JINS,JINE,K,ISG,NCROSS,JBACK,INREF,
     1        NB2,NP1B2
      REAL XM,YM,XS,XE,YS,YE,GRAD,C,F,TEST,YB
      LOGICAL LEFT
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTOT  Logical unit number for standard output
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  CELLWS  Array giving the numbers of the wall segments in each cell
C          CELLWS(I,J,K) gives the number of the Kth segment of cell
C          (I,J) of the uniform cross-section 
C  CLINOT  Array indicating the in/out status of cell of the uniform
C          cross-section.  CLINOT(1,I,J) is the value for the (I,J) cell
C          face in the plane z=0 (if the first argument is 2 then the 
C          value is for the (I,J) cell of the plane z=zmax).  On input to
C          this routine all values of CLINOT are 0, 1 or 2 as set in RECWS.
C          The only non-zero ones are those for cells containing wall segments
C  COINC   The largest amount by which 2 values may differ yet still be
C          deemed to coincide
C  GX      Array of co-ordinates of x grid lines
C  GY      Array of co-ordinates of y grid lines
C  MXWLCL  Maximum number of wall allowed ina cell
C  NGX     Number of x grid lines
C  NGY     Number of y grid lines
C  WSEGXS  Array of x co-ordinates at start of each wall segment
C  WSEGYS  Array of y co-ordinates at start of each wall segment
C
C**** Common variables - output
C  CLINOT  Array indicating the in/out status of cells in the uniform
C          cross-section.  On completion of this routine every value of 
C          CLINOT is -1, 1 or 2.
C
C**** Local variables
C  C       y intercept of the equation of a wall segment
C  F       Value of the quantity y-mx-c at the mid point of the current cell
C          The equation y-mx-c=0 is the equation of the current wall segment
C  GRAD    The gradient (m in the above) of the equation of the current wall
C          segment
C  I       Loop counter for x cell number
C  INREF   Value of CLINOT in the reference cell
C  ISG     Current wall segment number
C  J       Loop counter for y cell number
C  JBACK   Loop counter for y cell number starting from J and working back
C  JINE    The y cell number of the last cell in the current column which
C          is inside the enclosure (fully or partially)
C  JINS    The y cell number of the first cell in the current column which
C          is inside the enclosure (fully or partially)
C  K       Loop counter for line segments in current cell
C  LEFT    Logical variable indicating if the middle of the current cell is
C          to the left or the right of the sloping wall in the current cell
C  NB2     NCROSS/2 used to test if NCROSS is even or odd
C  NP1B2   (NCROSS+1)/2 used to test if NCROSS is even or odd
C  NCROSS  Number of wall crossed in moving from centre of the current cell
C          to the reference cell
C  TEST    Parameter used to test if line back from centre of current cell 
C          intersects the current  wall segment
C  XE      x co-ordinate of start of current wall segment
C  XM      x co-ordinate of the mid-point of the current cell
C  XS      x co-ordinate of end of current wall segment
C  YB      y co-ordinate at the bottom of the reference cell
C  YE      y co-ordinate at the end of the current wall segment
C  YM      y co-ordinate at the mid-point of the current cell
C  YS      y co-ordinate at the start of the current wall segment
C  
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Process the cells a column at a time
      DO 900 I=1,NGX-1

C        All cells below the enclosure are outside the enclosure so look in
C        column I for the first cell inside the enclosure
         DO 100 J=1,NGY-1	 
	    IF (CLINOT(1,I,J).EQ.0) THEN
C              RECWS has not set CLINOT for this cell or for a cell below it
C              so it must be outside
	       CLINOT(1,I,J)=-1
	    ELSE
C              RECWS has set CLINOT for this cell - this is the first cell in
C              this column to have CLINOT set so set JINS and proceed to find
C              JINE
	       JINS=J
	       GOTO 200
            END IF     
100	 CONTINUE

C        All cells above the enclosure are outside so look in column I for
C        the last cell which is inside the enclosure
200      DO 300 J=NGY-1,JINS+1,-1
            IF (CLINOT(1,I,J).EQ.0) THEN
C              RECWS has not set CLINOT for this cell or for any cell above
C              it so this cell is outside the enclosure
	       CLINOT(1,I,J)=-1
	    ELSE
C              RECWS has set CLINOT for this cell and so this is the last
C              cell in the enclosure for this column
	       JINE=J
	       GOTO 400
	    END IF
300	 CONTINUE
         JINE=JINS
      
C        Now determine the in/out status of all cells between JINS and JINE
C        in column I
400      DO 800 J=JINS+1,JINE-1
C           If CLINOT has already been set then go on to next cell in column
            IF (CLINOT(1,I,J).NE.0) GOTO 800
C           As CLINOT has not been set for this cell there are no walls here
C           CLINOT may be determined from its value in the last cell
C           If the cell below is out this cell must be out
C           If the cell below is fully in then this cell must be in unless
C           there is a wall across the top of the last cell
C           If the cell below is partially in then we have some work to do
	    IF (CLINOT(1,I,J-1).EQ.-1) THEN
C              Cell below is out
	       CLINOT(1,I,J)=-1
	    ELSE IF (CLINOT(1,I,J-1).EQ.2) THEN
C              Cell below is fully in
	       CLINOT(1,I,J)=2
C              Look for a wall along the top of the cell below
	       DO 500 K=1,MXWLCL
	          IF (CELLWS(I,J-1,K).EQ.0) GOTO 800   
		  ISG=CELLWS(I,J-1,K)
C                 Wall segment ISG is along the top of the cell below if
C                 the y co-ordinates of the beginning and end of the segment
C                 are the same and equal to the y co-ordinate of the relevant
C                 grid line
		  IF ((ABS(WSEGYS(ISG)-GY(J)).LE.COINC).AND.
     1                (ABS(WSEGYS(ISG+1)-GY(J)).LE.COINC)) THEN
C                    There is a wall along the top of the cell below
                     CLINOT(1,I,J)=-1
		     GOTO 800
		  END IF
500	       CONTINUE		  
            ELSE
C              Cell below is partially in so it has at least 1 sloping wall
C              If this is the only wall in the cell below the position of the
C              mid-point of the current cell relative to this wall will show
C              whether the current cell is fully in or fully out
C              If there is more than one wall in the cell below we must count
C              the number of walls crossed in moving from the mid-point of the
C              current cell to a reference cell which is completely inside or
C              completely outside the enclosure
               XM=0.5*(GX(I)+GX(I+1))
	       YM=0.5*(GY(J)+GY(J+1))
	       IF (CELLWS(I,J-1,2).EQ.0) THEN
C                 There is only one wall in the cell below - this is a
C                 sloping wall - determine whether mid-point of current cell
C                 is to the right or to the left of this sloping wall
C                 If it is to the left current cell in completely in
C                 If it is to the right current cell is completely out
                  ISG=CELLWS(I,J-1,1)
		  IF (ISG.EQ.0) THEN
		     WRITE (LUSTOT,550) I,J-1
550                  FORMAT (' NO WALL CELL ',I3,',',I3/
     1                       ' BUT CLINOT FOR THIS CELL IS 1'/
     2                       ' SHOWING THE CELL IS ONLY PARTIALLY IN'/
     3                       ' RUN ABORTED')
		     STOP
		  END IF
C                 Determine start and end co-ordinates of sloping wall
		  XS=WSEGXS(ISG)
		  XE=WSEGXS(ISG+1)
		  YS=WSEGYS(ISG)
		  YE=WSEGYS(ISG+1)
C                 As the wall is sloping it is not parallel to y-axis
                  IF (ABS(XE-XS).LT.COINC) THEN
C                    Wall is parallel to y axis
                     WRITE (LUSTOT,560) I,J-1
560                  FORMAT (' CELL ',I3,',',I3,' HAS ONLY ONE WALL',
     1                       ' AND THIS IS SLOPING'/
     2                       ' BUT THE GRADIENT OF THE WALL IS INFINITE'/
     3                       ' THIS IS A CONTRADICTION'/
     4                       ' RUN ABORTED')
		     STOP
                  END IF
C                 Wall genuinely does slope - finds its gradient & y-intercept
                  GRAD=(YE-YS)/(XE-XS)
     	          C=(XE*YS-XS*YE)/(XE-XS)
C                 Find the value of y-grad.x-c at the mid-point of current
C                 cell as this determines which side it is of sloping wall
		  F=YM-GRAD*XM-C
		  IF (F.GT.0) THEN
		    LEFT=.TRUE.
		  ELSE
		     LEFT=.FALSE.
		  END IF
		  IF (XE.LT.XS) LEFT=.NOT.LEFT
C                 If mid-point of current cell is on the left of the sloping
C                 wall then it is inside otherwise it is outside
		  IF (LEFT) THEN
		     CLINOT(1,I,J)=2
		  ELSE
		     CLINOT(1,I,J)=-1
		  END IF
               ELSE		 
C                 There is more than one wall in the cell below
C                 Draw the line x=XM back until it reaches a cell which
C                 is all in or all out (the reference cell) and count the
C                 number of walls crossed in reaching the reference cell
C                 If this number is odd then the current cell is the opposite
C                 of the reference cell; if this number is even the current
C                 cell is the same as the reference cell
C                 Set counter to 0
                  NCROSS=0
		  DO 700 JBACK=J-1,JINS,-1
C                    Count the number of walls in cell(I,JBACK) crossed
		     DO 600 K=1,MXWLCL
		        IF (CELLWS(I,JBACK,K).EQ.0) GOTO 650
			ISG=CELLWS(I,JBACK,K)
			TEST=(XM-WSEGXS(ISG))*(XM-WSEGXS(ISG+1))
			IF (TEST.LT.0) NCROSS=NCROSS+1
600                  CONTINUE
C                    If cell(I,JBACK) is all in or all out it is used as the
C                    reference cell
650                  IF (CLINOT(1,I,JBACK).NE.1) THEN
                        INREF=CLINOT(1,I,JBACK)
C                       If there is a wall across the bottom of the reference
C                       cell then this will have been counted in NCROSS so 
C                       we must deduct 1 from NCROSS
                        DO 675 K=1,MXWLCL
			   IF (CELLWS(I,JBACK,K).EQ.0) GOTO 750
			   ISG=CELLWS(I,JBACK,K)
			   YB=GY(JBACK-1)
			   IF ((ABS(WSEGYS(ISG+1)-YB).LE.COINC)
     1			   .AND.(ABS(WSEGYS(ISG)-YB).LE.COINC)) THEN
C                             There is a wall across the bottom of the 
C                             reference cell
			      NCROSS=NCROSS-1
       			      GOTO 750
			   END IF
675                     CONTINUE			   
	             END IF
700               CONTINUE
C                 If we reach here no reference cell was found but cell JINS-1
C                 is outside the enclosure so set INREF to -1
                  INREF=-1
C                 Determine if the number of walls crossed is odd or even
C                 by using integer division.  If NCROSS is even then both
C                 NCROSS/2 and (NCROSS+1)/2 are the same
750               NB2=NCROSS/2
                  NP1B2=(NCROSS+1)/2
C                 If the number of walls crossed is even current cell is the
C                 same as the reference cell otherwise it is opposite to the
C                 reference cell
		  IF (NB2.EQ.NP1B2) THEN
                     CLINOT(1,I,J)=INREF
		  ELSE
		     IF (INREF.EQ.2) THEN
		        CLINOT(1,I,J)=-1
	             ELSE
		        CLINOT(1,I,J)=2
	             END IF
		  END IF
	       END IF
	    END IF
800      CONTINUE
900   CONTINUE	       

C     The above has set CLINOT for the plane z=0.  At this stage CLINOT for 
C     the plane z=zmax is the same (it may become different when obstacles
C     are accounted for
      DO 1100 I=1,NGX-1
         DO 1200 J=1,NGY-1
	    CLINOT(2,I,J)=CLINOT(1,I,J)
1200     CONTINUE
1100  CONTINUE	    

      RETURN
      END


      SUBROUTINE AREACL(LUSTOT)
C
C  Routine to calculate the areas of all the surfaces of the enclosure
C  There are 2 kinds of surface to consider: those in the planes z=0 
C  and z=zmax and those which are wall segments.
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**06/06/90
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,I,J,K,ISG
      REAL DXG,DYG,DXSG,DYSG,XS,XE,YS,YE,SEGLEN,DZ
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTOT  Logical unit number for standard output
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  CELLWS  Array giving the segment numbers of the segments in each cell
C  CLINOT  Array giving the in/out status of each cell in the uniform
C          cross-section for the planes z=0 and z=zmax
C  COINC   Largest amount by which 2 values may differ yet still be deemed
C          to coincide
C  GX      Array of grid line x co-ordinates
C  GY      Array of grid line y co-ordinates
C  GZ      Array of grid line z co-ordinates
C  MXWLCL  Maximum number of walls allowed per cell
C  NGX     Number of x grid lines
C  NGY     Number of y grid lines
C  NGZ     Number of z grid lines
C  NSEG    Number of wall segments
C  WSEGXS  Array giving x co-ordinate at start of each wall segment
C  WSEGYS  Array giving y co-ordinate at start of each wall segment
C
C**** Common variables - output
C  AREA    Array giving the areas of the cell faces in the planes z=0 and
C          z=zmax
C  AREASG  Array giving the areas of the wall segments in each slice 
C
C**** Local variables
C  DXG     Difference in x co-ordinates across the current cell
C  DXSG    Difference in x co-ordinates along current segment
C  DYG     Difference in y co-ordinates across the current cell
C  DYSG    Difference in y co-ordinates along current segment
C  DZ      Thickness of current slice
C  I       Loop counter
C  ISG     Current segment number
C  J       Loop counter
C  K       Loop counter
C  SEGLEN  Length of current segment
C  XE      x co-ordinate at end of current segment
C  XS      x co-ordinate at start of current segment
C  YE      y co-ordinate at end of current segment
C  YS      y co-ordinate at start of current segment
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Calculate the cell face areas in plane z=0 first
C     Loop over each cell face in turn
C     For each cell face there are three possibilites:
C     a) face fully inside   b) face fully outside   c)face partially inside
      DO 600 I=1,NGX-1
         DO 500 J=1,NGY-1
	    IF (CLINOT(1,I,J).LT.0) THEN
C              Cell face is fully outside so area is 0
	       AREA(1,I,J)=0
	    ELSE IF (CLINOT(1,I,J).EQ.2) THEN
C              Cell face is fullly inside so area is a rectangle
	       AREA(1,I,J)=(GX(I+1)-GX(I))*(GY(J+1)-GY(J))
	    ELSE
C              Cell face is partially inside and partially outside
C              We wish to calculate the area of the part inside
C              If the sloping segment runs from one x grid line to the other
C              or from one y grid line to the other then the inside area is
C              a trapezium.  But if the sloping segment runs from an x grid
C              line to a y grid line (or vice versa) the inside area is either
C              a triangle of a rectangle with a triangular corner missing
	       DXG=GX(I+1)-GX(I)
	       DYG=GY(J+1)-GY(J)
C              Find the sloping wall
	       DO 200 K=1,MXWLCL
	          IF (CELLWS(I,J,K).EQ.0) THEN
C                    There is no sloping wall
                     WRITE (LUSTOT,100) I,J
100                  FORMAT (' NO SLOPING WALL IN CELL ',I2,',',I2/
     1                       ' BUT CLINOT IS 1 FOR THIS CELL'/
     2                       ' ERROR : RUN ABORTED ')
                     STOP
		  END IF
C                 See if this segment is the sloping wall
		  ISG=CELLWS(I,J,K)
		  DXSG=WSEGXS(ISG+1)-WSEGXS(ISG)
		  DYSG=WSEGYS(ISG+1)-WSEGYS(ISG)
C                 If it is sloping both DXSG and DYSG are non-zero
		  IF (ABS(DXSG).LE.COINC) GOTO 200
                  IF (ABS(DYSG).LE.COINC) GOTO 200
C                 If we reach here we have located the sloping wall
                  GOTO 300
200            CONTINUE
C              If we reach here all walls have been checked and none of them 
C              is sloping
               WRITE (LUSTOT,100) I,J		  
	       STOP

C              Process the sloping wall to determine the area
C              Firstly see if the sloping wall goes from one x grid line to
C              the other
300	       IF (ABS(DXG-ABS(DXSG)).LE.COINC) THEN
C                 Wall goes from 1 x grid line to the other so area is a 
C                 trapezium - if wall goes from left to right then inside
C                 area is the top otherwise it is the bottom
                  IF (DXSG.GT.0) THEN
C                    Inside area is the top trapezium
                     AREA(1,I,J)=0.5*((GY(J+1)-WSEGYS(ISG))
     1		                    +(GY(J+1)-WSEGYS(ISG+1)))*DXG
                  ELSE
C                    Inside area is the bottom trapezium
		     AREA(1,I,J)=0.5*((WSEGYS(ISG)-GY(J))
     1		                    +(WSEGYS(ISG+1)-GY(J)))*DXG
                  END IF
       	       ELSE IF (ABS(DYG-ABS(DYSG)).LE.COINC) THEN
C                 Wall goes from 1 y grid line to the other so area is a
C                 trapezium - if wall goes from bottom to top then inside
C                 area is the left otherwise it is the right
                  IF (DYSG.GT.0) THEN
C                    Inside area is the left trapezium
                     AREA(1,I,J)=0.5*((WSEGXS(ISG)-GX(I))
     1		                    +(WSEGXS(ISG+1)-GX(I)))*DYG
	          ELSE
C                    Inside area is the right trapezium
                     AREA(1,I,J)=0.5*((GX(I+1)-WSEGXS(ISG))
     1                              +(GX(I+1)-WSEGXS(ISG+1)))*DYG
                  END IF
	       ELSE
C                 Sloping wall goes from an x grid line to a y grid line
C                 or vice versa - so divides cell into a triangle & the rest
                  IF (ABS(GX(I)-WSEGXS(ISG)).LE.COINC) THEN
C                    Wall starts on left wall (x=GX(I))
C                    If it ends on top y grid line the triangle is inside
C                    If it ends on bottom y grid line the triangle is outside
                     IF (DYSG.GT.0) THEN
C                       The triangle is inside
                        AREA(1,I,J)=0.5*DYSG*DXSG
		     ELSE
C                       The triangle is outside
C                       NB The area of the triangle is -0.5*DXSG*DYSG since
C                       DYSG<0
                        AREA(1,I,J)=DXG*DYG+0.5*DYSG*DXSG
		     END IF
                  ELSE IF (ABS(GX(I+1)-WSEGXS(ISG)).LE.COINC) THEN
C                    Wall starts on right wall (x=GX(I+1))
C                    If it ends on top y grid line the triangle is outside
C                    If it ends on bottom y grid line the triangle is inside
                     IF (DYSG.GT.0) THEN
C                       The triangle is outside
C                       NB The area of the triangle is -0.5*DXSG*DYSG
C                       since DXSG<0
                        AREA(1,I,J)=DXG*DYG+0.5*DYSG*DXSG
		     ELSE
C                       The triangle is inside
C                       NB The area of the triangle is 0.5*DXSG*DYSG
C                       since both DXSG<0 and DYSG<0
                        AREA(1,I,J)=0.5*DYSG*DXSG
		     END IF
                  ELSE IF (ABS(GY(J)-WSEGYS(ISG)).LE.COINC) THEN
C                    Wall starts on bottom wall (y=GY(J))
C                    If it ends on the left wall the triangle is inside
C                    If it ends on the right wall the triangle is outside
                     IF (DXSG.GT.0) THEN
C                       The triangle is outside
                        AREA(1,I,J)=DXG*DYG-0.5*DYSG*DXSG
		     ELSE
C                       The triangle is inside
C                       NB The area of the triangle is -0.5*DXSG*DYSG
C                       since DXSG<0
                        AREA(1,I,J)=-0.5*DYSG*DXSG
		     END IF
                  ELSE IF (ABS(GY(J+1)-WSEGYS(ISG)).LE.COINC) THEN
C                    Wall starts on top wall (y=GY(J+1))
C                    If it ends on the left wall the triangle is outside
C                    If it ends on the right wall the triangle is inside
                     IF (DXSG.GT.0) THEN
C                       The triangle is inside
C                       NB The area of the triangle is -0.5*DXSG*DYSG
C                       since DYSG<0
                        AREA(1,I,J)=-0.5*DYSG*DXSG
		     ELSE
C                       The triangle is outside
C                       NB The area of the triangle is 0.5*DXSG*DYSG
C                       since both DXSG<0 and DYSG<0
                        AREA(1,I,J)=DXG*DYG-0.5*DYSG*DXSG
		     END IF
		  ELSE
C                    Wall does not start on a boundary of the cell
                     WRITE (LUSTOT,400) ISG,I,J,WSEGXS(ISG),
     1                      WSEGYS(ISG),GX(I),GX(I+1),GY(J),GY(J+1)
400                  FORMAT (' SEGMENT ',I2,' IN CELL ',I2,',',I2,
     1             ' DOES NOT START ANYWHERE ON THE CELL BOUNDARY'/
     2             ' IT STARTS AT ',G11.4,',',G11.4/
     3             ' THE X GRID LINES OF THIS CELL ARE ',G11.4,' AND ',
     4             G11.4/
     5             ' THE Y GRID LINES OF THIS CELL ARE ',G11.4,' AND ',
     6             G11.4/
     7             ' ERROR - RUN ABORTED')
                  END IF
               END IF
	    END IF
500      CONTINUE
600   CONTINUE		 

C     Set area in plane z=zmax to be the same as those in the plane z=0
      DO 800 I=1,NGX-1
         DO 700 J=1,NGY-1
	    AREA(2,I,J)=AREA(1,I,J)
700    	 CONTINUE
800   CONTINUE

C     Calculate the areas of the segments making up the perimeter
C     All these surfaces are rectangles
      DO 1000 I=1,NSEG
         XS=WSEGXS(I)
	 XE=WSEGXS(I+1)
	 YS=WSEGYS(I)
	 YE=WSEGYS(I+1)
	 SEGLEN=SQRT((XE-XS)*(XE-XS)+(YE-YS)*(YE-YS))
	 DO 900 J=1,NGZ-1
	    DZ=GZ(J+1)-GZ(J)
	    AREASG(I,J)=SEGLEN*DZ
900      CONTINUE
1000  CONTINUE	    
		    
      RETURN
      END


      SUBROUTINE AREAMD(LUSTOT)
C
C  Routine to modify the values of the surface areas calculated by
C  AREACL for those surfaces which have an obstacle in contact with them
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**06/06/90
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,I,IXS,IXE,IYS,IYE,IZS,IZE,ISEG,J,K,HI,LOW,X,Y
      REAL YS,YE,ZS,ZE,AREAOB,XS,XE
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  LUSTOT  Logical unit number for standard output
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  CLINOT  Array indicating in/out status of each cell face in the uniform
C          cross-section in planes z=0 and z=zmax.  On entry to this routine
C          all values are -1 (fully out), 1 (part in/part out) or 2 (fully in)
C  COINC   Largest amount by which two values may differ yet still be deemed
C          to coincide
C  GHI     Array of obstacle high co-ordinates
C  GLO     Array of obstacle low co-ordinates
C  GX      Array of x grid line co-ordinates
C  GY      Array of y grid line co-ordinates
C  GZ      Array of z grid line co-ordinates
C  NGZ     Number of z grid ine co-ordinates
C  NOB     Number of obstacles
C  PI      Pi (3.14159.....)
C  OBSTXE  Array of x cell number of end of each obstacle
C  OBSTXS  Array of x cell number of start of each obstacle
C  OBSTYE  Array of y cell number of end of each obstacle
C  OBSTYS  Array of y cell number of start of each obstacle
C  OBSTZE  Array of z cell number of end of each obstacle
C  OBSTZS  Array of z cell number of start of each obstacle
C  TYPE    Array of obstacle type (B for boxes or C for cylinders)
C
C**** Common variables - output
C  AREA    Array of cell face areas for planes z=0 and z=zmax
C  AREASG  Array of segment areas for each slice
C  CLINOT  Array indicating the in/out status of cell faces in the planes 
C          z=0 and z=zmax.  The values CLINOT may take and their meanings are
C          -1  cell entirely outside enclosure
C           1  cell face part in and part out, unobscured by obstacles
C           2  cell face fully in, unobscured by obstacles
C           3  cell face part in and part out, partly obscured by a cylinder
C           4  cell face part in and part out, partly obscured by a box
C           5  cell face fully in, partly obscured by a cylinder
C           6  cell face fully in, partly obscured by a box
C           7  cell face fully in, entirely obscured by a box
C  OBINOT  Logical array indicating if surfaces of obstacles are unobscured
C          (in) or obscured (out).  OBINOT(IOB,IX,IY,ISURF) relates to surface
C          ISURF of obstacle IOB in cell (IX,IY).  True shows in whilst false
C          shows out.  ISURF has a value in the range 1-7 as below:
C          ISURF = 1  - low x face
C                  2  - low z face
C                  3  - high z face
C                  4  - high y face
C                  5  - low y face
C                  6  - high x face
C                  7  -  curved surface of a cylinder
C  SGINOT  Array indicating the in/out status of wall segments in each slice
C          Values of SGINOT are:
C          SGINOT = 1  - segment unobscured
C                   2  - segment partly obscured by a cylinder
C                   3  - segment partly obscured by a box
C                   4  - segment entirely obscured by a box
C
C**** Local variables
C  AREAOB  Area of the obstacle which is obscured by contact with a surface
C  HI      Integer flag, value 1, passed to FINDSG
C  I       Loop counter on the obstacles
C  ISEG    Current segment number returned by FINDSG
C  IXE     x cell number at end of current obstacle
C  IXS     x cell number at start of current obstacle
C  IYE     y cell number at end of current obstacle
C  IYS     y cell number at start of current obstacle
C  IZE     z cell number at end of current obstacle
C  IZS     z cell number at start of current obstacle
C  J       Loop counter on cell numbers
C  K       Loop counter on cell numbers
C  LOW     Integer flag, value 0, passed to FINDSG
C  X       Integer flag, value 1, passed to FINDSG
C  XE      x co-ordinate at end of obstacle in current cell
C  XS      x co-ordinate at start of obstacle in current cell
C  Y       Integer flag, value 0, passed to FINDSG
C  YE      y co-ordinate at end of obstacle in current cell
C  YS      y co-ordinate at start of obstacle in current cell
C  ZE      z co-ordinate at end of obstacle in current cell
C  ZS      z co-ordinate at start of obstacle in current cell
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  FINDSG  Determine if there is a wall segment along a specified boundary
C          of the current cell.  If there is, find its segment number.
C
C******************************************************************************

C     Set HI, LOW, X and Y 
      HI=1
      LOW=0
      X=1
      Y=0

C     Process each obstacle in turn
      DO 2100 I=1,NOB
C        Determine starting and ending cell numbers
         IXS=OBSTXS(I)
         IXE=OBSTXE(I)
         IYS=OBSTYS(I)
         IYE=OBSTYE(I)
         IZS=OBSTZS(I)
         IZE=OBSTZE(I)

         IF ((TYPE(I).EQ.'C').OR.(TYPE(I).EQ.'c')) THEN
C           *****  obstacle is a cylinder *****

            IF (NINT(GHI(I,2)).EQ.1) THEN
C              **** parallel to x axis ****

C              See if low face is on an x grid line
C              if not go and check high face
               IF (ABS(GLO(I,1)-GX(IXS)).GT.COINC) GOTO 100
C              Low face is on an x grid line - see if there is a wall segment
C              along this grid line
	       CALL FINDSG(IXS,IYS,LOW,X,ISEG)
	       IF (ISEG.EQ.0) GOTO 100
C              There is a wall segment along this grid line so modify the area
C              and set the appropriate in/out array
	       SGINOT(ISEG,IZS)=2
	       AREASG(ISEG,IZS)=AREASG(ISEG,IZS)-PI*GHI(I,1)*GHI(I,1)
	       OBINOT(I,IYS,IZS,1)=.FALSE.

C              See if high face is on an x grid line
C              If not this obstacle is processed go on to next obstacle
100	       IF (ABS(GHI(I,3)-GX(IXE+1)).GT.COINC) GOTO 2100
C              High face is on an x grid line - see if there is a wall segment
C              along this grid line
               CALL FINDSG(IXE,IYS,HI,X,ISEG)
	       IF (ISEG.EQ.0) GOTO 2100
C              There is a wall segment along this grid line so modify the area
C              and set the appropriate in/out array
               SGINOT(ISEG,IZS)=2
	       AREASG(ISEG,IZS)=AREASG(ISEG,IZS)-PI*GHI(I,1)*GHI(I,1)
	       OBINOT(I,IYS,IZS,6)=.FALSE.

	    ELSE IF (NINT(GHI(I,2)).EQ.2) THEN
C              **** parallel to y axis ****

C              See if low face is along a y grid line
C              If not go and check high face
               IF (ABS(GLO(I,2)-GY(IYS)).GT.COINC) GOTO 200
C              Low face is on a y grid line - see if there is a wall segment
C              along this grid line
               CALL FINDSG(IXS,IYS,LOW,Y,ISEG)
	       IF (ISEG.EQ.0) GOTO 200
C              there is a wall segment along this grid line so modify the area
C              and set the appropriate in/out arrays
               SGINOT(ISEG,IZS)=2
	       AREASG(ISEG,IZS)=AREASG(ISEG,IZS)-PI*GHI(I,1)*GHI(I,1)
	       OBINOT(I,IXS,IZS,5)=.FALSE.
C              See if high face is along a y grid line
C              If not this obstacle has been processed go to next obstacle
200            IF (ABS(GHI(I,3)-GY(IYE+1)).GT.COINC) GOTO 2100
C              The high face is along a y grid line - see if there is a wall
C              segment along this grid line
               CALL FINDSG(IXS,IYE,HI,Y,ISEG)
	       IF (ISEG.EQ.0) GOTO 2100
C              There is a wall segment along this grid line so modify the area
C              and set the appropriate in/out arrays
               SGINOT(ISEG,IZS)=2
	       AREASG(ISEG,IZS)=AREASG(ISEG,IZS)-PI*GHI(I,1)*GHI(I,1)
	       OBINOT(I,IXS,IZS,4)=.FALSE.

	    ELSE
C              **** parallel to z axis ****

C              See if the low face is in z=0 plane
C              If not go and check the high face
               IF (ABS(GLO(I,3)).GT.COINC) GOTO 300
C              Check that this cell surface is part of the enclosure
	       IF (CLINOT(1,IXS,IYS).EQ.-1) THEN
                  WRITE (LUSTOT,250) IOB
250               FORMAT (' OBSTACLE ',I2,' STARTS AT A SURFACE OUTSIDE',
     1                    ' ENCLOSURE'/' RUN ABORTED')
		  STOP
	       END IF
C              Modify area and set appropraite in/out arrays
	       IF (CLINOT(1,IXS,IYS).EQ.1) CLINOT(1,IXS,IYS)=3
	       IF (CLINOT(1,IXS,IYS).EQ.2) CLINOT(1,IXS,IYS)=5
	       AREA(1,IXS,IYS)=AREA(1,IXS,IYS)-PI*GHI(I,1)*GHI(I,1)
	       OBINOT(I,IXS,IYS,2)=.FALSE.
C              See if high z face is in z=zmax plane
300	       IF (ABS(GHI(I,3)-GZ(NGZ)).GT.COINC) GOTO 2100
C              It is in z=zmax plane - check that the cell face is inside
C              the enclosure
               IF (CLINOT(2,IXS,IYS).EQ.-1) THEN
                  WRITE (LUSTOT,350) IOB
350               FORMAT (' OBSTACLE ',I2,' ENDS AT A SURFACE OUTSIDE',
     1                    ' ENCLOSURE '/' RUN ABORTED')
		  STOP
	       END IF
C              Modify area and sett appropriate in/out arrays
	       IF (CLINOT(2,IXS,IYS).EQ.1) CLINOT(2,IXS,IYS)=3
	       IF (CLINOT(2,IXS,IYS).EQ.2) CLINOT(2,IXS,IYS)=5
	       AREA(2,IXS,IYS)=AREA(2,IXS,IYS)-PI*GHI(I,1)*GHI(I,1)
	       OBINOT(I,IXS,IYS,3)=.FALSE.
 	    END IF


         ELSE
C           *****   obstacle is a box   *****

C           ****   low x end   ****

C           See if low x end lies along a grid line
C           If not go to high x end
C           IF NOT CHECK HIGH X END
            IF (ABS(GLO(I,1)-GX(IXS)).GT.COINC) GOTO 600
C           Low x end does lie along a grid line
C           Process each cell that makes up the low x face to see if it has
C           a segment along this grid line
            DO 500 J=IYS,IYE
C              Look for a segment along this grid line
	       CALL FINDSG(IXS,J,LOW,X,ISEG)	     
	       IF (ISEG.EQ.0) GOTO 500
C              There is a segment along this grid line - so area must
C              be modified and appropriate in/out arrays set
C              Calculate the area obscured
C              Find y co-ordinates at start and end of obstacle in this cell
	       IF (J.EQ.IYS) THEN
	          YS=GLO(I,2)
	       ELSE
		  YS=GY(J)
	       END IF
	       IF (J.EQ.IYE) THEN
		  YE=GHI(I,2)
	       ELSE
              	  YE=GY(J+1)
	       END IF
	       DO 400 K=IZS,IZE
C                 Find z co-ordinates at start & end of obstacle in this cell
		  IF (K.EQ.IZS) THEN
		     ZS=GLO(I,3)
	          ELSE
		     ZS=GZ(K)
		  END IF
		  IF (K.EQ.IZE) THEN
		     ZE=GHI(I,3)
		  ELSE
		     ZE=GZ(K+1)
		  END IF
C                 Calculate area obscured
		  AREAOB=(YE-YS)*(ZE-ZS)
C                 Modify area and set in/out arrays
		  IF (ABS(AREASG(ISEG,K)-AREAOB).LE.COINC) THEN
		     AREASG(ISEG,K)=0.
		     SGINOT(ISEG,K)=4
		  ELSE
		     AREASG(ISEG,K)=AREASG(ISEG,K)-AREAOB
		     SGINOT(ISEG,K)=3
		  END IF
		  OBINOT(I,J,K,1)=.FALSE.
400            CONTINUE
500         CONTINUE		   

C           ****   high x end    ****

C           See if high x end lies along a grid line
C           If not go to low y end
600         IF (ABS(GHI(I,1)-GX(IXE+1)).GT.COINC) GOTO 900
C           High x end does lie along an x grid line 
C           Process each cell that makes up the high x face to see if it has 
C           a segment along this grid line
            DO 800 J=IYS,IYE
C              Look for a wall along this grid line
	       CALL FINDSG(IXE,J,HI,X,ISEG)	     
	       IF (ISEG.EQ.0) GOTO 800
C              There is a segment along this grid line - so area must
C              be modified and appropriate in/out arrays set
C              Calculate the area obscured
C              Find y co-ordinates at start & end of obstacle in this cell
	       IF (J.EQ.IYS) THEN
	          YS=GLO(I,2)
	       ELSE
		  YS=GY(J)
	       END IF
	       IF (J.EQ.IYE) THEN
		  YE=GHI(I,2)
	       ELSE
              	  YE=GY(J+1)
	       END IF
	       DO 700 K=IZS,IZE
C                 Find z co-ordinates at start & end of obstacle in this cell
		  IF (K.EQ.IZS) THEN
		     ZS=GLO(I,3)
	          ELSE
		     ZS=GZ(K)
		  END IF
		  IF (K.EQ.IZE) THEN
		     ZE=GHI(I,3)
		  ELSE
		     ZE=GZ(K+1)
		  END IF
C                 Modify area and set in/out arrays
		  AREAOB=(YE-YS)*(ZE-ZS)
		  IF (ABS(AREASG(ISEG,K)-AREAOB).LE.COINC) THEN
		     AREASG(ISEG,K)=0.
		     SGINOT(ISEG,K)=4
		  ELSE
		     AREASG(ISEG,K)=AREASG(ISEG,K)-AREAOB
		     SGINOT(ISEG,K)=3
		  END IF
		  OBINOT(I,J,K,6)=.FALSE.
700            CONTINUE
800         CONTINUE		   

C           ****   low y end   ****
C           See if low y end lies along a grid line
C           If not go to high y end
900         IF (ABS(GLO(I,2)-GY(IYS)).GT.COINC) GOTO 1200
C           Process each cell that make up the low y face to see if it has
C           a segment along this grid line
            DO 1100 J=IXS,IXE
C              Look for a wall segment along this grid line
	       CALL FINDSG(J,IYS,LOW,Y,ISEG)	     
	       IF (ISEG.EQ.0) GOTO 1100
C              There is a segment along this grid line - so area must
C              be modified and appropriate in/out arrays set
C              Calculate the area obscured
C              Find x co-ordinates at start & end of obstacle in this cell
	       IF (J.EQ.IXS) THEN
	          XS=GLO(I,1)
	       ELSE
		  XS=GX(J)
	       END IF
	       IF (J.EQ.IXE) THEN
		  XE=GHI(I,1)
	       ELSE
              	  XE=GX(J+1)
	       END IF
	       DO 1000 K=IZS,IZE
C                 find z co-ordinates at start & end of obstacle in this cell
		  IF (K.EQ.IZS) THEN
		     ZS=GLO(I,3)
	          ELSE
		     ZS=GZ(K)
		  END IF
		  IF (K.EQ.IZE) THEN
		     ZE=GHI(I,3)
		  ELSE
		     ZE=GZ(K+1)
		  END IF
C                 Modify area and set in/out arrays
		  AREAOB=(XE-XS)*(ZE-ZS)
		  IF (ABS(AREASG(ISEG,K)-AREAOB).LE.COINC) THEN
		     AREASG(ISEG,K)=0.
		     SGINOT(ISEG,K)=4
		  ELSE
		     AREASG(ISEG,K)=AREASG(ISEG,K)-AREAOB
		     SGINOT(ISEG,K)=3
		  END IF
		  OBINOT(I,J,K,5)=.FALSE.
1000           CONTINUE
1100        CONTINUE		   

C           ****    high y end ****

C           See if high y end lies along a grid line
C           If not go to low z end
1200        IF (ABS(GHI(I,2)-GY(IYE+1)).GT.COINC) GOTO 1500
C           High y end does lie along a grid line
C           Process each cell that makes up the high y face to see if it has
C           a segment along this grid line
            DO 1400 J=IXS,IXE
C              Look for a wall segment along this grid line
	       CALL FINDSG(J,IYE,HI,Y,ISEG)	     
	       IF (ISEG.EQ.0) GOTO 1400
C              There is a segment along this grid line - so area must
C              be modified and appropriate in/out arrays set
C              Calculate the area obscured
C              Find x co-ordinate at start & end of obstacle in this cell
	       IF (J.EQ.IXS) THEN
	          XS=GLO(I,1)
	       ELSE
		  XS=GX(J)
	       END IF
	       IF (J.EQ.IXE) THEN
		  XE=GHI(I,1)
	       ELSE
              	  XE=GX(J+1)
	       END IF
	       DO 1300 K=IZS,IZE
C                 Find z co-ordinate at start & end of obstacle in this cell
		  IF (K.EQ.IZS) THEN
		     ZS=GLO(I,3)
	          ELSE
		     ZS=GZ(K)
		  END IF
		  IF (K.EQ.IZE) THEN
		     ZE=GHI(I,3)
		  ELSE
		     ZE=GZ(K+1)
		  END IF
C                 Modify area and set in/out flags
		  AREAOB=(XE-XS)*(ZE-ZS)
		  IF (ABS(AREASG(ISEG,K)-AREAOB).LE.COINC) THEN
		     AREASG(ISEG,K)=0.
		     SGINOT(ISEG,K)=4
		  ELSE
		     AREASG(ISEG,K)=AREASG(ISEG,K)-AREAOB
		     SGINOT(ISEG,K)=3
		  END IF
		  OBINOT(I,J,K,4)=.FALSE.
1300           CONTINUE
1400        CONTINUE		   

C           ****   low z end   ****

C           See if low z end lies along z=0
C           If not go to high z end
1500        IF (ABS(GLO(I,3)).GT.COINC) GOTO 1800
C           Low z end does lie along z=0
C           Process each cell that makes up the low z end - they all have
C           a wall at z=0 unless they are outside the enclosure
            DO 1700 J=IXS,IXE
C              Find x co-ordinate at start & end of obstacle in this cell
	       IF (J.EQ.IXS) THEN
	          XS=GLO(I,1)
	       ELSE
		  XS=GX(J)
	       END IF
	       IF (J.EQ.IXE) THEN
		  XE=GHI(I,1)
	       ELSE
              	  XE=GX(J+1)
	       END IF
	       DO 1600 K=IYS,IYE
C                 Find y co-ordinate at start & end of obstacle in this cell
		  IF (K.EQ.IYS) THEN
		     YS=GLO(I,2)
	          ELSE
		     YS=GY(K)
		  END IF
		  IF (K.EQ.IYE) THEN
		     YE=GHI(I,2)
		  ELSE
		     YE=GY(K+1)
		  END IF
C                 Check that this cell surface is inside the enclosure
                  IF (CLINOT(1,J,K).EQ.-1) THEN
                     WRITE (LUSTOT,1550) IOB
1550                 FORMAT (' OBSTACLE ',I2,' START AT A SURFACE',
     1                       ' OUTSIDE THE ENCLOSURE '/' RUN ABORTED')
		     STOP
		  END IF
C                 Modify area and set in/out arrays
		  AREAOB=(XE-XS)*(YE-YS)
		  IF (ABS(AREA(1,J,K)-AREAOB).LE.COINC) THEN
		     AREA(1,J,K)=0.
		     CLINOT(1,J,K)=7
		  ELSE
		     AREA(1,J,K)=AREA(1,J,K)-AREAOB
		     IF (CLINOT(1,J,K).EQ.1) CLINOT(1,J,K)=4
		     IF (CLINOT(1,J,K).EQ.2) CLINOT(1,J,K)=6
		  END IF
		  OBINOT(I,J,K,2)=.FALSE.
1600           CONTINUE
1700        CONTINUE		   

C           ****   high z end   ****

C           See if high z end is inplane z=zmax
C           If not this obstacle is finsihed - go to next one
1800        IF (ABS(GHI(I,3)-GZ(NGZ)).GT.COINC) GOTO 2100
C           High z end is in plane z=zmax
C           Process each cell that makes up the high z end - they all have a
C           wall at z=zmax unless they are outside the enclosure
            DO 2000 J=IXS,IXE
C              Find x co-ordinates at start & end of obstacle in this cell
	       IF (J.EQ.IXS) THEN
	          XS=GLO(I,1)
	       ELSE
		  XS=GX(J)
	       END IF
	       IF (J.EQ.IXE) THEN
		  XE=GHI(I,1)
	       ELSE
              	  XE=GX(J+1)
	       END IF
	       DO 1900 K=IYS,IYE
C                 Find y co-ordinates at start & end of obstacle in this cell
		  IF (K.EQ.IYS) THEN
		     YS=GLO(I,2)
	          ELSE
		     YS=GY(K)
		  END IF
		  IF (K.EQ.IYE) THEN
		     YE=GHI(I,2)
		  ELSE
		     YE=GY(K+1)
		  END IF
C                 Check that this cell surface is inside the enclosure
                  IF (CLINOT(2,J,K).EQ.-1) THEN
                     WRITE (LUSTOT,1850) IOB
1850                 FORMAT (' OBSTACLE ',I2,' ENDS AT A SURFACE',
     1                       ' OUTSIDE THE ENCLOSURE '/' RUN ABORTED')
		     STOP
		  END IF
C                 Modify area and set in/out arrays
		  AREAOB=(XE-XS)*(YE-YS)
		  IF (ABS(AREA(2,J,K)-AREAOB).LE.COINC) THEN
		     AREA(2,J,K)=0.
		     CLINOT(2,J,K)=7
		  ELSE
		     AREA(2,J,K)=AREA(2,J,K)-AREAOB
		     IF (CLINOT(2,J,K).EQ.1) CLINOT(2,J,K)=4
		     IF (CLINOT(2,J,K).EQ.2) CLINOT(2,J,K)=6
		  END IF
		  OBINOT(I,J,K,3)=.FALSE.
1900           CONTINUE
2000        CONTINUE		   
         END IF
2100  CONTINUE


      RETURN
      END


      SUBROUTINE FINDSG(IX,IY,HILOW,XORY,ISEG)
C
C  Routine to determine if cell (IX,IY) has a wall lying along the
C  cell boundary specified by HILOW and XORY (HILOW=0 for low and =1
C  for high; XORY=1 for x and =0 for y).  If a wall segment is found
C  ISEG is given the number of the segment otherwise ISEG=0
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**06/06/90
C
C
      include 'comvar.for'
C
      INTEGER IX,IY,HILOW,XORY,ISEG,J
      REAL COORD,START,FINISH
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  HILOW   Integer flag indicating if we are looking for a segment along
C          a high boundary (HILOW=1) or a low boundary (HILOW=0)
C  IX      x cell number
C  IY      y cell number
C  XORY    Integer flag indicating if we are looking for an x boundary
C          (XORY=1) or a y boundary (XORY=0)
C  
C**** Arguments - output
C  ISEG    The segment number if one is found along the appropriate boundary
C          otherwise zero
C
C**** Common variables - input
C  CELLWS  Array giving the segment numbers in each cell
C  GX      Array of x grid line co-ordinates
C  GY      Array of y grid line co-ordinates
C  MXWLCL  Maximum number of wall allowed in a cell
C  WSEGXS  Array giving x co-ordinates at start of each segment
C  WSEGYS  Array giving y co-ordinates at start of each segment
C
C**** Common variables - output
C  NONE
C
C**** Local variables
C  COORD   The co-ordinate value which does not change along the cell
C          boundary we are interested in
C  FINISH  The appropriate co-ordinate value at the end of the wall
C  J       Loop counter
C  START   The appropriate co-ordinate value at the start of the wall
C  
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     On the boundary in question one co-ordinate does not change
C     Find this unchanging value
      IF (XORY.EQ.0) THEN
C        Boundary is y=const
         COORD=GY(IY+HILOW)
      ELSE
C        Boundary is x=constant
         COORD=GX(IX+HILOW)
      END IF

C     Search through all the walls in this cell for one along the boundary
C     in question
      DO 100 J=1,MXWLCL
         ISEG=CELLWS(IX,IY,J)
	 IF (ISEG.EQ.0) GOTO 200
	 IF (XORY.EQ.0) THEN
C           Looking for a y boundary - find start and end y co-ordinates
	    START=WSEGYS(ISEG)
	    FINISH=WSEGYS(ISEG+1)
	 ELSE
C           Looking for an x boundary - find start and end x co-ordinates
	    START=WSEGXS(ISEG)
	    FINISH=WSEGXS(ISEG+1)
	 END IF
C        If start and end co-ordinates are both the same as COORD then
C        this segment does lie along the boundary in question
	 IF (ABS(START-COORD).GT.COINC) GOTO 100
	 IF (ABS(FINISH-COORD).GT.COINC) GOTO 100
	 GOTO 200
100   CONTINUE
      ISEG=0

200   CONTINUE

      RETURN
      END      	 


      SUBROUTINE ELMENT(LUSTOT,FINVOL,NVMX,NZMX)
C
C  Routine to count the number of surface elements in the grid and record the
C  relevant information about them also to counter the number of volume
C  elements and record the relevant information about them
C  
C*****VERSION 2.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**26/10/90
C
C
      include 'comvar.for'
C
      INTEGER LUSTOT,IX,IY,IZ,ISEG,IOB,IXS,IXE,IYS,IYE,IZS,IZE,IDIR,
     1        ISURFL,ISURFH,NVMX,NZMX
      REAL CLTYPE,DX,DY,DZ,DH,FINVOL(NVMX,NVMX,NZMX)
      LOGICAL LOW,HIGH
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  FINVOL  Array of fine grid cell volumes
C  LUSTOT  Logical unit number for standard input
C  NVMX    Maximum number of vertices allowed (dimension of FINVOL)
C  NZMX    Maximum number of z slices allowed (dimension of FINVOL)
C
C**** Arguments - output
C  NONE
C
C**** Common variables - input
C  AREA      Array of cell face areas
C  AREASG    Array of boundary segment areas
C  CLINOT    Array giving in/out status of cell surfaces
C  GHI       Array of obstacle high co-ordinates
C  GLO       Array of obstacle low co-ordinates
C  GX        Array of x grid line co-ordinates
C  GY        Array of y grid line co-ordinates
C  GZ        Array of z grid line co-ordinates
C  NGX       Number of x grid lines
C  NGY       Number of y grid lines
C  NGZ       Number of z grid lines
C  NOB       Number of obstacles
C  NSEG      Number of boundary segments
C  OBINOT    Logical array giving in/out status of obstacle surfaces
C  OBSTXE    Array of obstacle ending x cell numbers
C  OBSTXS    Array of obstacle starting x cell numbers
C  OBSTYE    Array of obstacle ending y cell numbers
C  OBSTYS    Array of obstacle starting y cell numbers
C  OBSTZE    Array of obstacle ending z cell numbers
C  OBSTZS    Array of obstacle starting z cell numbers
C  PI        Pi
C  SGINOT    Array giving in/out status of boundary segments
C  TYPE      Array of obstacle types
C
C**** Common variables - output
C  CLSFZN    Array giving the element number of each cell surface in the 
C            planes z=0 and z=zmax
C  CLVLZN    Array giving the volume element numbers of each fine grid cell
C  ELAREA    Array giving the area of each element
C  NELEMS    Number of surface elements
C  NELEMV    Number of volume elements
C  NSFZCP    Array giving the number of components in each element
C            N.B. This is always one - In effect this array is now entirely
C            redundant
C  OBSFZN    Array giving the element number of each obstacle surface
C  SFZN      Array of element information which varies according to the type
C            of the element.  SFZN(IELEM,1,1) gives the surface type of 
C            component 1 of element IELEM.  The second argument is now made
C            redundant as each element has only 1 component.
C            SFZN(IELEM,1,1) = 1 surface is a boundary segment
C                            = 2 surface is a cell face
C                            = 3 surface is an obstacle surface
C            If SFZN(IELEM,1,1)=1 then SFZN(IELEM,1,2) gives slice number
C                                      SFZN(IELEM,1,3) gives segment number
C            If SFZN(IELEM,1,1)=2 then SFZN(IELEM,1,2) gives z plane (1 means
C                                      plane z=0 and NGZ means plane z=zmax)
C                                      SFZN(IELEM,1,3) gives x cell number
C                                      SFZN(IELEM,1,4) gives y cell number
C            If SFZN(IELEM,1,1)=3 then SFZN(IELEM,1,2) gives x cell number
C                                      SFZN(IELEM,1,3) gives y cell number
C                                      SFZN(IELEM,1,4) gives z cell number
C                                      SFZN(IELEM,1,5) gives surface number
C                                      (1 for lo x, 2 for lo z, 3 for hi z,
C                                       4 for hi y, 5 for lo y, 6 for hi x)
C  SGSFZN    Array giving the element number of each boundary segment
C  VLZN      Array giving information about the volume elements
C            VLZN(IV,1) x cell number of volume element IV
C            VLZN(IV,2) y cell number of volume element IV
C            VLZN(IV,3) z cell number of volume element IV
C
C**** Local variables
C  CLTYPE    Value of CLINOT for the current cell
C  DH        Length of cylinder through current cell
C  DX        x length of obstacle through current cell
C  DY        y length of obstacle through current cell
C  DZ        z length of obstacle through current cell
C  HIGH      Logical flag indicating if the high surface of a cylinder is
C            unobscured by contact with a wall
C  IDIR      Direction of cylinder
C  IOB       Loop counter for obstacle numbers
C  ISEG      Loop counter for boundary segments
C  ISURFH    Surface number for high surface of a cylinder
C  ISURFL    Surface number for low surface of a cylinder
C  IX        Loop counter over x cells
C  IXE       Ending x cell number of current obstacle
C  IXS       Starting x cell number of current obstacle
C  IY        Loop counter over y cells
C  IYE       Ending y cell number of current obstacle
C  IYS       Starting y cell number of current obstacle
C  IZ        Loop counter over z cells
C  IZE       Ending z cell number of current obstacle
C  IZS       Starting z cell number of current obstacle
C  LOW       Logical flag indicating if the low surface of a cylinder is 
C            unobscured by a wall
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************
C-----------------------------------------------------------------------
C               COUNT AND RECORD THE SURFACE ELEMENTS
C-----------------------------------------------------------------------
C     Set number of surface elements counter to zero
      NELEMS=0

C     Count up the elements and record their information
C     Start in plane z=0
      DO 200 IX=1,NGX-1
         DO 100 IY=1,NGY-1
            CLTYPE=CLINOT(1,IX,IY)
C           Check that some of this cell face is visible
            IF ((CLTYPE.EQ.-1).OR.(CLTYPE.EQ.7)) GOTO 100
C           Increment number of elements and record data
            NELEMS=NELEMS+1
            CLSFZN(1,IX,IY)=NELEMS
            NSFZCP(NELEMS)=1
            SFZN(NELEMS,1,1)=2
            SFZN(NELEMS,1,2)=1
            SFZN(NELEMS,1,3)=IX
	    SFZN(NELEMS,1,4)=IY
            ELAREA(NELEMS)=AREA(1,IX,IY)
100      CONTINUE
200   CONTINUE

C     Next go round the boundary of each slice
      DO 400 IZ=1,NGZ-1
         DO 300 ISEG=1,NSEG
C           Check that some of the segment is visible
            IF (SGINOT(ISEG,IZ).EQ.4) GOTO 300
C           Increment counter and record data
            NELEMS=NELEMS+1
            SGSFZN(IZ,ISEG)=NELEMS
            NSFZCP(NELEMS)=1
            SFZN(NELEMS,1,1)=1
            SFZN(NELEMS,1,2)=IZ
            SFZN(NELEMS,1,3)=ISEG
            ELAREA(NELEMS)=AREASG(ISEG,IZ)
300      CONTINUE
400   CONTINUE

C     Next process plane z=zmax
      DO 600 IX=1,NGX-1
         DO 500 IY=1,NGY-1
            CLTYPE=CLINOT(2,IX,IY)
C           Check that some of the cell face is visible
            IF ((CLTYPE.EQ.-1).OR.(CLTYPE.EQ.7)) GOTO 500
C           Increment counter and record data
            NELEMS=NELEMS+1
            CLSFZN(2,IX,IY)=NELEMS
            NSFZCP(NELEMS)=1
            SFZN(NELEMS,1,1)=2
            SFZN(NELEMS,1,2)=NGZ
            SFZN(NELEMS,1,3)=IX
            SFZN(NELEMS,1,4)=IY
            ELAREA(NELEMS)=AREA(2,IX,IY)
500      CONTINUE
600   CONTINUE

C     Finally process the obstacles
      DO 2200 IOB=1,NOB
         IF (TYPE(IOB).EQ.'B') THEN
C           **** Box ****
C           Determien start and end cells
            IXS=OBSTXS(IOB)
            IXE=OBSTXE(IOB)
            IYS=OBSTYS(IOB)
            IYE=OBSTYE(IOB)
            IZS=OBSTZS(IOB)
            IZE=OBSTZE(IOB)
C           **** Record each face of the box in turn ****
C           Record low x face
            DO 800 IY=IYS,IYE
               DO 700 IZ=IZS,IZE
C                 Check that this face is not obscured
                  IF (.NOT.OBINOT(IOB,IY,IZ,1)) GOTO 700
C                 Increment counter and record data
                  NELEMS=NELEMS+1
                  OBSFZN(IXS,IY,IZ,1)=NELEMS
                  NSFZCP(NELEMS)=1
                  SFZN(NELEMS,1,1)=3
                  SFZN(NELEMS,1,2)=IXS
                  SFZN(NELEMS,1,3)=IY
                  SFZN(NELEMS,1,4)=IZ
                  SFZN(NELEMS,1,5)=1
                  DY=MIN(GY(IY+1),GHI(IOB,2))-MAX(GY(IY),GLO(IOB,2))
                  DZ=MIN(GZ(IZ+1),GHI(IOB,3))-MAX(GZ(IZ),GLO(IOB,3))
                  ELAREA(NELEMS)=DY*DZ
700            CONTINUE
800         CONTINUE
C           Record low z face
            DO 1000 IX=IXS,IXE
               DO 900 IY=IYS,IYE
C                 Check that this face is not obscured
                  IF (.NOT.OBINOT(IOB,IX,IY,2)) GOTO 900
C                 Increment counter and record data
                  NELEMS=NELEMS+1
                  OBSFZN(IX,IY,IZS,2)=NELEMS
                  NSFZCP(NELEMS)=1
                  SFZN(NELEMS,1,1)=3
                  SFZN(NELEMS,1,2)=IX
                  SFZN(NELEMS,1,3)=IY
                  SFZN(NELEMS,1,4)=IZS
                  SFZN(NELEMS,1,5)=2
                  DX=MIN(GX(IX+1),GHI(IOB,1))-MAX(GX(IX),GLO(IOB,1))
                  DY=MIN(GY(IY+1),GHI(IOB,2))-MAX(GY(IY),GLO(IOB,2))
                  ELAREA(NELEMS)=DX*DY
900            CONTINUE
1000        CONTINUE
C           Record high z face
            DO 1200 IX=IXS,IXE
               DO 1100 IY=IYS,IYE
C                 Check that this face is not obscured
                  IF (.NOT.OBINOT(IOB,IX,IY,3)) GOTO 1100
C                 Increment counter and record data
                  NELEMS=NELEMS+1
                  OBSFZN(IX,IY,IZE,3)=NELEMS
                  NSFZCP(NELEMS)=1
                  SFZN(NELEMS,1,1)=3
                  SFZN(NELEMS,1,2)=IX
                  SFZN(NELEMS,1,3)=IY
                  SFZN(NELEMS,1,4)=IZE
                  SFZN(NELEMS,1,5)=3
                  DX=MIN(GX(IX+1),GHI(IOB,1))-MAX(GX(IX),GLO(IOB,1))
                  DY=MIN(GY(IY+1),GHI(IOB,2))-MAX(GY(IY),GLO(IOB,2))
                  ELAREA(NELEMS)=DX*DY
1100           CONTINUE
1200        CONTINUE
C           Record high y face
            DO 1400 IX=IXS,IXE
               DO 1300 IZ=IZS,IZE
C                 Check that this face is not obscured
                  IF (.NOT.OBINOT(IOB,IX,IZ,4)) GOTO 1300
C                 Increment counter and record data
                  NELEMS=NELEMS+1
                  OBSFZN(IX,IYE,IZ,4)=NELEMS
                  NSFZCP(NELEMS)=1
                  SFZN(NELEMS,1,1)=3
                  SFZN(NELEMS,1,2)=IX
                  SFZN(NELEMS,1,3)=IYE
                  SFZN(NELEMS,1,4)=IZ
                  SFZN(NELEMS,1,5)=4
                  DX=MIN(GX(IX+1),GHI(IOB,1))-MAX(GX(IX),GLO(IOB,1))
                  DZ=MIN(GZ(IZ+1),GHI(IOB,3))-MAX(GZ(IZ),GLO(IOB,3))
                  ELAREA(NELEMS)=DX*DZ
1300           CONTINUE
1400        CONTINUE
C           Record low y face
            DO 1600 IX=IXS,IXE
               DO 1500 IZ=IZS,IZE
C                 Check that this face is not obscured
                  IF (.NOT.OBINOT(IOB,IX,IZ,5)) GOTO 1500
C                 Increment counter and record data
                  NELEMS=NELEMS+1
                  OBSFZN(IX,IYS,IZ,5)=NELEMS
                  NSFZCP(NELEMS)=1
                  SFZN(NELEMS,1,1)=3
                  SFZN(NELEMS,1,2)=IX
                  SFZN(NELEMS,1,3)=IYS
                  SFZN(NELEMS,1,4)=IZ
                  SFZN(NELEMS,1,5)=5
                  DX=MIN(GX(IX+1),GHI(IOB,1))-MAX(GX(IX),GLO(IOB,1))
                  DZ=MIN(GZ(IZ+1),GHI(IOB,3))-MAX(GZ(IZ),GLO(IOB,3))
                  ELAREA(NELEMS)=DX*DZ
1500           CONTINUE
1600        CONTINUE
C           Record high x face
            DO 1800 IY=IYS,IYE
               DO 1700 IZ=IZS,IZE
C                 Check that this face is not obscured
                  IF (.NOT.OBINOT(IOB,IY,IZ,6)) GOTO 1700
C                 Increment counter and record data
                  NELEMS=NELEMS+1
                  OBSFZN(IXE,IY,IZ,6)=NELEMS
                  NSFZCP(NELEMS)=1
                  SFZN(NELEMS,1,1)=3
                  SFZN(NELEMS,1,2)=IXE
                  SFZN(NELEMS,1,3)=IY
                  SFZN(NELEMS,1,4)=IZ
                  SFZN(NELEMS,1,5)=6
                  DY=MIN(GY(IY+1),GHI(IOB,2))-MAX(GY(IY),GLO(IOB,2))
                  DZ=MIN(GZ(IZ+1),GHI(IOB,3))-MAX(GZ(IZ),GLO(IOB,3))
                  ELAREA(NELEMS)=DY*DZ
1700           CONTINUE
1800        CONTINUE
         ELSE
C           **** Cylinder ****
C           Record low face, curved surface and high face
C           Determine direction and starting cells
            IDIR=NINT(GHI(IOB,2))
            IX=OBSTXS(IOB)
            IY=OBSTYS(IOB)
            IZ=OBSTZS(IOB)
	    LOW=.FALSE.
	    HIGH=.FALSE.
            IF (IDIR.EQ.1) THEN
C              ** Parallel x axis **
C              Check low face is not obscured
               IF (OBINOT(IOB,IY,IZ,1)) THEN
                  LOW=.TRUE.
                  ISURFL=1
               END IF
C              Check high face is not obscured
               IF (OBINOT(IOB,IY,IZ,6)) THEN
                  HIGH=.TRUE.
                  ISURFH=6
               END IF
            ELSE IF (IDIR.EQ.2) THEN
C              ** Parallel y axis **
C              Check low face is not obscured
               IF (OBINOT(IOB,IX,IZ,5)) THEN
                  LOW=.TRUE.
                  ISURFL=5
               END IF
C              Check high face is not obscured
               IF (OBINOT(IOB,IX,IZ,4)) THEN
                  HIGH=.TRUE.
                  ISURFH=4
               END IF
            ELSE IF (IDIR.EQ.3) THEN
C              ** Parallel z axis **
C              Check low face is not obscured
               IF (OBINOT(IOB,IX,IY,2)) THEN
                  LOW=.TRUE.
                  ISURFL=2
               END IF
C              Check high face is not obscured
               IF (OBINOT(IOB,IX,IY,3)) THEN
                  HIGH=.TRUE.
                  ISURFH=3
               END IF
            END IF
C           Where appropriate  increment counter and record low surface
            IF (LOW) THEN
               NELEMS=NELEMS+1
               OBSFZN(IX,IY,IZ,ISURFL)=NELEMS
               NSFZCP(NELEMS)=1
               SFZN(NELEMS,1,1)=3
               SFZN(NELEMS,1,2)=IX
               SFZN(NELEMS,1,3)=IY
               SFZN(NELEMS,1,4)=IZ
               SFZN(NELEMS,1,5)=ISURFL
               ELAREA(NELEMS)=PI*GHI(IOB,1)*GHI(IOB,1)
            END IF
C           Increment counter and record curved surface information
C           N.B. Curved surfaces cannot be obscured
            IF (IDIR.EQ.1) THEN
               DO 1900 IX=OBSTXS(IOB),OBSTXE(IOB)
                  NELEMS=NELEMS+1
                  OBSFZN(IX,IY,IZ,7)=NELEMS
                  NSFZCP(NELEMS)=1
                  SFZN(NELEMS,1,1)=3
                  SFZN(NELEMS,1,2)=IX
                  SFZN(NELEMS,1,3)=IY
                  SFZN(NELEMS,1,4)=IZ
                  SFZN(NELEMS,1,5)=7
                  DH=MIN(GX(IX+1),GHI(IOB,3))-MAX(GX(IX),GLO(IOB,1))
                  ELAREA(NELEMS)=2.*PI*GHI(IOB,1)*DH
1900           CONTINUE
            ELSE IF (IDIR.EQ.2) THEN
               DO 2000 IY=OBSTYS(IOB),OBSTYE(IOB)
                  NELEMS=NELEMS+1
                  OBSFZN(IX,IY,IZ,7)=NELEMS
                  NSFZCP(NELEMS)=1
                  SFZN(NELEMS,1,1)=3
                  SFZN(NELEMS,1,2)=IX
                  SFZN(NELEMS,1,3)=IY
                  SFZN(NELEMS,1,4)=IZ
                  SFZN(NELEMS,1,5)=7
                  DH=MIN(GY(IY+1),GHI(IOB,3))-MAX(GY(IY),GLO(IOB,2))
                  ELAREA(NELEMS)=2.*PI*GHI(IOB,1)*DH
2000           CONTINUE
            ELSE IF (IDIR.EQ.3) THEN
               DO 2100 IZ=OBSTZS(IOB),OBSTZE(IOB)
                  NELEMS=NELEMS+1
                  OBSFZN(IX,IY,IZ,7)=NELEMS
                  NSFZCP(NELEMS)=1
                  SFZN(NELEMS,1,1)=3
                  SFZN(NELEMS,1,2)=IX
                  SFZN(NELEMS,1,3)=IY
                  SFZN(NELEMS,1,4)=IZ
                  SFZN(NELEMS,1,5)=7
                  DH=MIN(GZ(IZ+1),GHI(IOB,3))-MAX(GZ(IZ),GLO(IOB,3))
                  ELAREA(NELEMS)=2.*PI*GHI(IOB,1)*DH
2100           CONTINUE
            END IF
C           Where appropriate increment counter and record high surface data
            IF (HIGH) THEN
	       IX=OBSTXE(IOB)
	       IY=OBSTYE(IOB)
	       IZ=OBSTYE(IOB)
               NELEMS=NELEMS+1
               OBSFZN(IX,IY,IZ,ISURFH)=NELEMS
               NSFZCP(NELEMS)=1
               SFZN(NELEMS,1,1)=3
               SFZN(NELEMS,1,2)=IX
               SFZN(NELEMS,1,3)=IY
               SFZN(NELEMS,1,4)=IZ
               SFZN(NELEMS,1,5)=ISURFH
               ELAREA(NELEMS)=PI*GHI(IOB,1)*GHI(IOB,1)
            END IF
         END IF
2200  CONTINUE

C     Check that NELEMS has not exceeded maximum permissible value
      IF (NELEMS.GT.MXELEM) THEN
         WRITE (LUSTOT,2300)
2300     FORMAT (' NUMBER OF SURFACE ELEMENTS IS TOO BIG'/
     1           ' INCREASE MXELEM & RECOMPILE THE PROGRAM'/
     2           ' RUN ABORTED')
         STOP
      END IF

C-----------------------------------------------------------------------
C               COUNT AND RECORD THE VOLUME ELEMENTS
C-----------------------------------------------------------------------
C     Initialise the counter
      NELEMV=0
C     Loop round all the cell
      DO 3300 K=1,NGZ-1
         DO 3200 J=1,NGY-1
            DO 3100 I=1,NGX-1
C              Cells with zero volume are not to be counted
               IF (FINVOL(I,J,K).LE.COINC) GOTO 3100
               NELEMV=NELEMV+1
               CLVLZN(I,J,K)=NELEMV
               VLZN(NELEMV,1)=I
               VLZN(NELEMV,2)=J
               VLZN(NELEMV,3)=K
3100        CONTINUE
3200     CONTINUE
3300  CONTINUE

C     Check that NELEMV has not exceeded maximum permissible value
      IF (NELEMV.GT.MXELMV) THEN
         WRITE (LUSTOT,3400)
3400     FORMAT (' NUMBER OF VOLUME ELEMENTS IS TOO BIG'/
     1           ' INCREASE MXELMV & RECOMPILE THE PROGRAM'/
     2           ' RUN ABORTED')
         STOP
      END IF

      RETURN
      END


      SUBROUTINE CRSGRD(NVXFS,NVXFE,NVXRS,NVXRE,SINK,LUSTOT,FINVOL,
     1                  NVMX,NZMX,NOBMX,MXZONE,MXVZON,NCGCX,NCGCY,
     2                  NCGCZ,CGX,CGY,CGZ,NSURF,NZONES,ZNAREA,ACTZON,
     3                  NVOL,NVZONE,ZNVOL,ACTVZN,CRSVOL,VOLZON,MXVZC)
C
C  Routine to determine the coarse grid information and assign each
C  fine grid surface element to a coarse grid surface zone
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**16/04/91
C
C
      include 'comvar.for'
C
      INTEGER NVXFS,NVXFE,NVXRS,NVXRE,NOBMX,LUSTOT,NCGCX,NCGCY,NCGCZ,
     1        CGX(NVMX),CGY(NVMX),CGZ(NZMX),NVMX,NZMX,MXZONE,NZONES,
     2        ACTZON(MXZONE),NSURF,NVOL,NVZONE,ACTVZN(MXVZON),MXVZON,
     3        MXVZC
      LOGICAL SINK(NOBMX)
      REAL ZNAREA(MXZONE),ZNVOL(MXVZON),FINVOL(NVMX,NVMX,NZMX),
     1     CRSVOL(NVMAX,NVMAX,NZMAX),VOLZON(NVMAX,NVMAX,NZMAX,3*MXVZC+1)
      INTEGER IV,IX,IY,IZ,NCGX,NCGY,NCGZ,NSEGFS,NSEGFE,NSEGRS,NSEGRE,
     1        IELEM,ISEG,ICGX,ICGY,ICGZ,ISURF,IOB,NCOMPS,ICOMP,
     2        IC,JC,KC,IF,JF,KF,IVOL,NSURFB
      LOGICAL GXCRS(NVMAX),GYCRS(NVMAX),BOTTOM,RIGHT,TOP,LEFT
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  ELAREA  Array of element areas
C  FINVOL  Array of fine grid volume element volumes
C  MXELM   Maximum number of elements allowed (size of array ELAREA)
C  MXVZC   Maximum number of components a coarse grid volume zone is allowed
C  MXVZON  Maximum number of volume elements allowed (dimension of some arrays)
C  MXZONE  Maximum number of coarse surface zones allowed (size of array ZNAREA)
C  LUSTOT  Logical unit number for standard output
C  NOBMX   Maximum number of obstacles allowed
C  NVMX    Maximum number of vertices allowed
C  NVXFE   Number of the vertex at the end of the floor
C  NVXFS   Number of the vertex at the start of the floor
C  NVXRE   Number of the vertex at the end of the roof
C  NVXRS   Number of the vertex at the start of the roof
C  NZMX    Maximum number of slices allowed
C  SINK    Logical array indicating if each obstacle is a sink or not
C
C**** Arguments - output
C  ACTVZN  Array giving the coarse zone numbers of those volume zones with
C          non-zero volume (the actual zones)
C  ACTZON  Array giving the coarse zone numbers of those zones with non-zero
C          area (the actual zones)
C  CGX     Array giving fine x grid line numbers of each x coarse grid line
C  CGY     Array giving fine y grid line numbers of each y coarse grid line
C  CGZ     Array giving fine z grid line numbers of each z coarse grid line
C  CRSVOL  Array of coarse grid cell volumes
C  NCGCX   Number of coarse grid x cells
C  NCGCY   Number of coarse grid y cells
C  NCGCZ   Number of coarse grid z cells
C  NSURF   Number of surface zones
C  NVOL    Number of volume zones
C  NVZONE  Number of coarse volume zones with non-zero area
C  NZONES  Number of coarse surface zones with non-zero area
C  VOLZON  Array of coarse grid volume zone information
C          VOLZON(I,J,K,1) - the number of components of coarse grid cell I,J,K
C          VOLZON(I,J,K,3N-1) -  x number of fine grid cell which is nth
C                                component of coarse grid cell I,J,K
C          VOLZON(I,J,K,3N)   -  y number of fine grid cell which is nth
C                                component of coarse grid cell I,J,K
C          VOLZON(I,J,K,3N+1) -  z number of fine grid cell which is nth
C                                component of coarse grid cell I,J,K
C  ZNAREA  Array of coarse surface zone areas
C  ZNVOL   Array of coarse volume zone areas
C
C**** Common variables - input
C  CELLOB  Array giving the obstacle numbers in each cell
C  COARSE  Logical array indicating if a vertex is on the coarse grid
C  COARSZ  Logical array indicating if a z grid line is on the coarse grid
C  MXCP    Maximum number of elements allowed to make up a coarse surface zone
C  NELEMS  Number of elements
C  NGX     Number of x grid lines on fine grid
C  NGY     Number of y grid lines on fine grid
C  NGZ     Number of z grid lines on fine grid
C  NV      Number of vertices
C  SEGCEL  Array giving cell numbers of each wall segment
C  SFZN    Array giving fine grid surface element data
C  SFZONE  Array which will hold details of the components of the coarse grid
C          surface zones; on entry the array is entirely zero
C  VXWSEG  Array giving the wall segment numbers immediately after each vertex
C
C**** Common variables - output
C  SFZONE  SFZONE(I,1)=n shows that coarse zone I is made up of n fine grid 
C          elements; SFZONE(I,2-n+1) give the numbers of those elements
C
C**** Local variables
C  BOTTOM  Logical flag showing if a boundary segment is associated with the
C          bottom (y=0) of bounding cuboid
C  IC      Loop counter over coarse grid x cells
C  ICGX    Loop counter over coarse grid x cells
C  ICGY    Loop counter over coarse grid y cells
C  ICGZ    Loop counter over coarse grid z cells
C  ICOMP   Loop counter over coarse surface zone components
C  IELEM   Loop counter over the fine grid surface elements
C          or current component's element number
C  IF      Loop counter over fine grid x cells
C  IOB     Obstacle number
C  ISEG    Segment number
C  ISURF   Coarse grid surface zone number
C  IV      Loop counter over fine grid vertices
C  IVOL    Number of the current volume zone
C  IX      Loop counter over fine grid x grid lines or fine grid x cell number
C  IY      Loop counter over fine grid y grid lines or fine grid y cell number
C  IZ      Loop counter over fine grid z grid lines or fine grid z slice number
C  JC      Loop counter over coarse grid y cells
C  JF      Loop counter over fine grid y cells
C  KC      Loop counter over coarse grid z cells
C  KF      Loop counter over fine grid z cells
C  LEFT    Logical flag showing if a wall segment is associated with the left
C          wall (x=0) of the bounding cuboid
C  NCGX    Number of coarse grid x grid lines
C  NCGY    Number of coarse grid y grid lines
C  NCGZ    Number of coarse grid z grid lines
C  NCOMPS  Number of elements making up a coarse surface zone
C  NSEGFE  Segment number at end of floor
C  NSEGFS  Segment number at start of floor
C  NSEGRE  Segment number at end of roof
C  NSEGRS  Segment number at start of roof
C  NSURFB  Number of coarse grid surface zones on the boundary of the enclosure
C  RIGHT   Logical flag showing if a wall segment is associated with the right
C          wall (x=xmax) of the bounding cuboid
C  TOP     Logical flag showing if a wall segment is associated with the top
C          (y=ymax) of the bounding cuboid
C
C**** Local Arrays
C  GXCRS   Logical array indicating which x fine grid lines are on coarse grid
C  GYCRS   Logical array indicating which y fine grid lines are on coarse grid
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

      DATA GXCRS /NVMAX*.FALSE./
      DATA GYCRS /NVMAX*.FALSE./
	 
C     Determine which x and y grid lines make up coarse grid
      DO 10 IV=1,NV
         IF (COARSE(IV)) THEN
            GXCRS(VXGRID(IV))=.TRUE.
            GYCRS(VYGRID(IV))=.TRUE.
         END IF
10    CONTINUE

C     Always include extreme x and y grid lines in coarse grid
      GXCRS(1)=.TRUE.
      GYCRS(1)=.TRUE.
      GXCRS(NGX)=.TRUE.
      GYCRS(NGY)=.TRUE.

C     Count x coarse grid lines & record their numbers from the fine grid
      NCGX=0
      DO 20 IX=1,NGX
         IF (GXCRS(IX)) THEN
            NCGX=NCGX+1
            CGX(NCGX)=IX
         END IF
20    CONTINUE
C     Count y coarse grid lines & record their numbers from the fine grid
      NCGY=0
      DO 30 IY=1,NGY
         IF (GYCRS(IY)) THEN
            NCGY=NCGY+1
            CGY(NCGY)=IY
         END IF
30    CONTINUE
C     Count z coarse grid lines & record their numbers from the fine grid
      NCGZ=0
      DO 40 IZ=1,NGZ
         IF (COARSZ(IZ)) THEN
            NCGZ=NCGZ+1
            CGZ(NCGZ)=IZ
         END IF
40    CONTINUE

C     Set the number of coarse grid cells & compute number of coarse grid
C     wall surfaces (ie not including obstacles)
      NCGCX=NCGX-1
      NCGCY=NCGY-1
      NCGCZ=NCGZ-1

C     Check that the number of coarse grid surface zones is not too big
      NSURFB=2*(NCGCX*NCGCY+NCGCY*NCGCZ+NCGCZ*NCGCX)
      NSURF=NSURFB+2*NCGCX*NCGCY*NCGCZ
      IF (NSURF.GT.MXZONE) THEN
         WRITE (LUSTOT,50) NSURF,MXZONE
50	  FORMAT (' THERE ARE ',I3,' SURFACE ZONES'/
     1           ' THE MAXIMUM ALLOWED IS ',i3/
     2           ' CHANGE THE VALUE OF MXZN IN comvar.for AND RECOMPILE'/
     3           ' RUN ABORTED')
         STOP
      END IF
C**** Check that the number of coarse grid volume zones is not too big
      IF (NCGCX*NCGCY*NCGCZ.GT.MXVZON) THEN
         WRITE (LUSTOT,60) NCGCX*NCGCY*NCGCZ,MXVZON
60	  FORMAT (' There are ',I3,' volume zones in this geometry'/
     1         ' The maximum currently permitted is ',I3/
     2         ' Change the value of MXVZN in comvar.for and recompile'
     3           //' RUN ABORTED'/)
         STOP
      END IF

C     Set wall segment numbers at start & end of floor & roof
      NSEGFS=1
      NSEGFE=VXWSEG(NVXFE)
      NSEGRS=VXWSEG(NVXRS)
      NSEGRE=VXWSEG(NVXRE)

C     Process each element in turn and allocate to the appropriate
C     coarse grid surface zone
      DO 1000 IELEM=1,NELEMS

         IF (SFZN(IELEM,1,1).EQ.1) THEN

C           ***************************
C           *    Boundary Segment     *
C           ***************************
C           Find slice and segment numbers
            IZ=SFZN(IELEM,1,2)
            ISEG=SFZN(IELEM,1,3)
C           Determine whether this segment is part of the top, left,
C           bottom or right of the bounding cuboid
            IF (ISEG.LT.NSEGFE) THEN
C              On the bottom
               BOTTOM=.TRUE.
               RIGHT=.FALSE.
               TOP=.FALSE.
               LEFT=.FALSE.
            ELSE
C              Not on the bottom
               BOTTOM=.FALSE.
               IF (ISEG.LT.NSEGRE) THEN
C                 On the right
                  RIGHT=.TRUE.
                  TOP=.FALSE.
                  LEFT=.FALSE.
               ELSE
C                 Not on the right
                  RIGHT=.FALSE.
                  IF (ISEG.LT.NSEGRS) THEN
C                    On the top
                     TOP=.TRUE.
                     LEFT=.FALSE.
                  ELSE
C                    Not on the top so must be on the right
                     TOP=.FALSE.
                     LEFT=.TRUE.
                  END IF
               END IF
            END IF

            IF (TOP.OR.BOTTOM) THEN
C              Find which x coarse grid cell this segment is in
               IX=SEGCEL(ISEG,1)
               DO 200 ICGX=1,NCGX-1
                  IF ((IX.GE.CGX(ICGX)).AND.(IX.LT.CGX(ICGX+1)))
     1               GOTO 210
200            CONTINUE
C              Find which z coarse grid segment this segment is in
210            DO 220 ICGZ=1,NCGZ-1
                  IF ((IZ.GE.CGZ(ICGZ)).AND.(IZ.LT.CGZ(ICGZ+1)))
     1               GOTO 230
220            CONTINUE
C              Determine to which coarse grid surface zone this fine grid 
C              element belongs
230            ISURF=NCGCY*NCGCZ+(ICGX-1)*2*(NCGCY+NCGCZ)+2*NCGCY+ICGZ
               IF (TOP) ISURF=ISURF+NCGCZ
            ELSE
C              Find which coarse grid y cell the segment is in
               IY=SEGCEL(ISEG,2)
               DO 240 ICGY=1,NCGY-1
                  IF ((IY.GE.CGY(ICGY)).AND.(IY.LT.CGY(ICGY+1)))
     1               GOTO 250
240            CONTINUE
C              Find which coarse grid z cell the segment is in
250            DO 260 ICGZ=1,NCGZ-1
                  IF ((IZ.GE.CGZ(ICGZ)).AND.(IZ.LT.CGZ(ICGZ+1)))
     1               GOTO 270
260            CONTINUE
C              Determine to which coarse grid surface zone this fine grid
C              element belongs
270            ISURF=(ICGY-1)*NCGCZ+ICGZ
               IF (RIGHT) ISURF=ISURF+NCGCY*NCGCZ+2*NCGCX*(NCGCY+NCGCZ)
            END IF

         ELSE IF (SFZN(IELEM,1,1).EQ.2) THEN

C           ************************
C           *     Cell Surface     *
C           ************************
C           Find coarse grid x and y cell numbers
            IZ=SFZN(IELEM,1,2)
            IX=SFZN(IELEM,1,3)
            IY=SFZN(IELEM,1,4)
            DO 280 ICGX=1,NCGX-1
               IF ((IX.GE.CGX(ICGX)).AND.(IX.LT.CGX(ICGX+1))) GOTO 290
280         CONTINUE
290         DO 300 ICGY=1,NCGY-1
               IF ((IY.GE.CGY(ICGY)).AND.(IY.LT.CGY(ICGY+1))) GOTO 310
300         CONTINUE
C           Determine to which coarse grid surface element this fine gird cell
C           surface belongs
310         ISURF=NCGCY*NCGCZ+(ICGX-1)*2*(NCGCY+NCGCZ)+2*(ICGY-1)+1
            IF (IZ.EQ.NGZ) ISURF=ISURF+1

         ELSE

C           ****************************
C           *     Obstacle Surface     *
C           ****************************
C           Find coarse cell number from the fine cell number
            IX=SFZN(IELEM,1,2)
	    IY=SFZN(IELEM,1,3)
	    IZ=SFZN(IELEM,1,4)
	    IOB=CELLOB(IX,IY,IZ,1)
	    DO 320 ICGX=1,NCGCX
	       IF ((IX.GE.CGX(ICGX)).AND.(IX.LT.CGX(ICGX+1))) GOTO 330
320	    CONTINUE
330         DO 340 ICGY=1,NCGCY
               IF ((IY.GE.CGY(ICGY)).AND.(IY.LT.CGY(ICGY+1))) GOTO 350
340         CONTINUE
350	    DO 360 ICGZ=1,NCGCZ
               IF ((IZ.GE.CGZ(ICGZ)).AND.(IZ.LT.CGZ(ICGZ+1))) GOTO 370
360	    CONTINUE
C           Determine to which coarse grid surface zone this fine grid element
C           belongs
370         ISURF=2*((ICGX-1)*NCGCY*NCGCZ+(ICGY-1)*NCGCZ+ICGZ)
            ISURF=ISURF+NSURFB
            IF (SINK(IOB)) ISURF=ISURF-1

         END IF

C        Record the element as part of appropriate coarse grid surface zone
C        checking first that this does not make too many components
         SFZONE(ISURF,1)=SFZONE(ISURF,1)+1
         NCOMPS=SFZONE(ISURF,1)
         IF (NCOMPS.GT.MXCP) THEN
            WRITE (LUSTOT,380) ISURF
380         FORMAT (' ZONE ',I3,' HAS TOO MANY COMPONENTS '/
     1              ' INCREASE MXCP AND RECOMPILE'/
     2              ' RUN ABORTED ')
            STOP
         END IF
         SFZONE(ISURF,NCOMPS+1)=IELEM

1000  CONTINUE

C**** Calculate the coarse grid surface zone areas
      DO 1200 ISURF=1,NSURF
         ZNAREA(ISURF)=0.
         NCOMPS=SFZONE(ISURF,1)
         IF (NCOMPS.GT.0) THEN
C****       Sum the areas of the components of the zone
            DO 1100 ICOMP=1,NCOMPS
               IELEM=SFZONE(ISURF,ICOMP+1)
               ZNAREA(ISURF)=ZNAREA(ISURF)+ELAREA(IELEM)
1100        CONTINUE
         END IF
1200   CONTINUE

C**** Determine the actual surface zones & how many there are
      NZONES=0
      DO 1300 ISURF=1,NSURF
         IF (ZNAREA(ISURF).GT.0.0) THEN
	    NZONES=NZONES+1
	    ACTZON(NZONES)=ISURF
	  END IF
1300  CONTINUE

C**** Calculate the number of coarse grid volume zones
      NVOL=NCGCX*NCGCY*NCGCZ

C**** Determine the number of components of each coarse grid volume zone
C**** and record the element numbers of the components
      DO 3000 KC=1,NCGCZ
         DO 2900 JC=1,NCGCY
	    DO 2800 IC=1,NCGCX
C****          Coarse volume zone (IC,JC,KC) is number IVOL	    
	       IVOL=(IC-1)*NCGCY*NCGCZ+(JC-1)*NCGCZ+KC
C****          Count up the fine grid components and sum their volumes	       
	       VLZONE(IVOL,1)=0
	       ZNVOL(IVOL)=0.
	       DO 2700 KF=CGZ(KC),CGZ(KC+1)-1
	          DO 2600 JF=CGY(JC),CGY(JC+1)-1
		     DO 2500 IF=CGX(IC),CGX(IC+1)-1
		        IF (FINVOL(IF,JF,KF).EQ.0) GOTO 2500
			VLZONE(IVOL,1)=VLZONE(IVOL,1)+1
			ICOMP=VLZONE(IVOL,1)
C****                   Check that ICOMP is not too big
                        IF (ICOMP.GT.MXVZNC) THEN
			   WRITE (LUSTOT,2100) IVOL,IC,JC,KC
2100			   FORMAT (' VOLUME ZONE ',I3,
     1                             ' IS COARSE GRID CELL ',3(I2,1X)/
     2                             ' IT HAS TOO MANY COMPONENTS'/
     3                             ' INCREASE MXVZNC IN comvar.for',
     4                             ' AND RECOMPILE'/' RUN ABORTED')
                           STOP
			END IF
			VLZONE(IVOL,ICOMP+1)=CLVLZN(IF,JF,KF)
			ZNVOL(IVOL)=ZNVOL(IVOL)+FINVOL(IF,JF,KF)
2500	             CONTINUE
2600              CONTINUE
2700           CONTINUE
2800        CONTINUE
2900     CONTINUE
3000  CONTINUE

C**** Determine the actual volume zones
      NVZONE=0
      DO 3100 IVOL=1,NVOL
         IF (ZNVOL(IVOL).GT.0.) THEN
	    NVZONE=NVZONE+1
	    ACTVZN(NVZONE)=IVOL
	  END IF
3100  CONTINUE	    

C**** Calculate the coarse grid zone volumes
      CALL VOLCRS(NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,FINVOL,NVMAX,NZMAX,
     1            MXVZNC,LUSTOT,CRSVOL,VOLZON)

      RETURN
      END

      SUBROUTINE VOLCAL(NGX,NGY,NGZ,GZ,NZMX,AREA,NVMX,FINVOL)
C
C  Routine to calculate the volumes of the fine grid cells without allowing
C  for the presence of obstacles
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**11/10/90
C
C
      REAL GZ(NZMX),AREA(2,NVMX,NVMX),FINVOL(NVMX,NVMX,NZMX),DZ
      INTEGER NGX,NGY,NGZ,NZMX,NVMX,I,J,K
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  AREA    Array of cell area in xy plane
C  FINVOL  Array to hold the fine grid cell volumes
C  GZ      Array of fine grid z co-ordinates
C  NGX     Number of x fine grid lines
C  NGY     Number of y fine grid lines
C  NGZ     Number of z fine grid lines
C  NVMX    Maximum number of x,y grid lines allowed (dimension of some arrays)
C  NZMX    Maximum number of z grid lines allowed (dimension of some arrays)
C
C**** Arguments - output
C  FINVOL  Array of fine grid cell volumes
C
C**** Local variables
C  DZ      Thickness of a z slice
C  I       Loop counter over x cells
C  J       Loop counter over y cells
C  K       Loop counter over z cells
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Uniformity in z direction means volume is cross sectional
C     area times thickness of cell
      DO 30 K=1,NGZ-1
C        Calculate cell thickness
         DZ=GZ(K+1)-GZ(K)
         DO 20 J=1,NGY-1
            DO 10 I=1,NGX-1
               FINVOL(I,J,K)=DZ*AREA(1,I,J)
10          CONTINUE
20       CONTINUE
30    CONTINUE

      RETURN
      END

      SUBROUTINE VOLMOD(NOB,NOBMX,TYPE,OBSTXS,OBSTXE,OBSTYS,OBSTYE,
     1                  OBSTZS,OBSTZE,NVMX,NZMX,GX,GY,GZ,GLO,GHI,PI,
     2                  FINVOL)
C
C  Routine to modify the fine grid cell volumes to account for the presence
C  of obstacles
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**11/10/90
C
C
      REAL GX(NVMX),GY(NVMX),GZ(NZMX),GLO(NOBMX,3),GHI(NOBMX,3),PI,
     1     FINVOL(NVMX,NVMX,NZMX)
      INTEGER  NOB,NOBMX,OBSTXS(NOBMX),OBSTXE(NOBMX),OBSTYS(NOBMX),
     1         OBSTYE(NOBMX),OBSTZS(NOBMX),OBSTZE(NOBMX),NVMX,NZMX
      CHARACTER*1 TYPE(NOBMX)
      REAL DX,DY,DZ,VOLOB,AREAOB
      INTEGER I,J,K,IOB
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  FINVOL  Array of (unmodified) fine grid cell volumes
C  GHI     Array of obstacle high information
C  GLO     Array of obstacle low co-ordinates
C  GX      Array of fine grid x co-ordinates
C  GY      Array of fine grid y co-ordinates
C  GZ      Array of fine grid z co-ordinates
C  NOB     Number of obstacles
C  NOBMX   Maximum number of obstacles allowed (dimension of some arrays)
C  NVMX    Maximum number of x,y fine grid lines (dimension of some arrays)
C  NZMX    Maximum number of z fine grid lines (dimension of some arrays)
C  OBSTXE  Array giving ending x cell number of each obstacle
C  OBSTXS  Array giving starting x cell number of each obstacle
C  OBSTYE  Array giving ending y cell number of each obstacle
C  OBSTYS  Array giving starting y cell number of each obstacle
C  OBSTZE  Array giving ending z cell number of each obstacle
C  OBSTZS  Array giving starting z cell number of each obstacle
C  PI      Pi
C  TYPE    Array giving type of each obstacle (box or cylinder)
C
C**** Arguments - output
C  FINVOL  Array of fine grid cell volumes modified where obstacles are present
C  
C**** Local Variables
C  AREAOB  Cross sectional area of obstacle in current cell
C  DX      x length of obstacle in current cell
C  DY      y length of obstacle in current cell
C  DZ      z length of obstacle in current cell
C  I       Loop counter over x cells
C  IOB     Number of current obstacle
C  J       Loop counter over y cells
C  K       Loop counter over z cells
C  VOLOB   Volume of obstacle in current cell
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Loop over obstacles
      DO 70 IOB=1,NOB

         IF (TYPE(IOB).EQ.'B') THEN
C           Obstacle is a box

            DO 30 K=OBSTZS(IOB),OBSTZE(IOB)
               DZ=MIN(GZ(K+1),GHI(IOB,3))-MAX(GZ(K),GLO(IOB,3))
               DO 20 J=OBSTYS(IOB),OBSTYE(IOB)
                  DY=MIN(GY(J+1),GHI(IOB,2))-MAX(GY(J),GLO(IOB,2))
                  DO 10 I=OBSTXS(IOB),OBSTXE(IOB)
                     DX=MIN(GX(I+1),GHI(IOB,1))-MAX(GX(I),GLO(IOB,1))
                     VOLOB=DX*DY*DZ
                     FINVOL(I,J,K)=FINVOL(I,J,K)-VOLOB
                     IF (FINVOL(I,J,K).LT.0.) FINVOL(I,J,K)=0.
10                CONTINUE
20             CONTINUE
30          CONTINUE

         ELSE
C           Obstacle is a cylinder

C           Compute cross sectional area of the cylinder
            AREAOB=PI*GHI(IOB,1)*GHI(IOB,1)
C           Process cylinder in manner appropriate to its direction
            IF (GHI(IOB,2).EQ.1) THEN
C              Cylinder parallel to x axis
               DO 40 I=OBSTXS(IOB),OBSTXE(IOB)
                  DX=MIN(GX(I+1),GHI(IOB,3))-MAX(GX(I),GLO(IOB,1))
                  VOLOB=AREAOB*DX
                  J=OBSTYS(IOB)
                  K=OBSTZS(IOB)
                  FINVOL(I,J,K)=FINVOL(I,J,K)-VOLOB
                  IF (FINVOL(I,J,K).LT.0.) FINVOL(I,J,K)=0.
40             CONTINUE
            ELSE IF (GHI(IOB,2).EQ.2) THEN
C              Cylinder parallel to y axis
               DO 50 J=OBSTYS(IOB),OBSTYE(IOB)
                  DY=MIN(GY(J+1),GHI(IOB,3))-MAX(GY(J),GLO(IOB,2))
                  VOLOB=AREAOB*DY
                  I=OBSTXS(IOB)
                  K=OBSTZS(IOB)
                  FINVOL(I,J,K)=FINVOL(I,J,K)-VOLOB
                  IF (FINVOL(I,J,K).LT.0.) FINVOL(I,J,K)=0.
50             CONTINUE
            ELSE
C              Cylinder parallel to z axis
               DO 60 K=OBSTZS(IOB),OBSTZE(IOB)
                  DZ=MIN(GZ(K+1),GHI(IOB,3))-MAX(GZ(K),GLO(IOB,3))
                  VOLOB=AREAOB*DZ
                  I=OBSTXS(IOB)
                  J=OBSTYS(IOB)
                  FINVOL(I,J,K)=FINVOL(I,J,K)-VOLOB
                  IF (FINVOL(I,J,K).LT.0.) FINVOL(I,J,K)=0.
60             CONTINUE
            END IF

         END IF

70    CONTINUE

      RETURN
      END

      SUBROUTINE VOLCRS(NCGCX,NCGCY,NCGCZ,CGX,CGY,CGZ,FINVOL,NVMX,NZMX,
     1                  MXVZNC,LUSTOT,CRSVOL,VOLZON)
C
C  Routine to assign each of the fine grid cells to a coarse volume cell (zone)
C  and to calculate the volumes of the coarse volume cells (zones)
C  
C*****VERSION 1.0********D.A.Lawson, Maths Dept, Coventry Poly.
C
C*****LAST MODIFICATION**12/10/90
C
C
      REAL FINVOL(NVMX,NVMX,NZMX),CRSVOL(NVMX,NVMX,NZMX),
     1     VOLZON(NVMX,NVMX,NZMX,3*MXVZNC+1)
      INTEGER NCGCX,NCGCY,NCGCZ,CGX(NVMX),CGY(NVMX),CGZ(NZMX),
     1        NVMX,NZMX,MXVZNC,LUSTOT
      INTEGER IC,JC,KC,IF,JF,KF
C
C******* List of variables, arrays and subprograms used *******
C
C**** Arguments - input
C  CGX      Array giving fine grid line numbers of coarse x grid lines
C  CGY      Array giving fine grid line numbers of coarse y grid lines
C  CGZ      Array giving fine grid line numbers of coarse z grid lines
C  CRSVOL   Array to hold the volumes of the coarse grid cells (volume zones)
C  FINVOL   Array holding the volumes of the fine grid cells
C  LUSTOT   Logical unit for standard output (for error message)
C  MXVZNC   Maximum number of components a coarse grid volume zone may have
C  NCGCX    Number of coarse grid x cells
C  NCGCY    Number of coarse grid y cells
C  NCGCZ    Number of coarse grid z cells
C  NVMX     Maximum number of vertices allowed (dimension of some arrays)
C  NZMX     Maximum number of z-slices alliwed (dimension of some arrays)
C  VOLZON   Array to hold the details of the coarse grid volume zones
C
C**** Arguments - output
C  CRSVOL  Array holding the coarse grid cell (zone) volumes 
C  VOLZON  Array holding the details of the coarse grid volume zones
C          VOLZON(I,J,K,1) - the number of components of coarse grid cell I,J,K
C          VOLZON(I,J,K,3N-1) - x number of fine grid cell which is nth
C                               component of coarse grid cell I,J,K
C          VOLZON(I,J,K,3N)   - y number of fine grid cell which is nth
C                               component of coarse grid cell I,J,K
C          VOLZON(I,J,K,3N+1) - z number of fine grid cell which is nth
C                               component of coarse grid cell I,J,K
C
C**** Local variables
C  IC      Loop counter over coarse grid x cells
C  ICOMP   Number of components, to date, of current volume zone
C  IF      Loop counter over fine grid xcells
C  JC      Loop counter over coarse grid y cells
C  JF      Loop counter over fine grid y cells
C  KC      Loop counter over coarse grid z cells
C  KF      Loop counter over fine grid z cells
C  NCOMPS  Number of components of current volume zone
C
C**** Local Arrays
C  NONE
C
C**** Functions and Subroutines
C  NONE
C
C******************************************************************************

C     Loop over the coarse grid cells (volume zones)
      DO 60 KC=1,NCGCZ
         DO 50 JC=1,NCGCY
            DO 40 IC=1,NCGCX
C              Calculate the number of components of this coarse grid cell
               NCOMPS=(CGX(IC+1)-CGX(IC))*(CGY(JC+1)-CGY(JC))
     1                *(CGZ(KC+1)-CGZ(KC))
C              Check that NCOMPS is not too big
               IF (NCOMPS.GT.MXVZNC) THEN
                  WRITE (LUSTOT,5) I,J,K,NCOMPS
5                 FORMAT (' COARSE GRID CELL ',I3,',',I3,',',I3,' HAS ',
     1                    I3,' COMPONENTS '/
     2                    ' THIS IS TOO MANY'/
     3                    ' INCREASE MXVZNC IN comvar.for AND RECOMPILE'/
     4                    ' RUN ABORTED')
                  STOP
               END IF
C              Set number of components for this cell
               VOLZON(IC,JC,KC,1)=NCOMPS
C              Initialise current component number
               ICOMP=0
C              Load details of each component into  VOLZON
               DO 30 KF=CGZ(KC),CGZ(KC+1)-1
                  DO 20 JF=CGY(JC),CGY(JC+1)-1
                     DO 10 IF=CGX(IC),CGX(IC+1)-1
C                       Update current component number
                        ICOMP=ICOMP+1
                        VOLZON(IC,JC,KC,3*ICOMP-1)=IF
                        VOLZON(IC,JC,KC,3*ICOMP)=JF
                        VOLZON(IC,JC,KC,3*ICOMP+1)=KF
C                       Add volume of current component on to CRSVOL
                        CRSVOL(IC,JC,KC)=CRSVOL(IC,JC,KC)
     1                                   +FINVOL(IF,JF,KF)
10                   CONTINUE
20                CONTINUE
30             CONTINUE
40          CONTINUE
50       CONTINUE
60    CONTINUE

      RETURN
      END


