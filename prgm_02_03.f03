      Program prgm_02_03
!
!     This program reads a file name from the command line, opens that
!     file, and loads a packed form of a symmetric matrix. Then, the packed
!     matrix is expanded assuming a column-wise lower-triangle form and
!     printed. Finally, the matrix is diagonalized using the LAPack routine
!     SSPEV. The eigenvalues and eigenvectors are printed.
!
!     The input file is expected to have the leading dimension (an integer
!     NDim) of the matrix on the first line. The next (NDim*(NDim+1))/2
!     lines each have one real number each given.
!
!
      Implicit None
      Integer::IIn=10,IError,NDim,i,j
      Double Precision,Dimension(:),Allocatable::Array_Input,EVals,Temp_Vector
      Double Precision,Dimension(:,:),Allocatable::Matrix,EVecs,Temp_Matrix
      Character(Len=256)::FileName
!
!     Begin by reading the input file name from the command line. Then,
!     open the file and read the input array, Array_Input.
!
      Call Get_Command_Argument(1,FileName)
      Open(Unit=IIn,File=TRIM(FileName),Status='OLD',IOStat=IError)
      If(IError.ne.0) then
        Write(*,*)' Error opening input file.'
        STOP
      endIf
      Read(IIn,*) NDim
      Allocate(Array_Input((NDim*(NDim+1))/2),Matrix(NDim,NDim))
      Allocate(EVals(NDim),EVecs(NDim,NDim),Temp_Vector(3*NDim))
      Allocate(Temp_Matrix(NDim,NDim))
!
! *************************************************************************
!
      do i=1,(NDim*(NDim+1))/2
            Read(IIn,*) Array_Input(i)
          endDo
! WRITE CODE HERE TO READ THE ARRAY ELEMENTS FROM THE INPUT FILE.
! *************************************************************************
!
      Close(Unit=IIn)
!
!     Convert Array_Input to Matrix and print the matrix.
!
      Write(*,*)' The matrix loaded (column) lower-triangle packed:'
      Call SymmetricPacked2Matrix_LowerPac(NDim,Array_Input,Matrix)
      Call Print_Matrix_Full_Real(Matrix,NDim,NDim)
      Call DSPEV('V','L',NDim,Array_Input,EVals,EVecs,NDim,  &
        Temp_Vector,IError)
      If(IError.ne.0) then
        Write(*,*)' Failure in DSPEV.'
        STOP
      endIf
      Write(*,*)' EVals:'
      Call Print_Matrix_Full_Real(RESHAPE(EVals,(/1,NDim/)),1,NDim)
      Write(*,*)' EVecs:'
      Call Print_Matrix_Full_Real(EVecs,NDim,NDim)
!
      End Program prgm_02_03
!
      Subroutine SymmetricPacked2Matrix_LowerPac(N,ArrayIn,AMatOut)
!
!     This subroutine accepts an array, ArrayIn, that is (N*(N+1))/2 long.
!     It then converts that form to the N-by-N matrix AMatOut taking
!     ArrayIn to be in lower-packed storage form. Note: The storage mode
!     also assumes the lower-packed storage is packed by columns.
!
      Implicit None
      Integer,Intent(In)::N
      Double Precision,Dimension((N*(N+1))/2),Intent(In)::ArrayIn
      Double Precision,Dimension(N,N),Intent(Out)::AMatOut
!
      Integer::i,j,k
!
!     Loop through the elements of AMatOut and fill them appropriately from
!     Array_Input.
!
      k = 1
      do i=1,N
        do j=i,N
          AMatOut(j,i) = ArrayIn(k)
          AMatOut(i,j) = AMatOut(j,i)
          k = k + 1
        endDo
      endDo
!
      Return
      End Subroutine SymmetricPacked2Matrix_LowerPac
!
!
      Subroutine Print_Matrix_Full_Real(AMat,M,N)
!
!     This subroutine prints the M-by-N matrix AMat to the screen.
!
      Implicit None
      Integer,Intent(In)::M,N
      Double Precision,Dimension(M,N),Intent(In)::AMat
!
!     Local variables.
      integer,parameter::IOut=6,NColumns=5
      Integer::i,j,IFirst,ILast
!
1000  Format(1X,A)
2000  Format(5X,5(7x,I7))
2010  Format(1X,I7,5F14.6)
!
      do IFirst = 1,N,NColumns
        ILast = MIN(IFirst+NColumns-1,N)
        Write(IOut,2000) (i,i=IFirst,ILast)
        do i=1,M
          Write(IOut,2010) i,(AMat(i,j),j=IFirst,ILast)
        endDo
      endDo

      Return
      End Subroutine Print_Matrix_Full_Real