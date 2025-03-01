      Program prgm_02_02
!
!     This program reads a file name from the command line, opens that
!     file, and loads a packed form of a symmetric matrix. Then, the packed
!     matrix is expanded assuming a column-wise lower-triangle packed form
!     and printed. Finally, the packed matrix is expanded as a column-wise
!     upper-triangle packed form and printed.
!
!     The input file is expected to have the leading dimension (an integer
!     NDim) of the matrix on the first line. The next (NDim*(NDim+1))/2
!     lines each have one real number each given.
!
!
      Implicit None
      Integer,Parameter::IIn=10
      Integer::IError,NDim,i,j
      Real,Dimension(:),Allocatable::Array_Input
      Real,Dimension(:,:),Allocatable::Matrix
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
!
! *************************************************************************

      do i=1,(NDim*(NDim+1))/2
            Read(IIn,*) Array_Input(i)
          endDo

! WRITE CODE HERE TO READ THE ARRAY ELEMENTS FROM THE INPUT FILE.
! *************************************************************************
!
!
!     Convert Array_Input to Matrix and print the matrix.
!
      Write(*,*)' The matrix loaded (column-wise) lower-tri packed:'
      Call SymmetricPacked2Matrix_LowerPac(NDim,Array_Input,Matrix)
      Call Print_Matrix_Full_Real(Matrix,NDim,NDim)
      Write(*,*)' The matrix loaded (column-wise) upper-tri packed:'
      Call SymmetricPacked2Matrix_UpperPac(NDim,Array_Input,Matrix)
      Call Print_Matrix_Full_Real(Matrix,NDim,NDim)
!
      End Program prgm_02_02


      Subroutine SymmetricPacked2Matrix_LowerPac(N,ArrayIn,AMatOut)
!
!     This subroutine accepts an array, ArrayIn, that is (N*(N+1))/2 long.
!     It then converts that form to the N-by-N matrix AMatOut taking
!     ArrayIn to be in lower-packed storage form. Note: The storage mode
!     also assumes the lower-packed storage is packed by columns.
!
      Implicit None
      Integer,Intent(In)::N
      Real,Dimension((N*(N+1))/2),Intent(In)::ArrayIn
      Real,Dimension(N,N),Intent(Out)::AMatOut
!
      Integer::i,j,k
!
!     Loop through the elements of AMatOut and fill them appropriately from
!     Array_Input.
!
!
! *************************************************************************
      k =1
      do i=1, N
        do j=i, N
          AMatOut(j,i) = ArrayIn(k)
          AMatOut(i,j) = AMatOut(j,i)
          k = k + 1
        endDo
      endDo
! WRITE CODE HERE TO UNPACK ARRYIN INTO AMATOUT.
! *************************************************************************
!
!
      Return
      End Subroutine SymmetricPacked2Matrix_LowerPac


      Subroutine SymmetricPacked2Matrix_UpperPac(N,ArrayIn,AMatOut)
!
!     This subroutine accepts an array, ArrayIn, that is (N*(N+1))/2 long.
!     It then converts that form to the N-by-N matrix AMatOut taking
!     ArrayIn to be in upper-packed storage form. Note: The storage mode
!     also assumes the upper-packed storage is packed by columns.
!
      Implicit None
      Integer,Intent(In)::N
      Real,Dimension((N*(N+1))/2),Intent(In)::ArrayIn
      Real,Dimension(N,N),Intent(Out)::AMatOut
!
      Integer::i,j,k
!
!     Loop through the elements of AMatOut and fill them appropriately from
!     Array_Input.
!
!
! *************************************************************************
      k =1
      do i=1, N
        do j=1,i
          AMatOut(j,i) = ArrayIn(k)
          AMatOut(i,j) = AMatOut(j,i)
          k = k + 1
        endDo
      endDo
! WRITE CODE HERE TO UNPACK ARRYIN INTO AMATOUT.
! *************************************************************************
!
!
      Return
      End Subroutine SymmetricPacked2Matrix_UpperPac

      Subroutine Print_Matrix_Full_Real(Matrix,M,N)
!
!     This subroutine prints the M-by-N matrix, Matrix, to the screen.
!
      Implicit None
      Integer,Intent(In)::M,N
      Real,Dimension(M,N),Intent(In)::Matrix
!      
!     Local variables
      Integer,Parameter::IOut=6,NColumns=5
      Integer::i,j,IFirst,ILast
!
1000 Format(1x,A)
2000 Format(5x,5(7x,I7))
2010 Format(1x,I7,5F14.6)
!
      do IFirst = 1,N,NColumns
        ILast = MIN(IFirst+NColumns-1,N)
        Write(IOut,2000) (i,i=IFirst,ILast)
        do i=1,M
          Write(IOut,2010)i,(Matrix(i,j),j=IFirst,ILast)
        endDo
      endDo

      Return
      End Subroutine Print_Matrix_Full_Real