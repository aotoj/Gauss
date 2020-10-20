 program gauss

  implicit none

  real, dimension(3,3) :: A
  real, dimension(3) :: b
  real, dimension(3) :: c

  integer :: i,j

  ! initialize matrix A and vector b
  A(1,:) = (/2., 3., -1./)
  A(2,:) = (/4., 7., 1./)
  A(3,:) = (/7., 10., -4./)

  b = (/1., 3., 4./)
  c = (/0., 0., 0./)
  ! print augmented matrix
  do i = 1, 3           ! i is row
    print *, A(i,:), "|", b(i)
  end do

  print *, ""    ! print a blank line
  print *, "Gaussian elimination........"
  ! gaussian elimination
  do j = 1, 2           ! j is column
    do i = j+1, 3       ! i is row
       ! STUDENTS: PLEASE FINISH THIS GAUSSIAN ELIMINATION
       A(i,:) = -A(i,j)/A(j,j) * A(j,:) + A(i,:)
       !b(i) = -A(i,j)/A(j,j) * b(j) + b(i)
    end do
  end do
  b(2) = -2 * b(1) + b(2)
  b(3) = -3.5 * b(1) + b(3)
  b(3) = .5 * b(2) + b(3)

  ! print augmented matrix again
  ! this should be an echelon form (or triangular form)
  !L1- 2, 3,-1    |  1
  !L2- 0, 1, 3    |  1
  !L3- 0, 0, 1    |  1
  print *, "***********************"
  do i = 1, 3
  
    print *, A(i,:), "|", b(i)
  end do

  print *, ""    ! print a blank line
  print *, "back subs......"

  ! doing back substitution
  do i = 1,2! FINISH THIS LOOP          ! j is column
    do j = i+1,3! FINISH THIS LOOP        ! i is row
       A(i,:) = -A(i,j)/A(j,j) * A(j,:) + A(i,:)
       !b(i) = -A(i,j)/A(j,j) * b(j) + b(i)
      ! STUDENTS: PLEASE FINISH THIS BACK SUBSTITUTION
      ! HINT: THIS PART IS PRETTY MUCH SIMILAR WITH GAUSSIAN ELIM. PART.
    end do
 end do
 b(2) = -3 * b(3) + b(2)
 b(1) = 1 * b(3) + b(1)
 b(1) = -3 * b(2) + b(1)
 

  ! print the results
  !L1- 2, 0, 0  |  8
  !L2- 0, 1, 0  | -2
  !L3- 0, 0, 1  |  1
  print *, "***********************"
  do i = 1, 3
    print *, A(i,:), "|", b(i)
  end do

  print *, "The solutions are:"
  do i = 1, 3
    c(i) = b(i)/A(i,i)
    print *, c(i)
  end do
  ! STUDENTS: PLEASE CALCULATE A SOLUTION VECTOR, AND PRINT TO THE SCREEN.

end program gauss
