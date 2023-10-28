program pascals

   implicit none
   integer :: i, j, m, k = 1

   do i = 1, 10
      k = 1
      do j = 1, i
         write (*, '(I12)', advance='NO') k
         k = k*(i - j)/j
      end do
      write (*, *)
   end do

end program pascals
