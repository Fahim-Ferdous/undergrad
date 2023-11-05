program array
   implicit none
   integer :: arr(7) = [1, 3, 2, -5, -2, 1, 5]
   integer :: do_sum, i, j
   integer :: res, has_
   has_ = 0
   do i = 1, (size(arr, dim=1) - 1)
      do j = i + 1, (size(arr, dim=1))
         res = do_sum(arr, i, j)
         if (res .eq. 0) then
            write (*, *) 1
            call EXIT(0)
         end if
      end do
   end do
   write (*, *) 0
end program

function do_sum(arr, m, n)
   implicit none
   integer, INTENT(IN) :: arr(7), m, n
   integer :: do_sum
   integer ::  i, s
   s = 0

   do i = m, n
      s = s + arr(i)
   end do
   do_sum = s

end function do_sum
