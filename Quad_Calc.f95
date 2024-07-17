program math
implicit none
real::x,y,p,q,r,r1,r2,disc
character::num
do while( 1 > 0)
print*,
print*,'Enter 1st number : '
read*, x
print*,'Enter 2st number : '
read*, y
print*, 'a for add, b for sub, c for mul, d for div : '
read*, num
        if (num=='a') then
               print*,x+y
        elseif (num=='b') then
               print*,x-y
        elseif (num=='c') then
               print*,x*y
        elseif (num=='d') then
               print*,x/y
        else 
               print*, 'error.......................'
               end if
print*,'---------------------------------------'
print*,
print*, 'Enter quadratic equation co-efficients as p , q , r [ without the comma(,) ]'
read*,p,q,r
disc = (q**2) - 4*p*r
r1 = -q + (sqrt(disc))/2*p
r2 = -q - (sqrt(disc))/2*p
if (disc >= 0) then
        r1 = (-q + sqrt(disc)) / (2 * p)
        r2 = (-q - sqrt(disc)) / (2 * p)
        print*, 'The roots of the equation are:'
        print*, 'r1 = ', r1
        print*, 'r2 = ', r2
    else
        print*, 'The equation has complex roots.'
    end if
    end do
end
