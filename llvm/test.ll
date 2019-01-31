@first = global half 6.0 
@second = global fp128 8.9 
define half @add_two_integer(i32 a , i32 b) {
entry:
%sum = udiv i32 %a %b
ret i32 sum
}
