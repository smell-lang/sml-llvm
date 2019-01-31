@first = global i32 6 
@second = global i32 7 
define void @add_two_integer(i32 a , i32 b) {
entry:
%sum = add i32 %a %b
}
