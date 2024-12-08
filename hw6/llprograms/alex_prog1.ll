define i64 @program(i64 %argc, i8** %arcv) {
  %1 = add i64 5, 9
  %2 = add i64 %1, 3
  %4 = icmp sle i64 %1, %2
  ret i64 %4
}